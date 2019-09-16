# Loading package(s)
library(tidyverse)
library(ggplot2)
library(tibble)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
library(magrittr)
library(modelr)
library(janitor)
library(leaps)
library(glmnet)
library(pls)
library(GGally)
library(class)
library(boot)
library(corrplot)

#Unicorns2 <- read_rds("Unicorns2.rds")
Updated3 <- read_rds("Updated3.rds") # an updated version of Unicorns2 data set with revenue information

Updated4 <- read_rds("Updated4.rds") # variant of Updated3 with some irrelevant predictors removed

set.seed(667)
  

#range(Updated4$valuation_b)

ggplot(aes(x=factor(0), y = valuation_b), data = Updated4) +
  geom_boxplot() + 
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank())

ggplot(aes(x=factor(0), y = funding_b), data = Updated4) +
  geom_boxplot() + 
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank())

ggplot(aes(x=factor(0), y = revenue_b), data = Updated4) +
  geom_boxplot() + 
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank())

ggplot(aes(x=factor(0), y = acquistions), data = Updated4) +
  geom_boxplot() + 
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank())

#ggpairs(Updated4)

ggplot(data = Updated3) + 
  geom_point(mapping = aes(x = acquistions, y = valuation_b, color = industry) ) +
  ggtitle("Acquistions vs valuation") 

ggplot(data = Updated3) + 
  geom_point(mapping = aes(x = revenue_b, y = valuation_b, color = industry) ) +
  ggtitle("Annual revenue vs valuation") 

ggplot(data = Updated3) + 
  geom_point(mapping = aes(x = founder_track_record, y = valuation_b, color = industry) ) +
  ggtitle("Founder track record vs valuation") 

ggplot(data = Updated3) + 
  geom_point(mapping = aes(x = funding_b, y = valuation_b, color = industry) ) +
  ggtitle("Total funding vs valuation") 

#ggpairs(Updated4)
Updated4b <- Updated4 %>% select(-gender)
cor(Updated4b)


u_validation <- tibble(train = Updated4 %>% sample_frac(0.60) %>% list(),
                       test  = Updated4 %>% dplyr::setdiff(train) %>% list())

#Updated4 %>% setdiff(u_validation %>% pluck("train", 1))

#u_validation %>% pluck("test", 1)

utrain <- u_validation %>%
  unnest(train) %>%
  as.data.frame()

utest <- u_validation %>%
  unnest(test) %>%
  as.data.frame()

#utest <- u_validation %>%
# pluck("test", 1) %>%
#as.data.frame()

results <- regsubsets(valuation_b ~., data = Updated4, nvmax = 6) %>%
  summary()

results # results of regression subset selection with exhaustive search for best predictors to predict valuation response variable

tibble(num_pred = 1:6,
       rss = results$rss,
       adjr2 = results$adjr2,
       bic = results$bic) %>%
  gather(key = "statistic", value = value, -num_pred)

#fmla1 <- "valuation_b ~ revenue_b + funding_b + acquistions + investors + rounds + founders + age"

fmla1 <- "valuation_b ~ revenue_b + funding_b   + rounds + growth_years + age"

lm1 <- lm(formula = fmla1, data = utrain)
lm1_preds <- predict(lm1, utest)
lm1_MSE <- mean(lm1_preds - utest$valuation_b)^2
lm1_MSE # test MSE of first linear model with formula: "valuation_b ~ revenue_b + funding_b  + rounds + growth_years + age"


utrain_boot <- utrain %>% 
  bootstrap(1000, id = "boot_id") %>% 
  mutate(model_fit = map(strap, ~ lm(formula = fmla1, data = .x)),
         mod_tidy  = map(model_fit, broom::tidy)) # need to map model_fit of each strap to test data  

#utrain_boot %>% 
#  unnest(mod_tidy) %>%
# group_by(term) %>% 
#select(term, estimate) %>% 
#skimr::skim() 

utrain_boot %>%
  unnest(mod_tidy) %>% 
  group_by(term) %>% 
  summarise(boot_est = mean(estimate),
            est_se   = sd(estimate))

utrain_boot %>% 
  unnest(mod_tidy) %>%
  ggplot(aes(x = "", y = estimate)) +
  geom_boxplot() +
  facet_wrap(. ~ term, scale = "free_x") +
  coord_flip()

#fit model onto each bootstrap resample, calculate MSE then can calculate average MSE among all resamples as well as standard deviation of the MSE to get est_se estimate for MSE precision 

lambdas <- 10^seq(10, -2, length=100)

y <- u_validation %>% unnest(train) %>% pull(valuation_b)

x2 <- u_validation %>% 
  unnest(train) %>%
  dplyr::select(-valuation_b) %>% 
  data.matrix()

x_test2 <- u_validation %>% 
  unnest(test) %>%
  dplyr::select(-valuation_b) %>% 
  data.matrix()

u_lasso <- cv.glmnet(x2, y, alpha = 1, lambda = lambdas, nfolds = 10)

#u_lasso$lambda.1se # 
#u_lasso$lambda.min # 

coef(u_lasso, u_lasso$lambda.1se) #results of lasso regression subset selection 

u_lasso_pred <- predict(u_lasso, s = u_lasso$lambda.1se, newx = x_test2)

mean((u_lasso_pred-utest$valuation_b)^2) #test MSE of lasso regression model

######

fmla2 <- "valuation_b ~ acquistions + funding_b + revenue_b"

lm2 <- lm(formula = fmla2 , data = utrain) #perform linear least-squares regression on the predictors from the lasso regression's subset selection 
lm2_preds <- predict(lm2, utest)
lm2_MSE <- mean(lm2_preds - utest$valuation_b)^2
lm2_MSE # test MSE of second linear model with formula: "valuation_b ~ acquistions + funding_b + revenue_b"


fmla3 <- "valuation_b ~ acquistions + funding_b : revenue_b" # MSE: 1.879463e-06

fmla3b <- "valuation_b ~ age_prop : acquistions + funding_b : revenue_b" #2.64775e-05

lm3 <- lm(formula = fmla3 , data = utrain) 
lm3_preds <- predict(lm3, utest)
lm3_MSE <- mean(lm3_preds - utest$valuation_b)^2
lm3_MSE  # test MSE of third linear model with formula: "valuation_b ~ acquistions + funding_b : revenue_b" 

lm3 <- lm(formula = fmla3b , data = utrain) 
lm3_preds <- predict(lm3, utest)
lm3_MSE <- mean(lm3_preds - utest$valuation_b)^2
lm3_MSE # test MSE of fourth linear model with formula: "valuation_b ~ acquistions + funding_b : revenue_b" 


#lm4 <- lm(formula = valuation_b ~ funding_b + revenue_b , data = utrain) 
fmla4 <- "valuation_b ~ funding_b + revenue_b"


# 10-fold cross-validation of linear models 
u_10fold1 <- Updated4 %>% 
  crossv_kfold(10, id = "fold") 

u_10fold1 %>% 
  mutate(model_fit = map2(fmla1, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse), 
         average_mse = mean(fold_mse)) 

u_10fold1 %>% 
  mutate(model_fit = map2(fmla2, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse), 
         average_mse = mean(fold_mse)) 

u_10fold1 %>% 
  mutate(model_fit = map2(fmla3, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse), 
         average_mse = mean(fold_mse)) 

u_10fold1 %>% 
  mutate(model_fit = map2(fmla3b, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse), 
         average_mse = mean(fold_mse)) 

u_10fold1 %>% 
  mutate(model_fit = map2(fmla4, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse), 
         average_mse = mean(fold_mse))

sv <- function(data1){
    tibble(train = data1 %>% as_tibble() %>% sample_frac(0.60) %>% list(),
    test  = data1 %>% as_tibble() %>% dplyr::setdiff(train) %>%
    list())
}
unnest_test <- function(data1){
    data1 %>% as_tibble() %>% unnest(test)
}

Updated4 %>%
    bootstrap(1000, id = "boot_id") %>%
    mutate(strap_validation = map(strap, ~sv(.x)),
        model_fit = map(strap_validation, ~ lm(formula = fmla1, data = .x %>%
            as_tibble() %>% unnest(train))),
        strap_test = map(strap_validation, ~unnest_test(.x)),
        strap_mse = map2_dbl(model_fit, strap_test, mse),
        average_mse = mean(strap_mse),
        est_se = sd(strap_mse)) %>%
        select(strap_mse, average_mse, est_se)

Updated4 %>%
    bootstrap(1000, id = "boot_id") %>%
    mutate(strap_validation = map(strap, ~sv(.x)),
        model_fit = map(strap_validation, ~ lm(formula = fmla2, data = .x %>%
            as_tibble() %>% unnest(train))),
        strap_test = map(strap_validation, ~unnest_test(.x)),
        strap_mse = map2_dbl(model_fit, strap_test, mse),
        average_mse = mean(strap_mse),
        est_se = sd(strap_mse)) %>%
        select(strap_mse, average_mse, est_se)

Updated4 %>%
    bootstrap(1000, id = "boot_id") %>%
    mutate(strap_validation = map(strap, ~sv(.x)),
            model_fit = map(strap_validation, ~ lm(formula = fmla3, data = .x %>%
                as_tibble() %>% unnest(train))),
            strap_test = map(strap_validation, ~unnest_test(.x)),
            strap_mse = map2_dbl(model_fit, strap_test, mse),
            average_mse = mean(strap_mse),
            est_se = sd(strap_mse)) %>%
        select(strap_mse, average_mse, est_se)

Updated4 %>%
    bootstrap(1000, id = "boot_id") %>%
    mutate(strap_validation = map(strap, ~sv(.x)),
        model_fit = map(strap_validation, ~ lm(formula = fmla3b, data = .x %>%
            as_tibble() %>% unnest(train))),
        strap_test = map(strap_validation, ~unnest_test(.x)),
        strap_mse = map2_dbl(model_fit, strap_test, mse),
        average_mse = mean(strap_mse),
        est_se = sd(strap_mse)) %>%
    select(strap_mse, average_mse, est_se)

Updated4 %>%
    bootstrap(1000, id = "boot_id") %>%
    mutate(strap_validation = map(strap, ~sv(.x)),
        model_fit = map(strap_validation, ~ lm(formula = fmla4, data = .x %>%
            as_tibble() %>% unnest(train))),
        strap_test = map(strap_validation, ~unnest_test(.x)),
        strap_mse = map2_dbl(model_fit, strap_test, mse),
        average_mse = mean(strap_mse),
        est_se = sd(strap_mse)) %>%
    select(strap_mse, average_mse, est_se)


fmla_pcr <- valuation_b ~ acquistions + funding_b + revenue_b
upcr <- pcr(fmla_pcr, data = utrain, scale = TRUE, validation = "CV")
#upcr$ncomp

upcr_pred <- predict(upcr, utest, ncomp = upcr$ncomp)

mean((upcr_pred-utest$valuation_b)^2)


u_10fold1 %>% 
  mutate(model_fit = map(train, ~ pcr(formula = fmla_pcr, data = .x)),
         # fold_pred = map2(model_fit, test, predict, ncomp = model_fit$ncomp),
         fold_mse = map2_dbl(model_fit, test, mse),
         #fold_mse = map(model_fit, ~ predict(model_fit, newdata = test, ncomp = model_fit$ncomp)))
         average_mse = mean(fold_mse))

uplsr <- plsr(fmla_pcr, data = utrain, scale = TRUE, validation = "CV")
#upls$ncomp

uplsr_pred <- predict(uplsr, utest, ncomp = uplsr$ncomp)

mean((uplsr_pred-utest$valuation_b)^2)

u_10fold1 %>% 
  mutate(model_fit = map(train, ~ plsr(formula = fmla_pcr, data = .x)),
         # fold_pred = map2(model_fit, test, predict, ncomp = model_fit$ncomp),
         fold_mse = map2_dbl(model_fit, test, mse),
         #fold_mse = map(model_fit, ~ predict(model_fit, newdata = test, ncomp = model_fit$ncomp)))
         average_mse = mean(fold_mse))


######

fmla_pcr1 <- valuation_b ~ revenue_b + funding_b  + rounds + growth_years + age

upcr1 <- pcr(fmla_pcr1, data = utrain, scale = TRUE, validation = "CV")
#upcr$ncomp

upcr_pred1 <- predict(upcr1, utest, ncomp = upcr1$ncomp)

mean((upcr_pred1-utest$valuation_b)^2)

u_10fold1 %>% 
  mutate(model_fit = map(train, ~ pcr(formula = fmla_pcr1, data = .x)),
         # fold_pred = map2(model_fit, test, predict, ncomp = model_fit$ncomp),
         fold_mse = map2_dbl(model_fit, test, mse),
         #fold_mse = map(model_fit, ~ predict(model_fit, newdata = test, ncomp = model_fit$ncomp)))
         average_mse = mean(fold_mse))

fmla_pcr2 <- valuation_b ~ age_prop + acquistions + funding_b + revenue_b + rounds + growth_years 

upcr2 <- pcr(fmla_pcr2, data = utrain, scale = TRUE, validation = "CV")
#upcr$ncomp

upcr_pred2 <- predict(upcr2, utest, ncomp = upcr2$ncomp)

mean((upcr_pred2-utest$valuation_b)^2)

u_10fold1 %>% 
  mutate(model_fit = map(train, ~ pcr(formula = fmla_pcr2, data = .x)),
         # fold_pred = map2(model_fit, test, predict, ncomp = model_fit$ncomp),
         fold_mse = map2_dbl(model_fit, test, mse),
         #fold_mse = map(model_fit, ~ predict(model_fit, newdata = test, ncomp = model_fit$ncomp)))
         average_mse = mean(fold_mse))

upls <- plsr(fmla_pcr2, data = utrain, scale = TRUE, validation = "CV")
#upls$ncomp

upls_pred <- predict(upls, utest, ncomp = upls$ncomp)

mean((upls_pred-utest$valuation_b)^2)

u_10fold1 %>% 
  mutate(model_fit = map(train, ~ plsr(formula = fmla_pcr2, data = .x)),
         # fold_pred = map2(model_fit, test, predict, ncomp = model_fit$ncomp),
         fold_mse = map2_dbl(model_fit, test, mse),
         #fold_mse = map(model_fit, ~ predict(model_fit, newdata = test, ncomp = model_fit$ncomp)))
         average_mse = mean(fold_mse))


######

fmla_pcr3 <- valuation_b ~ age + funding_b + revenue_b + rounds + growth_years  

upcr3 <- pcr(fmla_pcr3, data = utrain, scale = TRUE, validation = "CV")
#upcr$ncomp

upcr_pred3 <- predict(upcr3, utest, ncomp = upcr3$ncomp)

mean((upcr_pred3-utest$valuation_b)^2)

u_10fold1 %>% 
  mutate(model_fit = map(train, ~ pcr(formula = fmla_pcr3, data = .x)),
         # fold_pred = map2(model_fit, test, predict, ncomp = model_fit$ncomp),
         fold_mse = map2_dbl(model_fit, test, mse),
         #fold_mse = map(model_fit, ~ predict(model_fit, newdata = test, ncomp = model_fit$ncomp)))
         average_mse = mean(fold_mse))

u_10fold1 %>% 
  mutate(model_fit = map(train, ~ plsr(formula = fmla_pcr3, data = .x)),
         # fold_pred = map2(model_fit, test, predict, ncomp = model_fit$ncomp),
         fold_mse = map2_dbl(model_fit, test, mse),
         #fold_mse = map(model_fit, ~ predict(model_fit, newdata = test, ncomp = model_fit$ncomp)))
         average_mse = mean(fold_mse))


u_10fold <- u_validation %>% 
  unnest(train) %>%
  crossv_kfold(10, id = "fold")


model_def <- tibble(degree = 1:8,
                    fmla = str_c("valuation_b ~ poly(acquistions, ", degree, ")"))

model_def2 <- tibble(degree = 1:7,
                     fmla = str_c("valuation_b ~ poly(funding_b, ", degree, ")"))

model_def3 <- tibble(degree = 1:7,
                     fmla = str_c("valuation_b ~ poly(revenue_b, ", degree, ")"))

model_def4 <- tibble(degree = 1:10,
                     fmla = str_c("valuation_b ~ poly(rounds, ", degree, ")"))

model_def5 <- tibble(degree = 1:10,
                     fmla = str_c("valuation_b ~ poly(age, ", degree, ")"))

u_10fold2 <- u_10fold %>% 
  crossing(model_def) %>% 
  mutate(model_fit = map2(fmla, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse)) 

u_10fold2 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse))  %>%
  arrange(test_mse)

u_10fold2 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line() +
  geom_point()


u_10fold3 <- u_10fold %>% 
  crossing(model_def2) %>% 
  mutate(model_fit = map2(fmla, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse)) 

u_10fold3 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse))  %>%
  arrange(test_mse)

u_10fold3 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line() +
  geom_point()


u_10fold4 <- u_10fold %>% 
  crossing(model_def3) %>% 
  mutate(model_fit = map2(fmla, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse)) 

u_10fold4 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse))  %>%
  arrange(test_mse)

u_10fold4 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line() +
  geom_point()

u_10fold5 <- u_10fold %>% 
  crossing(model_def4) %>% 
  mutate(model_fit = map2(fmla, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse)) 

u_10fold5 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse))  %>%
  arrange(test_mse)

u_10fold5 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line() +
  geom_point()

u_10fold6 <- u_10fold %>% 
  crossing(model_def5) %>% 
  mutate(model_fit = map2(fmla, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse)) 

u_10fold6 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse))  %>%
  arrange(test_mse)

u_10fold6 %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line() +
  geom_point()
