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

Unicorn_Startups <- read_csv(file = "Unicorn-Startups.csv")
Unicorns <- as_tibble(Unicorn_Startups) %>%
clean_names()

Unicorns<- Unicorns %>%
mutate(unicorn_status_date = parse_date_time(unicorn_status_year, orders="mdy"),
unicorn_status_year2 = year(unicorn_status_date))

Unicorns2 <- Unicorns %>%
mutate(years_to_unicorn_status = unicorn_status_year2 - year_founded) %>%
select(-link, -f1_link, -f1_financial_worth_millions_usd, -unicorn_status_date,
-unicorn_status_year)


#calculate proportion of years to unicorn status to startup age
Unicorns2 <- Unicorns2 %>%
mutate(age = 2019 - year_founded) %>%
mutate(age_prop = years_to_unicorn_status/age)


#Categorize similar industries together
industries <- Unicorns2 %>%
select(industry)
unique(industries)

Unicorns2 <- Unicorns2 %>%
mutate(industry = fct_collapse(industry,
`Health/Biotech` = c("Healthcare", "Digital Health", "Biotech", "Biotechnology", "Genomics"),
Fintech = c("Fintech", "Fintech / Supply Chain"),
Space = c("Space", "SpaceTech"),
`Food/Restaurant` = c("Food", "Food Delivery", "Restaurant Tech", "Food and Beverage"),
Hardware = c("Robotics", "Hardware", "IoT", "3D Printing", "VR/AR"),
Data = c("Big Data", "BI & Analytics", "Data Storage"),
Media = c("Social", "Media"),
Commerce = c("eCommerce/Marketplace", "Supply Chain"),
Property = c("Facilities", "Real Estate"),
Sales = c("Sales Tech", "Adtech", "AdTech"),
Legal = c("RegTech", "HR Tech"),
Transportation = c("Travel Tech", "On-Demand", "Auto Tech"),
Gaming = c("Gaming", "eSports")
))


Unicorns2 <- Unicorns2 %>%
mutate(f1_education= fct_collapse(f1_education,
Business = c("Business", "Economics", "Entreprenuership", "Finance", "Marketing", "Accounting", "Law"),
`Life Sciences` = c("Medicine", "Chemistry", "Biology"),
STEM = c("Information Technology", "Computer Science", "Technology", "Engineering", "Mathematics"),
`Non-STEM` = c("Political Science", "Philosophy", "English", "American Studies", "Cognitive Science"),
`Arts and Media`  = c("Design", "Gaming", "Media")
))




Unicorns2 <- Unicorns2 %>%
mutate(f1_educational_degree = fct_collapse(f1_educational_degree,
`Bachelor's` = c("BFA", "BS", "BA"),
`Master's` = c("MS", "MSME", "MBA", "M.Phil"),
Doctorate = c("PhD", "JD", "MD")
))

#saveRDS(Unicorns2, "Updated2.rds")

```

```{r}
Updated3 <- Unicorns2  %>%
mutate(funding_b = total_funding_amount_millions_usd/1000,
revenue_b = revenue_m/1000) %>%
rename(acquistions = number_of_acquistions,
investors = number_of_investors,
rounds = number_of_funding_rounds,
founders = number_of_listed_founders,
founder_college = f1_college_education,
founder_degree = f1_educational_degree,
founder_track_record = f1_number_of_past_jobs,
founder_race = f1_race_ethnicinity,
growth_years = years_to_unicorn_status,
unicorn_status_year = unicorn_status_year2
)

Updated3 <- Updated3 %>%
mutate(founder_track_record = Updated3$founder_track_record %>% replace_na(0))

#saveRDS(Updated3, "Updated3.rds")


Updated4 <- Updated3 %>%
select(-company, -total_funding_amount_millions_usd, -f1_current_age,-f1_education,
-founder_degree, - f1_past_startup_work_industry,
-founder_college, - f1_education, -founder_degree, -industry, -revenue_m,
-founder_race) %>%
mutate_if(is.character, as.factor)

Updated4 <- Updated4 %>%
mutate(year_founded = as.factor(year_founded),
unicorn_status_year = as.factor(unicorn_status_year)) %>%
select(-year_founded, -unicorn_status_year)

#saveRDS(Updated4, "Updated4.rds")
