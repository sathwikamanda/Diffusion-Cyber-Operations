##################################################################
# Project: Offensive Cyber Operations Diffusion
# Author: Sathwika Manda
# Date: Dec, 2025
##################################################################

library(tidyverse)
library(lubridate)
library(MASS)
library(dplyr)

rm(list = ls())

eurepoc_raw <- read_csv("eurepoc_data.csv")

#Cleaning Data

## Removing rows with missing attacker category
eurepoc <- eurepoc_raw %>%
  filter(!is.na(initiator_category))

## Recode attacker type into State vs Non-state
eurepoc <- eurepoc %>%
  mutate(
    # 1) Choose the best available actor category label
    attacker_cat_raw = case_when(
      !is.na(attributed_initiator_category) & attributed_initiator_category != "Not available" ~ 
        attributed_initiator_category,
      !is.na(initiator_category) & initiator_category != "Not available" ~ 
        initiator_category,
      TRUE ~ NA_character_
    ),
    
    # 2) Collapse into State-linked (0) vs Non-state (1)
    attacker_type = case_when(
      attacker_cat_raw == "State" ~ 0,
      attacker_cat_raw == "Non-state actor, state-affiliation suggested" ~ 0,
      
      attacker_cat_raw == "Non-state-group" ~ 1,
      attacker_cat_raw == "Individual hacker(s)" ~ 1,
      attacker_cat_raw == "Other" ~ 1,
      
      attacker_cat_raw == "Unknown - not attributed" ~ NA_real_,
      is.na(attacker_cat_raw) ~ NA_real_,
      
      # catch-all in case there are a few unexpected labels
      TRUE ~ NA_real_
    )
  )


# Sophistication based on weighted cyber intensity
eurepoc <- eurepoc %>%
  mutate(
    wci_soph = suppressWarnings(as.numeric(weighted_cyber_intensity))  
  )

#zero day use
eurepoc <- eurepoc %>%
  mutate(
    zero_day_soph = case_when(
      zero_days == "Yes" ~ 3,   # confirmed zero-day = high sophistication boost
      zero_days == "No"  ~ 0,   # no zero-day used
      TRUE ~ NA_real_
    )
  )


# Creating a combined sophistication score
eurepoc <- eurepoc %>%
  mutate(
    soph_score = wci_soph + zero_day_soph,
    
    # Reclassify into low / medium / high using new thresholds
    sophistication = case_when(
      soph_score <= 2 ~ "low",       # low intensity and no zero-day
      soph_score <= 6 ~ "medium",    # moderate intensity OR zero-day with low intensity
      soph_score > 6  ~ "high"       # high intensity and/or zero-day + moderate intensity
    ),
    
    sophistication = factor(
      sophistication,
      levels = c("low", "medium", "high"),
      ordered = TRUE
    )
  )




eurepoc %>% 
  count(sophistication)





# Extracting an example of low, medium, and high sophistication attacks
low_example <- eurepoc[15]
med_example_highend <- eurepoc[189]
med_example_lowend <- eurepoc[19]
high_example <- eurepoc[20]

subset_rows <- eurepoc[c(15, 189, 19, 20), ]
View(subset_rows)
write_csv(subset_rows, "sophistication_example_rows.csv")




# Visualizations

## frequency over time by attacker type
eurepoc %>%
  mutate(year = year(start_date)) %>%
  group_by(year, attacker_type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = year, y = count, color = factor(attacker_type))) +
  geom_line(size = 1.2) +
  labs(title = "Cyber Incidents Over Time by Actor Type",
       x = "Year", y = "Number of Incidents",
       color = "Actor Type (0=State, 1=Non-state)")


# sophistication distr by attacker type
eurepoc %>%
  ggplot(aes(x = sophistication, fill = factor(attacker_type))) +
  geom_bar(position = "dodge") +
  labs(title = "Sophistication by Attacker Type",
       x = "Sophistication Level", y = "Count",
       fill = "Actor Type (0=State, 1=Non-state)")



# Logistic Regression Analysis

# fitting model
model <- polr(
  sophistication ~ attacker_type, 
  data = eurepoc, 
  Hess = TRUE
)

summary(model)

#get odds ratio
exp(coef(model))

#Getting p vals
coeff_table <- coef(summary(model))

p_values <- pnorm(abs(coeff_table[, "t value"]), lower.tail = FALSE) * 2
p_values

# Combining into table
results <- cbind(
  Estimate = coeff_table[, "Value"],
  `Odds Ratio` = exp(coeff_table[, "Value"]),
  `Std. Error` = coeff_table[, "Std. Error"],
  `t value` = coeff_table[, "t value"],
  `p value` = p_values
)

results



