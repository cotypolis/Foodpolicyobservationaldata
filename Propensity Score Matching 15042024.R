
#R Code for Propensity Match score
library(MatchIt)

library(tidyverse)
library(lmtest)
library(dplyr)

DS_individuals_exploration_10042024_full <- DS_individuals_exploration_10042024_full %>%
  mutate(Year = as.numeric(Year), # Convert SurveyYear to numeric
         Year = case_when(
           Year == 1 ~ 2009,
           Year == 2 ~ 2010,
           Year == 3 ~ 2011,
           Year == 4 ~ 2012,
           Year == 5 ~ 2013,
           Year == 6 ~ 2014,
           Year == 7 ~ 2015,
           Year == 8 ~ 2016,
           Year == 9 ~ 2017,
           Year == 10 ~ 2018,
           Year == 11 ~ 2019,
           TRUE ~ Year # Keeps the original value if none of the conditions above are met
         ))

## Weighting


# Multiply each weight by 15655

DS_individuals_exploration_10042024_full$adjustedweight <- DS_individuals_exploration_10042024_full$Weight * 15655

#Multiply each by the combined sum of the four weights

DS_individuals_exploration_10042024_full$FinalWeight <- ifelse(DS_individuals_exploration_10042024_full$Year >= 2009 & DS_individuals_exploration_10042024_full$Year <= 2012, 
                                                               (DS_individuals_exploration_10042024_full$adjustedweight / 6828) * 0.3636,
                                                               ifelse(DS_individuals_exploration_10042024_full$Year >= 2013 & DS_individuals_exploration_10042024_full$Year <= 2014,
                                                                      (DS_individuals_exploration_10042024_full$adjustedweight  / 2546) * 0.1818,
                                                                      ifelse(DS_individuals_exploration_10042024_full$Year >= 2015 & DS_individuals_exploration_10042024_full$Year <= 2016,
                                                                             (DS_individuals_exploration_10042024_full$adjustedweight  / 2723) * 0.1818,
                                                                             ifelse(DS_individuals_exploration_10042024_full$Year >= 2017 & DS_individuals_exploration_10042024_full$Year <= 2019,
                                                                                    (DS_individuals_exploration_10042024_full$adjustedweight  / 3558) * 0.2727,
                                                                                    NA))))  # NA for years outside specified ranges


mean_final_weight_per_year <- aggregate(FinalWeight ~ Year, DS_individuals_exploration_10042024_full, mean, na.rm = TRUE)
print(mean_final_weight_per_year)


# Convert -1 in Body Mass Index (bmival2) to NA, create treatment variable
DS_individuals_exploration_10042024_full <- DS_individuals_exploration_10042024_full %>%
  filter(bmivg5 != -1) %>%
  mutate(
    treatment = if_else(bmival2 > 25, 0, 1), # Include condition here with the condition that defines the treatment group
    post_policy = if_else(Year >= 2013, 1, 0)
  )

#create a contingency table
cont_table <- table(DS_individuals_exploration_10042024_full$Year, DS_individuals_exploration_10042024_full$treatment)
cont_table
percent_table <- prop.table(cont_table) * 100
percent_table
cont_table_bmi <- table(DS_individuals_exploration_10042024_full$Year, DS_individuals_exploration_10042024_full$bmivg5)
cont_table_bmi
cont_table_pol <- table(DS_individuals_exploration_10042024_full$Year, DS_individuals_exploration_10042024_full$post_policy)
cont_table_pol

#USING BMI

# Assuming 'Treatment' is your treatment indicator and other variables are covariates
propensity_model <- glm(treatment ~ Sex + AgeR + Income_tertiles, family = binomial(link = "logit"), data = DS_individuals_exploration_10042024_full, weights = FinalWeight)

# Calculate Propensity Score
DS_individuals_exploration_10042024_full$Propensity_score <- predict(propensity_model, type = "response")

#Check the Propensity Score Distribution
library(ggplot2)
ggplot(DS_individuals_exploration_10042024_full, aes(x = Propensity_score, fill = as.factor(treatment))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(fill = "Treatment", title = "Distribution of Propensity Scores by Treatment Status")

#Compare Outcomes Between Treated and Control Groups

# Assuming 'EnergyKcal' is the outcome variable
treated_outcomes <- DS_individuals_exploration_10042024_full$`Sum of Energykcal`[DS_individuals_exploration_10042024_full$treatment == 1]
control_outcomes <- DS_individuals_exploration_10042024_full$`Sum of Energykcal`[DS_individuals_exploration_10042024_full$treatment == 0]

# Perform a t-test to compare means
t_test_results <- t.test(treated_outcomes, control_outcomes)
print(t_test_results)

library(ggplot2)
ggplot(DS_individuals_exploration_10042024_full, aes(x = as.factor(treatment), y = `Sum of Energykcal`, fill = as.factor(treatment))) +
  geom_boxplot() +
  labs(x = "Treatment Group", y = "EnergyKcal", title = "Comparison of EnergyKcal Between Groups") +
  scale_fill_discrete(name = "Group", labels = c("Control", "Treated"))


# Assuming df_filtered has 'Year', 'Treatment', and 'EnergyKcal' columns
ggplot(DS_individuals_exploration_10042024_full, aes(x = Year, y = `Sum of Energykcal`, color = as.factor(treatment), group = treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly EnergyKcal Values by Treatment Group",
       x = "Year",
       y = "EnergyKcal",
       color = "Group") +
  theme_minimal()

ggplot(DS_individuals_exploration_10042024_full, aes(x = Year, y = `Sum of Energykcal`, color = as.factor(treatment))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +  # Linear model trend line without confidence interval
  labs(title = "Trend of EnergyKcal Values by Treatment Group",
       x = "Year",
       y = "EnergyKcal",
       color = "Group") +
  theme_minimal()

#USING SES

# Convert -1 in Body Mass Index (bmival2) to NA, create treatment variable
DS_individuals_exploration_10042024_full_ses <- DS_individuals_exploration_10042024_full %>%
  filter(Income_tertiles != -1) %>%
  mutate(
    treatment = if_else(Income_tertiles > 1, 0, 1), # Include condition here with the condition that defines the treatment group
    post_policy = if_else(Year >= 2013, 1, 0)
  )

#create a contingency table
cont_table <- table(DS_individuals_exploration_10042024_full_ses$Year, DS_individuals_exploration_10042024_full_ses$treatment)
cont_table
percent_table <- prop.table(cont_table) * 100
percent_table
cont_table_bmi <- table(DS_individuals_exploration_10042024_full_ses$Year, DS_individuals_exploration_10042024_full_ses$Income_tertiles)
cont_table_bmi
cont_table_pol <- table(DS_individuals_exploration_10042024_full_ses$Year, DS_individuals_exploration_10042024_full_ses$post_policy)
cont_table_pol

# Assuming 'Treatment' is your treatment indicator and other variables are covariates
propensity_model_ses <- glm(treatment ~ Sex + AgeR + bmival2, family = binomial(link = "logit"), data = DS_individuals_exploration_10042024_full_ses, weights = FinalWeight)

# Calculate Propensity Score
DS_individuals_exploration_10042024_full_ses$Propensity_score <- predict(propensity_model_ses, type = "response")

#Check the Propensity Score Distribution
library(ggplot2)
ggplot(DS_individuals_exploration_10042024_full_ses, aes(x = Propensity_score, fill = as.factor(treatment))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(fill = "Treatment", title = "Distribution of Propensity Scores by Treatment Status")

#Compare Outcomes Between Treated and Control Groups

# Assuming 'EnergyKcal' is the outcome variable
treated_outcomes <- DS_individuals_exploration_10042024_full_ses$`Sum of Energykcal`[DS_individuals_exploration_10042024_full_ses$treatment == 1]
control_outcomes <- DS_individuals_exploration_10042024_full_ses$`Sum of Energykcal`[DS_individuals_exploration_10042024_full_ses$treatment == 0]

# Perform a t-test to compare means
t_test_results <- t.test(treated_outcomes, control_outcomes)
print(t_test_results)

library(ggplot2)
ggplot(DS_individuals_exploration_10042024_full_ses, aes(x = as.factor(treatment), y = `Sum of Energykcal`, fill = as.factor(treatment))) +
  geom_boxplot() +
  labs(x = "Treatment Group", y = "EnergyKcal", title = "Comparison of EnergyKcal Between Groups") +
  scale_fill_discrete(name = "Group", labels = c("Control", "Treated"))


# Assuming df_filtered has 'Year', 'Treatment', and 'EnergyKcal' columns
ggplot(DS_individuals_exploration_10042024_full_ses, aes(x = Year, y = `Sum of Energykcal`, color = as.factor(treatment), group = treatment)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly EnergyKcal Values by Treatment Group",
       x = "Year",
       y = "EnergyKcal",
       color = "Group") +
  theme_minimal()


