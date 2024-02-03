library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(plyr)
library(ggplot2)

#Run nm_master.R document for "births_clean" dataset

########################  new - TABLE 2 - ODDS RATIO ######################## 

#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)

#Model 1. Rural / Urban odds ratio
model_1 <- svyglm(factor(neo_mort) ~ factor(v025), 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.exclude)

print(exp(coef(model_1)[2])) %>% round(2)
print (exp (confint(model_1)[2, ])) %>% round(2)
print(summary(model_1)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#Model 2. model_1 + Socio-demographic factors
# mother's age at pregnancy
# literacy re-leveled
# marital status re-leveled
# smoking
# BMI
# Wealth quintile
# pregnancy losses

model_2 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    mum_age_pregnancy + # mother's age at pregnancy - numeric
                    v155_cat + # literacy re-leveled - factor
                    v501_cat + # marital status re-leveled - factor
                    factor(v463aa_cat) + # smoking - factor
                    v445 + # BMI - numeric
                    factor(v190) + # Wealth quintile - not leveled - factor
                    v245_cat, # pregnancy losses - re-leveled + factored
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_2)[2])) %>% round(2)
print (exp (confint(model_2)[2, ])) %>% round(2)
print(summary(model_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#Model 3. model 2 + Pregnancy factors
#Number of pregnancies with hypertension (s1125) 
#Number of ANC visit
#Multiple pregnancies (v201) 

model_3 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    mum_age_pregnancy + # mother's age at pregnancy - numeric
                    v155_cat + # literacy re-leveled - factor
                    v501_cat + # marital status re-leveled - factor
                    factor(v463aa_cat) + # smoking - factor
                    v445 + # BMI - numeric
                    factor(v190) + # Wealth quintile - not leveled - factor
                    v245_cat + # pregnancy losses - re-leveled + factored
                    factor(s1125) +  #ever had hypertension - not leveled - factor 
                    ANC_visits + #number of ANC visits - factor
                    v201, #Multiple pregnancies - not leveled + numeric
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_3)[2])) %>% round(2)
print (exp (confint(model_3)[2, ])) %>% round(2)
print(summary(model_3)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#Model 4. model 3 + Labour / delivery / baby factors 
# Place of delivery (m15) 
# Assistance at delivery 
# Mode of delivery (m17) 
# Birth Weight (m19)  

model_4 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    mum_age_pregnancy + # mother's age at pregnancy - numeric
                    v155_cat + # literacy re-leveled - factor
                    v501_cat + # marital status re-leveled - factor
                    factor(v463aa_cat) + # smoking - factored
                    v445 + # BMI - numeric
                    factor(v190) + # Wealth quintile - not leveled - factored
                    v245_cat + # pregnancy losses - re-leveled + factored
                    factor(s1125) +  #ever had hypertension - not leveled - factor 
                    ANC_visits + #number of ANC visits - factor
                    v201 + #Multiple pregnancies - not leveled + numeric
                    v501_cat + #Assistance at delivery - levels + factor
                    m15 + #Place of delivery - levels + factor
                    factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                    m19, #Birth Weight - numeric
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_4)[2])) %>% round(2)
print (exp (confint(model_4)[2, ])) %>% round(2)
print(summary(model_4)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

######################## Figure to illustrate table 2  ######################## 
# Assuming you have model_1, model_2, model_3, model_4 fitted

# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients <- c(0.73, 1.50, 1.53, 1.55)
lower_ci <- c(0.46, 0.57, 0.60, 0.59)
upper_ci <- c(1.16, 3.93, 3.88, 4.09)
labels <- c("Model 1", "Model 2", "Model 3", "Model 4")

forest_data <- matrix(c(rep("", 4), lower_ci, coefficients, upper_ci), 
                      ncol = 4, byrow = FALSE)

forestplot(labeltext = labels, 
           mean = coefficients, 
           lower = lower_ci, 
           upper = upper_ci,
           xlab = "Coefficient Value",
           zero = 0,
           lineheight = "auto",
           boxsize = 0.5,
           col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"))


######################## Figure to illustrate table 2  ######################## 
#ORs of Rural / Urban coefficient OR changes by individual categorical models

#Creating individual models with v025
model_region <- svyglm(factor(neo_mort) ~ factor(v025), 
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.exclude)

print(exp(coef(model_region)[2])) %>% round(2)
print (exp (confint(model_region)[2, ])) %>% round(2)
print(summary(model_region)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

model_sociodem <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    mum_age_pregnancy + # mother's age at pregnancy - numeric
                    v155_cat + # literacy re-leveled - factor
                    v501_cat + # marital status re-leveled - factor
                    factor(v463aa_cat) + # smoking - factor
                    v445 + # BMI - numeric
                    factor(v190) + # Wealth quintile - not leveled - factor
                    v245_cat, # pregnancy losses - re-leveled + factored
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)


print(exp(coef(model_sociodem)[2])) %>% round(2)
print (exp (confint(model_sociodem)[2, ])) %>% round(2)
print(summary(model_sociodem)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

model_preg <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    factor(s1125) +  #ever had hypertension - not leveled - factor 
                    ANC_visits + #number of ANC visits - factor
                    v201, #Multiple pregnancies - not leveled + numeric
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_preg)[2])) %>% round(2)
print (exp (confint(model_preg)[2, ])) %>% round(2)
print(summary(model_preg)$coefficients[2,"Pr(>|t|)"]) %>% round(2)


model_labour <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                         v501_cat + #Assistance at delivery - levels + factor
                         m15 + #Place of delivery - levels + factor
                         factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                         m19, #Birth Weight - numeric
                       design = design, 
                       family = quasibinomial(), 
                       na.action = na.omit)

print(exp(coef(model_labour)[2])) %>% round(2)
print (exp (confint(model_labour)[2, ])) %>% round(2)
print(summary(model_labour)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

get_exp_coef_ci <- function(model, coef_index) {
  # Extract the exponentiated coefficient
  coef_value <- exp(coef(model)[coef_index]) %>% round(2)
  
  # Extract the confidence intervals and exponentiate them
  ci_values <- exp(confint(model)[coef_index, ]) %>% round(2)
  
  # Combine into a named vector
  c(coef = coef_value, lower = ci_values[1], upper = ci_values[2])
}

# Use the function to get the required values for the second coefficient
exp_coef_ci_region <- get_exp_coef_ci(model_region, 2)
exp_coef_ci_sociodem <- get_exp_coef_ci(model_sociodem, 2)
exp_coef_ci_preg <- get_exp_coef_ci(model_preg, 2)
exp_coef_ci_labour <- get_exp_coef_ci(model_labour, 2)

# Combine the coefficients and confidence intervals into a matrix for plotting
coefficients <- c(exp_coef_ci_region['coef.factor(v025)2'], exp_coef_ci_sociodem['coef.factor(v025)2'], exp_coef_ci_preg['coef.factor(v025)2'], exp_coef_ci_labour['coef.factor(v025)2'])
lower_ci <- c(exp_coef_ci_region['lower.2.5 %'], exp_coef_ci_sociodem['lower.2.5 %'], exp_coef_ci_preg['lower.2.5 %'], exp_coef_ci_labour['lower.2.5 %'])
upper_ci <- c(exp_coef_ci_region['upper.97.5 %'], exp_coef_ci_sociodem['upper.97.5 %'], exp_coef_ci_preg['upper.97.5 %'], exp_coef_ci_labour['upper.97.5 %'])

# Define the model names for the plot
labels <- c("Model Region", "Model Sociodemographic", "Model Pregnancy", "Model Labour")

# Create the forest plot
forestplot(labeltext = labels,
           mean = coefficients,
           lower = lower_ci,
           upper = upper_ci,
           xlab = "Odds Ratio",
           zero = 1, # Reference line at odds ratio of 1
           lineheight = "auto",
           boxsize = 0.5,
           col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"), 
           title = "Odds Ratio of Rural / Urban Coefficient OR Changes by Individual Categorical Models")

