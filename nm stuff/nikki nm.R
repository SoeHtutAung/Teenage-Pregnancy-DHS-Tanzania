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
                    m15 + #Place of delivery - levels + factor
                    factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                    m19, #Birth Weight - numeric
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_4)[2])) %>% round(2)
print (exp (confint(model_4)[2, ])) %>% round(2)
print(summary(model_4)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

######################## New - Figure 1  ######################## 
# Assuming you have model_1, model_2, model_3, model_4 fitted

# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients <- numeric(4)
lower_ci <- numeric(4)
upper_ci <- numeric(4)

models <- list(model_1, model_2, model_3, model_4)

for (i in seq_along(models)) {
  coefficients[i] <- exp(coef(models[[i]])[2])
  ci <- exp(confint(models[[i]])[2, ])
  lower_ci[i] <- ci[1]
  upper_ci[i] <- ci[2]
}

# Round the values
coefficients <- round(coefficients, 2)
lower_ci <- round(lower_ci, 2)
upper_ci <- round(upper_ci, 2)

# Prepare the data for ggplot2
forest_data <- data.frame(
  Model = paste("Model", 1:4),
  Coefficient = coefficients,
  LowerCI = lower_ci,
  UpperCI = upper_ci
)

# Duplicate Model 1's data
model_1_data <- forest_data[1, ]

# Create a new data frame for the adjusted order
adjusted_forest_data <- rbind(forest_data[2, ], model_1_data, 
                              forest_data[3, ], model_1_data,
                              forest_data[4, ], model_1_data)

# Reset the row names to ensure they are in sequential order
# rownames(adjusted_forest_data) <- NULL

# Reset the factor levels for 'Model'
# adjusted_forest_data$Model <- factor(adjusted_forest_data$Model, 
                                     # levels = unique(adjusted_forest_data$Model))

# Correct the 'Model' column to have unique identifiers
adjusted_forest_data$Model <- factor(c("Model 2", "Model 1", 
                                       "Model 3", "Model 1", 
                                       "Model 4", "Model 1"))

p <- ggplot(adjusted_forest_data, aes(x = Coefficient, y = fct_rev(Model))) +
  geom_point() +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  xlab("Odds Ratio") +
  ylab("") +
  labs(title = "Exploratory Investigation of Rural/Urban Region Based on Categorical Variables") +
  theme_classic()

print(p)


# Now create the adjusted forest plot
test <- ggplot(adjusted_forest_data, aes(x = Coefficient, y = Model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.1) +
  xlab("Odds Ratio") + ylab("") +
  labs(title = "Exploratory Investigation of Rural/Urban Region Odds Ratio Based on Categorical Variables") +
  theme_minimal()

# To display the plot
print(test)

######################## Old - TABLE 1 CONTEXT ######################## 
#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                                  strata =~v023, 
                                  weights=~wt,
                                  data= births_clean)


#1. Number of pregnancies with anaemia (v457) 
births_clean$v457
count(births_clean$v457) 

#Split by urban / rural by percentage (v025)
count(births_clean$v457[births_clean$v025 == 1]) #Urban
count(births_clean$v457[births_clean$v025 == 2]) #Rural 

round(prop.table(svytable(~ v457 + v025, design = design) ,margin = 2) *100, 2)

#2. Number of pregnancies ever told they had HT (s1125)
births_clean$s1125
count(births_clean$s1125) 

count(births_clean$s1125[births_clean$v025 == 1]) #Urban
count(births_clean$s1125[births_clean$v025 == 2]) #Rural 

round(prop.table(svytable(~ s1125 + v025, design = design) ,margin = 2) *100, 2)

#3. Number of pregnancies with number of ANC visits 
unique(births_clean$ANC_visits)
count(births_clean$ANC_visits) 

count(births_clean$ANC_visits[births_clean$v025 == 1]) #Urban
count(births_clean$ANC_visits[births_clean$v025 == 2]) #Rural

round(prop.table(svytable(~ ANC_visits + v025, design = design) ,margin = 2) *100, 2)

#4. Number of pregnancies in pregnancy intervals

#5. Number of pregnancies by mulitplicity of mothers (v201) 
births_clean <- births_clean %>%
  mutate(v201_cat = case_when(
    v201 == 0 ~ "0",
    v201 == 1 ~ "1",
    v201 == 2 ~ "2",
    v201 == 3 ~ "3",
    v201 == 4 ~ "4",
    v201 == 5 ~ "5",
    v201 == 6 ~ "6",
    v201 == 7 ~ "7",
    v201 == 8 ~ "8",
    v201 == 9 ~ "9",
    v201 == 10 ~ "10",
    v201 >= 11 ~ ">=11",
    TRUE ~ as.character(NA) # default case to return NA for values that don't fit any of the above conditions
  ))

count(births_clean$v201_cat) 

#By percentage
count_df <- count(births_clean$v201_cat)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

#By urban/rural (n)
count(births_clean$v201[births_clean$v025 == 1]) #Urban
count(births_clean$v201[births_clean$v025 == 2]) #Rural

round(prop.table(svytable(~ v201_cat + v025, design = design) ,margin = 2) *100, 2)

#6. Mum age at pregnancy (mum_age_pregnancy)
count_df <- count(births_clean$mum_age_pregnancy)
count_df$percentage <- (count_df$freq / 5619) * 100
count_df$x
summary(births_clean$mum_age_pregnancy)

#By urban/rural (n)
count(births_clean$mum_age_pregnancy[births_clean$v025 == 1]) #Urban
count(births_clean$mum_age_pregnancy[births_clean$v025 == 2]) #Rural

#7. Number by literacy (v155)
count_df <- count(births_clean$v155)
count_df$percentage <- (count_df$freq / 5619) * 100
print(count_df)


round(prop.table(svytable(~ v155 + v025, design = design) ,margin = 2) *100, 2)

#By urban/rural (n)
count(births_clean$v155[births_clean$v025 == 1]) #Urban
count(births_clean$v155[births_clean$v025 == 2]) #Rural

#8. Number of pregnancy losses (v245)
births_clean <- births_clean %>%
  mutate(v245_cat = case_when(
    v245 == 0 ~ "0",
    v245 == 1 ~ "1",
    v245 == 2 ~ "2",
    v245 >= 3 ~ ">=3",
    TRUE ~ as.character(NA) # default case to return NA for values that don't fit any of the above conditions
  ))

count_df <- count(births_clean$v245_cat)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

#By urban/rural (n)
count(births_clean$v155[births_clean$v245_cat == 1]) #Urban
count(births_clean$v155[births_clean$v245_cat == 2]) #Rural

round(prop.table(svytable(~ v245_cat + v025, design = design) ,margin = 2) *100, 2)


#9. Place of delivery for pregnancies (m15)
count_df <- count(births_clean$m15)
count_df$percentage <- (count_df$freq / 5619) * 100
print(count_df)

round(prop.table(svytable(~ m15 + v025, design = design) ,margin = 2) *100, 2)

#10. Mode of delivery (m17)
count_df <- count(births_clean$m17)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

round(prop.table(svytable(~ m17 + v025, design = design) ,margin = 2) *100, 2)

#11. Sex of the baby (b4) 
count_df <- count(births_clean$b4)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

round(prop.table(svytable(~ b4 + v025, design = design) ,margin = 2) *100, 2)

#12. Gestation at birth (b20)
count_df <- count(births_clean$b20)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

#13. Post natal check (m70)
count_df <- count(births_clean$m70)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

round(prop.table(svytable(~ m70 + v025, design = design) ,margin = 2) *100, 2)

#14. Age (mum_age_pregnancy)
births_clean$mum_age_pregnancy
summary(births_clean$mum_age_pregnancy)
count(births_clean$mum_age_pregnancy)

#Categorising the ages
births_clean <- births_clean %>%
  mutate(mum_age_pregnancy_cat = case_when(
    mum_age_pregnancy < 20 ~ "<20",
    mum_age_pregnancy >= 20 & mum_age_pregnancy < 25 ~ "20-24",
    mum_age_pregnancy >= 25 & mum_age_pregnancy < 35 ~ "25-34",
    mum_age_pregnancy >= 35 & mum_age_pregnancy < 45 ~ "35-44",
    mum_age_pregnancy >= 45 ~ "45-49",
    TRUE ~ as.character(NA) # default case to return NA for values that don't fit any of the above conditions
  ))

count_df <- count(births_clean$mum_age_pregnancy_cat)     
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

round(prop.table(svytable(~ mum_age_pregnancy_cat + v025, design = design) ,margin = 2) *100, 2)

#15. Birth weight (m19)
count(births_clean$m19)

births_clean <- births_clean %>%
  mutate(m19_cat = case_when(
    m19 <= 2500 ~ "<= 2500",
    m19 >= 4000 ~ "Macrosomia",
    m19 < 1000 ~ "",
    m19 >= 3 ~ ">=3",
    TRUE ~ as.character(NA) # default case to return NA for values that don't fit any of the above conditions
  ))

#16. BMI (v445)


############### Checking for missingness for table 2 regression ####################

# Checking initial numbers 
births_clean %>% summarise(total_weight = round(sum(v005)/1e6,0)) #total observations = 5652

#Checking missingness of values
na_list <- colnames(births_clean)[colSums(is.na(births_clean)) > 0]

#For NA details
for (col in na_list) {
  na_count <- sum(is.na(births_clean[, col]))
  cat("Variable", col, "> NAs", na_count, "\n")
}

births_clean$age_at_death_days

# Variable age_at_death_days > NAs 5493 
# Variable m13 > NAs 983 "timing of 1st antenatal check (months) "
# Variable ANC_visits > NAs 424 
# Variable m19 > NAs 1215 "birth weight in kilograms (3 decimals) "
# Variable m45 > NAs 424 "during pregnancy, given or bought iron tablets/syrup "
# Variable m66 > NAs 425 "respondent's health checked after discharge/delivery at home "
# Variable m70 > NAs 425 "child's health checked after discharge/delivery at home"
# Variable v457 > NAs 2787 "anaemia level" 
# Variable v456 > NAs 2787 "hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal) "
# Variable v445 > NAs 2782 "bmi"

##FINDINGS: 
table(births_clean$neo_mort, useNA = "always") #No NAs in neonatal mortality status
table(births_clean$neo_mort, births_clean$v025, useNA = "always")
#Only 126 records of neonatal mortality YES

#age_at_death_days
table(births_clean$age_at_death_days, births_clean$neo_mort, useNA = "always")
round(svytable(~age_at_death_days + v025, design = design, na.action = na.pass)/1e6,0)
# All NAs are in those with 'NO" neo_mort
# No records for age of death with urban and rural split

#m13 - timing of 1st antenatal check 
table(births_clean$m13, births_clean$neo_mort, useNA = "always") %>% prop.table(margin = 1)
#Most records are in 'no' neo_mort but there are some in 'yes' neo_mort, NAs in 'Yes' neo_mort is small but 50% of all yes records.
#"Don't know" or missing values on number of antenatal care visits and timing of first ANC are excluded from numerators but included in denominators.


#ANC_visits (m14), m45, m66, 70 
table(births_clean$ANC_visits, births_clean$neo_mort, useNA = "always")
table(births_clean$m45,births_clean$neo_mort, useNA = "always")
table(births_clean$m66,births_clean$neo_mort, useNA = "always")
table(births_clean$m70,births_clean$neo_mort, useNA = "always")
table(births_clean$m70,births_clean$neo_mort,  births_clean$v025, useNA = "always")
#Result: Only half of neo_mort YES have been interviewed for these questions - NA = 35 
#NAs and 'dont know' are assumed to have not received in the intervertion

#m19 - bw in kg
table(births_clean$m19, births_clean$neo_mort, useNA = "always")
#Result: YES Neo_mort -  NA = 47, NAs should be excluded from analysis in Table 2

#v457, v456, v445 (Done by specific households, randomised)
table(births_clean$v457, births_clean$neo_mort, useNA = "always")
table(births_clean$v456, births_clean$neo_mort, useNA = "always")
table(births_clean$v445, births_clean$neo_mort, useNA = "always")
#Result: Neo_mort YES (126), 68 are recorded to be NA for v457, v456
#About 2782 aren't recorded as they were not interviewed 

table(teendata$contra_current, teendata$contra_future, useNA = "always") # can't identify exact numbers
round(svytable(~contra_future + v025, design = dhs, na.action = na.pass)/1e6,0) # this table is used to


############### TABLE 2. BIVARAIATE ANALYSIS ####################
births_clean$neo_mort <- factor(births_clean$neo_mort)
levels(births_clean$neo_mort) #Checking levels of outcome 

#####  DATA CLEANING / PREPARATION #####  

#Post natal check - m70
# Replace "NA" and "8" with 0
births_clean$m70[is.na(births_clean$m70)] <- 0
births_clean$m70 <- ifelse(births_clean$m70 == 8, 0, births_clean$m70)
count(births_clean$m70)
table(births_clean$m70)



#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)

#1. Post natal check (m70)
unique(births_clean$m70) 
factor(births_clean$m70) %>% levels() #Checking levels of outcome  
count(births_clean$m70)

model_m70 <- svyglm(neo_mort ~ factor(m70), 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.exclude)

print(exp(coef(model_m70))) %>% round(2)
print (exp (confint(model_m70))) %>% round(2)
print(summary(model_m70)$coefficients[ , "Pr(>|t|)"]) %>% round(2)


#2. Gestation at birth (b20)
mode(births_clean$b20)
count(births_clean$b20)
births_clean$b20 <- factor(births_clean$b20)
levels(births_clean$b20) #Checking levels of outcome

model_b20 <- svyglm(neo_mort ~ b20,
                    design = design, 
                    family = quasibinomial(),
                    na.action = na.exclude)

print (exp (coef(model_b20)[2])) %>% round(2)
print (exp (confint(model_b20)[2, ])) %>% round(2)
print (summary(model_b20)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#3. Sex of baby (b4) 1 - male , 2 - female
mode(births_clean$b4)
births_clean$b4 <- factor(births_clean$b4)
levels(births_clean$b4) #Checking levels of outcome

model_b4 <- svyglm(neo_mort ~ b4, 
                   design = design, 
                   family = quasibinomial(), 
                   na.action = na.exclude)

print (exp (coef(model_b4)[2])) %>% round(2)
print(exp(confint(model_b4)[2, ])) %>% round(2)
print(summary(model_b4)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#4. Birth weight (m19)
count(births_clean$m19)
births_clean$m19 <- relevel(factor(births_clean$m19), ref = "notlowbw")
levels(births_clean$m19)

model_m19 <- svyglm(neo_mort ~ factor(m19), 
                   design = design, 
                   family = quasibinomial(), 
                   na.action = na.omit)

print (exp (coef(model_m19)[2])) %>% round(2)
print(exp(confint(model_m19)[2, ])) %>% round(2)
print(summary(model_m19)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#5. Delivery by C section  (m17) 
mode(births_clean$m17)
as.factor(births_clean$m17) %>% levels()
sum(is.na(births_clean$m17))

model_m17 <- svyglm(neo_mort ~ factor(m17), 
                   design = design, 
                   family = quasibinomial(), 
                   na.action = na.exclude)

print(exp(coef(model_m17)[2])) %>% round(2)
print (exp (confint(model_m17)[2, ])) %>% round(2)
print(summary(model_m17)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#6. Assistance at birth (senior_delivery_attendant)
mode(births_clean$senior_delivery_attendant)
unique(births_clean$senior_delivery_attendant)

model_aab <- svyglm(neo_mort ~ factor(senior_delivery_attendant), 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.exclude)

print(exp(coef(model_aab))) %>% round(2)
print (exp (confint(model_aab))) %>% round(2)
print(summary(model_aab)$coefficients[,"Pr(>|t|)"]) %>% round(2)

#7. Place of delivery (m15)
mode(births_clean$m15)
unique(births_clean$m15)

model_m15 <- svyglm(neo_mort ~ factor(m15), 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.exclude)

print(exp(coef(model_m15))) %>% round(2)
print (exp (confint(model_m15))) %>% round(2)
print(summary(model_m15)$coefficients[,"Pr(>|t|)"]) %>% round(2)

#8. Multiple pregnancies (v201)
mode(births_clean$v201)
unique(births_clean$v201)

model_v201 <- svyglm(neo_mort ~ v201, 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.exclude)

print(exp(coef(model_v201)[2])) %>% round(2)
print (exp (confint(model_v201)[2, ])) %>% round(2)
print(summary(model_v201)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#9. Number of ANC visits 
births_clean$ANC_visits
births_BR$ANC_visits

#10. Number of pregnancies with hypertension (s1125)
mode(births_clean$s1125)
unique(births_clean$s1125)
sum(is.na(births_clean$s1125)) #no NAs!!!
as.factor(births_clean$s1125) %>% levels() 

model_s1125 <- svyglm(neo_mort ~ factor(s1125), 
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.exclude)

print(exp(coef(model_s1125)[2])) %>% round(2)
print (exp (confint(model_s1125)[2, ])) %>% round(2)
print(summary(model_s1125)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#11. Number of pregnancies with anaemia (v457) 
mode(births_clean$v457)
unique(births_clean$v457)
sum(is.na(births_clean$v457)) #Will need to exclude NAs records with na.omit
as.factor(births_clean$v457) %>% levels()

model_v457 <- svyglm(neo_mort ~ factor(v457), 
                      design = design, 
                      family = quasibinomial(), 
                      na.action = na.omit)

print(exp(coef(model_v457))) %>% round(2)
print (exp (confint(model_v457))) %>% round(2)
print(summary(model_v457)$coefficients[,"Pr(>|t|)"]) %>% round(2)

#12. Pregnancy losses (v245)
mode(births_clean$v245)
unique(births_clean$v245)
sum(is.na(births_clean$v245)) #No NAs

model_v245 <- svyglm(neo_mort ~ v245, 
                      design = design, 
                      family = quasibinomial(), 
                      na.action = na.exclude)

print(exp(coef(model_v245)[2])) %>% round(2)
print (exp (confint(model_v245)[2, ])) %>% round(2)
print(summary(model_v245)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#13. Wealth quintile (v190)
mode(births_clean$v190)
unique(births_clean$v190)
sum(is.na(births_clean$v190)) #No NAs
as.factor(births_clean$v190) %>% levels()

model_v190 <- svyglm(neo_mort ~ factor(v190), 
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.exclude)

print(exp(coef(model_v190))) %>% round(2)
print (exp (confint(model_v190))) %>% round(2)
print(summary(model_v190)$coefficients[,"Pr(>|t|)"]) %>% round(2)

#14. BMI (v445)
mode(births_clean$v445)
unique(births_clean$v445)
levels(births_clean$v445)
sum(is.na(births_clean$v445)) #No NAs

model_v445 <- svyglm(neo_mort ~ v445, 
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_v445)[2])) %>% round(2)
print (exp (confint(model_v445)[2, ])) %>% round(2)
print(summary(model_v445)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

##################### MULTIVARIATE ANALYSIS #####################

#labour model 
modelm_labour <- svyglm(neo_mort ~ factor(v025) + 
                  factor(m17) + m19 + b20,
                  design = design, family = quasibinomial(), na.action = na.omit)

print (exp (coef(modelm_labour)))
print (exp (confint(modelm_labour)))
print (summary(modelm_labour)$coefficients[,"Pr(>|t|)"])

#Pregnancy model 
modelm_1 <- svyglm(neo_mort ~ factor(v025) + 
                     as.factor(s1125) + m14 + v201,
                   design, family = quasibinomial(), na.action = na.exclude)

print (exp (coef(modelm_1)))
print (exp (confint(modelm_1)))
print (summary(modelm_1)$coefficients[,"Pr(>|t|)"])

#Labour + Pregnancy model 
modelm_labour_preg <- svyglm(neo_mort ~ factor(v025) + 
                     factor(m17) + m19 + b20 +  as.factor(s1125) + m14 + v201,
                   design = design, family = quasibinomial(), na.action = na.omit)


print (exp (coef(modelm_labour_preg))) %>% round(2)
print (exp (confint(modelm_labour_preg))) %>% round(2)
print (summary(modelm_labour_preg)$coefficients[,"Pr(>|t|)"]) %>% round(2)
