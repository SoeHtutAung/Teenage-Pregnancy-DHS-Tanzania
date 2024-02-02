library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(plyr)
library(sf)
library(ggsci)


#######LOAD DATA
##read births data set
#births_BR <- read_dta("C:/Users/Saund/OneDrive - London School of Hygiene and Tropical Medicine/TZDHS/Survey Data/Births/TZBR82FL.DTA")
#births_BR <- read_dta("~/Downloads/UNICEF DATA/Births/TZBR82FL.DTA") # for nikki 

##load txt file with variable codes from code list
#NM_variables <- readLines("NM_variables.txt")
#NM_variables <- readLines("/Users/nikkiyu/Downloads/2 Data Challenge/unicef/nm stuff/NM_variables.txt") #for nikki

##FILTER DATA BY VARIABLE CODES
##use txt file of codes to filter births dataset
selected_vars <- c(
  "caseid", "bidx", "v023", "v025", "v001", "v002", "v005", "v012", "v011", "v008",
  "b3", "b0", "b11", "b12", "b20", "b4", "b5", "b6", "b7", "m13", "m14", "m15", "m17",
  "m19", "m3a", "m3b", "m3c", "m3d", "m3e", "m3f", "m3g", "m3h", "m3i", "m3k", "m3n",
  "m45", "m66", "m70", "s1125", "v190", "v457", "v011", "v456", "v155", "v201", "v245",
  "v445", "v463aa", "v485a", "v501"
)

births_subset <- births_BR %>%
  dplyr::select(all_of(selected_vars)) %>%
  dplyr::mutate(v024 = births_BR$v024)

##CREATE NEW VARS
##mum_age_pregancy = mum dob - baby dob
births_subset$mum_age_pregnancy<- round(((births_subset$b3 - births_subset$v011)/12),0)

##how many years ago from interview was the child born?
## date of interview - date of birth of child
births_subset$test <- round(((births_subset$v008 - births_subset$b3)/12),0)

##FILTER BY BIRTHS IN LAST 3 YRS
##filter test column to < 3 to use only births in the lat 3 years
births_last3years <- births_subset %>% filter(test < 3)

##summary
summary(births_last3years)


##NEW VAR FOR NEONATAL MORTALITY
births_last3years <- births_last3years %>%
  mutate(neo_mort = ifelse(is.na(b6) | b6 > 130, 0, 1),
         age_at_death_days = ifelse(neo_mort == 1, b6 %% 100, NA))

##RECODE DELIVERY ASSISTANT COLUMNS
##combine the columns m3a-m3n (assistant at delivery) into one column with factors
##From DHS statistics manual - 
##During data collection respondents may mention more than one provider. The percent distribution by type of provider takes the highest type of provider from the list above and does not include other providers mentioned by the respondent
## re coded as per stats manual although these columns vary by country and not sure where to put mch aide
##currently put if (m3f) in with auxillery nurse as it seems like a community clinical role

births_last3years <- births_last3years %>%
  mutate(
    senior_delivery_attendant = case_when(
      m3a == 1 ~ "Doctor",
      m3b == 1 | m3d == 1 | m3e == 1 ~ "Nurse/midwife",
      m3c == 1 | m3f ==1  ~ "Auxiliary midwife",
      m3g == 1 ~ "Traditional birth attendant",
      m3h == 1 | m3i == 1 | m3k == 1 ~ "Relative/other",
      m3n == 1 ~ "No one",
      TRUE ~ NA_character_
    )
  )

##RECODE PLACE OF DELIVERY AS PER DHS MANUAL
births_last3years <- births_last3years %>%
  mutate(m15 = case_when(
    m15 %in% 20:29 ~ "Public sector health facility",
    m15 %in% 30:39 ~ "Private medical sector (non_NGO) health facility",
    m15 %in% 40:49 ~ "Private medical sector (NGO) health facility",
    m15 %in% 10:12 ~ "At home",
    m15 %in% c(13, 14, 96) ~ "Other"
  ))

##as numeric for birthweight
##9996 is not weighed and 9998 is don't no, will convert to NAs for analysis
births_last3years$m19 <- as.numeric(births_last3years$m19)
births_last3years <- births_last3years %>%
  mutate(m19 = ifelse(m19 %in% c(9996, 9998), NA, m19))

births_last3years <- births_last3years %>% mutate(
  m19_cat = case_when(
    m19 <2500 ~ "lowbw",
    m19 >=2500 ~ "notlowbw",
    TRUE ~ NA_character_ 
  )
)
## total pregancies cat
births_last3years <- births_last3years %>%
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


##mug age cat
births_last3years <- births_last3years %>%
  mutate(mum_age_pregnancy_cat = case_when(
    mum_age_pregnancy < 20 ~ "<20",
    mum_age_pregnancy >= 20 & mum_age_pregnancy < 25 ~ "20-24",
    mum_age_pregnancy >= 25 & mum_age_pregnancy < 35 ~ "25-34",
    mum_age_pregnancy >= 35  ~ "35+",
    TRUE ~ as.character(NA) # default case to return NA for values that don't fit any of the above conditions
  ))


##catagorise ANC to 0, 1-3, 4+
births_last3years<- births_last3years %>%
  mutate(ANC_visits = case_when(
    m14 == 0 ~ "None",
    between(m14, 1, 3) ~ "1-3",
    m14 >= 4 ~ "4+",
    TRUE ~ NA_character_  # For any other cases, set to NA
  ))

births_last3years <- births_last3years %>%
  mutate(m14 = case_when(
    m14 %in% c(0, 98, NA) ~ 0,
    m14 == 1 ~ 1,
    m14 == 2 ~ 2,
    m14 == 3 ~ 3,
    m14 == 4 ~ 4,
    m14 == 5 ~ 5,
    m14 == 6 ~ 6,
    m14 == 7 ~ 7,
    m14 == 8 ~ 8,
    m14 == 9 ~ 9,
    m14 == 10 ~ 10,
    m14 == 11 ~ 11,
    m14 == 12 ~ 12,
    m14 == 13 ~ 13,
    m14 == 14 ~ 14,
    m14 == 15 ~ 15,
    m14 == 16 ~ 16
  ))

####marital status

births_last3years<- births_last3years %>%
  mutate(v501_cat = case_when(
    v501 == 0 ~ "Never Union",
    between(v501, 1, 2) ~ "Married/living with partner",
    v501 >= 3 ~ "Divorced/Widowed or Seperated",
    TRUE ~ NA_character_  # For any other cases, set to NA
  ))

##catagorise BMI Underweight <18.5, Normal 18.5-24.9, overweight 25-29.9, obese 30+

##divide bmi v445 by 100 and 9998 as flagged as probably incorrect calcuation - will set to NA
births_last3years$v445 <- as.numeric(births_last3years$v445)/100

births_last3years <- births_last3years %>% mutate(
  v445_cat = case_when(
    v445 < 18.5 ~ "Underweight",
    (18.5 <= v445 & v445 < 25) ~ "Normal",
    (25 <= v445 & v445 < 30) ~ "Overweight",
    v445 >= 30 ~ "Obese",
    TRUE ~ NA_character_
  )
)

######b20 gestation at bith recode
births_last3years <- births_last3years %>% mutate(
  b20_cat = case_when(
    b20 <= 8 ~ "preterm",
    b20 > 8 ~"fullterm",
    TRUE ~ NA
  )
)

##pregancy losses cat
births_last3years <- births_last3years %>%
  mutate(v245_cat = case_when(
    v245 == 0 ~ "0",
    v245 == 1 ~ "1",
    v245 == 2 ~ "2",
    v245 >= 3 ~ ">=3",
    TRUE ~ as.character(NA) # default case to return NA for values that don't fit any of the above conditions
  ))

##remove uneeded recoded variables
births_clean <- births_last3years %>% 
  dplyr::select(-v011, -v008, -b3, -m3a, -m3b, -m3c, -m3d
                , -m3e, -m3f, -m3g, -m3h, -m3i, -m3k, -m3n
                , - test, -v012, -b6, -b7)

##reorder dataframe
# births_clean <- births_clean %>%
#   select(1:7, mum_age_pregnancy, b5, neo_mort, age_at_death_days, b4, b0, b20, b11, b12,
#          m13, ANC_visits, m15, senior_delivery_attendant, m17, m19, m45, m66, m70, s1125, everything())
# 
# 
# ##DATA DICTIONARY

#births_clean data dictionary 
variable_names_BR_clean <- names(births_clean)
variable_labels_BR_clean <- sapply(births_clean, function(x) {
  lbl <- attr(x, "label")
  if (is.null(lbl)) "No label" else lbl
})

dict_births_BR <- data.frame(
  variable = variable_names_BR_clean,
  description = variable_labels_BR_clean
)

######################## NIKKI SECTION - TABLE 1 CONTEXT ######################## 
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

count(births_clean$v201_cat) 

#By percentage
count_df <- count(births_clean$v201_cat)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

#By urban/rural (n)
count(births_clean$v201_cat[births_clean$v025 == 1]) #Urban
count(births_clean$v201_cat[births_clean$v025 == 2]) #Rural

no_pregnancies <- round(prop.table(svytable(~ v201_cat + v025, design = design) ,margin = 2) *100, 2)

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


literacy <- round(prop.table(svytable(~ v155 + v025, design = design) ,margin = 2) *100, 2)

#By urban/rural (n)
count(births_clean$v155[births_clean$v025 == 1]) #Urban
count(births_clean$v155[births_clean$v025 == 2]) #Rural

#8. Number of pregnancy losses (v245)

count_df <- count(births_clean$v245_cat)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

#By urban/rural (n)


count(births_clean$v245_cat[births_clean$v025 == 1]) #Urban
count(births_clean$v245_cat[births_clean$v025 == 2]) #Rural


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
count_df <- count(births_clean$b20_cat)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

#13. Post natal check (m70)
count_df <- count(births_clean$m70)
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

round(prop.table(svytable(~ m70 + v025, design = design) ,margin = 2) *100, 2)

#14. Age (mum_age_pregnancy)

count_df <- count(births_clean$mum_age_pregnancy_cat)     
count_df$percentage <- round((count_df$freq / 5619) * 100,2)
print(count_df)

round(prop.table(svytable(~ mum_age_pregnancy_cat + v025, design = design) ,margin = 2) *100, 2)


#15. Wealth v190
unique(births_clean$v190)
count(births_clean$v190)/5619
count(births_clean$v190[births_clean$v025 == 1]) #Urban
count(births_clean$v190[births_clean$v025 == 2]) #Rural
wealth <- round(prop.table(svytable(~ v190 + v025, design = design) ,margin = 2) *100, 2)


#16.marital status v501
unique(births_clean$v501_cat)
count(births_clean$v501_cat)/5619
count(births_clean$v501_cat[births_clean$v025 == 1]) #Urban
count(births_clean$v501_cat[births_clean$v025 == 2]) #Rural
round(prop.table(svytable(~ v501_cat + v025, design = design) ,margin = 2) *100, 2)

#17.smoking v463aa
unique(births_clean$v463aa)
count(births_clean$v463aa)/5619
count(births_clean$v463aa[births_clean$v025 == 1]) #Urban
count(births_clean$v463aa[births_clean$v025 == 2]) #Rural
round(prop.table(svytable(~ v463aa + v025, design = design) ,margin = 2) *100, 2)


#18.BMI - v445################################
unique(births_clean$v445_cat)
count(births_clean$v445_cat)$freq/5619
count(births_clean$v445_cat[births_clean$v025 == 1]) #Urban
count(births_clean$v445_cat[births_clean$v025 == 2]) #Rural
round(prop.table(svytable(~ v445_cat + v025, design = design) ,margin = 2) *100, 2)

#19.assistance at delivery - senior person attednign
unique(births_clean$senior_delivery_attendant)
count(births_clean$senior_delivery_attendant)$freq/5619
count(births_clean$senior_delivery_attendant[births_clean$v025 == 1]) #Urban
count(births_clean$senior_delivery_attendant[births_clean$v025 == 2]) #Rural
attendant <- round(prop.table(svytable(~ senior_delivery_attendant + v025, design = design) ,margin = 2) *100, 2)


#20. preterm b20#
unique(births_clean$b20_cat)
count(births_clean$b20_cat)#$freq/5619
count(births_clean$b20_cat[births_clean$v025 == 1]) #Urban
count(births_clean$b20_cat[births_clean$v025 == 2]) #Rural
round(prop.table(svytable(~ b20_cat + v025, design = design) ,margin = 2) *100, 2)


#21.bw m19
unique(births_clean$m19_cat)
count(births_clean$m19_cat)$freq/5619
count(births_clean$m19_cat[births_clean$v025 == 1]) #Urban
count(births_clean$m19_cat[births_clean$v025 == 2]) #Rural
round(prop.table(svytable(~ m19_cat + v025, design = design) ,margin = 2) *100, 2)


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
table(births_clean$m13, births_clean$neo_mort, useNA = "always")
#Most records are in 'no' neo_mort but there are some in 'yes' neo_mort, NAs in 'Yes' neo_mort is small but 50% of all yes records.
#"Don't know" or missing values on number of antenatal care visits and timing of first ANC are excluded from numerators but included in denominators.


#ANC_visits, m45, m66, 70 
table(births_clean$ANC_visits, births_clean$neo_mort, useNA = "always")
table(births_clean$m45,births_clean$neo_mort, useNA = "always")
table(births_clean$m66,births_clean$neo_mort, useNA = "always")
table(births_clean$m70,births_clean$neo_mort, useNA = "always")
table(births_clean$m70,births_clean$neo_mort,  births_clean$v025, useNA = "always")
#Result: Only half of neo_mort YES have been interviewed for these questions - NA = 35 
#NAs and 'dont know' are assumed to have not received in the intervertion

#m19 - bw in kg
table(births_clean$m19, births_clean$neo_mort, useNA = "always")
#Result: YES Neo_mort -  NA = 47

#v457, v456, v445 (Done by specific households, randomised)
table(births_clean$v457, births_clean$neo_mort, useNA = "always")
table(births_clean$v456, births_clean$neo_mort, useNA = "always")
table(births_clean$v445, births_clean$neo_mort, useNA = "always")
#Result: Neo_mort YES (126), 68 are recorded to be NA for v457, v456
#About 2782 aren't recorded as they were not interviewed 

table(teendata$contra_current, teendata$contra_future, useNA = "always") # can't identify exact numbers
round(svytable(~contra_future + v025, design = dhs, na.action = na.pass)/1e6,0) # this table is used to




