
library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(plyr)
library(sf)
library(ggsci)
library(forestplot)


#######LOAD DATA
##read births data set
births_BR <- read_dta("C:/Users/Saund/OneDrive - London School of Hygiene and Tropical Medicine/TZDHS/Survey Data/Births/TZBR82FL.DTA")
births_BR <- read_dta("~/Downloads/UNICEF DATA/Births/TZBR82FL.DTA") # for nikki 

##load txt file with variable codes from code list
#NM_variables <- readLines("NM_variables.txt")
NM_variables <- readLines("/Users/nikkiyu/Downloads/2 Data Challenge/unicef/nm stuff/NM_variables.txt") #for nikki

##FILTER DATA BY VARIABLE CODES
##use txt file of codes to filter births dataset
selected_vars <- c(
  "caseid", "bidx", "v023", "v025", "v001", "v002", "v005", "v012", "v011", "v008",
  "b3", "b0", "b11", "b12","b19", "b20", "b4", "b5", "b6", "b7", "m13", "m14", "m15", "m17",
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


##FILTER BY BIRTHS IN LAST 3 YRS
##filter b19 column to < 36 to use only births in the lat 3 years
births_last3years <- births_subset %>% filter(b19 < 36)

##summary
summary(births_last3years)


##NEW VAR FOR NEONATAL MORTALITY
count(is.na(births_last3years$b6))
births_last3years <- births_last3years %>%
  mutate(neo_mort = ifelse(is.na(b6) | b6 > 130, 0, 1),
         age_at_death_days = ifelse(neo_mort == 1, b6 %% 100, NA))
table(births_last3years$neo_mort)

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
      m3b == 1 | m3d == 1 | m3e == 1 | m3c == 1 | m3f ==1 ~ "Nurse/midwife",
      m3g == 1 ~ "Traditional birth attendant",
      m3h == 1 | m3i == 1 | m3k == 1 ~ "Relative/other",
      m3n == 1 ~ "No one"
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


births_last3years <- births_last3years %>% mutate(
  m19_cat = case_when(
    m19 <2500 ~ "lowbw",
    m19 >=2500 & m19 < 9990 ~ "notlowbw",
    m19 %in% c(9996,9998) ~ NA
  )
)


count(births_last3years$m19_cat)
## total pregancies cat
births_last3years <- births_last3years %>%
  mutate(v201_cat = case_when(
    v201 == 0 ~ "0",
    v201 <=3 & v201 >= 1 ~ "1-3",
    v201 >= 4 ~ "4+"
  ))


##mug age cat
births_last3years <- births_last3years %>%
  mutate(mum_age_pregnancy_cat = case_when(
    mum_age_pregnancy < 20 ~ "<20",
    mum_age_pregnancy >= 20 & mum_age_pregnancy < 25 ~ "20-24",
    mum_age_pregnancy >= 25 & mum_age_pregnancy < 35 ~ "25-34",
    mum_age_pregnancy >= 35  ~ "35+"
  ))


##catagorise ANC to 0, 1-3, 4+
births_last3years<- births_last3years %>%
  mutate(ANC_visits = case_when(
    m14 %in% c(98, NA)  ~ "Don't Know or Missing",
    m14 == 0 ~ "None",
    between(m14, 1, 3) ~ "1-3",
    m14 >= 4 ~ "4+"
  ))

##From DHS stats manual - "Don't know" or missing values on number of antenatal care visits and timing of first ANC are excluded
##from numerators but included in denominators

births_last3years$ANC_visits %>% count()

####marital status

births_last3years<- births_last3years %>%
  mutate(v501_cat = case_when(
    v501 == 0 ~ "Never Union",
    between(v501, 1, 2) ~ "Married/living with partner",
    v501 >= 3 ~ "Divorced/Widowed or Seperated"
  ))



##catagorise BMI Underweight <18.5, Normal 18.5-24.9, overweight 25-29.9, obese 30+

##divide bmi v445 by 100 and 9998 as flagged as probably incorrect calcuation - will set to NA
births_last3years$v445 <- as.numeric(births_last3years$v445)/100

births_last3years <- births_last3years %>% mutate(
  v445_cat = case_when(
    v445 < 18.5 ~ "Underweight",
    (18.5 <= v445 & v445 < 25) ~ "Normal",
    (25 <= v445 & v445 < 30) ~ "Overweight",
    v445 >= 30 ~ "Obese"
  )
)

##From DHS Stats manual Women who were not weighed and measured and women whose values for weight and height were not
#recorded are excluded from both the denominator and the numerators


######b20 gestation at bith recode
births_last3years <- births_last3years %>% mutate(
  b20_cat = case_when(
    b20 <= 8 ~ "preterm",
    b20 > 8 ~"fullterm"
  )
)

##literacy cat v155
births_last3years <- births_last3years %>%
  mutate(v155_cat = case_when(
    v155 == 0 ~ "0",
    v155 == 1 ~ "1", 
    v155 == 2 ~ "2",
    v155 >=3 ~ "3"
  ))

##pregancy losses cat
births_last3years <- births_last3years %>%
  mutate(v245_cat = case_when(
    v245 == 0 ~ "0",
    v245 == 1 ~ "1",
    v245 >= 2 ~ "2"
  ))

##post nate check ##NAs counted as No's as per DHS stats manual
births_last3years <- births_last3years %>%
  mutate(m70_cat = case_when(
    m70 %in% c(0, NA) ~ "0",
    m70 == 1 ~ "1",
    m70 >= 3 ~ "Don't know or child died at facility"
  ))

births_last3years <- births_last3years %>%
  mutate(v463aa_cat = case_when(
    v463aa == 0 ~ "0",
    v463aa >= 1 ~ "1",
  ))


##remove uneeded recoded variables
births_clean <- births_last3years %>% 
  dplyr::select(-v011, -v008, -b3, -m3a, -m3b, -m3c, -m3d
                , -m3e, -m3f, -m3g, -m3h, -m3i, -m3k, -m3n
                , -v012, -b6, -b7)

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

######################## TABLE 1 CONTEXT - OR, Chi squared, P values ######################## 
#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)


##Urban/rural numbers

svytable(~v025, design = design)

#1. Number of pregnancies with anaemia (v457) 
count(births_clean$v457)

anaemia <- as.data.frame(svytable(~ v457, design = design)  %>% round(0))
anaemia$percent <- (anaemia[,2]/sum(anaemia[,2])*100) %>% round(2)
anaemia$prop<- round(prop.table(svytable(~ v457 + v025, design = design) ,margin = 2) *100, 2)
chi_anaemia <- (summary(svytable(~ v457 + v025, design = design), statistic = "Chisq"))
chi_anaemia$statistic$statistic
chi_anaemia$statistic$p.value
coef(svymean(~v457, design, na.rm = TRUE)%>% round(4))*100 
anaemia$totci<- confint(svymean(~factor(v457), design, na.rm = TRUE))%>% round(4)*100

births_clean$v457<- relevel(factor(births_clean$v457), ref = "4")

#2. Number of pregnancies ever told they had HT (s1125)
births_clean$s1125
count(births_clean$s1125)

hypt <- as.data.frame(svytable(~ s1125, design = design)  %>% round(0))
hypt$percent <- (hypt[,2]/sum(hypt[,2])*100) %>% round(2)
hypt$prop<- round(prop.table(svytable(~ s1125 + v025, design = design) ,margin = 2) *100, 2)
chi_hypt <- (summary(svytable(~ s1125 + v025, design = design), statistic = "Chisq"))
chi_hypt$statistic$statistic
chi_hypt$statistic$p.value

#3. Number of pregnancies with number of ANC visits 
unique(births_clean$ANC_visits)
count(births_clean$ANC_visits) 

anc <- as.data.frame(svytable(~ ANC_visits, design = design)  %>% round(0))
anc$percent <- (anc[,2]/sum(anc[,2])*100) %>% round(2)
anc$prop<- round(prop.table(svytable(~ ANC_visits + v025, design = design) ,margin = 2) *100, 2)
chi_anc <- (summary(svytable(~ ANC_visits + v025, design = design), statistic = "Chisq"))
chi_anc$statistic$statistic
chi_anc$statistic$p.value

births_clean$ANC_visits<- relevel(factor(births_clean$ANC_visits), ref = "4+")

#4. Number of pregnancies in pregnancy intervals - removed

#5. Number of pregnancies by mulitplicity of mothers (v201) 
count(births_clean$v201_cat) 
kids <- as.data.frame(svytable(~ v201_cat, design = design)  %>% round(0))
kids$percent <- (kids[,2]/sum(kids[,2])*100) %>% round(2)
kids$prop<- round(prop.table(svytable(~ v201_cat + v025, design = design) ,margin = 2) *100, 2)
chi_kids <- (summary(svytable(~ v201_cat + v025, design = design), statistic = "Chisq"))
chi_kids$statistic$statistic
chi_kids$statistic$p.value

#6. Mum age at pregnancy (mum_age_pregnancy)
count(births_clean$mum_age_pregnancy_cat)
mumage <- as.data.frame(svytable(~ mum_age_pregnancy_cat, design = design)  %>% round(0))
mumage$percent <- (mumage[,2]/sum(mumage[,2])*100) %>% round(2)
mumage$prop<- round(prop.table(svytable(~ mum_age_pregnancy_cat + v025, design = design) ,margin = 2) *100, 2)
chi_mumage <- (summary(svytable(~ mum_age_pregnancy_cat + v025, design = design), statistic = "Chisq"))
chi_mumage$statistic$statistic
chi_mumage$statistic$p.value

#7. Number by literacy (v155)
count(births_clean$v155_cat)
lit <- as.data.frame(svytable(~ v155_cat, design = design)  %>% round(0))
lit$percent <- (lit[,2]/sum(lit[,2])*100) %>% round(2)
lit$prop<- round(prop.table(svytable(~ v155_cat + v025, design = design) ,margin = 2) *100, 2)
chi_lit <- (summary(svytable(~ v155_cat + v025, design = design), statistic = "Chisq"))
chi_lit$statistic$statistic
chi_lit$statistic$p.value

births_clean$v155_cat<- relevel(factor(births_clean$v155_cat), ref = "2")

#8. Number of pregnancy losses (v245)
count(births_clean$v245_cat)
loss <- as.data.frame(svytable(~ v245_cat, design = design)  %>% round(0))
loss$percent <- (loss[,2]/sum(loss[,2])*100) %>% round(2)
loss$prop<- round(prop.table(svytable(~ v245_cat + v025, design = design) ,margin = 2) *100, 2)
chi_loss <- (summary(svytable(~ v245_cat + v025, design = design), statistic = "Chisq"))
chi_loss$statistic$statistic
chi_loss$statistic$p.value

births_clean$v245_cat<- relevel(factor(births_clean$v245_cat), ref = "0")

#9. Place of delivery for pregnancies (m15)
count(births_clean$m15)
pod <- as.data.frame(svytable(~ m15, design = design)  %>% round(0))
pod$percent <- (pod[,2]/sum(pod[,2])*100) %>% round(2)
pod$prop<- round(prop.table(svytable(~ m15 + v025, design = design) ,margin = 2) *100, 2)
chi_pod <- (summary(svytable(~ m15 + v025, design = design), statistic = "Chisq"))
chi_pod$statistic$statistic
chi_pod$statistic$p.value

births_clean$m15<- relevel(factor(births_clean$m15), ref = "Public sector health facility")

#10. Mode of delivery (m17)
count(births_clean$m17)
del <- as.data.frame(svytable(~ m17, design = design)  %>% round(0))
del$percent <- (del[,2]/sum(del[,2])*100) %>% round(2)
del$prop<- round(prop.table(svytable(~ m17 + v025, design = design) ,margin = 2) *100, 2)
chi_del <- (summary(svytable(~ m17 + v025, design = design), statistic = "Chisq"))
chi_del$statistic$statistic
chi_del$statistic$p.value


#11. Sex of the baby (b4) 
count(births_clean$b4)
sex <- as.data.frame(svytable(~ b4, design = design)  %>% round(0))
sex$percent <- (sex[,2]/sum(sex[,2])*100) %>% round(2)
sex$prop<- round(prop.table(svytable(~ b4 + v025, design = design) ,margin = 2) *100, 2)
chi_sex <- (summary(svytable(~ b4 + v025, design = design), statistic = "Chisq"))
chi_sex$statistic$statistic
chi_sex$statistic$p.value

#12. Gestation at birth (b20)
count(births_clean$b20_cat)
gest <- as.data.frame(svytable(~ b20_cat, design = design)  %>% round(0))
gest$percent <- (gest[,2]/sum(gest[,2])*100) %>% round(2)
gest$prop<- round(prop.table(svytable(~ b20_cat + v025, design = design) ,margin = 2) *100, 2)
chi_gest <- (summary(svytable(~ b20_cat + v025, design = design), statistic = "Chisq"))
chi_gest$statistic$statistic
chi_gest$statistic$p.value

#13. Post natal check (m70)
count(births_clean$m70_cat)
post <- as.data.frame(svytable(~ m70_cat, design = design)  %>% round(0))
post$percent <- (post[,2]/sum(post[,2])*100) %>% round(2)
post$prop<- round(prop.table(svytable(~ m70_cat + v025, design = design) ,margin = 2) *100, 2)
chi_post <- (summary(svytable(~ m70_cat + v025, design = design), statistic = "Chisq"))
chi_post$statistic$statistic
chi_post$statistic$p.value

#14. Wealth v190
count(births_clean$v190)
wlth <- as.data.frame(svytable(~ v190, design = design)  %>% round(0))
wlth$percent <- (wlth[,2]/sum(wlth[,2])*100) %>% round(2)
wlth$prop<- round(prop.table(svytable(~ v190 + v025, design = design) ,margin = 2) *100, 2)
chi_wlth <- (summary(svytable(~ v190 + v025, design = design), statistic = "Chisq"))
chi_wlth$statistic$statistic
chi_wlth$statistic$p.value


#16.marital status v501
count(births_clean$v501_cat)
wed <- as.data.frame(svytable(~ v501_cat, design = design)  %>% round(0))
wed$percent <- (wed[,2]/sum(wed[,2])*100) %>% round(2)
wed$prop<- round(prop.table(svytable(~ v501_cat + v025, design = design) ,margin = 2) *100, 2)
chi_wed <- (summary(svytable(~ v501_cat + v025, design = design), statistic = "Chisq"))
chi_wed$statistic$statistic
chi_wed$statistic$p.value

births_clean$v501_cat<- relevel(factor(births_clean$v501_cat), ref = "Married/living with partner")

#17.smoking v463aa
count(births_clean$v463aa_cat)
smo <- as.data.frame(svytable(~ v463aa_cat, design = design)  %>% round(0))
smo$percent <- (smo[,2]/sum(smo[,2])*100) %>% round(2)
smo$prop<- round(prop.table(svytable(~ v463aa_cat + v025, design = design) ,margin = 2) *100, 2)
chi_smo <- (summary(svytable(~ v463aa_cat + v025, design = design), statistic = "Chisq"))
chi_smo$statistic$statistic
chi_smo$statistic$p.value


#18.BMI - v445################################
count(births_clean$v445_cat)
bmi <- as.data.frame(svytable(~ v445_cat, design = design, na.action = na.omit)  %>% round(0))
bmi$percent <- (bmi[,2]/sum(bmi[,2])*100) %>% round(2)
bmi$prop<- round(prop.table(svytable(~ v445_cat + v025, design = design) ,margin = 2) *100, 2)
chi_bmi <- (summary(svytable(~ v445_cat + v025, design = design), statistic = "Chisq"))
chi_bmi$statistic$statistic
chi_bmi$statistic$p.value

#19.assistance at delivery - senior person attednign
count(births_clean$senior_delivery_attendant)
aad <- as.data.frame(svytable(~ senior_delivery_attendant, design = design)  %>% round(0))
aad$percent <- (aad[,2]/sum(aad[,2])*100) %>% round(2)
aad$prop<- round(prop.table(svytable(~ senior_delivery_attendant + v025, design = design) ,margin = 2) *100, 2)
chi_aad <- (summary(svytable(~ senior_delivery_attendant + v025, design = design), statistic = "Chisq"))
chi_aad$statistic$statistic
chi_aad$statistic$p.value

births_clean$v501_cat<- relevel(factor(births_clean$v501_cat), ref = "Married/living with partner")


#21.bw m19
count(births_clean$m19_cat)
lowbw <- as.data.frame(svytable(~ m19_cat, design = design)  %>% round(0))
lowbw$percent <- (lowbw[,2]/sum(lowbw[,2])*100) %>% round(2)
lowbw$prop<- round(prop.table(svytable(~ m19_cat + v025, design = design) ,margin = 2) *100, 2)
chi_lowbw <- (summary(svytable(~ m19_cat + v025, design = design), statistic = "Chisq"))
chi_lowbw$statistic$statistic
chi_lowbw$statistic$p.value

births_clean$m19_cat<- relevel(factor(births_clean$m19_cat), ref = "notlowbw")



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


##############!!!!!!!!!NIKKI CHOOSE THIS ONE - DIFFERENT MODELS WITH DIFFERENT CATAGOREIS#######################

##model2_2 ##NEW - SOCIO-DEMOGRAPHIC FACTORS
# mother's age at pregnancy
# literacy re-leveled
# marital status re-leveled
# Wealth quintile


model_2_2 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      mum_age_pregnancy + # mother's age at pregnancy - numeric
                      v155_cat + # literacy re-leveled - factor
                      v501_cat + # marital status re-leveled - factor
                      factor(v190), # Wealth quintile - not leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_2_2)[2])) %>% round(2)
print (exp (confint(model_2_2)[2, ])) %>% round(2)
print(summary(model_2_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)


##model maternal factors ##NEW##
model_3_2 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      mum_age_pregnancy + # mother's age at pregnancy - numeric
                      v155_cat + # literacy re-leveled - factor
                      v501_cat + # marital status re-leveled - factor
                      factor(v190) + # Wealth quintile - not leveled - factor
                      factor(v463aa_cat) + # smoking - factor
                      v445 + # BMI - numeric  
                      factor(s1125), #ever had hypertension - not leveled - factor 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_3_2)[2])) %>% round(2)
print (exp (confint(model_3_2)[2, ])) %>% round(2)
print(summary(model_3_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

##model pregnancy ##NEW

model_4_2 <-  svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                       mum_age_pregnancy + # mother's age at pregnancy - numeric
                       v155_cat + # literacy re-leveled - factor
                       v501_cat + # marital status re-leveled - factor
                       factor(v463aa_cat) + # smoking - factored
                       v445 + # BMI - numeric
                       factor(v190) + # Wealth quintile - not leveled - factored
                       factor(s1125) +  #ever had hypertension - not leveled - factor 
                       ANC_visits + #number of ANC visits - factor
                       v501_cat + #Assistance at delivery - levels + factor
                       m15 + #Place of delivery - levels + factor
                       factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                       m19, #Birth Weight - numeric
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_4_2)[2])) %>% round(2)
print (exp (confint(model_4_2)[2, ])) %>% round(2)
print(summary(model_4_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

##model previous preganacies######################

model_5_2 <-  svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                       mum_age_pregnancy + # mother's age at pregnancy - numeric
                       v155_cat + # literacy re-leveled - factor
                       v501_cat + # marital status re-leveled - factor
                       factor(v463aa_cat) + # smoking - factored
                       v445 + # BMI - numeric
                       factor(v190) + # Wealth quintile - not leveled - factored
                       factor(s1125) +  #ever had hypertension - not leveled - factor 
                       ANC_visits + #number of ANC visits - factor
                       v501_cat + #Assistance at delivery - levels + factor
                       m15 + #Place of delivery - levels + factor
                       factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                       m19+ #Birth Weight - numeric
                       v245_cat + # pregnancy losses - re-leveled + factored
                       v201, #Multiple pregnancies - not leveled + numeric
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_5_2)[2])) %>% round(2)
print (exp (confint(model_5_2)[2, ])) %>% round(2)
print(summary(model_5_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)


######################## Figure to illustrate table 2  ######################## 
# Assuming you have model_1, model_2, model_3, model_4, model_5 fitted

# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients <- c(0.73, 0.86, 1.50, 1.54, 1.55)
lower_ci <- c(0.46, 0.48, 0.58, 0.59, 0.38)
upper_ci <- c(1.16, 1.53, 3.91, 4.01, 4.09)
labels <- c("Model 1: NMR ~ Urban/Rural ", "Model 2: Model 1 + Sociodemographics", "Model 3: Model 2 + Maternal Health", "Model 4: Model 3 + Pregnacy", "Model 5: Model 4 + Previous pregnancies")

forest_data <- matrix(c(rep("", 5), lower_ci, coefficients, upper_ci), 
                      ncol = 4, byrow = FALSE)

nm_cat_plot <- forestplot(labeltext = labels, 
           mean = coefficients, 
           lower = lower_ci, 
           upper = upper_ci,
           xlab = "Coefficient Value",
           zero = 0,
           lineheight = "auto",
           boxsize = 0.5,
           col = fpColors(box = "darkred", line = "grey", summary = "royalblue"))


pdf("nm_cat_forestplot.pdf", width = 10, height = 5)
print(nm_cat_plot)
dev.off()



######################## Figure of individual ORs  ######################## 
#ORs of Rural / Urban coefficient OR changes by individual variables

### FOR FOREST PLOT#####

nm_forest <- as.data.frame(matrix(nrow=15,ncol=5))
names(nm_forest) <- c("each_factor","p-value","ORadj","OR_lower","OR_higher")

nm_forest[1,] <-
  c("Residence","NA",
    exp(coef(model_1))[2],
    exp(confint(model_1))[2,1],
    exp(confint(model_1))[2,2]
  )


#Creating individual models with v025 (which are significant from table one)
##1.AGE#######################

model_age <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      mum_age_pregnancy, # mother's age at pregnancy - numeric
                     design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_age)[2])) %>% round(2)
print (exp (confint(model_age)[2, ])) %>% round(2)
print(summary(model_age)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[2,] <-
  c("Age","NA",
    exp(coef(model_age))[2],
    exp(confint(model_age))[2,1],
    exp(confint(model_age))[2,2]
  )

##2.LITERACY##################

model_edu <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v155_cat, # literacy re-leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_edu)[2])) %>% round(2)
print (exp (confint(model_edu)[2, ])) %>% round(2)
print(summary(model_edu)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[3,] <-
  c("Literacy","NA",
    exp(coef(model_edu))[2],
    exp(confint(model_edu))[2,1],
    exp(confint(model_edu))[2,2]
  )


##3.MARITAL STATUS##############

model_mar <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v501_cat, # marital status re-leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_mar)[2])) %>% round(2)
print (exp (confint(model_mar)[2, ])) %>% round(2)
print(summary(model_mar)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[4,] <-
  c("Marital status","NA",
    exp(coef(model_mar))[2],
    exp(confint(model_mar))[2,1],
    exp(confint(model_mar))[2,2]
  )


##4.WEALTH#####################
model_wealth <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                         factor(v190), # Wealth quintile - not leveled - factored
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_wealth)[2])) %>% round(2)
print (exp (confint(model_wealth)[2, ])) %>% round(2)
print(summary(model_wealth)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[5,] <-
  c("Wealth","NA",
    exp(coef(model_wealth))[2],
    exp(confint(model_wealth))[2,1],
    exp(confint(model_wealth))[2,2]
  )

##5.SMOKING####################
model_smo <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      factor(v463aa_cat),# smoking - factored
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_smo)[2])) %>% round(2)
print (exp (confint(model_smo)[2, ])) %>% round(2)
print(summary(model_smo)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[6,] <-
  c("Smoking","NA",
    exp(coef(model_smo))[2],
    exp(confint(model_smo))[2,1],
    exp(confint(model_smo))[2,2]
  )


##6.HTN########################
model_htn <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      factor(s1125), #ever had hypertension - not leveled - factor 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_htn)[2])) %>% round(2)
print (exp (confint(model_htn)[2, ])) %>% round(2)
print(summary(model_htn)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[7,] <-
  c("Hypertension","NA",
    exp(coef(model_htn))[2],
    exp(confint(model_htn))[2,1],
    exp(confint(model_htn))[2,2]
  )


##7.BMI#########################
model_bmi <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v445,# BMI - numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_bmi)[2])) %>% round(2)
print (exp (confint(model_bmi)[2, ])) %>% round(2)
print(summary(model_bmi)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[8,] <-
  c("BMI","NA",
    exp(coef(model_bmi))[2],
    exp(confint(model_bmi))[2,1],
    exp(confint(model_bmi))[2,2]
  )


##8.ANC############
model_anc <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      ANC_visits,#number of ANC visits - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_anc)[2])) %>% round(2)
print (exp (confint(model_anc)[2, ])) %>% round(2)
print(summary(model_anc)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[9,] <-
  c("Antenatal Care Visits","NA",
    exp(coef(model_anc))[2],
    exp(confint(model_anc))[2,1],
    exp(confint(model_anc))[2,2]
  )

##9.POD###########
model_pod <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      m15,#Place of delivery - levels + factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_pod)[2])) %>% round(2)
print (exp (confint(model_pod)[2, ])) %>% round(2)
print(summary(model_pod)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[10,] <-
  c("Place of delivery","NA",
    exp(coef(model_pod))[2],
    exp(confint(model_pod))[2,1],
    exp(confint(model_pod))[2,2]
  )

##10.AAD#######
model_aad <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v501_cat,#Assistance at delivery - levels + factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_aad)[2])) %>% round(2)
print (exp (confint(model_aad)[2, ])) %>% round(2)
print(summary(model_aad)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[11,] <-
  c("Assistance at delivery","NA",
    exp(coef(model_aad))[2],
    exp(confint(model_aad))[2,1],
    exp(confint(model_aad))[2,2]
  )


##11. MOD ###########
model_mod <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      factor(m17), #Mode of delivery (C-section?) - not leveled + factored
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_mod)[2])) %>% round(2)
print (exp (confint(model_mod)[2, ])) %>% round(2)
print(summary(model_mod)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[12,] <-
  c("Mode of delivery","NA",
    exp(coef(model_mod))[2],
    exp(confint(model_mod))[2,1],
    exp(confint(model_mod))[2,2]
  )


##12. BW #############
model_bw <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                     m19, #Birth Weight - numeric
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_bw)[2])) %>% round(2)
print (exp (confint(model_bw)[2, ])) %>% round(2)
print(summary(model_bw)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[13,] <-
  c("Birthweight","NA",
    exp(coef(model_bw))[2],
    exp(confint(model_bw))[2,1],
    exp(confint(model_bw))[2,2]
  )


##13. TOTAL PREG###########
model_tot <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v201, #Multiple pregnancies - not leveled + numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_tot)[2])) %>% round(2)
print (exp (confint(model_tot)[2, ])) %>% round(2)
print(summary(model_tot)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[14,] <-
  c("Total preg","NA",
    exp(coef(model_tot))[2],
    exp(confint(model_tot))[2,1],
    exp(confint(model_tot))[2,2]
  )


##14. LOSSES #############
model_loss <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                       v245_cat,# pregnancy losses - re-leveled + factored
                       design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_loss)[2])) %>% round(2)
print (exp (confint(model_loss)[2, ])) %>% round(2)
print(summary(model_loss)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[15,] <-
  c("Prev preg losses","NA",
    exp(coef(model_loss))[2],
    exp(confint(model_loss))[2,1],
    exp(confint(model_loss))[2,2]
  )


# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients_ind <- as.numeric(nm_forest$ORadj)
lower_ci_ind <- as.numeric(nm_forest$OR_lower)
upper_ci_ind <- as.numeric(nm_forest$OR_higher)
labels_ind <- nm_forest$each_factor

base_data_nm <-
  tibble::tibble(
    mean  = as.numeric(nm_forest$ORadj),
    lower = as.numeric(nm_forest$OR_lower),
    upper = as.numeric(nm_forest$OR_higher),
    labels = nm_forest$each_factor, 
    OR = c("0.73", "0.73", "0.72", "0.75", "0.85", 
           "0.81", "0.73", "1.00", "0.68", "0.73", 
           "0.75", "0.77", "0.56", "0.68", "0.74"
    ))

#NEW FOREST DESIGN
base_data %>%
  forestplot(
    title = "OR for Rural vs Urban Rates of Neonatal Mortality by Confounding Factor",
    labeltext = c(labels, OR),
    clip = c(0.1,2.5),
    zero = 1,
    vertices = TRUE,
    lineheight = "auto",
    boxsize = 0.5
  ) %>%
  fp_add_header(
    labels = c("", "Factor"),
    OR = c("", "Residence OR")
  ) %>%
  fp_set_style(
    box = "#74B72E",
    line = "darkgreen",
    summary = "#74B72E",
    txt_gp = fpTxtGp(
      ticks = gpar(cex = 1),
      xlab  = gpar(cex = 1.5)))

#Saving forestplot
pdf("nm_ind_forestplot.pdf", width = 10, height = 5)
print(nm_ind_plot)
dev.off()


##OLD FOREST DESIGN
# forest_data_ind <-
#   matrix(
#     c(rep("", 15),
#       lower_ci,
#       coefficients,
#       upper_ci),
#     ncol = 4,
#     byrow = FALSE
#   )
# 
# nm_ind_plot<- forestplot(
#   labeltext = labels_ind , 
#   mean = coefficients_ind , 
#   lower = lower_ci_ind , 
#   upper = upper_ci_ind ,
#   clip = c(0.1,3.0),
#   xlab = "OR for Urban/Rural when accounting for each factor",
#   zero = 1,
#   lineheight = "auto",
#   boxsize = 0.5,
#   col = fpColors(box = "darkred", line = "grey", summary = "royalblue")
# )


################################### INTERACTION PLOT ################################### 

#Creating individual models with v025 (which are significant from table one)
##WITH INTERACTION IN FORMULA
##1.AGE#######################

model_age <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      mum_age_pregnancy, # mother's age at pregnancy - numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_age)[2])) %>% round(2)
print (exp (confint(model_age)[2, ])) %>% round(2)
print(summary(model_age)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[2,] <-
  c("Age","NA",
    exp(coef(model_age))[2],
    exp(confint(model_age))[2,1],
    exp(confint(model_age))[2,2]
  )

##2.LITERACY##################

model_edu <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      v155_cat, # literacy re-leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_edu)[2])) %>% round(2)
print (exp (confint(model_edu)[2, ])) %>% round(2)
print(summary(model_edu)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[3,] <-
  c("Literacy","NA",
    exp(coef(model_edu))[2],
    exp(confint(model_edu))[2,1],
    exp(confint(model_edu))[2,2]
  )


##3.MARITAL STATUS##############

model_mar <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      v501_cat, # marital status re-leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_mar)[2])) %>% round(2)
print (exp (confint(model_mar)[2, ])) %>% round(2)
print(summary(model_mar)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[4,] <-
  c("Marital status","NA",
    exp(coef(model_mar))[2],
    exp(confint(model_mar))[2,1],
    exp(confint(model_mar))[2,2]
  )


##4.WEALTH#####################
model_wealth <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                         factor(v190), # Wealth quintile - not leveled - factored
                       design = design, 
                       family = quasibinomial(), 
                       na.action = na.omit)

print(exp(coef(model_wealth)[2])) %>% round(2)
print (exp (confint(model_wealth)[2, ])) %>% round(2)
print(summary(model_wealth)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[5,] <-
  c("Wealth","NA",
    exp(coef(model_wealth))[2],
    exp(confint(model_wealth))[2,1],
    exp(confint(model_wealth))[2,2]
  )

##5.SMOKING####################
model_smo <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      factor(v463aa_cat),# smoking - factored
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_smo)[2])) %>% round(2)
print (exp (confint(model_smo)[2, ])) %>% round(2)
print(summary(model_smo)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[6,] <-
  c("Smoking","NA",
    exp(coef(model_smo))[2],
    exp(confint(model_smo))[2,1],
    exp(confint(model_smo))[2,2]
  )


##6.HTN########################
model_htn <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      factor(s1125), #ever had hypertension - not leveled - factor 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_htn)[2])) %>% round(2)
print (exp (confint(model_htn)[2, ])) %>% round(2)
print(summary(model_htn)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[7,] <-
  c("Hypertension","NA",
    exp(coef(model_htn))[2],
    exp(confint(model_htn))[2,1],
    exp(confint(model_htn))[2,2]
  )


##7.BMI#########################
model_bmi <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      v445,# BMI - numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_bmi)[2])) %>% round(2)
print (exp (confint(model_bmi)[2, ])) %>% round(2)
print(summary(model_bmi)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[8,] <-
  c("BMI","NA",
    exp(coef(model_bmi))[2],
    exp(confint(model_bmi))[2,1],
    exp(confint(model_bmi))[2,2]
  )


##8.ANC############
model_anc <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      ANC_visits,#number of ANC visits - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_anc)[2])) %>% round(2)
print (exp (confint(model_anc)[2, ])) %>% round(2)
print(summary(model_anc)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[9,] <-
  c("Antenatal Care Visits","NA",
    exp(coef(model_anc))[2],
    exp(confint(model_anc))[2,1],
    exp(confint(model_anc))[2,2]
  )

##9.POD###########
model_pod <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      m15,#Place of delivery - levels + factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_pod)[2])) %>% round(2)
print (exp (confint(model_pod)[2, ])) %>% round(2)
print(summary(model_pod)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[10,] <-
  c("Place of delivery","NA",
    exp(coef(model_pod))[2],
    exp(confint(model_pod))[2,1],
    exp(confint(model_pod))[2,2]
  )

##10.AAD#######
model_aad <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      v501_cat,#Assistance at delivery - levels + factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_aad)[2])) %>% round(2)
print (exp (confint(model_aad)[2, ])) %>% round(2)
print(summary(model_aad)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[11,] <-
  c("Assistance at delivery","NA",
    exp(coef(model_aad))[2],
    exp(confint(model_aad))[2,1],
    exp(confint(model_aad))[2,2]
  )


##11. MOD ###########
model_mod <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      factor(m17), #Mode of delivery (C-section?) - not leveled + factored
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_mod)[2])) %>% round(2)
print (exp (confint(model_mod)[2, ])) %>% round(2)
print(summary(model_mod)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[12,] <-
  c("Mode of delivery","NA",
    exp(coef(model_mod))[2],
    exp(confint(model_mod))[2,1],
    exp(confint(model_mod))[2,2]
  )


##12. BW #############
model_bw <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                     m19, #Birth Weight - numeric
                   design = design, 
                   family = quasibinomial(), 
                   na.action = na.omit)

print(exp(coef(model_bw)[2])) %>% round(2)
print (exp (confint(model_bw)[2, ])) %>% round(2)
print(summary(model_bw)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[13,] <-
  c("Birthweight","NA",
    exp(coef(model_bw))[2],
    exp(confint(model_bw))[2,1],
    exp(confint(model_bw))[2,2]
  )


##13. TOTAL PREG###########
model_tot <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                      v201, #Multiple pregnancies - not leveled + numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_tot)[2])) %>% round(2)
print (exp (confint(model_tot)[2, ])) %>% round(2)
print(summary(model_tot)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[14,] <-
  c("Total preg","NA",
    exp(coef(model_tot))[2],
    exp(confint(model_tot))[2,1],
    exp(confint(model_tot))[2,2]
  )


##14. LOSSES #############
model_loss <- svyglm(factor(neo_mort) ~ factor(v025) * # rural/urban
                       v245_cat,# pregnancy losses - re-leveled + factored
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_loss)[2])) %>% round(2)
print (exp (confint(model_loss)[2, ])) %>% round(2)
print(summary(model_loss)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[15,] <-
  c("Prev preg losses","NA",
    exp(coef(model_loss))[2],
    exp(confint(model_loss))[2,1],
    exp(confint(model_loss))[2,2]
  )


# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients_ind <- as.numeric(nm_forest$ORadj)
lower_ci_ind <- as.numeric(nm_forest$OR_lower)
upper_ci_ind <- as.numeric(nm_forest$OR_higher)
labels_ind <- nm_forest$each_factor

base_data <-
  tibble::tibble(
    mean  = nm_forest$ORadj,
    lower = nm_forest$OR_lower,
    upper = nm_forest$OR_higher,
    labels = nm_forest$each_factor
  )


forest_data_ind <-
  matrix(
    c(rep("", 15),
      lower_ci,
      coefficients,
      upper_ci),
    ncol = 4,
    byrow = FALSE
  )

nm_ind_plot<- forestplot(
  labeltext = labels_ind , 
  mean = coefficients_ind , 
  lower = lower_ci_ind , 
  upper = upper_ci_ind ,
  clip = c(0.1,3.0),
  xlab = "OR for Urban/Rural when accounting for each factor",
  zero = 1,
  lineheight = "auto",
  boxsize = 0.5,
  col = fpColors(box = "darkred", line = "grey", summary = "royalblue")
)

###################################

pdf("nm_ind_forestplot.pdf", width = 10, height = 5)
print(nm_ind_plot)
dev.off()


