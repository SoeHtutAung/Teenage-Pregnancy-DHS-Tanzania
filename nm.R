#setwd("C:/Users/Saund/OneDrive/MSc_HDS_2022/Data Challenge")
##LOAD LIBRARIES
library(tidyverse)
library(haven)
library(survey)

##added this comment to test push/pull

##LOAD DATA
##read births data set
births_BR <- read_dta("TZBR82FL.DTA")

##load txt file with variable codes from code list
NM_variables <- readLines("NM_variables.txt")

##FILTER DATA BY VARIABLE CODES
##use txt file of codes to filter births dataset
births_subset <- births_BR %>% select(all_of(NM_variables))

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

##I used the summary to reduce the variable list and altered the txt file accordingly
##I kept BMI(v445) (even though 2782 records missing BMI
## Kept Hb/anaemia level(v456/v457) as we know not all women were checked
## m70 from NR (baby postnatal checked within 2 months - maybe not so useful as neo_mort defined as death within 1 month)
## m70 and m66 are regarding health checks for mother and baby were well completed
## however the other variables related to health checks (time and person who did check) had >50% missingness

##NEW VAR FOR NEONATAL MORTALITY
##create new column to indicate if neonatal death (130 is 30 days, so if b6 <= 130 it will be 1)
## the second line gives the age in days of the baby when it died
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

##remove uneeded recoded variables
births_clean <- births_last3years %>% 
  select(-v011, -v008, -b3, -m3a, -m3b, -m3c, -m3d
  , -m3e, -m3f, -m3g, -m3h, -m3i, -m3k, m3n
  , - test, -v012, -b6, -b7)

##reorder dataframe
births_clean <- births_clean %>%
  select(1:7, mum_age_pregnancy, b5, neo_mort, age_at_death_days, b4, b0, b20, b11, b12,
         m13, m14, m15, senior_delivery_attendant, m17, m19, m45, m66, m70, s1125, everything())


##CALCULATIONS
##how many neonatal deaths?
sum(births_clean$neo_mort == 1)

## table of unweighted frequencies for neonatal deaths in rural and urban areas
tbl <- table(births_clean$neo_mort,births_clean$v025)

##proportions by rural and urban births(unweighted)
proportions(tbl, margin = 2)

##using survey package
##create survey design to use in survey package calculations
## i used v001 for cluster id, but noticed Soe used v021, not sure what the differecne is
NM_weighted <- survey::svydesign(id=~v001, 
                       strata =~v023, 
                       weights=~v005, 
                       data=births_clean)

### Sample size individuals and weighted in urban and rural
sumtotal <- births_clean %>% group_by (v025) %>%
  summarise(total = n(), total_weight = round(sum(v005)/1e6,0)) 
sumNM <- births_clean %>% filter (neo_mort == 1) %>% group_by (v025) %>%
  summarize (NM = n(), NM_weight = round(sum(v005)/1e6,0))
samplesize_ur <- cbind(sumtotal,sumNM[,2:3]) # create dataframe
print(samplesize_ur) # print table

#weighted nm table rural vs urban
nm_table <- svytable(~neo_mort + v025, design = NM_weighted)
nm_table <- round(nm_table / 1e6,0)
print(nm_table)
proportions(nm_table, margin = 2)

##weighted nm rates urban vs rural
nmr_ur_weight <- svyby(~(neo_mort == 1), ~v025, NM_weighted, svymean, vartype=c("ci"))
nmr_ur <- nmr_ur_weight [,c(1,3,5,7)] # filter for proportion and CI
print(nmr_ur*1000)


                               