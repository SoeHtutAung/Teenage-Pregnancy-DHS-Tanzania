##### Setting up births_clean dataset ##### 

#Load library
library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(plyr)
library(sf)
library(ggsci)
library(forestplot)

#######LOAD DATA
#Read births data set
births_BR <- read_dta("TZBR82FL.DTA")

#Filter variable code data
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

#Create new variables
##mum_age_pregancy = mum dob - baby dob
births_subset$mum_age_pregnancy<- round(((births_subset$b3 - births_subset$v011)/12),0)

##Filter births from the past 3 years
##filter b19 column to < 36 to use only births in the lat 3 years
births_last3years <- births_subset %>% filter(b19 < 36)

##summary
summary(births_last3years)

##New variable for neonatal mortality
count(is.na(births_last3years$b6))
births_last3years <- births_last3years %>%
  mutate(neo_mort = ifelse(is.na(b6) | b6 > 130, 0, 1),
         age_at_death_days = ifelse(neo_mort == 1, b6 %% 100, NA))
table(births_last3years$neo_mort)


#Recode delivery assistant column
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

#Recode place of delivery as per DHS manual
births_last3years <- births_last3years %>%
  mutate(m15 = case_when(
    m15 %in% 20:29 ~ "Public sector health facility",
    m15 %in% 30:39 ~ "Private medical sector (non_NGO) health facility",
    m15 %in% 40:49 ~ "Private medical sector (NGO) health facility",
    m15 %in% 10:12 ~ "At home",
    m15 %in% c(13, 14, 96) ~ "Other"
  ))

##As numeric for birthweight
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

## Total pregancies cat
births_last3years <- births_last3years %>%
  mutate(v201_cat = case_when(
    v201 == 0 ~ "0",
    v201 <=3 & v201 >= 1 ~ "1-3",
    v201 >= 4 ~ "4+"
  ))

##Age category
births_last3years <- births_last3years %>%
  mutate(mum_age_pregnancy_cat = case_when(
    mum_age_pregnancy < 20 ~ "<20",
    mum_age_pregnancy >= 20 & mum_age_pregnancy < 25 ~ "20-24",
    mum_age_pregnancy >= 25 & mum_age_pregnancy < 35 ~ "25-34",
    mum_age_pregnancy >= 35  ~ "35+"
  ))


##Catagorise ANC to 0, 1-3, 4+
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

####Marital status
births_last3years<- births_last3years %>%
  mutate(v501_cat = case_when(
    v501 == 0 ~ "Never Union",
    between(v501, 1, 2) ~ "Married/living with partner",
    v501 >= 3 ~ "Divorced/Widowed or Seperated"
  ))


##Catagorise BMI Underweight <18.5, Normal 18.5-24.9, overweight 25-29.9, obese 30+

##Divide bmi v445 by 100 and 9998 as flagged as probably incorrect calcuation - will set to NA
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
#Recorded are excluded from both the denominator and the numerators


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

##### CREATING DATA DICTIONARY ##### 

#Births_clean data dictionary 
variable_names_BR_clean <- names(births_clean)
variable_labels_BR_clean <- sapply(births_clean, function(x) {
  lbl <- attr(x, "label")
  if (is.null(lbl)) "No label" else lbl
})

dict_births_BR <- data.frame(
  variable = variable_names_BR_clean,
  description = variable_labels_BR_clean
)