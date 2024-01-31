
##LOAD LIBRARIES
library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(plyr)


##LOAD DATA
##read births data set
births_BR <- read_dta("TZBR82FL.DTA")
births_BR <- read_dta("~/Downloads/UNICEF DATA/Births/TZBR82FL.DTA") # for nikki 

##load txt file with variable codes from code list
NM_variables <- readLines("NM_variables.txt")
NM_variables <- readLines("/Users/nikkiyu/Downloads/2 Data Challenge/unicef/nm stuff/NM_variables.txt") #for nikki

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
  mutate(neo_mort = ifelse(is.na(b6) | b6 > 130, "No", "Yes"),
         age_at_death_days = ifelse(neo_mort == "Yes", b6 %% 100, NA))

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

##divide bmi v445 by 100 and 9998 as flagged
births_last3years$v445 <- as.numeric(births_last3years$v445)/100

##catagorise ANC to 0, 1-3, 4-6, 7+ (m14)? can change this!
births_last3years<- births_last3years %>%
  mutate(ANC_visits = case_when(
    m14 == 0 ~ "None",
    between(m14, 1, 3) ~ "1-3",
    between(m14, 4, 6) ~ "4-6",
    m14 >= 7 ~ "7+",
    TRUE ~ NA_character_  # For any other cases, set to NA
  ))


##remove uneeded recoded variables
births_clean <- births_last3years %>% 
  select(-v011, -v008, -b3, -m3a, -m3b, -m3c, -m3d
         , -m3e, -m3f, -m3g, -m3h, -m3i, -m3k, -m3n
         , - test, -v012, -b6, -b7, -m14)

##reorder dataframe
births_clean <- births_clean %>%
  select(1:7, mum_age_pregnancy, b5, neo_mort, age_at_death_days, b4, b0, b20, b11, b12,
         m13, ANC_visits, m15, senior_delivery_attendant, m17, m19, m45, m66, m70, s1125, everything())

##DATA DICTIONARY

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

##CALCULATIONS
##how many neonatal deaths?
sum(births_clean$neo_mort == "Yes")

## table of unweighted frequencies for neonatal deaths in rural and urban areas
tbl <- table(births_clean$neo_mort,births_clean$v025)

##proportions by rural and urban births(unweighted)
proportions(tbl, margin = 2)

#see DHS weighted statistics, need to divide by 1000000
births_clean$wt <- births_clean$v005/1000000
births_clean <- births_clean %>%
  mutate(urban_rural = factor(v025, levels = c(1, 2), labels = c("Urban", "Rural")))


##using survey package
##create survey design to use in survey package calculations
## i used v001 for cluster id, but noticed Soe used v021, not sure what the difference is

all_weighted <- survey::svydesign(id=~v001, 
                                  strata =~v023, 
                                  weights=~wt,
                                  data= births_clean)

##used plyr as this example was in the DHS statistic manual
##however they used survey package for more complex stats which is what we will need
#mean mum age at pregnancy
ddply(births_clean,~urban_rural,summarise,mean=weighted.mean(mum_age_pregnancy, wt))


#weighted nm table rural vs urban
nm_table <- svytable(~neo_mort + urban_rural, design = all_weighted)
print(nm_table)


##weighted nm rates urban vs rural
nmr_weight <- svyby(~(neo_mort == "Yes"), ~urban_rural, all_weighted, svymean, vartype=c("ci"))
nmr <- nmr_weight [,c(1,3,5,7)] # filter for proportion and CI
print(nmr[,2:3]*1000)

plot(nmr_weight)

subset
NM <- subset(all_weighted,neo_mort == "Yes")
survived <- subset(all_weighted,neo_mort == "No")

##AGE 
##quantiles by urban/rural
age_quantiles_ur_nm <- svyby(~mum_age_pregnancy, ~ urban_rural + neo_mort, all_weighted, svyquantile, c(0,0.25, 0.5, 0.75,1), ci = TRUE)

##use subsets to make boxplot of ages 
svyboxplot(mum_age_pregnancy ~ urban_rural, NM,
           main="Weighted mothers age for those who had a neonatal mortality by urban and rural areas", ylab = "Age")
svyboxplot(mum_age_pregnancy ~ urban_rural, survived,
           main="Weighted mothers age for those without a neonatal mortality by urban and rural areas", ylab = "Age")

##subsets for hist
svyhist(~mum_age_pregnancy, NM)
svyhist(~mum_age_pregnancy, survived)



##Place of delivery (m15) and no_mort
##POD count by urban/rural
POD_count_ur_nm<- svytable(~m15 + urban_rural, NM)
POD_count_ur_survived<- svytable(~m15 + urban_rural, survived)


as.data.frame(POD_count_ur_nm)
as.data.frame(POD_count_ur_survived)

# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(POD_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=m15)) +
  geom_bar(stat="identity")

ggplot(data= as.data.frame(POD_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=m15)) +
  geom_bar(stat="identity")

##Literacy
Lit_count_ur_nm<- svytable(~v155 + urban_rural, NM)
Lit_count_ur_survived<- svytable(~v155 + urban_rural, survived)


as.data.frame(Lit_count_ur_nm)
as.data.frame(Lit_count_ur_survived)

# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(Lit_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=v155)) +
  geom_bar(stat="identity")

ggplot(data= as.data.frame(Lit_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=v155)) +
  geom_bar(stat="identity")


##ANC visits - maybe need to do it grouped???? i.e 1-4, 5-10, 10+??
ANC_count_ur_nm<- svytable(~ANC_visits + urban_rural, NM)
ANC_count_ur_survived<- svytable(~ANC_visits + urban_rural, survived)


as.data.frame(ANC_count_ur_nm)
as.data.frame(ANC_count_ur_survived)

# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(ANC_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=ANC_visits)) +
  geom_histogram(stat="identity")

ggplot(data= as.data.frame(ANC_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=ANC_visits)) +
  geom_histogram(stat="identity")



##Assistance at delivery
Assist_count_ur_nm<- svytable(~senior_delivery_attendant + urban_rural, NM)
Assist_count_ur_survived<- svytable(~senior_delivery_attendant + urban_rural, survived)


as.data.frame(Assist_count_ur_nm)
as.data.frame(Assist_count_ur_survived)

# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(Assist_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=senior_delivery_attendant)) +
  geom_bar(stat="identity")

ggplot(data= as.data.frame(Assist_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=senior_delivery_attendant)) +
  geom_bar(stat="identity")

##Mode of delivery (m17)
md_count_ur_nm<- svytable(~m17 + urban_rural, NM)
md_count_ur_survived<- svytable(~m17 + urban_rural, survived)


as.data.frame(md_count_ur_nm)
as.data.frame(md_count_ur_survived)

# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(md_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=m17)) +
  geom_bar(stat="identity")

ggplot(data= as.data.frame(md_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=m17)) +
  geom_bar(stat="identity")

##Birth weight (m19)
##quantiles by urban/rural
##need to remove rows with NA
bw_quantiles_ur_nm <- svyby(~m19, ~urban_rural + neo_mort, design = all_weighted, 
                            svyquantile, c(0, 0.25, 0.5, 0.75, 1), ci = TRUE)
##use subsets to make boxplot of ages 
svyboxplot(m19 ~ urban_rural, NM,
           main="Weighted bw for those who had a neonatal mortality by urban and rural areas", ylab = "BW")
svyboxplot(m19 ~ urban_rural, survived,
           main="Weighted bw for those without a neonatal mortality by urban and rural areas", ylab = "BW")

##subsets for hist
svyhist(~m19, NM)
svyhist(~m19, survived)

##sex of baby
sex_count_ur_nm<- svytable(~b4 + urban_rural, NM)
sex_count_ur_survived<- svytable(~b4 + urban_rural, survived)


# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(sex_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=b4)) +
  geom_bar(stat="identity")

ggplot(data= as.data.frame(sex_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=b4)) +
  geom_bar(stat="identity")

##duration of preganacy (b20)
gest_quantiles_ur_nm <- svyby(~b20, ~urban_rural + neo_mort, design = all_weighted, 
                              svyquantile, c(0, 0.25, 0.5, 0.75, 1), ci = TRUE)
##use subsets to make boxplot of ages 
svyboxplot(b20 ~ urban_rural, NM,
           main="Weighted gestation for those who had a neonatal mortality by urban and rural areas", ylab = "Gestation")
svyboxplot(b20 ~ urban_rural, survived,
           main="Weighted gestation for those without a neonatal mortality by urban and rural areas", ylab = "Gestation")

##subsets for hist
svyhist(~b20, NM)
svyhist(~b20, survived)

##baby health checked after delivery (m70)
check_count_ur_nm<- svytable(~m70 + urban_rural, NM)
check_count_ur_survived<- svytable(~m70 + urban_rural, survived)


# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(check_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=m70)) +
  geom_bar(stat="identity")

ggplot(data= as.data.frame(check_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=m70)) +
  geom_bar(stat="identity")


##tiwn? b0
twin_count_ur_nm<- svytable(~b0 + urban_rural, NM)
twin_count_ur_survived<- svytable(~b0 + urban_rural, survived)


# Stacked barplot with multiple groups for place of delivery
ggplot(data= as.data.frame(twin_count_ur_nm) , aes(x=urban_rural, y=Freq, fill=b0)) +
  geom_bar(stat="identity")

ggplot(data= as.data.frame(twin_count_ur_survived) , aes(x=urban_rural, y=Freq, fill=b0)) +
  geom_bar(stat="identity")
births_clean$b0

##BMI v445
bmi_quantiles_ur_nm <- svyby(~v445, ~urban_rural + neo_mort, design = all_weighted, 
                             svyquantile, c(0, 0.25, 0.5, 0.75, 1), ci = TRUE)
##use subsets to make boxplot of ages 
svyboxplot(v445 ~ urban_rural, NM,
           main="BMI for those who had a neonatal mortality by urban and rural areas", ylab = "BMI")
svyboxplot(v445 ~ urban_rural, survived,
           main="BMI for those without a neonatal mortality by urban and rural areas", ylab = "BMI")

##subsets for hist
svyhist(~v445, NM)
svyhist(~v445, survived)


##v201 number of children ever born
mp_quantiles_ur_nm <- svyby(~v201, ~urban_rural + neo_mort, design = all_weighted, 
                            svyquantile, c(0, 0.25, 0.5, 0.75, 1), ci = TRUE)
##use subsets to make boxplot of ages 
svyboxplot(v201 ~ urban_rural, NM,
           main="No. of pregancies for those who had a neonatal mortality by urban and rural areas", ylab = "Pregancies")
svyboxplot(v201 ~ urban_rural, survived,
           main="No. of pregancies for those without a neonatal mortality by urban and rural areas", ylab = "Pregnanies")

##subsets for hist
svyhist(~v201, NM)
svyhist(~v201, survived)

##v245 pregnancy losses
pl_quantiles_ur_nm <- svyby(~v245, ~urban_rural + neo_mort, design = all_weighted, 
                            svyquantile, c(0, 0.25, 0.5, 0.75, 1), ci = TRUE)
##use subsets to make boxplot of ages 
svyboxplot(v245 ~ urban_rural, NM,
           main="No. of pregancies lost for those who had a neonatal mortality by urban and rural areas", ylab = "Pregancies")
svyboxplot(v245 ~ urban_rural, survived,
           main="No. of pregancies lost for those without a neonatal mortality by urban and rural areas", ylab = "Pregnanies")

##subsets for hist
svyhist(~v245, NM)
svyhist(~v245, survived)


