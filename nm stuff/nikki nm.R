
####LOAD LIBRARIES
library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(plyr)


#######LOAD DATA
##read births data set
births_BR <- read_dta("TZBR82FL.DTA")
births_BR <- read_dta("~/Downloads/UNICEF DATA/Births/TZBR82FL.DTA") # for nikki 

##load txt file with variable codes from code list
NM_variables <- readLines("NM_variables.txt")
NM_variables <- readLines("/Users/nikkiyu/Downloads/2 Data Challenge/unicef/nm stuff/NM_variables.txt") #for nikki

##FILTER DATA BY VARIABLE CODES
##use txt file of codes to filter births dataset
births_subset <- births_BR %>% select(all_of(NM_variables)) %>% mutate(v024 = births_BR$v024)



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
Lit_count_ur_nm <- svytable(~v155 + urban_rural, NM)
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


####################### RATES OF NEONATAL MORTALITY ############################
#Add v024 to "births_clean"


svyby(~(neo_death == "Yes"), ~v024 +v025, design, svymean, na.rm = TRUE)



                                      