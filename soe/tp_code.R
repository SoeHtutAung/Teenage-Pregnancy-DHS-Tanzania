library(dplyr)
#install.packages("labelled") for labelled data
library(labelled)
# install.packages("survey") # for survey design
library(survey)

# Extract
## extract selected variables from IR
data <- ir %>% select(1:8, # ID and weight
                      c(v012,v013, # age
                        v021,v023, # cluster(psu), strata (region*ur)
                        v024,v025, # place
                        v106, v190, v731, # edu, wealth, employment
                        v201, v213, v245, # pregnancy history for calculation
                        v225, v485a, # wantedness, alcohol,  
                        v304_01:v304_20, v313, v362,# contraception knowledge, current use, intention to use in future
                        v501, #current marital status
                        v701, v730,# partner age, education
                        p32_01:p32_20, # pregnancy outcomes
                        v531,v511, # Age at first sex/marriage
                        v384a:v384h, # exposure to family planning messages
                        v467b, v467c, v467d, v467f # problem access to health services
                      ))

## extract selected variables from HR
hh_data <- hr %>% select(1:7,
                         c(hv109_01:hv109_56,# highest education attained in the HH
                           hv005, hv021,hv023, # weight, cluster(psu), strata (region*ur)
                           hv012, # total number of HH members
                           hv219 # sex of hh head
                         )) 

# Recode
### teenage pregnancy recode - outcome 1 = Yes, 0 = No 
data <- data %>% mutate(
  v013 = factor(v013),
  teen_preg = ifelse(v013 == 1 & (v201 > 0 | v245 > 0 | v213 == 1), 1, 0))
data %>% group_by(teen_preg) %>% summarise(n = n()) # check grouping

### current employment - covariate 1 = Yes, 0 = No
data <- data %>% mutate(
  employ_current = ifelse(v731 %in% c(2,3), 1, 0))
table(data$employ_current,data$v731) # check grouping

### knowledge on any contraceptive method - covariate 1 = Yes, 0 = No
data <- data %>% mutate(
  contra_know_any = ifelse(v304_01== 1 | v304_02== 1 | v304_03== 1 | v304_04== 1 | v304_05== 1 |
                             v304_06== 1 | v304_07== 1 | v304_08== 1 | v304_09== 1 | v304_10== 1 |
                             v304_11== 1 | v304_12== 1 | v304_13== 1 | v304_14== 1 | v304_15== 1 |
                             v304_16== 1 | v304_17== 1 | v304_18== 1 | v304_19== 1 | v304_20== 1, 1, 0))
data %>% group_by(contra_know_any) %>% summarise(n = n()) # check grouping

### current contraceptive use - covariate 1 = Yes, 0 = No
data <- data %>% mutate(
  contra_current = ifelse(v313 %in% c(1,2,3), 1, 0))
table(data$contra_current,data$v313) # check grouping

### intention to use contraception in the future - covariate 1 = Yes, 0 = No
data <- data %>% mutate(
  contra_future = ifelse(v362 %in% c(2,4), 1, 0))
table(data$contra_future,data$v362) # check grouping

### marital status recode from 5 to 3 categories
data <- data %>% mutate(
  marital = recode(v501, "0" = "0", "1" = "1", "2" = "1", "3" = "2", "4" = "2", "5" = "2"))
table(data$marital,data$v501) # check grouping

### age at first sex recode to recode 0, 97, 98 to na
data <- data %>% 
  mutate(v531 = ifelse(v531 %in% c(0, 97, 98), NA, v531))
summary(data$v531) # check range

### partner education recode 8 to na
data <- data %>% 
  mutate(v701 = ifelse(v701 == 8, NA, v701))
data %>% group_by(v701) %>% summarize (n = n()) # check range

### access to informaiton about family planning - covariate 1 = Yes, 0 = No
data <- data %>% mutate(
  fp_info = ifelse(v384a == 1 | v384b == 1 | v384c == 1 | v384d == 1 | 
                     v384e == 1 | v384f == 1 | v384g == 1 | v384h == 1, 1, 0))
data %>% group_by(fp_info) %>% summarise(n = n()) # check grouping
# alternative: data %>% group_by(v384a,v384b,v384c,v384d,v384e,v384f,v384g,v384h) %>% summarize (count = n())

### recode hh education 98 to na
hh_data <- hh_data %>%
  mutate(across(starts_with("hv109_"), ~ifelse(. == 98, NA, .)))
summary(hh_data$hv109_01) # check range

### recode highest education attainment of a household
hh_data <- hh_data %>%
  rowwise() %>% # rowwise (household-wise) calculation
  mutate(edu = ifelse(all(is.na(c_across(hv109_01:hv109_56))), NA, # return NA if all NA
                      pmax(c_across(hv109_01:hv109_56), na.rm = TRUE))) # return max value of all columns

hh_data %>% group_by(edu) %>% summarise(n = n()) # check grouping

# join dataset
join_data <- left_join(data, select(hh_data, hv001, hv002, hv005, hv021, hv023, hv012, hv219, edu), 
                       by = c("v001" = "hv001", "v002" = "hv002")) # join by cluster and household numbers
View(join_data) # check join

# summary statistics
## sample size
### Sample size individuals and weighted in urban and rural
sumtotal <- data %>% group_by (v025) %>%
  summarise(total = n(), total_weight = round(sum(v005)/1e6,0)) 
sumteen <- data %>% filter (v013 ==1) %>% group_by (v025) %>%
  summarize (teen = n(), teen_weight = round(sum(v005)/1e6,0))
samplesize_ur <- cbind(sumtotal,sumteen[,2:3]) # create dataframe
print(samplesize_ur) # print table

### Sample size individuals and weighted in regions
sumtotal_region <- data %>% group_by (v024) %>%
  summarise(total = n(), total_weight = round(sum(v005)/1e6,0)) 
sumteen_region <- data %>% filter (v013 ==1) %>% group_by (v024) %>%
  summarize (teen = n(), teen_weight = round(sum(v005)/1e6,0))
samplesize_region <- cbind(sumtotal_region,sumteen_region[,2:3]) # create dataframe
print(samplesize_region) # print table

## teenage pregnancy rate
### survey design
teendata <- data %>% filter(v013 == 1) # create dataset with only teenagers
dhs <- svydesign(id=~v021, strata =~v023, weights=~v005, data=teendata) # create design

### teenage pregnancy rate in regions
tp_prop_region_raw <- svyby(~(teen_preg == 1), ~v024, dhs, svymean, vartype=c("se","ci"))
tp_prop_region <- tp_prop_region_raw [,c(1,3)]
### teenage pregnancy rate in urban and rural
svytable(~teen_preg + v025, design = dhs) # summary figure
tp_prop_ur_raw <- svyby(~(teen_preg == 1), ~v025, dhs, svymean, vartype=c("ci"))
tp_prop_ur <- tp_prop_ur_raw [,c(1,3,5,7)] # filter for proportion and CI

## socio-demographic characteristics
### age by urban and rural
age_ur <- svyby(formula = ~v012, by = ~v025, design = dhs, FUN = svymean, vartype=c("ci"))
t_test_age <- svyttest(v012 ~ v025, design = dhs) # t-test for pvalue

### education
svytable(~v106 + v025, design = dhs)
svychisq(~v106 + v025, design = dhs, statistics = "design")

### HH wealth index
svytable(~v190 + v025, design = dhs)
svychisq(~v190 + v025, design = dhs, statistics = "design")

### employment
svytable(~employ_current + v025, design = dhs)
svychisq(~employ_current + v025, design = dhs, statistics = "design")

### wantness (no respondent - 2,982)
svytable(~v225 + v025, design = dhs)
svychisq(~v225 + v025, design = dhs, statistics = "design")

### knoweldge on contraceptive methods
svytable(~contra_know_any + v025, design = dhs)
svychisq(~contra_know_any + v025, design = dhs, statistics = "design")

### current contraceptive use
svytable(~contra_current + v025, design = dhs)
svychisq(~contra_current + v025, design = dhs, statistics = "design")

### intention to use contraception in the future
svytable(~contra_future + v025, design = dhs)
svychisq(~contra_future + v025, design = dhs, statistics = "design")

### current marital status
svytable(~marital + v025, design = dhs)
svychisq(~marital + v025, design = dhs, statistics = "design")

### age at first marriage/union
svyby(formula = ~v511, by = ~v025, design = dhs, FUN = svymean, vartype=c("ci"),
      na.rm = TRUE) # exlude NA - 2,576 did not respond/never been in a union
svyttest(v511 ~ v025, design = dhs)

### age at first sex
svyby(formula = ~v531, by = ~v025, design = dhs, FUN = svymean, vartype=c("ci"),
      na.rm = TRUE) # exlude NA - 2,576 did not respond/never been in a union
svyttest(v531 ~ v025, design = dhs)

### access to information about family planning
svytable(~fp_info + v025, design = dhs)
svychisq(~fp_info + v025, design = dhs, statistics = "design")

### problem with access to health services - permission to go
svytable(~v467b + v025, design = dhs)
svychisq(~v467b + v025, design = dhs, statistics = "design")

### problem with access to health services - money needed
svytable(~v467c + v025, design = dhs)
svychisq(~v467c + v025, design = dhs, statistics = "design")

### problem with access to health services - distance
svytable(~v467d + v025, design = dhs)
svychisq(~v467d + v025, design = dhs, statistics = "design")

### problem with access to health services - not want to go alone
svytable(~v467f + v025, design = dhs)
svychisq(~v467f + v025, design = dhs, statistics = "design")

### partner education status
svytable(~v701 + v025, design = dhs)
svychisq(~v701 + v025, design = dhs, statistics = "design")

### partner age
svyby(formula = ~v730, by = ~v025, design = dhs, FUN = svymean, vartype=c("ci"),
      na.rm = TRUE) # exlude NA - 2,576 did not respond/never been in a union
svyttest(v730 ~ v025, design = dhs)

## household characteristics
### design using join dataset
join_datateen <- join_data %>% filter (v013 ==1)
dhs_join <- svydesign(id=~v021, strata =~v023, weights=~v005, data=join_datateen) # create design

### household size
svyby(formula = ~hv012, by = ~v025, design = dhs_join, FUN = svymean, vartype=c("ci"))
svyttest(hv012 ~ v025, design = dhs_join)

### highest household education attainment
svytable(~edu + v025, design = dhs_join)
svychisq(~edu + v025, design = dhs_join, statistics = "design")

### sex of household head
svytable(~hv219 + v025, design = dhs_join)
svychisq(~hv219 + v025, design = dhs_join, statistics = "design")

# to correct and round up the numbers in svytables
#test <- svytable(~hv219 + v025, design = dhs_join)
#test1 <- round(test/1e6,0)

# Missing values
View(teendata)

## check missing values return column 
na_list <- colnames(teendata)[colSums(is.na(teendata)) > 0] # 2,982 missing values
for (col in na_list) {
  na_count <- sum(is.na(teendata[[col]]))
  cat("Variable", col, "> NAs", na_count, "\n")
}

## check numbers
teendata %>% summarise(total_weight = round(sum(v005)/1e6,0))  # number of total sample - 3,083
round(svytotal(~teen_preg==1, design = dhs) / 1e6,0) # number of who ever pregnant (TP) - 678
round(svytotal(~marital==1, design = dhs) / 1e6,0) # currently married - 564
round(svytotal(~marital==2, design = dhs) / 1e6,0) # ended married - 56
round(svytotal(~marital==0, design = dhs) / 1e6,0) # never married - 2463
round(svytotal(~v213==1, design = dhs) / 1e6,0) # currently pregnant - 186

### v225 - questions about current pregnancy - 186 are currently pregnant
table(teendata$v213, teendata$v225, useNA = "always") # no missing variable
round(svytable(~v213 + v225, design = dhs, na.action = na.pass)/1e6,0) 

### p32 - questions about pregnancy outcome againsts livebirths
table(teendata$v201, teendata$p32_01, useNA = "always") # first preg - all NA are never had a child v201 = 0 > no missing
table(teendata$v201, teendata$p32_02, useNA = "always") # second preg - all NA v201 = 0 or 1 > no missing
table(teendata$v201, teendata$p32_03, useNA = "always") # third preg - all NA v201 = 0,1,2 > no missing
table(teendata$v201, teendata$p32_04, useNA = "always") # fourth preg - all NA v201 = 0,1,2,3 > no missing
table(teendata$v201, teendata$p32_05, useNA = "always") # no one has fith preg

### p32 - questions about pregnancy outcome againsts pregnancy loss
table(teendata$v245, teendata$p32_01, useNA = "always") # all NA are among who never loss pregnancy
round(svytable(~v245 + p32_02, design = dhs, na.action = na.pass)/1e6,0) # 473 never loss pregnancy, 

### edu - MAR? 
table (teendata$edu, teendata$teen_preg, useNA = "always") # 48 NAs (44 - No TP; 4 - TP)
table (teendata$edu, teendata$v025, useNA = "always") # 48 NAs (23 - Urban; 25 - Rural)


# check values of v201 and v245
teendata %>% group_by(v025) %>% summarize(n = n())
teendata$p32_01

### v701, v730 - questions about partners 2,519 (2,463 - never and 56 - ended) are not currently married 
table(teendata$marital, teendata$v701, useNA = "always") # can see all never/ended married are NA, and some NA from married
round(svytable(~marital + v730, design = dhs, na.action = na.pass)/1e6,0) # cannot see where are those NA values 

### v511 - age at first marriage/union - 2,576 did not respond/never been in a union
table(teendata$marital, teendata$v511, useNA = "always") # can see all NA from never got married
table(teendata$marital, teendata$v531, useNA = "always") # most NA from never got married
round(svytable(~marital + v531, design = dhs, na.action = na.pass)/1e6,0) # cannot see where are those NA values

### outcomes of teenage pregnancy by urban and rural
round(svytotal(~teen_preg, design = dhs) / 1e6,0) # number of who ever pregnant (TP) - 678
# total chidren born - 183 never had a life birth
round(svytable(~teen_preg + v201 + v025, design = dhs, na.action = na.pass)/1e6,0)
# pregnancy loss - 618 never had a pregnancy loss
round(svytable(~teen_preg + v245 + v025, design = dhs, na.action = na.pass)/1e6,0)
svychisq(~v245 + v025, design = dhs, statistics = "design") # chi-square test
# currently pregnant - 186 are currently pregnant
round(svytable(~teen_preg + v213 + v025, design = dhs, na.action = na.pass)/1e6,0)


### compare tpr among regions

### pregnancy outcomes matrix
teendata %>% group_by(v201) %>% summarise(number = round(sum(v005)/1e6,0))
round(svytable(~v245 + v201, design = dhs, na.action = na.pass)/1e6,0) # cross table of pregnancy loss and livebirths


# Univariate analysis
## tp vs urban/rural
