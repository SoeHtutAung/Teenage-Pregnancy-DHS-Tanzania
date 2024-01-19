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
                        v467b, v467c, v467d, v467f # access to health services
                      ))

## extract selected variables from HR
hh_data <- hr %>% select(1:7,
                         c(hv109_01:hv109_56,# highest education attained in the HH
                           hv012, # total number of HH members
                           hv219 # sex of hh head
                         )) 

# Recode
## teenage pregnancy recode - outcome 1 = Yes, 0 = No 
data <- data %>% mutate(
  v013 = factor(v013),
  teen_preg = ifelse(v013 == 1 & (v201 > 0 | v245 > 0 | v213 == 1), 1, 0))
data %>% group_by(teen_preg) %>% summarise(n = n()) # check grouping

## knowledge on any contraceptive method - covariate 1 = Yes, 0 = No
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

### access to informaiton about family planning - covariate 1 = Yes, 0 = No
data <- data %>% mutate(
  fp_info = ifelse(v384a == 1 | v384b == 1 | v384c == 1 | v384d == 1 | 
                     v384e == 1 | v384f == 1 | v384g == 1 | v384h == 1, 1, 0))
data %>% group_by(fp_info) %>% summarise(n = n()) # check grouping
# alternative: data %>% group_by(v384a,v384b,v384c,v384d,v384e,v384f,v384g,v384h) %>% summarize (count = n())

# summary statistics
## sample size
### Sample size individuals and weighted in urban and rural
sumtotal <- data %>% group_by (v025) %>%
  summarise(total = n(), total_weight = round(sum(v005)/1000000,0)) 
sumteen <- data %>% filter (v013 ==1) %>% group_by (v025) %>%
  summarize (teen = n(), teen_weight = round(sum(v005)/1000000,0))
samplesize_ur <- cbind(sumtotal,sumteen[,2:3]) # create dataframe
print(samplesize_ur) # print table

### Sample size individuals and weighted in regions
sumtotal_region <- data %>% group_by (v024) %>%
  summarise(total = n(), total_weight = round(sum(v005)/1000000,0)) 
sumteen_region <- data %>% filter (v013 ==1) %>% group_by (v024) %>%
  summarize (teen = n(), teen_weight = round(sum(v005)/1000000,0))
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
tp_prop_ur_raw <- svyby(~(teen_preg == 1), ~v025, dhs, svymean, vartype=c("se","ci"))
tp_prop_ur <- tp_prop_ur_raw [,c(1,3)]

## socio-demographic characteristics
### age by urban and rural
age_ur <- svyby(formula = ~v012, by = ~v025, design = dhs, FUN = svymean, vartype=c("ci"))
t_test_age <- svyttest(v012 ~ v025, design = dhs) # t-test for pvalue

### education by urban and rural
table(teendata$v106, teendata$v025) # check grouping
svytable(~v106 + v025, design = dhs) # check grouping

# stratified analysis
## pregnancy outcomes
