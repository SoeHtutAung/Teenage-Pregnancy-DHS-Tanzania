library(haven)
NR <- read_dta("~/Desktop/Data_Challenge /Pregnancy and Postnatal Care/TZNR82FL.DTA")
View(NR)
library(dplyr)

#load data 
NR <- NR %>%
  mutate(
    p32_stillbirth = ifelse(p32 == 2, 1, 0)  
  )

count(NR,v106)
NR$p32_stillbirth <- as.numeric(as.character(NR$p32_stillbirth))
#set survey design 
library(survey)
DHS <- svydesign(
  id = NR$v021,       
  strata = NR$v023,    
  weights = NR$v005/1e6,  
  data = NR
)

NR$v025

###proportion of stillbirth 
svytable(~v025 + p32_stillbirth, design = DHS)


# Basic Rates
prop.table(table(NR$p32_stillbirth, NR$v025))

###make a bar plot here 

#Un-udjusted model 
model_unadjusted <- svyglm(p32_stillbirth ~ factor(v025), design = DHS, family = quasibinomial())
exp(coef(model_unadjusted))
summary(model_unadjusted)

#adjust for hypertension 
Model_cof1 <- svyglm(p32_stillbirth ~ v025 *s1125, design = DHS, family = quasibinomial())
exp(coef(Model_cof1))
summary(Model_cof1)
confint_adjusted <- exp(confint(Model_cof1))

# un-adjust for hypertension 
Model_cof1_u <- svyglm(p32_stillbirth ~ factor(s1125), design = DHS, family = quasibinomial())
exp(coef(Model_cof1_u))
summary(Model_cof1_u)
prop.table(svytable(~p32_stillbirth + v025 + m80, design = DHS))

#adjust for education
model_cof2 <- svyglm(p32_stillbirth ~ v025 *v106, design = DHS, family = quasibinomial())
exp(coef(model_cof2))
summary(model_cof2)

count(NR,v106)

#un-adjust for education
model_cof2_u <- svyglm(p32_stillbirth ~  factor(v106), design = DHS, na.action=na.exclude, family = quasibinomial())
exp(coef(model_cof2_u))
summary(model_cof2_u)
prop.table(table(NR$p32_stillbirth, NR$v106))
NR$v106
#adjust for marital status 
model_cof3 <- svyglm(p32_stillbirth ~ v025 + factor(v501), design = DHS, family = quasibinomial())
exp(coef(model_cof3))
summary(model_cof3)

count(NR,v501)
count(NR,v732)
count(NR,m17)
count(NR,m14)
count(NR,v457)
count(NR,m80_combined)
count(NR,v190)
count(NR,v732)
count(NR,p0)
count(NR,p32_stillbirth)
svytable(~p32_stillbirth + v025 + v501,design = DHS)
#un-adjust for marital status 
model_cof3 <- svyglm(p32_stillbirth ~  factor(v501), design = DHS, family = quasibinomial())
exp(coef(model_cof3))
summary(model_cof3)
count(NR,v501)
#adjust for work status 
model_cof4 <- svyglm(p32_stillbirth ~ v025 *factor(v732), design = DHS, family = quasibinomial())
exp(coef(model_cof4))
summary(model_cof4)
NR$v732
#un-adjust for work status 
model_cof4_u <- svyglm(p32_stillbirth ~ factor(v732), design = DHS, family = quasibinomial())
exp(coef(model_cof4_u))
summary(model_cof4_u)

#adjust for mode of delivery 
model_cof5 <- svyglm(p32_stillbirth ~ v025 *factor(m17), design = DHS, family = quasibinomial())
exp(coef(model_cof5))
summary(model_cof5)
#un-adjust for mode of delivery 
model_cof5_u <- svyglm(p32_stillbirth ~ factor(m17), design = DHS, family = quasibinomial())
exp(coef(model_cof5_u))
summary(model_cof5_u)

#adjusted for ANC visits and care (there is missing data )
model_cof6 <- svyglm(p32_stillbirth ~ v025 * m14, design = DHS, family = quasibinomial(),na.action = na.exclude)
exp(coef(model_cof6))
summary(model_cof6)
#un-adjusted for ANC visits and care (there is missing data )
model_cof6_u <- svyglm(p32_stillbirth ~ m14, design = DHS, family = quasibinomial(), na.action = na.exclude)
exp(coef(model_cof6_u))
summary(model_cof6_u)
#anemia adjusted 
model_cof7 <- svyglm(p32_stillbirth ~ v025 *v457, design = DHS, family = quasibinomial())
exp(coef(model_cof7))
summary(model_cof7)
#anemia un-adjusted 
model_cof7_u <- svyglm(p32_stillbirth ~  v457, design = DHS, family = quasibinomial())
exp(coef(model_cof7_u))
summary(model_cof7_u)
#wantedness of pregnancy adjusted (there is missing data)
model_cof8 <- svyglm(p32_stillbirth ~ v025 *v225, design = DHS, family = quasibinomial(),na.action=na.exclude)
exp(coef(model_cof8))
summary(model_cof8)
#wantedness of pregnancy un-adjusted (there is missing data)
model_cof8_u <- svyglm(p32_stillbirth ~ v225, design = DHS, family = quasibinomial(),na.action=na.exclude)
exp(coef(model_cof8_u))
summary(model_cof8_u)


# adjust for gestational age 
model_cof9 <- svyglm(p32_stillbirth ~ v025 * p20, design = DHS, family = quasibinomial())
exp(coef(model_cof9))
summary(model_cof9)
# un-adjust for gestational age 
model_cof9_u <- svyglm(p32_stillbirth ~ p20, design = DHS, family = quasibinomial())
exp(coef(model_cof9_u))
summary(model_cof9_u)

# adjusted multivariate for sociodemograhic 

count(NR,v012)

# adjust for maternal age ?? or age at pregnancy 
model_cof19 <- svyglm(p32_stillbirth ~ v025 * v012, design = DHS, family = quasibinomial())
exp(coef(model_cof9))
summary(model_cof19)
# un-adjust for maternal age 
model_cof19_u <- svyglm(p32_stillbirth ~ v012, design = DHS, family = quasibinomial())
exp(coef(model_cof19_u))
summary(model_cof19_u)

v245

##history of stillbirth 3 and 4 (recent and prior)
count(NR,m80)
library(haven)
library(dplyr)
# Convert m80 to a numeric or factor type
NR$m80 <- as.numeric(as_factor(NR$m80))

# Now use mutate to create the new column
NR <- NR %>% 
  mutate(m80_right = ifelse(m80 == 4, 1, 0))


# adjust for history of stillbirth
model_cof10 <- svyglm(p32_stillbirth ~ v025 * m80, design = DHS, family = quasibinomial())
exp(coef(model_cof10))
summary(model_cof10)
# un-adjust for history of stillbirth 
model_cof10_u <- svyglm(p32_stillbirth ~ factor(m80), design = DHS, family = quasibinomial())
exp(coef(model_cof10_u))
summary(model_cof10_u)

NR$m80

# un-adjusted for multiplicity of pregnancy 
svytable(~p0 + v025 + p32_stillbirth, design = DHS)
model_cof11_u <- svyglm(p32_stillbirth ~ p0, design = DHS, family = quasibinomial())
exp(coef(model_cof11_u))
summary(model_cof11_u)

#Adjusted for multiplicity 
svytable(~p0 + v025 + p32_stillbirth, design = DHS)
model_cof11 <- svyglm(p32_stillbirth ~ v025 * p0, design = DHS, family = quasibinomial())
exp(coef(model_cof11))
summary(model_cof11)

#Adjusted for wealth 
svytable(~v190 + v025 + p32_stillbirth, design = DHS)
prop.table(table(NR$v190,NR$v025))
model_cof12 <- svyglm(p32_stillbirth ~ v025 * factor(v190), design = DHS, family = quasibinomial())
exp(coef(model_cof12))
summary(model_cof12)


##history of stillbirth 3 and 4 (recent and prior) 1 and 2 are most recent live birth and previous live birth 
# 5 is miscarriage 

svytable(~p32_stillbirth + v025 + m80, design = DHS)
count(NR,m80)
count(NR,m80)
prop.table(table(NR$m80, NR$v025))
count(NR,v025)

# correct prop 
prop.table(svytable(~ p32_stillbirth + v025, design = DHS) ,margin = 2) *100


NR$m80
svytable(~v025 + m80, design = DHS)/100
###### Ratio #####
count(NR,history_of_stillbirth)
count(NR,p32_stillbirth)


prop.table()
NR$v029
count(NR,v190)
count(NR,v106_combined)
count(NR,mat_age)
count(NR,v732)
count(NR,v190)
count(NR,m14_grouped)

count(NR,p0)
count(NR,v141)
count(NR,v156)
count(NR,v166)
count(NR,v167)
count(NR,v168)