library(haven)
library(dplyr)

Maternal <- read_dta("Individual/TZIR82FL.DTA")
Maternal$v483a
Maternal$v483b
View(Maternal)
head(Maternal)
#urban vs rural 
count(Maternal,v025)
#age of husband 
Maternal$v730
#level education husband 
Maternal$v732
#Education 
count(Maternal,v106)
#employement 
count(Maternal,v714)
#Ethnicity
count(Maternal,v131)
#Marital status of Maternal 
count(Maternal,v501)
#religion 
count(Maternal,v131)
#wanted pregnancy 
count(Maternal,v225)
#number of cigarettes in last 24 hrs 
count(Maternal,v464)
#smoke cigarette 
Maternal$v463a
#frequency smokes cigarettes
Maternal$v463aa
#frequent use other type of tobacco 
Maternal$v463ab
#smoke pipe full of tobacco 
Maternal$v463b
#chews tobacco 
Maternal$v463c
#snuff by nose 
Maternal$v463d
#smoke kreteks 
Maternal$v463e
#smokes cigars, cheroots or cigarillos
Maternal$v463f
#smokes water pipe
Maternal$v463g
#s Currently smokes or uses tobacco besides cigarettes 
Maternal$v463x
Maternal$v463a


Maternal$b3_01
Maternal$b3_20
###### analysis start here 



summary(Maternal$v730)  # Summary statistics for husband's age


# Using chi-square test for categorical variables smoking status 
chisq.test(table(Maternal$v025, Maternal$v463a))

# Using t-test for continuous variables (age)
t.test(Maternal$v730 ~ Maternal$v025)

table(Maternal$v106, Maternal$v463a)  # Cross-tabulating education with smoking status

#Multivariate Analysis (logistic regression for smoking status): 

model <- glm(v463a ~ v025 + v106 + v714, data=Maternal, family="binomial")
summary(model)
#as education level increases smoking decreases

#plot smoking 
library(ggplot2)
ggplot(Maternal, aes(x=v106, fill=factor(v463a))) + 
  geom_bar(position="dodge") +
  labs(x="Education Level", fill="Smoking Status")


Maternal$v463a_logical <- Maternal$v463a == 1
Maternal$v463b_logical <- Maternal$v463b == 1
Maternal$v463c_logical <- Maternal$v463c == 1
Maternal$v463d_logical <- Maternal$v463d == 1
Maternal$v463e_logical <- Maternal$v463e == 1
Maternal$v463f_logical <- Maternal$v463f == 1
Maternal$v463g_logical <- Maternal$v463g == 1


count(Maternal,v463a)
Maternal$tobacco_use <- with(Maternal, v463a_logical | v463b_logical | v463c_logical | v463d_logical | v463e_logical | v463f_logical | v463g_logical)
library(dplyr)
library(tidyr)

summary_table <- Maternal %>%
  mutate(tobacco_use = ifelse(tobacco_use, "Yes", "No")) %>%
  group_by(v106, v025, tobacco_use) %>%  # grouping by education level and urban/rural
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100)
print(summary_table)

Maternal$v106
Maternal$v005



install.packages("survey")
library(survey)
dhs_design <- svydesign(ids = ~1, data = Maternal, weights = ~v025)
svymean(~v463a, dhs_design)


library(haven)
Births <- read_dta("~/Desktop/Births/TZBR82FL.DTA")
View(Births)
Births$b5
Births$b7

PR <- read_dta("~/Desktop/Household Member/TZPR82FL.DTA")
View(PR)

PR$hv005

count(Maternal,v730)

Maternal %>% count(v730)

Maternal$m14_1

count(Maternal,m14_1)

result = Maternal.groupby('v025')['m14_1'].count()


library(dplyr)

library(dplyr)

result1 <- Maternal %>%
  filter(!is.na(m14_1)) %>% 
  group_by(v025) %>%
  summarise(count = n())

# Display the result
print(result1)

antenatal <- Maternal %>%
  group_by(v025) %>%
  summarise(
    total_visits = sum(m14_1, na.rm = TRUE), # Total number of antenatal visits including 'don't know'
    valid_visits_total = sum(ifelse(m14_1 < 98, m14_1, 0), na.rm = TRUE), # Total of valid antenatal visits
    average_valid_visits = mean(ifelse(m14_1 < 98, m14_1, NA), na.rm = TRUE), # Average number of valid visits
    min_valid_visits = min(ifelse(m14_1 < 98, m14_1, NA), na.rm = TRUE), # Minimum of valid visits
    max_valid_visits = max(ifelse(m14_1 < 98, m14_1, NA), na.rm = TRUE), # Maximum of valid visits
    dont_know_count = sum(m14_1 == 98, na.rm = TRUE) # Count of 'don't know' responses
  )
# Display the result
print(antenatal)




##### start here stillbirth analysis######

library(haven)
library(tidyverse)

Individual <- read_dta("Individual/TZIR82FL.DTA")

library(dplyr)

# stillbirth outcome
Individual$p32_01
#history of pregnancy outcomes 
Individual$m80_1
#age of mother 
Individual$v012
#Education
Individual$v106
#marital_status 
Individual$v501
#wealth
Individual$v190
#antenatal_care remove missing data 
Individual$m14_1
#Weight
Individual$m19_1
#duration of pregnancy
Individual$b20_01
# mode transportation
Individual$v483b 
#anemia 
Individual$v457
#BMI
Individual$v445
#sex of child 
Individual$b4_01
#C section
Individual$m17_1
#blood pressure 
Individual$s1125
#occupation Numerator divided by the denominator, multiplied by 100.
Individual$v717

####using pregnancy dataset 
library(haven)
pregnancy <- read_dta("~/Library/Containers/com.microsoft.Outlook/Data/Desktop/Data_Challenge /Pregnancies/TZGR82FL.DTA")
View(pregnancy)


library(dplyr)
# Sociodemographic characteristics
# maternal age - strong - usually cut up into age groups
hist(pregnancy$v013) # age at interview
hist((pregnancy$p3 - pregnancy$v011)/12)
# educational attainment - strong - cut along primary edu
count(pregnancy,v106)
# 0 none, 1 primary, 2 secondary, 3 higher
# no 8's in NR!
# marital status
count(pregnancy, v501)
# current status, not-ordered factor
# 0 never, 1 married, 2 living with, 3 widowed, 4 divorced, 5 separated
# exploratory: v221, v535, v542, v544, v705
# husband: v701:v705, v715, v729:v730, v739, v743a:v746
# place of residence
count(pregnancy, v025)
# factor of interest!
# exploratory: v115, v483a:v483b
# 483a: 0-599, 600+
# 483b: 11-14 motorised, 21-24 non-motorised, 96 other
# Ethnicity (v131) not asked in DHS-8; all NAs
count(pregnancy,v190) #wealth
count(pregnancy,v717)


# Pregnancy-related factors
# antenatal care - SIG
count(pregnancy, m14)
# 0 none (635), 1-16 (most), 98 don't know (23), NA (1328)
# exploratory: m13, m13a, m57a:m57x, s417aa:s417ad
# antepartum hemorrhage - SIG
# birth weight - SIG - cutoffs 2.5kg , 4kg
count(pregnancy, m19)
# 500:6000 grams, 9996 not weighed (1240), 9998 dk (176), NA (707)
# m19a: of 5158 w/ wt, 3625 from written card, 1533 from memory
# gestational age
count(pregnancy, p20)
# parity
#v224?
# mode of arrival (referral or home) - SIG
# sex of the newborn
count(pregnancy, p4)
# 1 male, 2 female, NA (termination) n=707
# history of previous stillbirth - SIG
# v228?
# mode of delivery
# multiple gestations
count(pregnancy, p0)
# 0 single (7057), 1 & 2 twins (ordered; 112 each)
# PROM (Premature Rupture of Membrane)
count(Individual,v225) #wanted pregnancy 
# 1 then 2 later 3 not at all
# Medical-related factors
# anemia - SIG
count(pregnancy, v457)
# ordered 1:4 decreasing severity, NA (3643)
# DM (Diabetes Mellitus)
# HIV serostatus
# hypertension (HPN) - SIG
# Other Medical
count(pregnancy, m17)
# 0 no, 1 yes, NA (573)
pregnancy$v445 #BMI
pregnancy$m17 #C section
pregnancy$s1125 #blood pressure 

pregnancy$p32
pregnancy$v025


dhs_design <- svydesign(
  ids = ~v001,
  strata = ~v023,
  weights = ~v005,
  data = pregnancy
)

### survey design

three_yr <- pregnancy[pregnancy$p19<36,]

three_yr <- three_yr %>% mutate(
  stillbirth = p32==2
)


DHSdesign <-
  svydesign(
    id = three_yr$v021 ,
    strata = three_yr$v023,
    weights = three_yr$v005,
    data = three_yr
  )

mother_age <-
  svyby(~stillbirth, ~v024, DHSdesign, svymean, vartype=c("se","ci"))

stillb <- mother_age[,c(1,3)]

RUdesign <-
  svydesign(
    id = pregnancy$v021 ,
    strata = pregnancy$v023,
    weights = pregnancy$v005/1e6,
    data = pregnancy
  )


rur_urb <-
  svyby(~p32==2, ~v025, RUdesign, svymean, vartype=c("se","ci"))
View(rur_urb[,c(1,3)])
library(survey)
library(ggplot2)
# Create a data frame for plotting
# Create a data frame for plotting
plot_data <- data.frame(
  Area = c("Urban", "Rural"),
  Stillbirth_Rate = c(rur_urb[rur_urb$v025 == 1, "p32 == 2TRUE"], rur_urb[rur_urb$v025 == 2, "p32 == 2TRUE"]),
  SE = c(rur_urb[rur_urb$v025 == 1, "se.p32 == 2TRUE"], rur_urb[rur_urb$v025 == 2, "se.p32 == 2TRUE"])
)

# Create a bar plot with error bars
ggplot(plot_data, aes(x = Area, y = Stillbirth_Rate, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = Stillbirth_Rate - 1.96 * SE, ymax = Stillbirth_Rate + 1.96 * SE), position = "dodge", width = 0.2) +
  labs(
    title = "Stillbirth Rates by Urban and Rural Areas",
    y = "Stillbirth Rate per 1,000 Live Births",
    fill = "Area"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = c("Urban" = "darkblue", "Rural" = "skyblue"))

#study design 
RUdesign <-
  svydesign(
    id = pregnancy$v021 ,
    strata = pregnancy$v023,
    weights=pregnancy$v005/1e6,
    data = pregnancy
  )

# Ensure that the hypertension variable is a factor
pregnancy$s1125 <- factor(pregnancy$s1125, levels = c(0, 1), labels = c("No", "Yes"))

# Update the survey design object to include the hypertension variable
RUdesign <- update(RUdesign, hypertension = s1125)

# Calculate stillbirth rates by hypertension status for urban areas
urban_hypertension_stillbirth <- svyby(~p32 == 2, ~hypertension, subset(RUdesign, v025 == 1), svymean, na.rm = TRUE)

# Calculate stillbirth rates by hypertension status for rural areas
rural_hypertension_stillbirth <- svyby(~p32 == 2, ~hypertension, subset(RUdesign, v025 == 2), svymean, na.rm = TRUE)

# View the results
View(urban_hypertension_stillbirth)
View(rural_hypertension_stillbirth)

pregnancy <- pregnancy %>%
  mutate(
    v106 = factor(v106, levels = c(0, 1, 2, 3), labels = c("None", "Primary", "Secondary", "Higher")),
    v501 = factor(v501, levels = c(0, 1, 2, 3, 4, 5), labels = c("Never married", "Married", "Living with partner", "Widowed", "Divorced", "Separated")),
    v190 = factor(v190, levels = c(1, 2, 3, 4, 5), labels = c("Wealth Level 1", "Wealth Level 2", "Wealth Level 3", "Wealth Level 4", "Wealth Level 5")),
    v717 = factor(v717, levels = c(0, 1, 2, 3, 5, 7, 8, 9, 96), labels = c("Not working", "Professional/technical/managerial", "Clerical", "Sales", "Agricultural - employee", "Services", "Skilled manual", "Unskilled manual", "Other")),
    p32_stillbirth = factor(ifelse(p32 == 2, 1, 0), levels = c(0, 1), labels = c("Other Outcomes", "Stillbirth"))
  )

pregnancy$p32_stillbirth
### hypertension
svytable(~s1125 + v025 + p32_stillbirth, design = RUdesign)
svychisq(~s1125 + v025, design = RUdesign, statistics = "design")

pregnancy$s1125

hyp <- svyby(
  formula = ~ p32_stillbirth + s1125, 
  by = ~ v025, 
  design = RUdesign, 
  FUN = svymean, 
  vartype = c("ci")
)
pregnancy$p32
nrows(pregnancy)
View(hyp)

pregnancy$v025
hypertension_counts <- pregnancy %>%
  group_by(v025, s1125,p32_stillbirth) %>%
  summarize(count = n())
print(hypertension_counts)

count(pregnancy %>% group_by(v025,s1125,p32_stillbirth))



library(haven)
NR <- read_dta("~/Desktop/Data_Challenge /Pregnancy and Postnatal Care/TZNR82FL.DTA")
View(NR)
library(dplyr)
#load data 
NR <- NR %>%
  mutate(
    p32_stillbirth = factor(ifelse(p32 == 2, 1, 0), levels = c(0, 1), labels = c("o", "1"))
  )

# filter 

Stillbirth <- data %>% filter(p32 ==2 ) # create dataset with only teenagers

#set survey design 
library(survey)
DHS <-
  svydesign(
    id = NR$v021 ,
    strata = NR$v023,
    weights=NR$v005/1e6,
    data = NR,
  )

#hypertension 
hypertension <- svytable(~s1125 + v025 + p32_stillbirth, design = DHS)
svychisq(~s1125 + v025, design = DHS)

count(NR,v106)

#education
svytable(~v106 + v025 + p32_stillbirth, design = DHS)
svychisq(~v106 + v025, design = DHS)
svychisq(~v106 + p32_stillbirth, design = DHS)

#marital status 
svytable(~v501 + v025 + p32_stillbirth, design = DHS)
svychisq(~v501 + v025, design = DHS)
svychisq(~v501 + p32_stillbirth, design = DHS)

#work status 
svytable(~v717 + v025 + p32_stillbirth, design = DHS)
svychisq(~v717 + v025, design = DHS)
svychisq(~v717 + p32_stillbirth, design = DHS)

# C section 

svytable(~m17 + v025 + p32_stillbirth, design = DHS)
svychisq(~m17 + v025, design = DHS)
svychisq(~m17 + p32_stillbirth, design = DHS)

#ANC antenatal care 


svytable(~m14 + v025 + p32_stillbirth, design = DHS)
svychisq(~m14 + v025, design = DHS)
NR$m14
count(NR,m14)

#Anemia 
count(NR,v457)
svytable(~v457 + v025 + p32_stillbirth, design = DHS)
svychisq(~v457 + v025, design = DHS)
svychisq(~v457 + p32_stillbirth, design = DHS)


#pregnancy wanted missing data 
count(NR,v225) 

#multiplicity of pregnancy 
count(NR,p0)

svytable(~p0 + v025 + p32_stillbirth, design = DHS)
svychisq(~p0 + v025, design = DHS)
svychisq(~p0 + p32_stillbirth, design = DHS)


#gestational age 

svytable(~p20 + v025 + p32_stillbirth, design = DHS)
svychisq(~p20 + v025, design = DHS)
svychisq(~p20 + p32_stillbirth, design = DHS)


NR$v501
NR$v717
NR$p32_stillbirth

rur_urb <-
  svyby(~p32_stillbirth=='Stillbirth', ~v025, DHS, svymean, vartype=c("se","ci"))
View(rur_urb[,c(1,3)])
library(survey)
library(ggplot2)
# Create a data frame for plotting
# Create a data frame for plotting
plot_data <- data.frame(
  Area = c("Urban", "Rural"),
  Stillbirth_Rate = c(rur_urb[rur_urb$v025 == 1, "p32 == 2TRUE"], rur_urb[rur_urb$v025 == 2, "p32 == 2TRUE"]),
  SE = c(rur_urb[rur_urb$v025 == 1, "se.p32 == 2TRUE"], rur_urb[rur_urb$v025 == 2, "se.p32 == 2TRUE"])
)

# Create a bar plot with error bars
ggplot(plot_data, aes(x = Area, y = Stillbirth_Rate, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = Stillbirth_Rate - 1.96 * SE, ymax = Stillbirth_Rate + 1.96 * SE), position = "dodge", width = 0.2) +
  labs(
    title = "Stillbirth Rates by Urban and Rural Areas",
    y = "Stillbirth Rate per 1,000 Live Births",
    fill = "Area"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("Urban" = "darkblue", "Rural" = "skyblue"))


NR$p32_stillbirth <- factor(NR$p32_stillbirth, levels = c("Stillbirth", "Other_Outcomes"))


crude_model <- svyglm(p32_stillbirth ~ v025, design = DHS, family = quasibinomial())
crude_or <- exp(coef(crude_model))
confint(crude_model)
#for region 
#highest odds of still birth ratio is in iringa and lowest is kilimanjaro  
univariate <- 
  svyglm(
    p32_stillbirth ~ as.factor(v024),
    design = DHS,
    family=quasibinomial(),
    na.action = na.exclude
  )
crude_or_region <- exp(coef(univariate))
summary(univariate)
NR$v024

#education 
crude_edu <- svyglm(as.factor(v106)~ p32_stillbirth, design = DHS, family = quasibinomial())
crude_or_edu <- exp(coef(crude_edu))
confint(crude_model)


# Calculate Adjusted Odds Ratio for Urban vs Rural adjusting for confounders
adjusted_model <- svyglm(p32_stillbirth ~ v025 + Education + MaritalStatus + Wealth + HypertensionHistory + WorkStatus + ANCVists + GestationalAge + ModeOfDelivery + Anaemia + WantedPregnancy + Multiplicity, design = design, family = quasibinomial())
adjusted_or <- exp(coef(adjusted_model))

# Print results
print(crude_or)
print(adjusted_or)

NR$v025

###proportion of stillbirth 
library(survey)
svytable(~v025 + p32_stillbirth, design = DHS)

#Un-udjusted model 
model_unadjusted <- svyglm(p32_stillbirth ~ v025, design = DHS, family = binomial)
exp(coef(model_unadjusted))

