library(survey)
library(dplyr)
library(haven)

setwd("...")
perinatal <- read_dta("Survey Data/Pregnancy and Postnatal Care/TZNR82FL.DTA")

fullbirth <-
  perinatal[
    which(perinatal$p32 <= 2) , # 6709 records out of 7281
    c( "caseid", "v025", "p32",
       "v005", "v011", "v021", "v023", "v024", # interview characteristics
       "pord", "p0", "p3", "p20", # pregnancy outcome information
       "v106", "v175", "v190", "v225", # maternal background
       "v445", "v457", "v463z", "v485b", # health behaviours
       "v501", "v731", # maternal characteristics
       "m3a","m3b","m3c","m3d","m3e","m3f","m3g","m3h","m3i","m3k","m3n",
       "m14", "m17", "m45", # pregnancy experiences
       "s1125" # hypertension
    )
  ] %>%
  mutate(
    wt = v005/1e6,
    stillbirth = ifelse(p32==2, 1, 0),
    matage = cut((p3-v011)/12, breaks = c(0,20,25,35,50), right = FALSE),
    gest = cut(p20, breaks = c(0,8,10), right = TRUE),
    anc = ifelse(
      is.na(m14) | m14==98, "unknown" ,
      cut(m14, breaks = c(0,1,4,98,100), right = FALSE)
    ),
    emp_year = ifelse(v731==0, 0, 1),
    edu = ifelse(
      v106==0,0, ifelse(v106==1,1,2)
    ),
    bmi = cut(v445, breaks = c(1200, 1850, 2500, 3000, 6000)),
    marr = factor(
      ifelse(
        v501==0,"Never",
        ifelse(
          v501==1 | v501==2, "Current", "Formerly"
        )
      )
    )
)

sb_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=fullbirth
  )

urb_pop <- svytable(~v025, sb_design)[[1]]
rur_pop <- svytable(~v025, sb_design)[[2]]

# Calculations for age (matage), education (edu), hypertension (s1125),
# # mode of delivery (m17), ANC visits (anc), marital status (marr),
# # wealth index (v190), gestational age (gest),
# # employment in the last 12 months (emp_year)

# svychisq(~matage+v025,sb_design, statistic = "Chisq")
# summary(svytable(~matage+v025, sb_design),statistic = "Chisq")
# svyby(~factor(edu),~v025, sb_design, FUN=svymean,vartype = c("ci"))*100
# svytable(~edu+v025, sb_design)/urb_pop*100
# confint(svymean(~factor(matage), sb_design))*100

# For table 2, here is the starting model
model1 <-
  svyglm( stillbirth~relevel(factor(v025), ref = 2),
          sb_design, family = quasibinomial() )
exp(coef(model1))
exp(confint(model1))

# For anaemia, bmi, and history of previous stillbirth, please see below

######################################################################

# anaemia, 3359 weighted out of 6731

anm <- fullbirth[!is.na(fullbirth$v457),]
anm_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=anm
  )
svytable(~v457, anm_design)
svytable(~v025, anm_design)

# bmi, 3367 weighted out of 6731

bmi_design <-
  fullbirth[!is.na(fullbirth$bmi),] %>%
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=.
  )
summary(svytable(~bmi+v025, bmi_design),statistic = "Chisq")

####################### History of stillbirth, miscarriage, abortion

# Still working

####### Regional rates for the maps ###############################

reg <- svytable(~stillbirth+v024+v025, sb_design)
reg_outcome <-
    1000 * reg[2,,] / (reg[2,,] + reg[1,,])
reg_df <-
  as.data.frame(cbind(reg_outcome[,1],reg_outcome[,2]))
names(reg_df) <- c("urban", "rural")
reg_round <- round(reg_df,1)
reg_round[7,2] <- 0
reg_round <-
  reg_round %>%
  mutate(
  diff = urban - rural  
  )
write.csv(reg_round, "sb_rates_revised.csv")
