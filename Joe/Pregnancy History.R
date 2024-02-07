################ Unused since sample size was too small

library(survey)
library(dplyr)
library(haven)

# setwd(...)
perinatal <- read_dta("Survey Data/Pregnancy and Postnatal Care/TZNR82FL.DTA")

perinatal <-
  perinatal %>%
  mutate(
    wt = v005/1e6,
    livebirth = ifelse(p32==1, 1, 0),
    stillbirth = ifelse(p32==2, 1, 0),
    miscarriage = ifelse(p32==3, 1, 0),
    abortion = ifelse(p32==4, 1, 0),
    my_pid = seq(1:7281)
  )
# new column <- my_pid

lb_id <- perinatal[which(perinatal$livebirth==1),]$caseid
sb_id <- perinatal[which(perinatal$stillbirth==1),]$caseid

psb <- # create a special df to assess previous stillbirths
  perinatal[ # include only if:
    perinatal$caseid %in% lb_id | # the woman has experienced a live birth, OR
      perinatal$caseid %in% sb_id, # the woman has experienced a stillbirth
  ]
# women who experienced at least one live birth or stillbirth in the past 36 months
# accounted for 6920 of the 7281 recorded pregnancy outcomes

# 53 women had exactly three recorded pregnancy outcomes, making up 159 rows in NR file
# There are 106 rows that we can assess for history of previous stillbirth
threecase <- psb[which(psb$pidx==3),]$caseid
psb3 <-
  psb[psb$caseid %in% threecase,
      c("caseid", "pidx","my_pid",
        "livebirth","stillbirth","miscarriage","abortion")
  ]

three_recent <-
  psb3[psb3$pidx==1,] %>%
  mutate(
    prev_sb = ifelse(
      psb3[psb3$pidx==2,]$stillbirth==1 |
        psb3[psb3$pidx==3,]$stillbirth==1 ,
      1 , 0
    ),
    prev_mc = ifelse(
      psb3[psb3$pidx==2,]$miscarriage==1 |
        psb3[psb3$pidx==3,]$miscarriage==1 ,
      1 , 0
    ),
    prev_ab = ifelse(
      psb3[psb3$pidx==2,]$abortion==1 |
        psb3[psb3$pidx==3,]$abortion==1 ,
      1 , 0
    )
  )

three_middle <- 
  psb3[psb3$pidx==2,] %>%
  mutate(
    prev_sb = psb3[psb3$pidx==3,]$stillbirth,
    prev_mc = psb3[psb3$pidx==3,]$miscarriage,
    prev_ab = psb3[psb3$pidx==3,]$abortion
  )

# 903 women had exactly two pregnancy outcomes, making up 1806 rows in the NR file
# 903 rows can be assessed for history of previous stillbirth
twos <- psb[which(psb$pidx==2),]$caseid # 956 - 53
twocase <- twos[!(twos %in% threecase)]
psb2 <-
  psb[psb$caseid %in% twocase,
      c("caseid", "pidx","my_pid",
        "livebirth","stillbirth","miscarriage","abortion")]
two_recent <- 
  psb2[psb2$pidx==1,] %>%
  mutate(
    prev_sb = psb2[psb2$pidx==2,]$stillbirth,
    prev_mc = psb2[psb2$pidx==2,]$miscarriage,
    prev_ab = psb2[psb2$pidx==2,]$abortion
  )

# 53 rows each for the two assessable 3-outcomes table
# # plus 938 rows of the 2-outcomes table
# 53 + 53 + 903 = 1009 assessable pregnancy outcomes
assessable <-
  rbind(
    three_recent,
    three_middle,
    two_recent
  )

# Join the 1009 values of prev_xx into the original data frame (6920)
# Exclude rows where the current outcome is miscarriage or abortion (-212)
# # 96 of the 1009 were excluded, leaving 913 assessable pregnancy outcomes
# There will be 5795 NAs of 6708 rows (6708-913=5795)
psb <- left_join(
  psb[which(psb$p32<=2),] ,
  assessable[,c("my_pid","prev_sb","prev_mc","prev_ab")] ,
  by = join_by("my_pid")
)

##################################### Table 1 analysis

# Too many NAs in full table for R to handle,
# so we will make smaller tables for analysis
# The weighted total is  (913 unweighted)

assessable <- psb[!is.na(psb$prev_sb),]
prev_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=assessable
  )
svytable(~prev_sb, prev_design)
coef(svymean(~factor(prev_sb), prev_design))*100
confint(svymean(~factor(prev_sb), prev_design))*100
svyby(~factor(prev_sb),~v025, prev_design, FUN=svymean,vartype = c("ci"))*100
summary(svytable(~prev_sb+v025, prev_design), statistic="Chisq")


###################### History of miscarriage

pmc <- psb[!is.na(psb$prev_mc),]
mc_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=pmc
  )
svytable(~prev_mc, prev_design)
coef(svymean(~factor(prev_mc), mc_design))*100
confint(svymean(~factor(prev_mc), mc_design))*100
svyby(~factor(prev_mc),~v025, mc_design, FUN=svymean,vartype = c("ci"))*100
summary(svytable(~prev_mc+v025, mc_design), statistic="Chisq")

############### History of abortion - CANNOT USE
# N = 5 / 1009 unweighted , 6 / 921 weighted

pab <- psb[!is.na(psb$prev_ab),]
ab_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=pab
  )
svytable(~prev_ab, ab_design)
coef(svymean(~factor(prev_ab), ab_design))*100
confint(svymean(~factor(prev_ab), ab_design))*100
svyby(~factor(prev_ab),~v025, ab_design, FUN=svymean,vartype = c("ci"))*100
summary(svytable(~prev_ab+v025, ab_design), statistic="Chisq")


################################################### GLMs below

glm_prev_mc <-
  svyglm(stillbirth~prev_mc+relevel(factor(v025),ref=2),
         mc_design,
         family = quasibinomial())

exp(coef(glm_prev_mc)) # OR = 2.46
exp(confint(glm_prev_mc)) # CI: 0.82-7.41
# OR confidence interval crosses 1.00, so this is not significant
# # HOWEVER, this actually pushes the OR away from 1,
# # so this is a confounder in the opposing direction

############## without manually excluding NAs, the answer is the same
glm_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=psb
  )
glm_mc <-
  svyglm(stillbirth~prev_mc+relevel(factor(v025),ref=2),
         glm_design,
         family = quasibinomial())
exp(coef(glm_mc))
exp(confint(glm_mc))
