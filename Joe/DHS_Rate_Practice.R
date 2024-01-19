library(DHS.rates)
library(dplyr)
library(ggplot2)
library(haven)
library(survey)
library(labelled)

setwd("C:/Users/Joe/OneDrive - London School of Hygiene and Tropical Medicine/TZDHS/Survey Data")

# each of these takes a while to load
births <- read_dta("Births/TZBR82FL.DTA")
  # 40394 obs. of 1350 variables
  # 11116 unique caseid
children <- read_dta("Children/TZKR82FL.DTA")
  # 10783 obs. of 1324 variables
  # 7681 unique caseid
couples <- read_dta("Couples/TZCR82FL.DTA")
  # 2531 obs. of 1777 variables
  # 2531 unique caseid
household <- read_dta("Household/TZHR82FL.DTA")
  # 15705 obs. of 9246 variables
    # 15705 unique hhid
hh_member <- read_dta("Household Member/TZPR82FL.DTA")
  # 73774 obs. of 657 variables
    # 15705 unique hhid
individual <- read_dta("Individual/TZIR82FL.DTA")
  # 15254 obs. of 6379 variables
  # 15254 unique caseid
men <- read_dta("Men/TZMR82FL.DTA")
  # 5763 obs. of 647 variables
    # 5763 unique mcaseid
pregnancies <- read_dta("Pregnancies/TZGR82FL.DTA")
  # 43432 obs. of 1054 variables
  # 11271 unique caseid
perinatal <- read_dta("Pregnancy and Postnatal Care/TZNR82FL.DTA")
  # 7281 obs. of 928 variables
  # 6220 unique caseid
siblings <- read_dta("Siblings/TZSR82FL.DTA")
  # 82467 obs. of 143 variables
  # 14891 unique caseid

which(!(seq(1:629) %in% individual$v001))
# cluster 37 was skipped for all 10 tables
which(!(seq(1:629) %in% couples$v001))
# 17 clusters (including cluster 37) have no couples data

length(!duplicated(which(!(individual$caseid %in% births$caseid))))
# 4138 women have no births data
length(which(!duplicated(pregnancies[which(!(pregnancies$caseid %in% births$caseid)),"caseid"])))

??chmort
# here is a specialty function from DHS.rates package

# select the 8 columns necessary for the chmort function
mort_calc <-
  TZBR82FL %>%
  select(
    v005,
    v007,
    v008,
    v021,
    v022,
    b3,
    b7,
    v025
  )

data("mort_calc")
mort_calc %>% group_by(v005) %>% count(n()) # 628 clusters

chmort_TZ <- chmort( mort_calc, JK = "Yes" )
# takes up to a minute to calculate

###

traveling <- TZIR82FL %>%
  select(
    v005,
    v483a,
    v483b,
    v025
  )

ggplot(traveling, aes(x = factor(v025), y = v483a)) +
  geom_boxplot() +
  labs(x = 'location type', y = 'minutes to hospital')

print(count(TZBR82FL,m15),n=24)

pregnancies %>% select(
  v005,
  p19,
  p32
)

###

iron_supp <- TZIR82FL[!is.na(TZIR82FL$m45_1),
                      c('m45_1','m46_1')]

hist(iron_supp$m46_1, xlim = c(0,300), breaks = 100)
# bins are every 10 days

### Contraception

b_contraception <- births[,180:299]
k_contraception <- children[,180:299]
i_contraception <- individual[,1199:1401]
g_contraception <- pregnancies[,180:299]
n_contraception <- perinatal[,180:299]

i_vcal <- individual[,5150:5153]

### Location

i_loc <- individual %>%
  select(
    v001, v002, v003,
    v005,
    v022, v023, v024, v025,
    v101, v102
  )

### Wealth index merging

wi_compare <- inner_join(
  select(individual, 'v001','v002','v190') ,
  select(household, 'hv001','hv002','hv270') ,
  join_by(v001==hv001,v002==hv002)
) 

which(wi_compare$v190 != wi_compare$hv270)
# v190 and hv270 are the same in all instances


### survey design

three_yr <- pregnancies[pregnancies$p19<36,]

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
    id = pregnancies$v021 ,
    strata = pregnancies$v023,
    weights = pregnancies$v005,
    data = pregnancies
  )

rur_urb <-
  svyby(~p32==2, ~v025, RUdesign, svymean, vartype=c("se","ci"))
View(rur_urb[,c(1,3)])
