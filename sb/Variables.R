## Load NR file into "perinatal"

#####

dict_indv_IR <- data.frame(
  variable = sapply(Individual, function(x) attr(x, "label")),
  description = names(Individual)
)

dict_postnatal_NR <- data.frame(
  variable = sapply(perinatal, function(x) attr(x, "label")),
  description = names(perinatal)
)

##### Individuals

perinatal %>% count(pidx)
# 1  6220 # 6220 - 999 = 5221 had 1
# 2   999 # 999 - 61 = 938 had 2
# 3    61 # 61 - 1 = 60 had 3
# 4     1 # 1 had 4
# 5221 + 938 + 60 + 1 = 6220

perinatal[
  !duplicated(perinatal$caseid) ,
  "caseid"
]
# 6220 with >0 pregnancy outcomes

######### Create the survey object ############################################
sbp <- perinatal %>%
  mutate(
    wt = v005/1e6,
    stillbirth = (p32==2),
    birth_age = (p3-v011)/12
  )
mysb_design <- svydesign( id=~v021, strata=~v023, weights=~wt, data=sbp )

###############################################################################

##### Main Variables of Interest

count(Individual, p32_01)
# 1 [born alive]  10492
# 2 [born dead]     141
# 3 [miscarriage]   620
# 4 [abortion]       18
#NA                3983

count(Individual, m80_1)
# 1 [most recent live birth]  5745
# 2       prior live birth       0
# 3 [most recent stillbirth]    91
# 4       prior stillbirth       0
# 5 [miscarriage/abortion]     384
#NA                           9034

count(perinatal, p32)
# 1 [born alive]   6574
# 2 [born dead]     134
# 3 [miscarriage]   556
# 4 [abortion]       17
#NA                   0

count(Individual,v025)
# 5441 urban, 9813 rural

count(perinatal,v025)
# 2087 urban, 5194 rural

# 19 risk factors assessed in the systematic review (from 54 mentioned across the papers)

# Sociodemographic characteristics
  # maternal age - strong - usually cut up into age groups
    hist(perinatal$v013) # age group at interview
    hist((perinatal$p3 - perinatal$v011)/12)
    svyby(~birth_age, ~v025+stillbirth, mysb_design, svymean)
    # round(svytable(~v013+v025+stillbirth, mysb_design),2)
    # 
  # educational attainment - strong - cut along primary edu
    count(perinatal,v106)
    # 0 none, 1 primary, 2 secondary, 3 higher
    # no 8's in NR!
    sb_edu <- svytable(~v106+v025+stillbirth, mysb_design)
    
  # marital status
    count(perinatal, v501)
    # current status, unordered factor
    # 0 never, 1 married, 2 living with, 3 widowed, 4 divorced, 5 separated
    # exploratory: v221, v535, v542, v544, v705
    # husband: v701:v705, v715, v729:v730, v739, v743a:v746
    svytable(~v501+v025+stillbirth, mysb_design)
    
  # place of residence
    count(perinatal, v025)
    # factor of interest!
      # exploratory: v115, v483a:v483b
      # 483a: 0-599, 600+
      # 483b: 11-14 motorised, 21-24 non-motorised, 96 other
# Ethnicity (v131) not asked in DHS-8; all NAs

    count(perinatal,v190) #wealth
    svytable(~v190+stillbirth+v025,mysb_design)
    # explain why we're not using 190a

count(Individual,v714) # currently working
head(Individual$v732) # respondent employed all year/seasonal
count(perinatal,v717)


# Pregnancy-related factors
  # antenatal care - SIG
    count(perinatal, m14)
    # 0 none (635), 1-16 (most), 98 don't know (23), NA (1328)
    # exploratory: m13, m13a, m57a:m57x, s417aa:s417ad
    # 0, 1-3, 4+, 98, NA
    sb_anc <- as.data.frame(
      svytable(~m14+stillbirth+v025, mysb_design)
    )
      
  # antepartum hemorrhage - SIG
  # birth weight - SIG - cutoffs 2.5kg , 4kg
    count(perinatal, m19) # all stillbirths not weighed
    # 500:6000 grams, 9996 not weighed (1240), 9998 dk (176), NA (707)
    # m19a: of 5158 w/ wt, 3625 from written card, 1533 from memory
  # gestational age
    count(perinatal, p20)
    sb_gest <- as.data.frame(
      svytable(~p20+stillbirth+v025, mysb_design)
    )
    
  # parity
    #v224?
  # mode of arrival (referral or home) - SIG
  # sex of the newborn
    # count(perinatal, p4)
    # 1 male, 2 female, NA (termination) n=707
    # all stillbirths are in NAs!
  # history of previous stillbirth - SIG
    # v228 is only history of previous termination of pregnancy
  # mode of delivery
  # multiple gestations
    count(perinatal, p0)
    # 0 single (7057), 1 & 2 twins (ordered; 112 each)
  # PROM (Premature Rupture of Membrane)
count(perinatal,v225) #wanted pregnancy 
  # 1 then 2 later 3 not at all
  # 575 responses, 6706 NAs
  svytable(~v225+stillbirth+v025, mysb_design)

# Medical-related factors
  # anemia - SIG
    count(perinatal, v457)
    # ordered 1:4 decreasing severity, NA (3643)
    svytable(~v457+stillbirth+v025, mysb_design)
  # DM (Diabetes Mellitus)
  # HIV serostatus
  # hypertension (HPN) - SIG
# Other Medical
    count(perinatal, m17)
    # 0 no, 1 yes, NA (573)
perinatal$v445 #BMI
perinatal$m17 #C section
  svytable(~m17+stillbirth+v025, mysb_design)
perinatal$s1125 #blood pressure
  svytable(~s1125+stillbirth+v025, mysb_design)


##################################################################
  
  ##### The 'p's
  
  # pord: pregnancy order number; cts 1-17
  # p0: pregnancy is multiple; 0 is singleton, 1 first twin, 2 second twin
  # p3: End of pregnancy (cmc)
  # p4: sex of child; 1 male, 2 female, NA (termination) n=707
  # p11 & p12?
  # p16: child's line number in HH; 0 unlisted (173), 2-50, NA (921)
  # p18: End of pregnancy (cdc)
  
  ##### The 'm's
  
  # m1: number of tetanus injections before birth;
  # 0 none (1234), 7 7+ (4), 8 dk (30), NA (1456) 
  # m1a: number of tetanus injections before pregnancy;
  # 0 none (653), 7 7+ (16), 8 dk (39), NA (4674) 
  # m1d: years ago received last tetanus injection before pregnancy; 5344 NAs
  # m1e: last tetanus injection before pregnancy (cmc); 5344 NAs
  # m2's: attended by... with 1328 NA's more than 2 yrs
  
  # m10: wanted pregnancy when became pregnant;
  # 1 then 2 later 3 no more
  # m11: desired time would have waited;
  # 101:152 months, 201:233 years, 998 dk (119), NA (5441) 
  # :m82, mh17:mh25
  
  ##### The 's's
  
  
  "
b0 multiple birth indicator
b3 DOB of child
b11 preceding birth interval
b20 duration of pregnancy

"
  
  
##### Behavioural

###### Tobacco

tobacco_ids <-
  which(
    Individual$v463a != 0 | # 69 with yes (smoke cigs)
      Individual$v463b != 0 | # 3 with yes (smoke pipe)
      Individual$v463c != 0 | # 5 with yes (chew)
      Individual$v463d != 0 | # 7 with yes (snuff)
      Individual$v463e != 0 | # 8 with yes (kretek)
      Individual$v463f != 0 | # 8 with yes (cigars)
      # Individual$v463g != 0 | # 0 with yes
      # Individual$v463h != 0 | # 0 with yes
      # Individual$v463i != 0 | # 0 with yes
      # Individual$v463j != 0 | # 15254 nas
      # Individual$v463k != 0 | # 15254 nas
      # Individual$v463l != 0 | # 15254 nas
      # Individual$v463x != 0 | # 0 with yes
      Individual$v463z != 1 | # 86 tobacco users
      Individual$v463aa != 0 | # 43 with 1 or 2
      Individual$v463ab != 0 | # 69 with 1 or 2
      Individual$v464 %in% 1:80 # 15229 nas # only 25 smokers
  ) # 86 total, maps perfectly with v463z

smokers <-
  Individual[
    tobacco_ids ,
    c(4754:4759, 4767:4770)
  ]
