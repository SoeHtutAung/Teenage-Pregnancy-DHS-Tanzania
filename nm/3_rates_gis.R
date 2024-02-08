library(dplyr)
library(haven)
library(survey)
library(tidyverse)
library(plyr)

#Assuming births_clean has been loaded

####################### RATES OF NEONATAL MORTALITY ############################
#Add v024 to "births_clean"

#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)

nm_strata_raw <- svyby(~(neo_mort == 1), ~v024 +v025, design, svymean, na.rm = TRUE)
nm_strata <- nm_strata_raw[,c(1,2,4,6)]  
nm_strata <- nm_strata %>% mutate (nm = nm_strata[,3]*1000)
View(nm_strata)
write.csv(nm_strata, "nm_strata.csv", row.names = T)