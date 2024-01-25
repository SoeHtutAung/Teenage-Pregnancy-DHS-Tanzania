getwd() # setwd to 'TZDHS' folder
setwd("...")

# install.packages("sf")
library(sf) # new package for spatial data
# replaces 'rgdal' package which was discontinued Oct 2023

library(haven) # to read '.dta' files
library(ggplot2)
library(dplyr)
library(patchwork)

# Extract the '.shp' file
# Bound 2: Regions
bound2 <- st_read("Geospatial Data/SDR data/DHS Boundaries/shps/sdr_subnational_boundaries2.shp")
# ggplot(bound2) + geom_sf() # basic map of Tanzania regions

# Extract the '.dta' file for "Pregnancy and Postnatal Care"
# I've named this "perinatal"
perinatal <- read_dta("Survey Data/Pregnancy and Postnatal Care/TZNR82FL.DTA")
# 7281 obs. of 928 variables; 6220 unique caseid

### this needs to be put through the survey package still!! ###

# create a data frame (row=2,col=31) that has the stillbirth rate by region
sb_region <-
  perinatal %>%
  group_by(v024) %>%
  summarise(
    stillbirth = 1000*mean(p32==2)
  )

# Now add those stillbirth rates to the '.shp' data frame using a join
sb_region <-
  inner_join( bound2, sb_region, by = join_by(REGCODE==v024) )
ggplot(sb_region) + geom_sf(aes(fill=stillbirth))
