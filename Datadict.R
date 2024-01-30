library(haven)
library(dplyr)
individual_IR <- read_dta("~/Downloads/UNICEF DATA/Individual/TZIR82FL.DTA")
household_member_PR <- read_dta("~/Downloads/UNICEF DATA/Household Member/TZPR82FL.DTA")
pregnancy_GR <- read_dta("~/Downloads/UNICEF DATA/Pregnancies/TZGR82FL.DTA")
births_BR <- read_dta("~/Downloads/UNICEF DATA/Births/TZBR82FL.DTA")
pregnancy_postnatal_NR <- read_dta("~/Downloads/UNICEF DATA/Pregnancy and Postnatal Care/TZNR82FL.DTA")

#Data dictionary####

#Births dictionary 
variable_names <- names(births_BR)
variable_labels_BR <- sapply(births_BR, function(x) attr(x, "label"))

dict_births_BR <- data.frame(
  variable = variable_names,
  description = variable_labels_BR
)

#Household member dictionary
variable_labels_PR <- sapply(household_member_PR, function(x) attr(x, "label"))
variable_names_PR <- names(household_member_PR)

dict_household_PR <- data.frame(
  variable = variable_names_PR,
  description = variable_labels_PR
)


#Individual dictionary
variable_labels_IR <- sapply(individual_IR, function(x) attr(x, "label"))
variable_names_IR <- names(individual_IR)

dict_indv_IR <- data.frame(
  variable = variable_names_IR,
  description = variable_labels_IR
)


#Pregnancy dictionary
variable_labels_GR <- sapply(pregnancy_GR, function(x) attr(x, "label"))
variable_names_GR <- names(pregnancy_GR)

dict_preg_GR <- data.frame(
  variable = variable_names_GR,
  description = variable_labels_GR
)

#Pregnancy and postnatal care dictionary
variable_labels_NR <- sapply(pregnancy_postnatal_NR, function(x) attr(x, "label"))
variable_names_NR <- names(pregnancy_postnatal_NR)

dict_postnatal_NR <- data.frame(
  variable = variable_names_NR,
  description = variable_labels_NR
)
