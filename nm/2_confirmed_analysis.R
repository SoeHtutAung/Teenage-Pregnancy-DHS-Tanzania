######################## TABLE 1 CONTEXT - OR, Chi squared, P values ######################## 
#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)


##Urban/rural numbers

svytable(~v025, design = design)

#1. Number of pregnancies with anaemia (v457) 
count(births_clean$v457)

anaemia <- as.data.frame(svytable(~ v457, design = design)  %>% round(0))
anaemia$percent <- (anaemia[,2]/sum(anaemia[,2])*100) %>% round(2)
anaemia$prop<- round(prop.table(svytable(~ v457 + v025, design = design) ,margin = 2) *100, 2)
chi_anaemia <- (summary(svytable(~ v457 + v025, design = design), statistic = "Chisq"))
chi_anaemia$statistic$statistic
chi_anaemia$statistic$p.value
coef(svymean(~v457, design, na.rm = TRUE)%>% round(4))*100 
anaemia$totci<- confint(svymean(~factor(v457), design, na.rm = TRUE))%>% round(4)*100

births_clean$v457<- relevel(factor(births_clean$v457), ref = "4")

#2. Number of pregnancies ever told they had HT (s1125)
births_clean$s1125
count(births_clean$s1125)

hypt <- as.data.frame(svytable(~ s1125, design = design)  %>% round(0))
hypt$percent <- (hypt[,2]/sum(hypt[,2])*100) %>% round(2)
hypt$prop<- round(prop.table(svytable(~ s1125 + v025, design = design) ,margin = 2) *100, 2)
chi_hypt <- (summary(svytable(~ s1125 + v025, design = design), statistic = "Chisq"))
chi_hypt$statistic$statistic
chi_hypt$statistic$p.value

#3. Number of pregnancies with number of ANC visits 
unique(births_clean$ANC_visits)
count(births_clean$ANC_visits) 

anc <- as.data.frame(svytable(~ ANC_visits, design = design)  %>% round(0))
anc$percent <- (anc[,2]/sum(anc[,2])*100) %>% round(2)
anc$prop<- round(prop.table(svytable(~ ANC_visits + v025, design = design) ,margin = 2) *100, 2)
chi_anc <- (summary(svytable(~ ANC_visits + v025, design = design), statistic = "Chisq"))
chi_anc$statistic$statistic
chi_anc$statistic$p.value

births_clean$ANC_visits<- relevel(factor(births_clean$ANC_visits), ref = "4+")

#4. Number of pregnancies in pregnancy intervals - removed

#5. Number of pregnancies by mulitplicity of mothers (v201) 
count(births_clean$v201_cat) 
kids <- as.data.frame(svytable(~ v201_cat, design = design)  %>% round(0))
kids$percent <- (kids[,2]/sum(kids[,2])*100) %>% round(2)
kids$prop<- round(prop.table(svytable(~ v201_cat + v025, design = design) ,margin = 2) *100, 2)
chi_kids <- (summary(svytable(~ v201_cat + v025, design = design), statistic = "Chisq"))
chi_kids$statistic$statistic
chi_kids$statistic$p.value

#6. Mum age at pregnancy (mum_age_pregnancy)
count(births_clean$mum_age_pregnancy_cat)
mumage <- as.data.frame(svytable(~ mum_age_pregnancy_cat, design = design)  %>% round(0))
mumage$percent <- (mumage[,2]/sum(mumage[,2])*100) %>% round(2)
mumage$prop<- round(prop.table(svytable(~ mum_age_pregnancy_cat + v025, design = design) ,margin = 2) *100, 2)
chi_mumage <- (summary(svytable(~ mum_age_pregnancy_cat + v025, design = design), statistic = "Chisq"))
chi_mumage$statistic$statistic
chi_mumage$statistic$p.value

#7. Number by literacy (v155)
count(births_clean$v155_cat)
lit <- as.data.frame(svytable(~ v155_cat, design = design)  %>% round(0))
lit$percent <- (lit[,2]/sum(lit[,2])*100) %>% round(2)
lit$prop<- round(prop.table(svytable(~ v155_cat + v025, design = design) ,margin = 2) *100, 2)
chi_lit <- (summary(svytable(~ v155_cat + v025, design = design), statistic = "Chisq"))
chi_lit$statistic$statistic
chi_lit$statistic$p.value

births_clean$v155_cat<- relevel(factor(births_clean$v155_cat), ref = "2")

#8. Number of pregnancy losses (v245)
count(births_clean$v245_cat)
loss <- as.data.frame(svytable(~ v245_cat, design = design)  %>% round(0))
loss$percent <- (loss[,2]/sum(loss[,2])*100) %>% round(2)
loss$prop<- round(prop.table(svytable(~ v245_cat + v025, design = design) ,margin = 2) *100, 2)
chi_loss <- (summary(svytable(~ v245_cat + v025, design = design), statistic = "Chisq"))
chi_loss$statistic$statistic
chi_loss$statistic$p.value

births_clean$v245_cat<- relevel(factor(births_clean$v245_cat), ref = "0")

#9. Place of delivery for pregnancies (m15)
count(births_clean$m15)
pod <- as.data.frame(svytable(~ m15, design = design)  %>% round(0))
pod$percent <- (pod[,2]/sum(pod[,2])*100) %>% round(2)
pod$prop<- round(prop.table(svytable(~ m15 + v025, design = design) ,margin = 2) *100, 2)
chi_pod <- (summary(svytable(~ m15 + v025, design = design), statistic = "Chisq"))
chi_pod$statistic$statistic
chi_pod$statistic$p.value

births_clean$m15<- relevel(factor(births_clean$m15), ref = "Public sector health facility")

#10. Mode of delivery (m17)
count(births_clean$m17)
del <- as.data.frame(svytable(~ m17, design = design)  %>% round(0))
del$percent <- (del[,2]/sum(del[,2])*100) %>% round(2)
del$prop<- round(prop.table(svytable(~ m17 + v025, design = design) ,margin = 2) *100, 2)
chi_del <- (summary(svytable(~ m17 + v025, design = design), statistic = "Chisq"))
chi_del$statistic$statistic
chi_del$statistic$p.value


#11. Sex of the baby (b4) 
count(births_clean$b4)
sex <- as.data.frame(svytable(~ b4, design = design)  %>% round(0))
sex$percent <- (sex[,2]/sum(sex[,2])*100) %>% round(2)
sex$prop<- round(prop.table(svytable(~ b4 + v025, design = design) ,margin = 2) *100, 2)
chi_sex <- (summary(svytable(~ b4 + v025, design = design), statistic = "Chisq"))
chi_sex$statistic$statistic
chi_sex$statistic$p.value

#12. Gestation at birth (b20)
count(births_clean$b20_cat)
gest <- as.data.frame(svytable(~ b20_cat, design = design)  %>% round(0))
gest$percent <- (gest[,2]/sum(gest[,2])*100) %>% round(2)
gest$prop<- round(prop.table(svytable(~ b20_cat + v025, design = design) ,margin = 2) *100, 2)
chi_gest <- (summary(svytable(~ b20_cat + v025, design = design), statistic = "Chisq"))
chi_gest$statistic$statistic
chi_gest$statistic$p.value

#13. Post natal check (m70)
count(births_clean$m70_cat)
post <- as.data.frame(svytable(~ m70_cat, design = design)  %>% round(0))
post$percent <- (post[,2]/sum(post[,2])*100) %>% round(2)
post$prop<- round(prop.table(svytable(~ m70_cat + v025, design = design) ,margin = 2) *100, 2)
chi_post <- (summary(svytable(~ m70_cat + v025, design = design), statistic = "Chisq"))
chi_post$statistic$statistic
chi_post$statistic$p.value

#14. Wealth v190
count(births_clean$v190)
wlth <- as.data.frame(svytable(~ v190, design = design)  %>% round(0))
wlth$percent <- (wlth[,2]/sum(wlth[,2])*100) %>% round(2)
wlth$prop<- round(prop.table(svytable(~ v190 + v025, design = design) ,margin = 2) *100, 2)
chi_wlth <- (summary(svytable(~ v190 + v025, design = design), statistic = "Chisq"))
chi_wlth$statistic$statistic
chi_wlth$statistic$p.value


#16.marital status v501
count(births_clean$v501_cat)
wed <- as.data.frame(svytable(~ v501_cat, design = design)  %>% round(0))
wed$percent <- (wed[,2]/sum(wed[,2])*100) %>% round(2)
wed$prop<- round(prop.table(svytable(~ v501_cat + v025, design = design) ,margin = 2) *100, 2)
chi_wed <- (summary(svytable(~ v501_cat + v025, design = design), statistic = "Chisq"))
chi_wed$statistic$statistic
chi_wed$statistic$p.value

births_clean$v501_cat<- relevel(factor(births_clean$v501_cat), ref = "Married/living with partner")

#17.smoking v463aa
count(births_clean$v463aa_cat)
smo <- as.data.frame(svytable(~ v463aa_cat, design = design)  %>% round(0))
smo$percent <- (smo[,2]/sum(smo[,2])*100) %>% round(2)
smo$prop<- round(prop.table(svytable(~ v463aa_cat + v025, design = design) ,margin = 2) *100, 2)
chi_smo <- (summary(svytable(~ v463aa_cat + v025, design = design), statistic = "Chisq"))
chi_smo$statistic$statistic
chi_smo$statistic$p.value


#18.BMI - v445################################
count(births_clean$v445_cat)
bmi <- as.data.frame(svytable(~ v445_cat, design = design, na.action = na.omit)  %>% round(0))
bmi$percent <- (bmi[,2]/sum(bmi[,2])*100) %>% round(2)
bmi$prop<- round(prop.table(svytable(~ v445_cat + v025, design = design) ,margin = 2) *100, 2)
chi_bmi <- (summary(svytable(~ v445_cat + v025, design = design), statistic = "Chisq"))
chi_bmi$statistic$statistic
chi_bmi$statistic$p.value

#19.assistance at delivery - senior person attednign
count(births_clean$senior_delivery_attendant)
aad <- as.data.frame(svytable(~ senior_delivery_attendant, design = design)  %>% round(0))
aad$percent <- (aad[,2]/sum(aad[,2])*100) %>% round(2)
aad$prop<- round(prop.table(svytable(~ senior_delivery_attendant + v025, design = design) ,margin = 2) *100, 2)
chi_aad <- (summary(svytable(~ senior_delivery_attendant + v025, design = design), statistic = "Chisq"))
chi_aad$statistic$statistic
chi_aad$statistic$p.value

births_clean$v501_cat<- relevel(factor(births_clean$v501_cat), ref = "Married/living with partner")


#21.bw m19
count(births_clean$m19_cat)
lowbw <- as.data.frame(svytable(~ m19_cat, design = design)  %>% round(0))
lowbw$percent <- (lowbw[,2]/sum(lowbw[,2])*100) %>% round(2)
lowbw$prop<- round(prop.table(svytable(~ m19_cat + v025, design = design) ,margin = 2) *100, 2)
chi_lowbw <- (summary(svytable(~ m19_cat + v025, design = design), statistic = "Chisq"))
chi_lowbw$statistic$statistic
chi_lowbw$statistic$p.value

births_clean$m19_cat<- relevel(factor(births_clean$m19_cat), ref = "notlowbw")

########################  new - TABLE 2 - ODDS RATIO ######################## 

#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)

#Model 1. Rural / Urban odds ratio
model_1 <- svyglm(factor(neo_mort) ~ factor(v025), 
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.exclude)

print(exp(coef(model_1)[2])) %>% round(2)
print (exp (confint(model_1)[2, ])) %>% round(2)
print(summary(model_1)$coefficients[2,"Pr(>|t|)"]) %>% round(2)


##############!!!!!!!!!NIKKI CHOOSE THIS ONE - DIFFERENT MODELS WITH DIFFERENT CATAGOREIS#######################

##model2_2 ##NEW - SOCIO-DEMOGRAPHIC FACTORS
# mother's age at pregnancy
# literacy re-leveled
# marital status re-leveled
# Wealth quintile


model_2_2 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      mum_age_pregnancy + # mother's age at pregnancy - numeric
                      v155_cat + # literacy re-leveled - factor
                      v501_cat + # marital status re-leveled - factor
                      factor(v190), # Wealth quintile - not leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_2_2)[2])) %>% round(2)
print (exp (confint(model_2_2)[2, ])) %>% round(2)
print(summary(model_2_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)


##model maternal factors ##NEW##
model_3_2 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      mum_age_pregnancy + # mother's age at pregnancy - numeric
                      v155_cat + # literacy re-leveled - factor
                      v501_cat + # marital status re-leveled - factor
                      factor(v190) + # Wealth quintile - not leveled - factor
                      factor(v463aa_cat) + # smoking - factor
                      v445 + # BMI - numeric  
                      factor(s1125), #ever had hypertension - not leveled - factor 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_3_2)[2])) %>% round(2)
print (exp (confint(model_3_2)[2, ])) %>% round(2)
print(summary(model_3_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

##model pregnancy ##NEW

model_4_2 <-  svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                       mum_age_pregnancy + # mother's age at pregnancy - numeric
                       v155_cat + # literacy re-leveled - factor
                       v501_cat + # marital status re-leveled - factor
                       factor(v463aa_cat) + # smoking - factored
                       v445 + # BMI - numeric
                       factor(v190) + # Wealth quintile - not leveled - factored
                       factor(s1125) +  #ever had hypertension - not leveled - factor 
                       ANC_visits + #number of ANC visits - factor
                       v501_cat + #Assistance at delivery - levels + factor
                       m15 + #Place of delivery - levels + factor
                       factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                       m19, #Birth Weight - numeric
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_4_2)[2])) %>% round(2)
print (exp (confint(model_4_2)[2, ])) %>% round(2)
print(summary(model_4_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

##model previous preganacies######################

model_5_2 <-  svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                       mum_age_pregnancy + # mother's age at pregnancy - numeric
                       v155_cat + # literacy re-leveled - factor
                       v501_cat + # marital status re-leveled - factor
                       factor(v463aa_cat) + # smoking - factored
                       v445 + # BMI - numeric
                       factor(v190) + # Wealth quintile - not leveled - factored
                       factor(s1125) +  #ever had hypertension - not leveled - factor 
                       ANC_visits + #number of ANC visits - factor
                       v501_cat + #Assistance at delivery - levels + factor
                       m15 + #Place of delivery - levels + factor
                       factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                       m19+ #Birth Weight - numeric
                       v245_cat + # pregnancy losses - re-leveled + factored
                       v201, #Multiple pregnancies - not leveled + numeric
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_5_2)[2])) %>% round(2)
print (exp (confint(model_5_2)[2, ])) %>% round(2)
print(summary(model_5_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)


######################## Figure to illustrate table 2  ######################## 
# Assuming you have model_1, model_2, model_3, model_4, model_5 fitted

# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients <- c(0.73, 0.86, 1.50, 1.54, 1.55)
lower_ci <- c(0.46, 0.48, 0.58, 0.59, 0.38)
upper_ci <- c(1.16, 1.53, 3.91, 4.01, 4.09)
labels <- c("Model 1: NMR ~ Urban/Rural ", "Model 2: Model 1 + Sociodemographics", "Model 3: Model 2 + Maternal Health", "Model 4: Model 3 + Pregnacy", "Model 5: Model 4 + Previous pregnancies")

forest_data <- matrix(c(rep("", 5), lower_ci, coefficients, upper_ci), 
                      ncol = 4, byrow = FALSE)

nm_cat_plot <- forestplot(labeltext = labels, 
                          mean = coefficients, 
                          lower = lower_ci, 
                          upper = upper_ci,
                          xlab = "Coefficient Value",
                          zero = 0,
                          lineheight = "auto",
                          boxsize = 0.5,
                          col = fpColors(box = "darkred", line = "grey", summary = "royalblue"))


pdf("nm_cat_forestplot.pdf", width = 10, height = 5)
print(nm_cat_plot)
dev.off()



######################## Figure of individual ORs  ######################## 
#ORs of Rural / Urban coefficient OR changes by individual variables

### FOR FOREST PLOT#####

nm_forest <- as.data.frame(matrix(nrow=15,ncol=5))
names(nm_forest) <- c("each_factor","p-value","ORadj","OR_lower","OR_higher")

nm_forest[1,] <-
  c("Residence","NA",
    exp(coef(model_1))[2],
    exp(confint(model_1))[2,1],
    exp(confint(model_1))[2,2]
  )


#Creating individual models with v025 (which are significant from table one)
##1.AGE#######################

model_age <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      mum_age_pregnancy, # mother's age at pregnancy - numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_age)[2])) %>% round(2)
print (exp (confint(model_age)[2, ])) %>% round(2)
print(summary(model_age)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[2,] <-
  c("Age","NA",
    exp(coef(model_age))[2],
    exp(confint(model_age))[2,1],
    exp(confint(model_age))[2,2]
  )

##2.LITERACY##################

model_edu <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v155_cat, # literacy re-leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_edu)[2])) %>% round(2)
print (exp (confint(model_edu)[2, ])) %>% round(2)
print(summary(model_edu)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[3,] <-
  c("Literacy","NA",
    exp(coef(model_edu))[2],
    exp(confint(model_edu))[2,1],
    exp(confint(model_edu))[2,2]
  )


##3.MARITAL STATUS##############

model_mar <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v501_cat, # marital status re-leveled - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_mar)[2])) %>% round(2)
print (exp (confint(model_mar)[2, ])) %>% round(2)
print(summary(model_mar)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[4,] <-
  c("Marital status","NA",
    exp(coef(model_mar))[2],
    exp(confint(model_mar))[2,1],
    exp(confint(model_mar))[2,2]
  )


##4.WEALTH#####################
model_wealth <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                         factor(v190), # Wealth quintile - not leveled - factored
                       design = design, 
                       family = quasibinomial(), 
                       na.action = na.omit)

print(exp(coef(model_wealth)[2])) %>% round(2)
print (exp (confint(model_wealth)[2, ])) %>% round(2)
print(summary(model_wealth)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[5,] <-
  c("Wealth","NA",
    exp(coef(model_wealth))[2],
    exp(confint(model_wealth))[2,1],
    exp(confint(model_wealth))[2,2]
  )

##5.SMOKING####################
model_smo <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      factor(v463aa_cat),# smoking - factored
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_smo)[2])) %>% round(2)
print (exp (confint(model_smo)[2, ])) %>% round(2)
print(summary(model_smo)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[6,] <-
  c("Smoking","NA",
    exp(coef(model_smo))[2],
    exp(confint(model_smo))[2,1],
    exp(confint(model_smo))[2,2]
  )


##6.HTN########################
model_htn <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      factor(s1125), #ever had hypertension - not leveled - factor 
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_htn)[2])) %>% round(2)
print (exp (confint(model_htn)[2, ])) %>% round(2)
print(summary(model_htn)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[7,] <-
  c("Hypertension","NA",
    exp(coef(model_htn))[2],
    exp(confint(model_htn))[2,1],
    exp(confint(model_htn))[2,2]
  )


##7.BMI#########################
model_bmi <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v445,# BMI - numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_bmi)[2])) %>% round(2)
print (exp (confint(model_bmi)[2, ])) %>% round(2)
print(summary(model_bmi)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[8,] <-
  c("BMI","NA",
    exp(coef(model_bmi))[2],
    exp(confint(model_bmi))[2,1],
    exp(confint(model_bmi))[2,2]
  )


##8.ANC############
model_anc <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      ANC_visits,#number of ANC visits - factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_anc)[2])) %>% round(2)
print (exp (confint(model_anc)[2, ])) %>% round(2)
print(summary(model_anc)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[9,] <-
  c("Antenatal Care Visits","NA",
    exp(coef(model_anc))[2],
    exp(confint(model_anc))[2,1],
    exp(confint(model_anc))[2,2]
  )

##9.POD###########
model_pod <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      m15,#Place of delivery - levels + factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_pod)[2])) %>% round(2)
print (exp (confint(model_pod)[2, ])) %>% round(2)
print(summary(model_pod)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[10,] <-
  c("Place of delivery","NA",
    exp(coef(model_pod))[2],
    exp(confint(model_pod))[2,1],
    exp(confint(model_pod))[2,2]
  )

##10.AAD#######
model_aad <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v501_cat,#Assistance at delivery - levels + factor
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_aad)[2])) %>% round(2)
print (exp (confint(model_aad)[2, ])) %>% round(2)
print(summary(model_aad)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[11,] <-
  c("Assistance at delivery","NA",
    exp(coef(model_aad))[2],
    exp(confint(model_aad))[2,1],
    exp(confint(model_aad))[2,2]
  )


##11. MOD ###########
model_mod <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      factor(m17), #Mode of delivery (C-section?) - not leveled + factored
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_mod)[2])) %>% round(2)
print (exp (confint(model_mod)[2, ])) %>% round(2)
print(summary(model_mod)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[12,] <-
  c("Mode of delivery","NA",
    exp(coef(model_mod))[2],
    exp(confint(model_mod))[2,1],
    exp(confint(model_mod))[2,2]
  )


##12. BW #############
model_bw <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                     m19, #Birth Weight - numeric
                   design = design, 
                   family = quasibinomial(), 
                   na.action = na.omit)

print(exp(coef(model_bw)[2])) %>% round(2)
print (exp (confint(model_bw)[2, ])) %>% round(2)
print(summary(model_bw)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[13,] <-
  c("Birthweight","NA",
    exp(coef(model_bw))[2],
    exp(confint(model_bw))[2,1],
    exp(confint(model_bw))[2,2]
  )


##13. TOTAL PREG###########
model_tot <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                      v201, #Multiple pregnancies - not leveled + numeric
                    design = design, 
                    family = quasibinomial(), 
                    na.action = na.omit)

print(exp(coef(model_tot)[2])) %>% round(2)
print (exp (confint(model_tot)[2, ])) %>% round(2)
print(summary(model_tot)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[14,] <-
  c("Total preg","NA",
    exp(coef(model_tot))[2],
    exp(confint(model_tot))[2,1],
    exp(confint(model_tot))[2,2]
  )


##14. LOSSES #############
model_loss <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                       v245_cat,# pregnancy losses - re-leveled + factored
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_loss)[2])) %>% round(2)
print (exp (confint(model_loss)[2, ])) %>% round(2)
print(summary(model_loss)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

nm_forest[15,] <-
  c("Prev preg losses","NA",
    exp(coef(model_loss))[2],
    exp(confint(model_loss))[2,1],
    exp(confint(model_loss))[2,2]
  )


# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients_ind <- as.numeric(nm_forest$ORadj)
lower_ci_ind <- as.numeric(nm_forest$OR_lower)
upper_ci_ind <- as.numeric(nm_forest$OR_higher)
labels_ind <- nm_forest$each_factor

base_data_nm <-
  tibble::tibble(
    mean  = as.numeric(nm_forest$ORadj),
    lower = as.numeric(nm_forest$OR_lower),
    upper = as.numeric(nm_forest$OR_higher),
    labels = nm_forest$each_factor, 
    OR = c("0.73", "0.73", "0.72", "0.75", "0.85", 
           "0.81", "0.73", "1.00", "0.68", "0.73", 
           "0.75", "0.77", "0.56", "0.68", "0.74"
    ))

#NEW FOREST DESIGN
base_data %>%
  forestplot(
    title = "OR for Rural vs Urban Rates of Neonatal Mortality by Confounding Factor",
    labeltext = c(labels, OR),
    clip = c(0.1,2.5),
    zero = 1,
    vertices = TRUE,
    lineheight = "auto",
    boxsize = 0.5
  ) %>%
  fp_add_header(
    labels = c("", "Factor"),
    OR = c("", "Residence OR")
  ) %>%
  fp_set_style(
    box = "#74B72E",
    line = "darkgreen",
    summary = "#74B72E",
    txt_gp = fpTxtGp(
      ticks = gpar(cex = 1),
      xlab  = gpar(cex = 1.5)))

#Saving forestplot
pdf("nm_ind_forestplot.pdf", width = 10, height = 5)
print(nm_ind_plot)
dev.off()

########################  new - TABLE 2 - ODDS RATIO ######################## 

#Setting up survey package 
births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)

#Model 1. Rural / Urban odds ratio
model_1 <- svyglm(factor(neo_mort) ~ factor(v025), 
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.exclude)

print(exp(coef(model_1)[2])) %>% round(2)
print (exp (confint(model_1)[2, ])) %>% round(2)
print(summary(model_1)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#Model 2. model_1 + Socio-demographic factors
# mother's age at pregnancy
# literacy re-leveled
# marital status re-leveled
# smoking
# BMI
# Wealth quintile
# pregnancy losses

model_2 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    mum_age_pregnancy + # mother's age at pregnancy - numeric
                    v155_cat + # literacy re-leveled - factor
                    v501_cat + # marital status re-leveled - factor
                    factor(v463aa_cat) + # smoking - factor
                    v445 + # BMI - numeric
                    factor(v190) + # Wealth quintile - not leveled - factor
                    v245_cat, # pregnancy losses - re-leveled + factored
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_2)[2])) %>% round(2)
print (exp (confint(model_2)[2, ])) %>% round(2)
print(summary(model_2)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#Model 3. model 2 + Pregnancy factors
#Number of pregnancies with hypertension (s1125) 
#Number of ANC visit
#Multiple pregnancies (v201) 

model_3 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    mum_age_pregnancy + # mother's age at pregnancy - numeric
                    v155_cat + # literacy re-leveled - factor
                    v501_cat + # marital status re-leveled - factor
                    factor(v463aa_cat) + # smoking - factor
                    v445 + # BMI - numeric
                    factor(v190) + # Wealth quintile - not leveled - factor
                    v245_cat + # pregnancy losses - re-leveled + factored
                    factor(s1125) +  #ever had hypertension - not leveled - factor 
                    ANC_visits + #number of ANC visits - factor
                    v201, #Multiple pregnancies - not leveled + numeric
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_3)[2])) %>% round(2)
print (exp (confint(model_3)[2, ])) %>% round(2)
print(summary(model_3)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

#Model 4. model 3 + Labour / delivery / baby factors 
# Place of delivery (m15) 
# Assistance at delivery 
# Mode of delivery (m17) 
# Birth Weight (m19)  

model_4 <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                    mum_age_pregnancy + # mother's age at pregnancy - numeric
                    v155_cat + # literacy re-leveled - factor
                    v501_cat + # marital status re-leveled - factor
                    factor(v463aa_cat) + # smoking - factored
                    v445 + # BMI - numeric
                    factor(v190) + # Wealth quintile - not leveled - factored
                    v245_cat + # pregnancy losses - re-leveled + factored
                    factor(s1125) +  #ever had hypertension - not leveled - factor 
                    ANC_visits + #number of ANC visits - factor
                    v201 + #Multiple pregnancies - not leveled + numeric
                    v501_cat + #Assistance at delivery - levels + factor
                    m15 + #Place of delivery - levels + factor
                    factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                    m19, #Birth Weight - numeric
                  design = design, 
                  family = quasibinomial(), 
                  na.action = na.omit)

print(exp(coef(model_4)[2])) %>% round(2)
print (exp (confint(model_4)[2, ])) %>% round(2)
print(summary(model_4)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

######################## Figure to illustrate table 2  ######################## 
# Assuming you have model_1, model_2, model_3, model_4 fitted

# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients <- c(0.73, 1.50, 1.53, 1.55)
lower_ci <- c(0.46, 0.57, 0.60, 0.59)
upper_ci <- c(1.16, 3.93, 3.88, 4.09)
labels <- c("Model 1", "Model 2", "Model 3", "Model 4")

forest_data <- matrix(c(rep("", 4), lower_ci, coefficients, upper_ci), 
                      ncol = 4, byrow = FALSE)

forestplot(labeltext = labels, 
           mean = coefficients, 
           lower = lower_ci, 
           upper = upper_ci,
           xlab = "Coefficient Value",
           zero = 0,
           lineheight = "auto",
           boxsize = 0.5,
           col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"))


######################## Figure of categorical ORs  ######################## 
#ORs of Rural / Urban coefficient OR changes by individual categorical models

#Creating individual models with v025
model_region <- svyglm(factor(neo_mort) ~ factor(v025), 
                       design = design, 
                       family = quasibinomial(), 
                       na.action = na.exclude)

print(exp(coef(model_region)[2])) %>% round(2)
print (exp (confint(model_region)[2, ])) %>% round(2)
print(summary(model_region)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

model_sociodem <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                           mum_age_pregnancy + # mother's age at pregnancy - numeric
                           v155_cat + # literacy re-leveled - factor
                           v501_cat + # marital status re-leveled - factor
                           factor(v463aa_cat) + # smoking - factor
                           v445 + # BMI - numeric
                           factor(v190) + # Wealth quintile - not leveled - factor
                           v245_cat, # pregnancy losses - re-leveled + factored
                         design = design, 
                         family = quasibinomial(), 
                         na.action = na.omit)


print(exp(coef(model_sociodem)[2])) %>% round(2)
print (exp (confint(model_sociodem)[2, ])) %>% round(2)
print(summary(model_sociodem)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

model_preg <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                       factor(s1125) +  #ever had hypertension - not leveled - factor 
                       ANC_visits + #number of ANC visits - factor
                       v201, #Multiple pregnancies - not leveled + numeric
                     design = design, 
                     family = quasibinomial(), 
                     na.action = na.omit)

print(exp(coef(model_preg)[2])) %>% round(2)
print (exp (confint(model_preg)[2, ])) %>% round(2)
print(summary(model_preg)$coefficients[2,"Pr(>|t|)"]) %>% round(2)


model_labour <- svyglm(factor(neo_mort) ~ factor(v025) + # rural/urban
                         v501_cat + #Assistance at delivery - levels + factor
                         m15 + #Place of delivery - levels + factor
                         factor(m17) + #Mode of delivery (C-section?) - not leveled + factored
                         m19, #Birth Weight - numeric
                       design = design, 
                       family = quasibinomial(), 
                       na.action = na.omit)

print(exp(coef(model_labour)[2])) %>% round(2)
print (exp (confint(model_labour)[2, ])) %>% round(2)
print(summary(model_labour)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

get_exp_coef_ci <- function(model, coef_index) {
  # Extract the exponentiated coefficient
  coef_value <- exp(coef(model)[coef_index]) %>% round(2)
  
  # Extract the confidence intervals and exponentiate them
  ci_values <- exp(confint(model)[coef_index, ]) %>% round(2)
  
  # Combine into a named vector
  c(coef = coef_value, lower = ci_values[1], upper = ci_values[2])
}

# Use the function to get the required values for the second coefficient
exp_coef_ci_region <- get_exp_coef_ci(model_region, 2)
exp_coef_ci_sociodem <- get_exp_coef_ci(model_sociodem, 2)
exp_coef_ci_preg <- get_exp_coef_ci(model_preg, 2)
exp_coef_ci_labour <- get_exp_coef_ci(model_labour, 2)

# Combine the coefficients and confidence intervals into a matrix for plotting
coefficients <- c(exp_coef_ci_region['coef.factor(v025)2'], exp_coef_ci_sociodem['coef.factor(v025)2'], exp_coef_ci_preg['coef.factor(v025)2'], exp_coef_ci_labour['coef.factor(v025)2'])
lower_ci <- c(exp_coef_ci_region['lower.2.5 %'], exp_coef_ci_sociodem['lower.2.5 %'], exp_coef_ci_preg['lower.2.5 %'], exp_coef_ci_labour['lower.2.5 %'])
upper_ci <- c(exp_coef_ci_region['upper.97.5 %'], exp_coef_ci_sociodem['upper.97.5 %'], exp_coef_ci_preg['upper.97.5 %'], exp_coef_ci_labour['upper.97.5 %'])

# Define the model names for the plot
labels <- c("Model Region", "Model Sociodemographic", "Model Pregnancy", "Model Labour")

# Specify the full file path where you want to save the PNG file
file_path <- "/unicef/nm/forest_plot.png"

# Open a PNG device with the specified file path
png(file_path, width = 1200, height = 800)

# Create the forest plot
forestplot(labeltext = labels,
           mean = coefficients,
           lower = lower_ci,
           upper = upper_ci,
           xlab = "Odds Ratio",
           zero = 1, # Reference line at odds ratio of 1
           lineheight = "auto",
           boxsize = 0.5,
           col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"), 
           title = "Odds Ratio of Rural / Urban Coefficient OR Changes by Individual Categorical Models")

dev.off()
