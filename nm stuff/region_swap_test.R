births_clean$v025_ru <- relevel(factor(births_clean$v025), ref = 2)

births_clean$wt <- births_clean$v005/1000000

design <- survey::svydesign(id=~v001, 
                            strata =~v023, 
                            weights=~wt,
                            data= births_clean)

model_region1 <- svyglm(factor(neo_mort) ~ v025_ru, 
                       design = design, 
                       family = quasibinomial(), 
                       na.action = na.exclude)

print(exp(coef(model_region)[2])) %>% round(2)
print(exp(coef(model_region1)[2])) %>% round(2)
print (exp (confint(model_region1)[2, ])) %>% round(2)
print(summary(model_region1)$coefficients[2,"Pr(>|t|)"]) %>% round(2)

levels(factor(births_clean$v025)) #Urban to rural #0.73
levels(births_clean$v025_ru) #Rural to urban #1.37

round(prop.table(svytable(~ neo_mort + v025, design = design) ,margin = 2) *100, 2)
