library(survey)
library(dplyr)
library(haven)
library(ggplot2)

# setwd("...")
perinatal <- read_dta("Survey Data/Pregnancy and Postnatal Care/TZNR82FL.DTA")

OR_df <-
  perinatal[
    which(perinatal$p32 <= 2) , # 6709 records out of 7281
    c( "caseid", "v025", "p32",
       "v005", "v011", "v021", "v023", "v024", # interview characteristics
       "p3", "p20", "m14", "m17", # pregnancy outcome information
       "v106", "v190", "v445", "v501", "v731", # maternal background
       "s1125" # hypertension
    )
  ] %>%
  mutate(
    wt = v005/1e6,
    stillbirth = ifelse(p32==2, 1, 0),
    residence = relevel(factor(v025), ref=2),
    matage = cut((p3-v011)/12, breaks = c(0,20,25,35,50), right = FALSE),
    gest = relevel(
      cut(p20, breaks = c(0,8,10), right = TRUE), ref = 2
      ),
    anc = ifelse(
      is.na(m14) | m14==98, "unknown" ,
      cut(m14, breaks = c(0,1,4,98,100), right = FALSE)
    ),
    emp_year = ifelse(v731==0, 0, 1),
    edu = factor(
      ifelse( v106==0,0, ifelse(v106==1,1,2) )
    ),
    bmi = relevel(
      cut(v445, breaks = c(1200, 1850, 2500, 3000, 6000)), ref = 2
    ),
    marr = factor(
      ifelse(
        v501==0,"Never",
        ifelse(
          v501==1 | v501==2, "Current", "Formerly"
        )
      )
    )
  )

OR_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=OR_df
  )

sb_forest <- as.data.frame(matrix(nrow=11,ncol=5))
names(sb_forest) <- c("each_factor","p-value","ORadj","OR_lower","OR_higher")

glm_res <- svyglm(stillbirth~residence, OR_design, family=quasibinomial())
sb_forest[1,] <-
  c("residence","NA",
    exp(coef(glm_res))[2],
    exp(confint(glm_res))[2,1],
    exp(confint(glm_res))[2,2]
  )

glm_age <- svyglm(stillbirth~residence+matage, OR_design, family=quasibinomial())
sb_forest[2,] <-
  c("age","0.003",
    exp(coef(glm_age))[[2]],
    exp(confint(glm_age))[[2,1]],
    exp(confint(glm_age))[[2,2]]
  )

glm_edu <- svyglm(stillbirth~residence+edu, OR_design, family=quasibinomial())
sb_forest[3,] <-
  c("edu","<0.001",
    exp(coef(glm_edu))[[2]],
    exp(confint(glm_edu))[[2,1]],
    exp(confint(glm_edu))[[2,2]]
  )

glm_hyp <- svyglm(stillbirth~residence+s1125, OR_design, family=quasibinomial())
sb_forest[4,] <-
  c("hypertension","<0.001",
    exp(coef(glm_hyp))[[2]],
    exp(confint(glm_hyp))[[2,1]],
    exp(confint(glm_hyp))[[2,2]]
  )

glm_mode <- svyglm(stillbirth~residence+m17, OR_design, family=quasibinomial())
sb_forest[5,] <-
  c("c-section","<0.001",
    exp(coef(glm_mode))[[2]],
    exp(confint(glm_mode))[[2,1]],
    exp(confint(glm_mode))[[2,2]]
  )

glm_marr <- svyglm(stillbirth~residence+marr, OR_design, family=quasibinomial())
sb_forest[6,] <-
  c("married","0.002",
    exp(coef(glm_marr))[[2]],
    exp(confint(glm_marr))[[2,1]],
    exp(confint(glm_marr))[[2,2]]
  )

glm_wealth <- svyglm(stillbirth~residence+v190, OR_design, family=quasibinomial())
sb_forest[7,] <-
  c("wealth","<0.001",
    exp(coef(glm_wealth))[[2]],
    exp(confint(glm_wealth))[[2,1]],
    exp(confint(glm_wealth))[[2,2]]
  )

glm_emp <- svyglm(stillbirth~residence+emp_year, OR_design, family=quasibinomial())
sb_forest[8,] <-
  c("employment","0.021",
    exp(coef(glm_emp))[[2]],
    exp(confint(glm_emp))[[2,1]],
    exp(confint(glm_emp))[[2,2]]
  )

glm_gest <- svyglm(stillbirth~residence+gest, OR_design, family=quasibinomial())
sb_forest[9,] <-
  c("preterm","0.011",
    exp(coef(glm_gest))[[2]],
    exp(confint(glm_gest))[[2,1]],
    exp(confint(glm_gest))[[2,2]]
  )

glm_bmi <- svyglm(stillbirth~residence+bmi, OR_design, family=quasibinomial())
sb_forest[10,] <-
  c("bmi","<0.001",
    exp(coef(glm_bmi))[[2]],
    exp(confint(glm_bmi))[[2,1]],
    exp(confint(glm_bmi))[[2,2]]
  )

########################## FROM prev_sb.R #############################

pmc <- psb[!is.na(psb$prev_mc),]
mc_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=pmc
  )

glm_prev_mc <-
  svyglm(stillbirth~relevel(factor(v025),ref=2)+prev_mc,
         mc_design,
         family = quasibinomial())
sb_forest[11,] <-
  c("recent miscarriage","<0.001",
    exp(coef(glm_prev_mc))[[2]],
    exp(confint(glm_prev_mc))[[2,1]],
    exp(confint(glm_prev_mc))[[2,2]]
  )
exp(coef(glm_prev_mc)) # OR = 2.46
exp(confint(glm_prev_mc)) # CI: 0.82-7.41

######################## Figure to illustrate changes in OR  ###########

library(forestplot)

# Vectors for storing the exponentiated coefficients and their confidence intervals
coefficients <- as.numeric(sb_forest$ORadj)
lower_ci <- as.numeric(sb_forest$OR_lower)
upper_ci <- as.numeric(sb_forest$OR_higher)
labels <- sb_forest$each_factor

base_data <-
  tibble::tibble(
    mean  = sb_forest$ORadj,
    lower = sb_forest$OR_lower,
    upper = sb_forest$OR_higher,
    labels = sb_forest$each_factor,
    OR = c("1.511",
           "1.505", "1.398", "1.414", "1.353", "1.481",
           "0.938", "1.564", "1.248", "1.330", "2.459"
    )
  )

forest_data <-
  matrix(
    c(rep("", 11),
      lower_ci,
      coefficients,
      upper_ci),
    ncol = 4,
    byrow = FALSE
  )

forestplot(
  labeltext = labels, 
  mean = coefficients, 
  lower = lower_ci, 
  upper = upper_ci,
  clip = c(0.1,3.0),
  xlab = "OR for Urban/Rural when accounting for each factor",
  zero = 1,
  lineheight = "auto",
  boxsize = 0.5,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue")
)

###################################
