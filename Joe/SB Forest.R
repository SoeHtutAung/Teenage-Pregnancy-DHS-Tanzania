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
    matage = (p3-v011)/12,
    anc4 = ifelse( is.na(m14) | m14<4, 0 , 1 ),
    emp_year = ifelse(v731==0, 0, 1),
    edu = factor( ifelse( v106==0,0, ifelse(v106==1,1,2) ) ),
    married = ifelse( v501==1 | v501==2 , 1, 0),
    wealth = factor(v190)
  )

OR_design <-
  svydesign(
    id=~v021,
    strata=~v023,
    weights=~wt,
    data=OR_df
  )

# We will store all information needed for the plot in sb_forest
sb_forest <- as.data.frame(matrix(nrow=9,ncol=5))
names(sb_forest) <- c("each_factor","p-value","ORadj","OR_lower","OR_higher")

glm_res <- svyglm(stillbirth~v025, OR_design, family=quasibinomial())
sb_forest[1,] <-
  c("Rural residence","N/A",
    exp(coef(glm_res))[2],
    exp(confint(glm_res))[2,1],
    exp(confint(glm_res))[2,2]
  )

glm_age <- svyglm(stillbirth~v025*matage, OR_design, family=quasibinomial())
sb_forest[2,] <-
  c("Mother's Age","0.003",
    exp(coef(glm_age))[[2]],
    exp(confint(glm_age))[[2,1]],
    exp(confint(glm_age))[[2,2]]
  )

glm_edu <- svyglm(stillbirth~v025*edu, OR_design, family=quasibinomial())
sb_forest[3,] <-
  c("Education","<0.001",
    exp(coef(glm_edu))[[2]],
    exp(confint(glm_edu))[[2,1]],
    exp(confint(glm_edu))[[2,2]]
  )

glm_married <- svyglm(stillbirth~v025*married, OR_design, family=quasibinomial())
sb_forest[4,] <-
  c("Married","0.002",
    exp(coef(glm_married))[[2]],
    exp(confint(glm_married))[[2,1]],
    exp(confint(glm_married))[[2,2]]
  )

glm_wealth <- svyglm(stillbirth~v025*wealth, OR_design, family=quasibinomial())
sb_forest[5,] <-
  c("Wealth Quintile","<0.001",
    exp(coef(glm_wealth))[[2]],
    exp(confint(glm_wealth))[[2,1]],
    exp(confint(glm_wealth))[[2,2]]
  )

glm_emp <- svyglm(stillbirth~v025*emp_year, OR_design, family=quasibinomial())
sb_forest[6,] <-
  c("Employment","0.021",
    exp(coef(glm_emp))[[2]],
    exp(confint(glm_emp))[[2,1]],
    exp(confint(glm_emp))[[2,2]]
  )

glm_hyp <- svyglm(stillbirth~v025*s1125, OR_design, family=quasibinomial())
sb_forest[7,] <-
  c("Hypertension","<0.001",
    exp(coef(glm_hyp))[[2]],
    exp(confint(glm_hyp))[[2,1]],
    exp(confint(glm_hyp))[[2,2]]
  )

glm_mode <- svyglm(stillbirth~v025*m17, OR_design, family=quasibinomial())
sb_forest[8,] <-
  c("C-Section","<0.001",
    exp(coef(glm_mode))[[2]],
    exp(confint(glm_mode))[[2,1]],
    exp(confint(glm_mode))[[2,2]]
  )

glm_anc4 <- svyglm(stillbirth~v025*anc4, OR_design, family=quasibinomial())
sb_forest[9,] <-
  c("4+ ANC visits","<0.001",
    exp(coef(glm_anc4))[[2]],
    exp(confint(glm_anc4))[[2,1]],
    exp(confint(glm_anc4))[[2,2]]
  )

######################## Figure to illustrate changes in OR  ###########

library(forestplot)

# Create a tibble for use in the forestplot object
base_data <-
  tibble::tibble(
    mean  = as.numeric(sb_forest$ORadj),
    lower = as.numeric(sb_forest$OR_lower),
    upper = as.numeric(sb_forest$OR_higher),
    labels = sb_forest$each_factor,
    OR = c("0.66",
           "1.27", "1.47", "0.45", "0.37",
           "0.65", "0.71", "0.55", "0.46"
    )
  )

base_data %>%
  forestplot(
    title = "OR for Rural vs Urban Rates of Stillbirth by Confounding Factor",
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
    box = "#FA8128",
    line = "darkorange",
    summary = "#FA8128",
    txt_gp = fpTxtGp(
      ticks = gpar(cex = 1),
      xlab  = gpar(cex = 1.5)))
