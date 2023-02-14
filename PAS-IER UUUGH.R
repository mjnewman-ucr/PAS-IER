#> PAS-IER Analyses for ONLY IER responsiveness, cog support, and seeking
#> *basically, the variables in my manuscript



library("tidyverse")
library("dplyr")
library("readr")
library("lubridate")
library("Hmisc")
library("DataExplorer")
library("ggpubr")
library("moments")
library("scales")
library("sjmisc")
library("pequod")
library("stargazer")
library("effects")
library("emmeans")
library("lsr")
library("ggpubr")
library("rstatix")
library("vcov")
library("apaTables")
library("reghelper")
library("car")


#-------------------------------------------------------------------------------------------------------------

#Function for making correlation matrix prettier
flatten_corr_matrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Function for centering
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

#-------------------------------------------------------------------------------------------------------------

#Reading in data
ds <- read_csv("data/pasier_data_cleaned_updated.csv", col_names = T, na = "NA")
summary(ds)
describe(ds)
glimpse(ds)

#removing the one participant who has a shit ton of missing data
ds <- ds[-128,]

#SEEKING

#NOTE: If I want to run a correlation with seeking, it can't be a factor
ds$seeking_unfactored <- ds$seeking

ds$seeking <- factor(ds$seeking,
                     levels = c(1,2),
                     labels = c("Yes", "No"))

class(ds$seeking)

ds$seeking <- relevel(ds$seeking, ref="No") 

levels(ds$seeking)


#-------------------------------------------------------------------------------------------------------------
#DESCRIPTIVES

demos <- select(ds, c(age:location))
describe(demos)
summary(demos)
sd(demos$age, na.rm = T)

#-------------------------------------------------------------------------------------------------------------

#Centering variables for interaction terms later

ds$as_centered <- center_scale(ds$ppass_as)
ds$iris_cs_centered <- center_scale(ds$iris_cs)
ds$iris_r_centered <- center_scale(ds$iris_r)


#-------------------------------------------------------------------------------------------------------------

#Checking for multicollinearity
VIFmodel <- lm(eff_comp ~ seeking + ppass_as + iris_r + iris_cs, 
               data = ds)
vif(VIFmodel)

VIFmodel <- lm(eff_comp ~ seeking + ppass_as + iris_r, 
               data = ds)
vif(VIFmodel)

VIFmodel <- lm(eff_comp ~ seeking + ppass_as + iris_r, 
               data = ds)

vif(VIFmodel)

#okay, all good!

#Correlation matrices
ds_cor <- select(ds, c(iris_r, iris_cs, seeking_unfactored, ppass_as, eff_comp))
rcorr(as.matrix(ds_cor))
cor_mat <- rcorr(as.matrix(ds_cor))
flatten_corr_matrix(cor_mat$r, cor_mat$P)


#apa.cor.table(
#  ds_cor,
#  filename = "main_correlations.doc",
#  landscape = T)

ds_cor_demo <- select(ds, c(iris_r, iris_cs, seeking_unfactored, ppass_as, eff_comp,
                            c(age:relationship), c(home:location)))
rcorr(as.matrix(ds_cor_demo))
cor_mat <- rcorr(as.matrix(ds_cor_demo))
flatten_corr_matrix(cor_mat$r, cor_mat$P)


#Scatterplots of IER processes with IER effectiveness
plot(ds_cor$eff_comp, ds_cor$iris_cs, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER cognitive support", pch=20)
plot(ds_cor$eff_comp, ds_cor$iris_r, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER responsiveness", pch=20)

#-------------------------------------------------------------------------------------------------------------

#t-test for IER effectiveness by IER seeking

(seeking_ftest <- var.test(eff_comp ~ seeking, data = ds))

t.test(eff_comp ~ seeking, data = ds)
cohens_d(eff_comp ~ seeking, data = ds, ci = T, conf.level = 0.95)
describe(ds$seeking)
summary(ds$seeking)
ds %>%
  group_by(seeking) %>%
  get_summary_stats(eff_comp, type = "mean_sd")

#t-test for parental autonomy support by IER seeking (just making sure)
t.test(ppass_as ~ seeking, data = ds)

#t-test for IER responsiveness by IER seeking (curious if youth who sought out 
#IER were more likely to get responsive support)
t.test(iris_r ~ seeking, data = ds)
#oooooh, interesting

t.test(iris_cs ~ seeking, data = ds)
#no sig difference for cognitive support

#-------------------------------------------------------------------------------------------------------------

#> Model:
#> IER Effectiveness = b0 + b1PAS + b2NO_SEEKING + b3PAS*NO_SEEKING

#Regressions for IER seeking with autonomy support as a moderator


summary(seeking_model1 <- lm(eff_comp ~ seeking + as_centered, data = ds))

summary(seeking_model2 <- lm(eff_comp ~ seeking*as_centered, data = ds))

apa.reg.table(
  seeking_model1,
  seeking_model2,
  filename = NA)

apa.reg.table(
  seeking_model1,
  seeking_model2,
  filename = "reg_solicitation.doc")

emtrends(seeking_model2, pairwise ~ seeking, var="as_centered")

ppass_a <- mean(ds$as_centered, na.rm = T) + sd(ds$as_centered, na.rm = T)
ppass_m <- mean(ds$as_centered, na.rm = T)
ppass_b <- mean(ds$as_centered, na.rm = T) - sd(ds$as_centered, na.rm = T)

(ppass_a_r <- round(ppass_a,1))
(ppass_m_r <- round(ppass_m,1))
(ppass_b_r <- round(ppass_b,1))

(seeking_list <- list(as_centered = c(ppass_a_r, ppass_m_r, ppass_b_r), seeking = c("Yes", "No")))
emmip(seeking_model2, as_centered ~ seeking, at = seeking_list, CIs = TRUE)

#Simple Slopes Significance
(seeking_list_2 <- list(as_centered = c(ppass_a_r, ppass_m_r, ppass_b_r, 'sstest'), 
                        seeking = c("Yes", "No", 'sstest')))
simple_slopes(seeking_model2, levels = seeking_list_2)


#for excel simple slope analyses (if needed)
vcov(seeking_model2)
summary(ds$as_centered)




#-------------------------------------------------------------------------------------------------------------

#> Model:
#> IER Effectiveness = b0 + b1PAS + b2IER_PROCESS + b3PAS*IER_PROCESS

#Regressions for IER processes with autonomy support as a moderator

#IER: Responsiveness *SIG*


summary(process_r_model <- lm(eff_comp ~ 1, data = ds))
summary(process_r_model1 <- lm(eff_comp ~ iris_r_centered + as_centered, data = ds))
summary(process_r_model2 <- lm(eff_comp ~ iris_r_centered*as_centered, data = ds))
summary(process_r_model3 <- lm(eff_comp ~ iris_r_centered + as_centered + seeking, 
                               data = ds))
summary(process_r_model4 <- lm(eff_comp ~ iris_r_centered*as_centered + seeking, data = ds))
summary(process_r_model5 <- lm(eff_comp ~ iris_r_centered + as_centered*seeking, data = ds))
summary(process_r_model6 <- lm(eff_comp ~ iris_r_centered*as_centered + 
                                 as_centered*seeking, data = ds))
summary(process_r_model7 <- lm(eff_comp ~ iris_r_centered*as_centered*seeking, data = ds))

apa.reg.table(
  process_r_model1,
  process_r_model2,
  filename = NA)

#apa.reg.table(
#  process_r_model1,
#  process_r_model2,
#  filename = "reg_responsiveness.doc")

#*this won't work until I deal with my missing data point
#okay, I removed participant 8134
anova(process_r_model, process_r_model1, process_r_model2)

#apa.reg.table(
#  process_r_model1,
#  process_r_model2,
#  filename = "responsivness_as_interaction_comp.doc")

#didn't need to do this
#iris_r_a <- mean(ds$iris_r_centered, na.rm = T) + sd(ds$iris_r_centered, na.rm = T)
#iris_r_m <- mean(ds$iris_r_centered, na.rm = T)
#iris_r_b <- mean(ds$iris_r_centered, na.rm = T) - sd(ds$iris_r_centered, na.rm = T)

#(iris_r_a_r <- round(iris_r_a,1))
#(iris_r_m_r <- round(iris_r_m,1))
#(iris_r_b_r <- round(iris_r_b,1))

#(process_r_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_r_centered = 
#                           c(iris_r_a_r, iris_r_b_r)))

(process_r_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_r_centered = 
                          seq(-5, 3, by = 1)))

emmip(process_r_model2, as_centered ~ iris_r_centered, at = process_r_list, CIs = TRUE)

(process_r_list_2 <- list(as_centered = c(ppass_a_r, ppass_b_r, 'sstest'), iris_r_centered = 
                            c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(process_r_model2, levels = process_r_list_2)

#for excel simple slope analyses
vcov(process_r_model2)

#What is I included IER cognitive support in the modeL
summary(process_rcs_model <- lm(eff_comp ~ 1, data = ds))
summary(process_rcs_model1 <- lm(eff_comp ~ iris_r_centered + iris_cs_centered + as_centered, data = ds))
summary(process_rcs_model2 <- lm(eff_comp ~ iris_r_centered*as_centered + iris_cs_centered, data = ds))

apa.reg.table(
  process_rcs_model1,
  process_rcs_model2,
  filename = NA)

#IER: Cognitive Support
summary(process_cs_model <- lm(eff_comp ~ 1, data = ds))
summary(process_cs_model1 <- lm(eff_comp ~ iris_cs_centered + as_centered, data = ds))
summary(process_cs_model2 <- lm(eff_comp ~ iris_cs_centered*as_centered, data = ds))
summary(process_cs_model3 <- lm(eff_comp ~ iris_cs_centered + as_centered + 
                                  seeking, data = ds))
summary(process_cs_model4 <- lm(eff_comp ~ iris_cs_centered*as_centered + 
                                  seeking, data = ds))
summary(process_cs_model5 <- lm(eff_comp ~ iris_cs_centered*as_centered + 
                                  seeking*as_centered, data = ds))
summary(process_cs_model6 <- lm(eff_comp ~ iris_cs_centered*as_centered*seeking, data = ds))

apa.reg.table(
  process_cs_model1,
  process_cs_model2,
  filename = NA)

#apa.reg.table(
#  process_cs_model1,
#  process_cs_model2,
#  filename = "reg_cognitive_support.doc")



(process_cs_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_cs_centered = 
                           seq(-5, 3, by = 1)))

emmip(process_cs_model2, as_centered ~ iris_cs_centered, at = process_cs_list, CIs = TRUE)

(process_cs_list_2 <- list(as_centered = c(ppass_a_r, ppass_b_r, 'sstest'), iris_cs_centered = 
                             c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(process_cs_model2, levels = process_cs_list_2)

#for excel simple slope analyses
vcov(process_cs_model2)

#What if I looked at this interaction when autonomy support is explored +/- 2 SDs
ppass_a2 <- mean(ds$as_centered, na.rm = T) + 2*(sd(ds$as_centered, na.rm = T))
ppass_m2 <- mean(ds$as_centered, na.rm = T)
ppass_b2 <- mean(ds$as_centered, na.rm = T) - 2*(sd(ds$as_centered, na.rm = T))

(ppass_a2_r <- round(ppass_a2,1))
(ppass_m2_r <- round(ppass_m2,1))
(ppass_b2_r <- round(ppass_b2,1))

(process_cs_2_list <- list(as_centered = c(ppass_a2_r, ppass_b2_r),iris_cs_centered = 
                             seq(-5, 3, by = 1)))

emmip(process_cs_model2, as_centered ~ iris_cs_centered, at = process_cs_2_list, CIs = TRUE)

(process_cs_2_list_2 <- list(as_centered = c(ppass_a2_r, ppass_b2_r, 'sstest'), iris_cs_centered = 
                               c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(process_cs_model2, levels = process_cs_2_list_2)

#for excel simple slope analyses
vcov(process_cs_model2)

#What is I included IER repsonsiveness in the modeL
summary(process_csr_model <- lm(eff_comp ~ 1, data = ds))
summary(process_csr_model1 <- lm(eff_comp ~ iris_cs_centered + iris_r_centered + as_centered, data = ds))
summary(process_csr_model2 <- lm(eff_comp ~ iris_cs_centered*as_centered + iris_r_centered, data = ds))

apa.reg.table(
  process_csr_model1,
  process_csr_model2,
  filename = NA)

#-------------------------------------------------------------------------------------------------------------

#Exploring possible gender differences

describe(ds$gender)

ds_gender <- ds %>% filter(gender %in% c(1,2))
describe(ds_gender$gender)

(gender_ftest <- var.test(ders_total ~ gender, data = ds_gender))

t.test(ders_total ~ gender, data = ds_gender)
t.test(ders_goals ~ gender, data = ds_gender)
t.test(ders_impulse ~ gender, data = ds_gender)
t.test(ders_strategies ~ gender, data = ds_gender)
t.test(ders_clarity ~ gender, data = ds_gender)

t.test(cerq_otherblame ~ gender, data = ds_gender)

#----------------------------------------------------------------------------------------------------
#
describe(ds$parent)
ds_parent <- ds %>% filter(parent %in% c(1,2))

(parent_ftest <- var.test(ders_total ~ parent, data = ds_parent))

t.test(ders_total ~ parent, data = ds_parent)

chisq.test(table(ds_parent$seeking, ds_parent$parent))

ds_home <- ds %>% filter(home %in% c(1,2))
chisq.test(table(ds_home$seeking, ds_home$home))

describe(ds$age)
