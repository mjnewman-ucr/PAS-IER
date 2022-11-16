#> PAS-IER Main Data Analyses
#> 



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
ds <- read_csv("data/pasier_data_cleaned.csv", col_names = T, na = "NA")
summary(ds)
describe(ds)
glimpse(ds)

#removing the one participant who has a shit ton of missing data
ds <- ds[-134,]

#SEEKING
ds$seeking <- factor(ds$seeking,
                     levels = c(1,2),
                     labels = c("Yes", "No"))

#-------------------------------------------------------------------------------------------------------------

#Centering variables for interaction terms later
ds$as_centered <- center_scale(ds$ppass_as)
ds$pc_centered <- center_scale(ds$ppass_pc)
ds$iris_cs_centered <- center_scale(ds$iris_cs)
ds$iris_r_centered <- center_scale(ds$iris_r)
ds$iris_pp_centered <- center_scale(ds$iris_pp)
ds$iris_h_centered <- center_scale(ds$iris_h)
ds$ders_total_centered <- center_scale(ds$ders_total)

#-------------------------------------------------------------------------------------------------------------

#Correlation matrices
ds_cor <- select(ds, c(iris_cs, iris_r, iris_pp, iris_h, ppass_as, eff_comp, eff_total))
rcorr(as.matrix(ds_cor))
cor_mat <- rcorr(as.matrix(ds_cor))
flatten_corr_matrix(cor_mat$r, cor_mat$P)

ds_eff_comp <-  select (ds, c(eff_help:eff_control))
rcorr(as.matrix(ds_eff_comp))
cor_eff <- rcorr(as.matrix(ds_eff_comp))
flatten_corr_matrix(cor_eff$r, cor_eff$P)

ds_cor2 <-  select (ds, c(eff_total, cerq_reappraisal, age, ders_total, ppass_as, ppass_pc, 
                          sdt_autonomy_comp, sdt_competence_comp, sdt_relatedness_comp))
rcorr(as.matrix(ds_cor2))
cor_2 <- rcorr(as.matrix(ds_cor2))
flatten_corr_matrix(cor_2$r, cor_2$P)

ds_cor3 <-  select (ds, c(cerq_reappraisal, cerq_selfblame, cerq_acceptance,
                          cerq_rumination, cerq_refocusing, cerq_planning, 
                          cerq_perspective, cerq_catastrophizing, cerq_otherblame,
                          sdt_autonomy_comp, sdt_competence_comp, sdt_relatedness_comp,
                          eff_total))
rcorr(as.matrix(ds_cor3))
cor_3 <- rcorr(as.matrix(ds_cor3))
flatten_corr_matrix(cor_3$r, cor_3$P)

#apa.cor.table(
#  ds_cor3,
#  filename = "sdt_cerq_correlations.doc",
#  landscape = T)


#Scatterplots of IER strategies with IER effectiveness
plot(ds_cor$eff_total, ds_cor$iris_cs, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER cognitive support", pch=20)
plot(ds_cor$eff_total, ds_cor$iris_r, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER responsiveness", pch=20)
plot(ds_cor$eff_total, ds_cor$iris_pp, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER physical presence", pch=20)
plot(ds_cor$eff_total, ds_cor$iris_h, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER hostility", pch=20)

ds_pp_cor <- select(ds_pp, c(iris_cs, iris_r, iris_pp, iris_h, ppass_as, eff_comp, eff_total))
plot(ds_pp_cor$eff_total, ds_pp_cor$iris_pp, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER physical presence", pch=20)

#-------------------------------------------------------------------------------------------------------------

#t-test for IER effectiveness by IER seeking

(seeking_ftest <- var.test(eff_total ~ seeking, data = ds))

t.test(eff_total ~ seeking, data = ds)
cohens_d(eff_total ~ seeking, data = ds, ci = T, conf.level = 0.95)
describe(ds$seeking)
summary(ds$seeking)
ds %>%
  group_by(seeking) %>%
  get_summary_stats(eff_total, type = "mean_sd")

#t-test for parental autonomy support by IER seeking (just making sure)
t.test(ppass_as ~ seeking, data = ds)

#-------------------------------------------------------------------------------------------------------------

#> Model:
#> IER Effectiveness = b0 + b1PAS + b2NO_SEEKING + b3PAS*NO_SEEKING

#Regressions for IER seeking * autonomy support moderation


summary(seeking_model1 <- sem(eff_total ~ seeking + as_centered, data = ds))

summary(seeking_model2 <- lm(eff_total ~ seeking*as_centered, data = ds))

apa.reg.table(
  seeking_model1,
  seeking_model2,
  filename = NA)

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
#> IER Effectiveness = b0 + b1PAS + b2IER_STRATEGY + b3PAS*IER_STRATEGY

#Regressions for IER strategies * autonomy support moderation

#IER: Responsiveness *SIG*

library(lavaan)

summary(strategy_r_model_fiml <- sem('eff_total ~ iris_r_centered*as_centered', data = ds, missing = 'fiml', fixed.x = F))

summary(strategy_r_model <- lm(eff_total ~ 1, data = ds))
summary(strategy_r_model1 <- lm(eff_total ~ iris_r_centered + as_centered, data = ds))
summary(strategy_r_model2 <- lm(eff_total ~ iris_r_centered*as_centered, data = ds))
summary(strategy_r_model3 <- lm(eff_total ~ iris_r_centered + as_centered + seeking, 
                                data = ds))
summary(strategy_r_model4 <- lm(eff_total ~ iris_r_centered*as_centered + seeking, data = ds))
summary(strategy_r_model5 <- lm(eff_total ~ iris_r_centered + as_centered*seeking, data = ds))
summary(strategy_r_model6 <- lm(eff_total ~ iris_r_centered*as_centered + 
                                  as_centered*seeking, data = ds))
summary(strategy_r_model7 <- lm(eff_total ~ iris_r_centered*as_centered*seeking, data = ds))

apa.reg.table(
  strategy_r_model1,
  strategy_r_model2,
  filename = NA)

#*this won't work until I deal with my missing data point
#okay, I removed participant 8134
anova(strategy_r_model, strategy_r_model1, strategy_r_model2)

#apa.reg.table(
#  strategy_r_model1,
#  strategy_r_model2,
#  filename = "responsivness_as_interaction_comp.doc")

#didn't need to do this
#iris_r_a <- mean(ds$iris_r_centered, na.rm = T) + sd(ds$iris_r_centered, na.rm = T)
#iris_r_m <- mean(ds$iris_r_centered, na.rm = T)
#iris_r_b <- mean(ds$iris_r_centered, na.rm = T) - sd(ds$iris_r_centered, na.rm = T)

#(iris_r_a_r <- round(iris_r_a,1))
#(iris_r_m_r <- round(iris_r_m,1))
#(iris_r_b_r <- round(iris_r_b,1))

#(strategy_r_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_r_centered = 
#                           c(iris_r_a_r, iris_r_b_r)))

(strategy_r_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_r_centered = 
                           seq(-5, 3, by = 1)))

emmip(strategy_r_model2, as_centered ~ iris_r_centered, at = strategy_r_list, CIs = TRUE)

(strategy_r_list_2 <- list(as_centered = c(ppass_a_r, ppass_b_r, 'sstest'), iris_r_centered = 
                            c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(strategy_r_model2, levels = strategy_r_list_2)

#for excel simple slope analyses
vcov(strategy_r_model2)

#IER: Cognitive Support
summary(strategy_cs_model1 <- lm(eff_total ~ iris_cs_centered + as_centered, data = ds))
summary(strategy_cs_model2 <- lm(eff_total ~ iris_cs_centered*as_centered, data = ds))
summary(strategy_cs_model3 <- lm(eff_total ~ iris_cs_centered + as_centered + 
                                   seeking, data = ds))
summary(strategy_cs_model4 <- lm(eff_total ~ iris_cs_centered*as_centered + 
                                   seeking, data = ds))
summary(strategy_cs_model5 <- lm(eff_total ~ iris_cs_centered*as_centered + 
                                   seeking*as_centered, data = ds))
summary(strategy_cs_model6 <- lm(eff_total ~ iris_cs_centered*as_centered*seeking, data = ds))

apa.reg.table(
  strategy_cs_model1,
  strategy_cs_model2,
  filename = NA)

#IER: Physical Presence (needs more cleaning to weed out virtual IER)

ds_pp <- ds %>% filter(location=='1')
summary(strategy_pp_filtered_model1 <- lm(eff_total ~ iris_pp_centered + as_centered, data = ds_pp))
summary(strategy_pp_filtered_model2 <- lm(eff_total ~ iris_pp_centered*as_centered, data = ds_pp))

apa.reg.table(
  strategy_pp_filtered_model1,
  strategy_pp_filtered_model2,
  filename = NA)

(strategy_pp_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_pp_centered = 
                           seq(-5, 4, by = 1)))

emmip(strategy_pp_filtered_model2, as_centered ~ iris_pp_centered, at = strategy_pp_list, CIs = TRUE)

(strategy_pp_filtered_list_2 <- list(as_centered = c(ppass_a_r, ppass_b_r, 'sstest'), iris_pp_centered = 
                             c(seq(-5, 4, by = 1), 'sstest')))
simple_slopes(strategy_pp_filtered_model2, levels = strategy_pp_filtered_list_2)

summary(strategy_pp_model1 <- lm(eff_total ~ iris_pp_centered + as_centered, data = ds))
summary(strategy_pp_model2 <- lm(eff_total ~ iris_pp_centered*as_centered, data = ds))
summary(strategy_pp_model3 <- lm(eff_total ~ iris_pp_centered + as_centered + seeking, data = ds))
summary(strategy_pp_model4 <- lm(eff_total ~ iris_pp_centered*as_centered + seeking, data = ds))
summary(strategy_pp_model5 <- lm(eff_total ~ iris_pp_centered*as_centered + 
                                   seeking*as_centered, data = ds))
summary(strategy_pp_model6 <- lm(eff_total ~ iris_pp_centered*as_centered*seeking, data = ds))

apa.reg.table(
  strategy_pp_model1,
  strategy_pp_model2,
  filename = NA)

#IER: Hostility
summary(strategy_h_model1 <- lm(eff_total ~ iris_h_centered + as_centered, data = ds))
summary(strategy_h_model2 <- lm(eff_total ~ iris_h_centered*as_centered, data = ds))
summary(strategy_h_model3 <- lm(eff_total ~ iris_h_centered + as_centered + seeking, data = ds))
summary(strategy_h_model4 <- lm(eff_total ~ iris_h_centered*as_centered + seeking, data = ds))
summary(strategy_h_model5 <- lm(eff_total ~ iris_h_centered*as_centered + 
                                  seeking*as_centered, data = ds))
summary(strategy_h_model6 <- lm(eff_total ~ iris_h_centered*as_centered*seeking, data = ds))
summary(strategy_h_model7 <- lm(eff_total ~ iris_h_centered*seeking, data = ds))
summary(strategy_h_model7 <- lm(eff_total ~ iris_h_centered*seeking + as_centered, data = ds))

apa.reg.table(
  strategy_h_model1,
  strategy_h_model2,
  filename = NA)

#-------------------------------------------------------------------------------------------------------------
#Exploring three-way hostility interaction

ds_sought <- ds %>% filter(seeking=='Yes')
summary(lm(eff_total ~ iris_h_centered*as_centered, data = ds_sought))

ds_unsought <- ds %>% filter(seeking=='No')
summary(lm(eff_total ~ iris_h_centered*as_centered, data = ds_unsought))

#> Hmm, didn't see any differences in significance... does that mean that parental
#> autonomy support may be the driving moderator? Obviously both autonomy support
#> and seeking are theoretically moderating hostility, but it's interesting that
#> the interaction didn't hold when I split the file by seeking

#-------------------------------------------------------------------------------------------------------------
#Messing around with responsiveness*as*seeking

#This was such a clusterfuck. I had to re-center all my variables

#Sought

ds_sought$as_centered <- center_scale(ds_sought$ppass_as)
ds_sought$pc_centered <- center_scale(ds_sought$ppass_pc)
ds_sought$iris_cs_centered <- center_scale(ds_sought$iris_cs)
ds_sought$iris_r_centered <- center_scale(ds_sought$iris_r)
ds_sought$iris_pp_centered <- center_scale(ds_sought$iris_pp)
ds_sought$iris_h_centered <- center_scale(ds_sought$iris_h)

summary(split_sought_strategy_r_model1 <- lm(eff_total ~ iris_r_centered*as_centered, data = ds_sought))

sought_ppass_a <- mean(ds_sought$as_centered, na.rm = T) + sd(ds_sought$as_centered, na.rm = T)
sought_ppass_m <- mean(ds_sought$as_centered, na.rm = T)
sought_ppass_b <- mean(ds_sought$as_centered, na.rm = T) - sd(ds_sought$as_centered, na.rm = T)

(sought_ppass_a_r <- round(sought_ppass_a,1))
(sought_ppass_m_r <- round(sought_ppass_m,1))
(sought_ppass_b_r <- round(sought_ppass_b,1))

describe(ds_sought$iris_r_centered)

(split_sought_strategy_r_list <- list(as_centered = c(sought_ppass_a_r, sought_ppass_b_r),iris_r_centered = 
                           seq(-5, 3, by = 1)))

emmip(split_sought_strategy_r_model1, as_centered ~ iris_r_centered, at = split_sought_strategy_r_list, CIs = TRUE)

(split_sought_strategy_r_list_2 <- list(as_centered = c(sought_ppass_a_r, sought_ppass_b_r, 'sstest'), iris_r_centered = 
                             c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(split_sought_strategy_r_model1, levels = split_sought_strategy_r_list_2)


#Unsought

ds_unsought$as_centered <- center_scale(ds_unsought$ppass_as)
ds_unsought$pc_centered <- center_scale(ds_unsought$ppass_pc)
ds_unsought$iris_cs_centered <- center_scale(ds_unsought$iris_cs)
ds_unsought$iris_r_centered <- center_scale(ds_unsought$iris_r)
ds_unsought$iris_pp_centered <- center_scale(ds_unsought$iris_pp)
ds_unsought$iris_h_centered <- center_scale(ds_unsought$iris_h)

summary(split_unsought_strategy_r_model1 <- lm(eff_total ~ iris_r_centered*as_centered, data = ds_unsought))

unsought_ppass_a <- mean(ds_unsought$as_centered, na.rm = T) + sd(ds_unsought$as_centered, na.rm = T)
unsought_ppass_m <- mean(ds_unsought$as_centered, na.rm = T)
unsought_ppass_b <- mean(ds_unsought$as_centered, na.rm = T) - sd(ds_unsought$as_centered, na.rm = T)

(unsought_ppass_a_r <- round(unsought_ppass_a,1))
(unsought_ppass_m_r <- round(unsought_ppass_m,1))
(unsought_ppass_b_r <- round(unsought_ppass_b,1))


(split_unsought_strategy_r_list <- list(as_centered = c(unsought_ppass_a_r, unsought_ppass_b_r),iris_r_centered = 
                                        seq(-5, 3, by = 1)))

emmip(split_unsought_strategy_r_model1, as_centered ~ iris_r_centered, at = split_unsought_strategy_r_list, CIs = TRUE)

(split_unsought_strategy_r_list_2 <- list(as_centered = c(unsought_ppass_a_r, unsought_ppass_b_r, 'sstest'), iris_r_centered = 
                                          c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(split_unsought_strategy_r_model1, levels = split_unsought_strategy_r_list_2)

#----------------------------------------------------------------------------------------------------
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


