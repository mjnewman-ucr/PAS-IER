#> Analyses for Vincent's submission to WPA
#> 
#> This project is examining the relation between the satisfaction of 
#> psychology needs (within the parent-child relationship) and youths'
#> ER abilities. Might also explore some moderators.


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
library("aod")


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
#DESCRIPTIVES

demos <- select(ds, c(ID:seeking))
describe(demos)
summary(demos)
sd(demos$age, na.rm = T)

#-------------------------------------------------------------------------------------------------------------
#Filtering out remote participants for physical presence
ds$iris_pp[ds$location == 3] <- NA
ds$iris_pp[ds$location == 2] <- NA

#Centering variables for interaction terms later
ds$as_centered <- center_scale(ds$ppass_as)
ds$pc_centered <- center_scale(ds$ppass_pc)
ds$iris_cs_centered <- center_scale(ds$iris_cs)
ds$iris_r_centered <- center_scale(ds$iris_r)
ds$iris_pp_centered <- center_scale(ds$iris_pp)
ds$iris_h_centered <- center_scale(ds$iris_h)
ds$ders_total_centered <- center_scale(ds$ders_total)
ds$ders_clarity_centered <- center_scale(ds$ders_clarity)
ds$bnsr_autonomy_centered <- center_scale(ds$bnsr_autonomy)
ds$bnsr_competence_centered <- center_scale(ds$bnsr_competence)
ds$bnsr_relatedness_centered <- center_scale(ds$bnsr_relatedness)
ds$cerq_rumination_centered <- center_scale(ds$cerq_rumination)
ds$cerq_otherblame_centered <- center_scale(ds$cerq_otherblame)


shapiro.test(ds$bnsr_autonomy)
ggplot(ds, aes(x = bnsr_autonomy)) + 
  geom_histogram(bins = 100)
skewness(ds$bnsr_autonomy, na.rm = TRUE)

shapiro.test(ds$bnsr_competence)
ggplot(ds, aes(x = bnsr_competence)) + 
  geom_histogram(bins = 100)
skewness(ds$bnsr_competence, na.rm = TRUE)

shapiro.test(ds$bnsr_relatedness)
ggplot(ds, aes(x = bnsr_relatedness)) + 
  geom_histogram(bins = 100)
skewness(ds$bnsr_relatedness, na.rm = TRUE)

shapiro.test(ds$ders_total)
ggplot(ds, aes(x = ders_total)) + 
  geom_histogram(bins = 100)
skewness(ds$ders_total, na.rm = TRUE)

shapiro.test(ds$ppass_pc)
ggplot(ds, aes(x = ppass_pc)) + 
  geom_histogram(bins = 100)
skewness(ds$ppass_pc, na.rm = TRUE)

shapiro.test(ds$ppass_as)
ggplot(ds, aes(x = ppass_as)) + 
  geom_histogram(bins = 100)
skewness(ds$ppass_as, na.rm = TRUE)

#Correlations
ds_cor <- select(ds, c(bnsr_autonomy, bnsr_competence, bnsr_relatedness, ppass_as, 
                       ppass_pc, ders_total, cerq_reappraisal, eff_comp))
rcorr(as.matrix(ds_cor))
cor_mat <- rcorr(as.matrix(ds_cor))
flatten_corr_matrix(cor_mat$r, cor_mat$P)

ds_cor_ders_sdt <- select(ds, c(ders_nonaccept, ders_goals, ders_impulse, ders_awareness,
                                ders_strategies, ders_clarity, ders_total,
                                bnsr_autonomy, bnsr_competence, bnsr_relatedness))
rcorr(as.matrix(ds_cor_ders_sdt))
cor_mat <- rcorr(as.matrix(ds_cor_ders_sdt))
flatten_corr_matrix(cor_mat$r, cor_mat$P)

ds_cor_iris_sdt <- select(ds, c(iris_cs, iris_r, iris_pp, iris_h,
                                bnsr_autonomy, bnsr_competence, bnsr_relatedness))
rcorr(as.matrix(ds_cor_iris_sdt))
cor_mat <- rcorr(as.matrix(ds_cor_iris_sdt))
flatten_corr_matrix(cor_mat$r, cor_mat$P)


plot(ds_cor_iris_sdt$iris_r, ds_cor_iris_sdt$bnsr_autonomy, main="Scatterplot",
     xlab="IER responsiveness", ylab="bnsr autonomy", pch=10)


#logistic regressions

mylogit <- glm(seeking ~ ders_clarity + ppass_pc, data = ds, family = "binomial")
mylogit <- glm(seeking ~ ders_clarity_centered*pc_centered, data = ds, family = "binomial")
summary(mylogit)
exp(coef(mylogit))
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

sd(ds$ders_clarity_centered)

VIFmodel <- glm(seeking ~ ders_clarity + ppass_pc, data = ds, family = "binomial")

vif(VIFmodel)

t.test(cerq_otherblame ~ seeking, data = ds)
t.test(ders_clarity ~ seeking, data = ds)
t.test(ppass_pc ~ seeking, data = ds)

ds_cor <- select(ds, c(ppass_as, ppass_pc, ders_clarity))
rcorr(as.matrix(ds_cor))
cor_mat <- rcorr(as.matrix(ds_cor))
flatten_corr_matrix(cor_mat$r, cor_mat$P)
