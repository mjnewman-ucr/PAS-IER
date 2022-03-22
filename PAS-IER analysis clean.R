##DATA ANALYSIS

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


ds <- read_csv("data/pasier_data_cleaned.csv", col_names = T, na = "NA")

ds$eff_total <- rowMeans(subset(ds, select = c(eff_help:eff_control)), na.rm = T)

## Survey Descriptives

#Function I found online that makes correlation matrices prettier
flatten_corr_matrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

df <- select(ds, c(iris_r:ppass_pc, eff_help:eff_control, sdt_autonomy:sdt_relatedness, bnsr_autonomy:cerq_otherblame,
                   ders_nonaccept:ders_total))

df <- select(ds, c(iris_cs, ppass_as, ppass_pc))
rcorr(as.matrix(df))
iris_ppass <- rcorr(as.matrix(df))
flatten_corr_matrix(iris_ppass$r, iris_ppass$P)

#Dichotomising IER because it's just too skewed

ds$iris_r_dicho <- dicho(ds$iris_r, dich.by = "md", as.num = T, val.labels = c("low", "high"))
ds$iris_cs_dicho <- dicho(ds$iris_cs, dich.by = "md", as.num = T, val.labels = c("low", "high"))
ds$iris_pp_dicho <- dicho(ds$iris_pp, dich.by = "md", as.num = T, val.labels = c("low", "high"))
ds$iris_h_dicho <- dicho(ds$iris_h, dich.by = "md", as.num = T, val.labels = c("low", "high"))

ds$pc_dicho <- dicho(ds$ppass_pc, dich.by = "md", as.num = T, val.labels = c("low", "high"))

#Centering variables 

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

ds$as_centered <- center_scale(ds$ppass_as)
ds$pc_centered <- center_scale(ds$ppass_pc)
ds$iris_cs_centered <- center_scale(ds$iris_cs)
ds$ders_total_centered <- center_scale(ds$ders_total)

#Interaction terms
ds$csXas <- ds$as_centered*ds$iris_cs_centered
ds$csXders <- ds$iris_cs_centered*ds$ders_total_centered


##------------------------------------------------------------------------------
#Exploring total effectiveness and IRIS

t.test(eff_total ~ iris_r_dicho, data = ds)
wilcox.test(eff_total ~ iris_r_dicho, data = ds)
cor.test(ds$eff_total, ds$iris_r_dicho, method = "kendall")

t.test(eff_total ~ iris_cs_dicho, data = ds)
wilcox.test(eff_total ~ iris_cs_dicho, data = ds)
cor.test(ds$eff_total, ds$iris_cs_dicho, method = "kendall")

plot(ds$eff_total, ds$iris_cs, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER Cognitive Support", pch=20)

plot(ds$eff_total, ds$iris_r, main="Scatterplot",
     xlab="IER Effectiveness", ylab="IER Responsiveness", pch=20)

##------------------------------------------------------------------------------
#Exploring IER Coping and IRIS

t.test(eff_coping ~ iris_r_dicho, data = ds)
wilcox.test(eff_coping ~ iris_r_dicho, data = ds)
cor.test(ds$eff_coping, ds$iris_r_dicho, method = "kendall")

t.test(eff_coping ~ iris_cs_dicho, data = ds)
wilcox.test(eff_coping ~ iris_cs_dicho, data = ds)
cor.test(ds$eff_coping, ds$iris_cs_dicho, method = "kendall")


####### Multiple regressions with interactions ############

##Parental autonomy support + ier cognitive support

##SIG---------
reg <- lm(eff_control ~ as_centered + iris_cs_centered, data = ds)
summary(reg)
reg <- lm(eff_control ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)

reg <- lm(eff_control ~ ders_total_centered + iris_cs_centered, data = ds)
summary(reg)
reg <- lm(eff_control ~ ders_total_centered + iris_cs_centered + csXders, data = ds)
summary(reg)

reg <- lm(eff_coping ~ as_centered + iris_cs_centered, data = ds)
summary(reg)
reg <- lm(eff_coping ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)
##------------

summary(aov(eff_control ~ as_centered + iris_r_dicho, data = ds))
