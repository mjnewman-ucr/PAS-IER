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
library("emmeans")


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

#Dichotomising certain measures because of possible skewness

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
ds$iris_r_centered <- center_scale(ds$iris_r)
ds$iris_pp_centered <- center_scale(ds$iris_pp)
ds$iris_h_centered <- center_scale(ds$iris_h)
ds$ders_total_centered <- center_scale(ds$ders_total)

#Interaction terms
ds$csXas <- ds$as_centered*ds$iris_cs_centered
ds$rXas <- ds$as_centered*ds$iris_r_centered
ds$ppXas <- ds$as_centered*ds$iris_pp_centered
ds$hXas <- ds$as_centered*ds$iris_h_centered
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

t.test(eff_coping ~ iris_pp_dicho, data = ds)
wilcox.test(eff_coping ~ iris_pp_dicho, data = ds)
cor.test(ds$eff_coping, ds$iris_pp_dicho, method = "kendall")

t.test(eff_coping ~ iris_h_dicho, data = ds)
wilcox.test(eff_coping ~ iris_h_dicho, data = ds)
cor.test(ds$eff_coping, ds$iris_h_dicho, method = "kendall")


####### Multiple regressions with interactions ############

####Coping: Parental autonomy support + 4 IRIS subscales

reg1 <- lm(eff_coping ~ as_centered + iris_cs_centered, data = ds)
summary(reg1)
reg1 <- lm(eff_coping ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg1)

reg2 <- lm(eff_coping ~ as_centered + iris_r_centered, data = ds)
summary(reg2)
reg2 <- lm(eff_coping ~ as_centered + iris_r_centered + rXas, data = ds)
summary(reg2)

reg3 <- lm(eff_coping ~ as_centered + iris_pp_centered, data = ds)
summary(reg3)
reg3 <- lm(eff_coping ~ as_centered + iris_pp_centered + ppXas, data = ds)
summary(reg3)

reg4 <- lm(eff_coping ~ as_centered + iris_h_centered, data = ds)
summary(reg4)
reg4 <- lm(eff_coping ~ as_centered + iris_h_centered + hXas, data = ds)
summary(reg4)

####Coping: Parental autonomy support + 3 IRIS subscales (dicho)

reg2d <- lm(eff_coping ~ as_centered + iris_r_dicho, data = ds)
summary(reg2d)
reg2d <- lm(eff_coping ~ as_centered*iris_r_dicho, data = ds)
summary(reg2d)

reg3d <- lm(eff_coping ~ as_centered + iris_pp_dicho, data = ds)
summary(reg3d)
reg3d <- lm(eff_coping ~ as_centered*iris_pp_dicho, data = ds)
summary(reg3d)

reg4d <- lm(eff_coping ~ as_centered + iris_h_dicho, data = ds)
summary(reg4d)
reg4d <- lm(eff_coping ~ as_centered*iris_h_dicho, data = ds)
summary(reg4d)

##Parental autonomy support + ier cognitive support (various eff_ outcomes)

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

reg <- lm(eff_total ~ as_centered + iris_cs_centered, data = ds)
summary(reg)
reg <- lm(eff_total ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)


model_s<-lmres(eff_coping~ppass_as*iris_cs, centered = c("ppass_as", "iris_cs"), data = ds)
(S_slopes<-simpleSlope(model_s, pred = "iris_cs", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Autonomy Support (-1 SD)", 
                                       "High Parental Autonomy Support (+1 SD)"),
                 namex = "IER Cognitive Support", namey = "Perceived Emotional Coping"))

model_s<-lmres(eff_coping~ppass_as*iris_r, centered = c("ppass_as", "iris_r"), data = ds)
(S_slopes<-simpleSlope(model_s, pred = "iris_r", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Autonomy Support (-1 SD)", 
                                       "High Parental Autonomy Support (+1 SD)"),
                 namex = "IER Responsiveness", namey = "Perceived Emotional Coping"))

model_s<-lmres(eff_coping~ppass_as*iris_pp, centered = c("ppass_as", "iris_pp"), data = ds)
(S_slopes<-simpleSlope(model_s, pred = "iris_pp", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Autonomy Support (-1 SD)", 
                                       "High Parental Autonomy Support (+1 SD)"),
                 namex = "IER Physical Presence", namey = "Perceived Emotional Coping"))
