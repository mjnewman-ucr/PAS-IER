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

#install.packages("moments")
#install.packages("sjmisc")
install.packages("pequod")

ds <- read_csv("data/pasier_data_cleaned.csv", col_names = T, na = "NA")
summary(ds)
glimpse(ds)
##-----------------------------------------------------------------------------
##Testing assumptions

ggqqplot(ds$iris_r)
ggqqplot(ds$iris_cs)
ggqqplot(ds$iris_h)
ggqqplot(ds$iris_pp)
ggqqplot(ds$ppass_as)
ggqqplot(ds$ppass_pc)
ggqqplot(ds$ders_total)
ggqqplot(ds$ders_goals) 
ggqqplot(ds$ders_awareness)
ggqqplot(ds$cerq_reappraisal)
ggqqplot(ds$sdt_autonomy)
ggqqplot(ds$sdt_competence)
ggqqplot(ds$sdt_relatedness)

shapiro.test(ds$eff_connect)
ggplot(ds, aes(x = eff_connect)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_connect, na.rm = TRUE)

shapiro.test(ds$eff_self)
ggplot(ds, aes(x = eff_self)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_self, na.rm = TRUE)

shapiro.test(ds$eff_help)
ggplot(ds, aes(x = eff_help)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_help, na.rm = TRUE)

shapiro.test(ds$eff_control)
ggplot(ds, aes(x = eff_control)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_control, na.rm = TRUE)

shapiro.test(ds$eff_coping)
ggplot(ds, aes(x = eff_coping)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_coping, na.rm = TRUE)

shapiro.test(ds$iris_cs)
ggplot(ds, aes(x = iris_cs)) + 
  geom_histogram(bins = 100) + 
skewness(ds$iris_cs, na.rm = TRUE)


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

df <- select(ds, c(iris_r:iris_pp, ppass_as, ppass_pc))
rcorr(as.matrix(df))
iris_ppass <- rcorr(as.matrix(df))
flatten_corr_matrix(iris_ppass$r, iris_ppass$P)

df <- select(ds, c(iris_r:iris_pp, bnsr_autonomy:bnsr_relatedness))
rcorr(as.matrix(df))
iris_bnsr <- rcorr(as.matrix(df))
flatten_corr_matrix(iris_bnsr$r, iris_bnsr$P)

df <- select(ds, c(iris_r:iris_pp, ders_nonaccept:ders_total))
rcorr(as.matrix(df))
iris_ders <- rcorr(as.matrix(df))
flatten_corr_matrix(iris_ders$r, iris_ders$P)

df <- select(ds, c(iris_r:iris_pp, sdt_autonomy:sdt_relatedness))
rcorr(as.matrix(df))
iris_sdt <- rcorr(as.matrix(df))
flatten_corr_matrix(iris_sdt$r, iris_sdt$P)


##------------------------------------------------------------------------------

#Dichotomising IER because it's just too skewed

ds$iris_r_dicho <- dicho(ds$iris_r, dich.by = "md", as.num = T, val.labels = c("low", "high"))
ds$iris_cs_dicho <- dicho(ds$iris_cs, dich.by = "md", as.num = T, val.labels = c("low", "high"))
ds$iris_pp_dicho <- dicho(ds$iris_pp, dich.by = "md", as.num = T, val.labels = c("low", "high"))
ds$iris_h_dicho <- dicho(ds$iris_h, dich.by = "md", as.num = T, val.labels = c("low", "high"))

ds$pc_dicho <- dicho(ds$ppass_pc, dich.by = "md", as.num = T, val.labels = c("low", "high"))

count(ds, iris_r_dicho)
count(ds, iris_cs_dicho)
count(ds, iris_pp_dicho)
count(ds, iris_h_dicho)


#Centering variables 

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

ds$as_centered <- center_scale(ds$ppass_as)
ds$pc_centered <- center_scale(ds$ppass_pc)
ds$eff_coping_centered <- center_scale(ds$eff_coping)
ds$eff_connect_centered <- center_scale(ds$eff_connect)
ds$eff_self_centered <- center_scale(ds$eff_self)
ds$eff_control_centered <- center_scale(ds$eff_control)
ds$eff_help_centered <- center_scale(ds$eff_help)
ds$iris_cs_centered <- center_scale(ds$iris_cs)


####### Multiple regressions with interactions ############

##Parental autonomy support + ier responsiveness

ds$respXas <- ds$as_centered*ds$iris_r_dicho

reg <- lm(eff_coping_centered ~ as_centered + iris_r_dicho + respXas, data = ds)
summary(reg)

##SIG---------
reg <- lm(eff_connect_centered ~ as_centered + iris_r_dicho + respXas, data = ds)
summary(reg)
##------------

reg <- lm(eff_self_centered ~ as_centered + iris_r_dicho + respXas, data = ds)
summary(reg)

##SIG??-------
reg <- lm(eff_control_centered ~ as_centered + iris_r_dicho + respXas, data = ds)
summary(reg)
##------------

reg <- lm(eff_help_centered ~ as_centered + iris_r_dicho + respXas, data = ds)
summary(reg)
##------------------------------------------------------------------------------

##Parental autonomy support + ier cognitive support

ds$csXas <- ds$as_centered*ds$iris_cs_centered

reg <- lm(eff_help_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)

##SIG---------
reg <- lm(eff_control_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)
##------------

reg <- lm(eff_self_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)

##SIG---------
reg <- lm(eff_coping_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)
##------------

reg <- lm(eff_connect_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)
##------------------------------------------------------------------------------

#Parental Control + IER responsiveness

ds$respXpc <- ds$pc_centered*ds$iris_r_dicho

reg <- lm(eff_coping_centered ~ pc_centered + iris_r_dicho + respXpc, data = ds)
summary(reg)
reg <- lm(eff_self_centered ~ pc_centered + iris_r_dicho + respXpc, data = ds)
summary(reg)
reg <- lm(eff_connect_centered ~ pc_centered + iris_r_dicho + respXpc, data = ds)
summary(reg)
reg <- lm(eff_control_centered ~ pc_centered + iris_r_dicho + respXpc, data = ds)
summary(reg)

##SIG---------
reg <- lm(eff_help_centered ~ pc_centered + iris_r_dicho + respXpc, data = ds)
summary(reg)
ds$respXpcd <- ds$pc_dicho*ds$iris_r_dicho
reg <- lm(eff_help_centered ~ pc_dicho + iris_r_dicho + respXpcd, data = ds)
summary(reg)
##------------

##------------------------------------------------------------------------------

#Parental Control + IER cognitive support

ds$csXpc <- ds$pc_centered*ds$iris_cs_centered

reg <- lm(eff_help_centered ~ pc_centered + iris_cs_centered + csXpc, data = ds)
summary(reg)
reg <- lm(eff_control_centered ~ pc_centered + iris_cs_centered + csXpc, data = ds)
summary(reg)
reg <- lm(eff_connect_centered ~ pc_centered + iris_cs_centered + csXpc, data = ds)
summary(reg)
reg <- lm(eff_self_centered ~ pc_centered + iris_cs_centered + csXpc, data = ds)
summary(reg)
reg <- lm(eff_coping_centered ~ pc_centered + iris_cs_centered + csXpc, data = ds)
summary(reg)

#Parental Control (dicho) + IER cognitive support

ds$csXpcd <- ds$pc_dicho*ds$iris_cs_centered

reg <- lm(eff_help_centered ~ pc_dicho+ iris_cs_centered + csXpcd, data = ds)
summary(reg)
reg <- lm(eff_control_centered ~ pc_dicho + iris_cs_centered + csXpcd, data = ds)
summary(reg)
reg <- lm(eff_connect_centered ~ pc_dicho + iris_cs_centered + csXpcd, data = ds)
summary(reg)
reg <- lm(eff_self_centered ~ pc_dicho + iris_cs_centered + csXpcd, data = ds)
summary(reg)
reg <- lm(eff_coping_centered ~ pc_dicho + iris_cs_centered + csXpcd, data = ds)
summary(reg)
##------------------------------------------------------------------------------

#Plotting simple slops

model<-lmres(eff_help~ppass_pc*iris_r_dicho, centered = c("ppass_pc", "iris_r_dicho"), data = ds)
(S_slopes<-simpleSlope(model, pred = "ppass_pc", mod1 = "iris_r_dicho"))
(Plot<-PlotSlope(S_slopes))

model<-lmres(eff_help~ppass_pc*iris_r_dicho, centered = c("ppass_pc", "iris_r_dicho"), data = ds)
(S_slopes<-simpleSlope(model, pred = "iris_r_dicho", mod1 = "ppass_pc"))
(Plot<-PlotSlope(S_slopes))

model<-lmres(eff_coping~ppass_as*iris_cs, centered = c("ppass_as", "iris_cs"), data = ds)
(S_slopes<-simpleSlope(model, pred = "iris_cs", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes))



###
model<-lmres(eff_control~ppass_as*iris_cs, centered = c("ppass_as", "iris_cs"), data = ds)
(S_slopes<-simpleSlope(model, pred = "iris_cs", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes))




