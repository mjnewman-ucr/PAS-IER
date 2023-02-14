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
summary(ds)
describe(ds)
glimpse(ds)

#removing the one participant who has a shit ton of missing data
ds <- ds[-134,]

ds$seeking <- factor(ds$seeking,
                     levels = c(1,2),
                     labels = c("Yes", "No"))


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
describe(ds)
glimpse(ds)

demos <- select(ds, c(ID:seeking))
describe(demos)
summary(demos)
sd(demos$age, na.rm = T)


apa.cor.table(
  demos,
  filename = "descriptives.doc",
  table.number = NA,
  show.conf.interval = TRUE,
  show.sig.stars = TRUE,
  landscape = TRUE
)

df <- select(ds, c(iris_r:ppass_pc, eff_help:eff_control, eff_total, eff_comp, 
                   sdt_autonomy_s:sdt_relatedness_comp, bnsr_autonomy:cerq_otherblame,
                   ders_nonaccept:ders_total))

corr <- select(ds, c(iris_cs, iris_r, iris_pp, iris_h, seeking, ppass_as, eff_comp))
rcorr(as.matrix(corr))
corr_mat <- rcorr(as.matrix(corr))
flatten_corr_matrix(corr_mat$r, corr_mat$P)

plot(corr$eff_comp, corr$iris_cs, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER cognitive support", pch=20)
plot(corr$eff_comp, corr$iris_r, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER responsiveness", pch=20)
plot(corr$eff_comp, corr$iris_pp, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER physical presence", pch=20)
plot(corr$eff_comp, corr$iris_h, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER hostility", pch=20)

library(apaTables)
apa.cor.table(
  corr,
  filename = "correlations_SSEA.doc",
  table.number = NA,
  show.conf.interval = TRUE,
  show.sig.stars = TRUE,
  landscape = TRUE
)

apa.d.table(
  iv = seeking,
  dv = eff_comp,
  data = ds,
  filename = "seeking_ttest_SSEA.doc",
  table.number = NA,
  show.conf.interval = TRUE,
  landscape = TRUE
)

apa.d.table(
  iv = seeking,
  dv = ppass_as,
  data = ds,
  filename = "seeking2_ttest_SSEA.doc",
  table.number = NA,
  show.conf.interval = TRUE,
  landscape = TRUE
)


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
ds$eff_total_centered <- center_scale(ds$eff_total)
ds$iris_cs_centered <- center_scale(ds$iris_cs)
ds$iris_r_centered <- center_scale(ds$iris_r)
ds$iris_pp_centered <- center_scale(ds$iris_pp)
ds$iris_h_centered <- center_scale(ds$iris_h)


(iris_model1 <- lm(eff_comp ~ as_centered + iris_cs_centered, data = ds))
(iris_model2 <- lm(eff_comp ~ as_centered*iris_cs_centered, data = ds))
apa.reg.table(iris_model2)

(seeking_model1 <- lm(eff_comp ~ as_centered + seeking, data = ds))
(seeking_model2 <- lm(eff_comp ~ as_centered*seeking, data = ds))
apa.reg.table(
  seeking_model1,
  seeking_model2,
  filename = "seeking_blocks_interaction.doc")

summary(lm(eff_comp ~ as_centered*iris_r_centered, data = ds))
summary(lm(eff_comp ~ as_centered*iris_cs_centered, data = ds))
summary(lm(eff_comp ~ as_centered*iris_pp_centered, data = ds))
summary(lm(eff_comp ~ as_centered*iris_h_centered, data = ds))

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

apa.reg.table(reg1, filename = "reg_cs.doc")

reg2 <- lm(eff_coping ~ as_centered + iris_r_centered, data = ds)
summary(reg2)
reg2 <- lm(eff_coping ~ as_centered + iris_r_centered + rXas, data = ds)
summary(reg2)

apa.reg.table(reg2, filename = "reg_r.doc")

reg3 <- lm(eff_coping ~ as_centered + iris_pp_centered, data = ds)
summary(reg3)
reg3 <- lm(eff_coping ~ as_centered + iris_pp_centered + ppXas, data = ds)
summary(reg3)

apa.reg.table(reg3, filename = "reg_pp.doc")

reg4 <- lm(eff_coping ~ as_centered + iris_h_centered, data = ds)
summary(reg4)
reg4 <- lm(eff_coping ~ as_centered + iris_h_centered + hXas, data = ds)
summary(reg4)

apa.reg.table(reg4, filename = "reg_h.doc")

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
