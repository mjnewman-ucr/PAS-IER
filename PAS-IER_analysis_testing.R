##DATA ANALYSIS

#Recommendations
#> The models we ran for eff_comp didn't look tooooo bad... I could pick the
#> one that looks the best, and then potentially add the other models to the 
#> appendices 
#> If I use eff_coping, it should be an ordinal regression (could use linear, but
#> that isn't the best science)

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
summary(ds)
describe(ds)
glimpse(ds)

ds$eff_total <- rowMeans(subset(ds, select = c(eff_help:eff_control)), na.rm = T)
ds$eff_comp <- rowMeans(subset(ds, select = c(eff_help, eff_coping, eff_control)), na.rm = T)
ds$eff_comp2 <- rowMeans(subset(ds, select = c(eff_help, eff_coping, eff_control, 
                                               eff_self)), na.rm = T)


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
ggqqplot(ds$sdt_autonomy_comp)
ggqqplot(ds$sdt_competence_comp)
ggqqplot(ds$sdt_relatedness_comp)
ggqqplot(ds$eff_comp)
ggqqplot(ds$eff_total)


shapiro.test(ds$ppass_as)
ggplot(ds, aes(x = ppass_as)) + 
  geom_histogram(bins = 30)
skewness(ds$ppass_as, na.rm = TRUE)

shapiro.test(ds$eff_total)
ggplot(ds, aes(x = eff_total)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_total, na.rm = TRUE)

shapiro.test(ds$eff_comp2)
ggplot(ds, aes(x = eff_comp2)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_comp2, na.rm = TRUE)

shapiro.test(ds$eff_coping)
ggplot(ds, aes(x = eff_coping)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_coping, na.rm = TRUE)

shapiro.test(ds$eff_help)
ggplot(ds, aes(x = eff_help)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_help, na.rm = TRUE)

shapiro.test(ds$eff_control)
ggplot(ds, aes(x = eff_control)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_control, na.rm = TRUE)

shapiro.test(ds$eff_self)
ggplot(ds, aes(x = eff_self)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_self, na.rm = TRUE)

shapiro.test(ds$eff_connect)
ggplot(ds, aes(x = eff_connect)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_connect, na.rm = TRUE)

#three options
#> 1. ignore it (and ask: is this good enough for me)
(model2 <- lm(eff_comp ~ as_centered*iris_cs_centered, data = ds))
plot(model2)

#> 2. transformation of dv 
ggplot(ds, aes(x = eff_comp)) + 
  geom_histogram(bins = 30) +
  scale_x_log10()
(model3 <- lm(log(eff_comp) ~ as_centered*iris_cs_centered, data = ds))
plot(model3)
  #three very extreme low values seem to be the issue...

#> 2. Use a different model (instead of linear reg, use a different distribution,
#> (e.g. gamma distribution (for positive continuous values)))
model4 <- glm(eff_comp ~ as_centered*iris_cs_centered, data = ds, family = Gamma(link = "log"))
plot(model4)

#> Don't not include this dv just beacuse of the errors 
#> 
#> 3. Robust linear model (uses a students t distribution), which constrains the effect
#> of the outliers (package is called MASS rlm())
library(MASS)
model5 <- rlm(log(eff_comp) ~ as_centered*iris_cs_centered, data = ds)
plot(model5)

describe(ds$eff_comp)
summary(ds$eff_comp)

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
  geom_histogram(bins = 30) 
skewness(ds$iris_cs, na.rm = TRUE)

shapiro.test(ds$iris_r)
ggplot(ds, aes(x = iris_r)) + 
  geom_histogram(bins = 30) 
skewness(ds$iris_r, na.rm = TRUE)

shapiro.test(ds$iris_pp)
ggplot(ds, aes(x = iris_pp)) + 
  geom_histogram(bins = 30) 
skewness(ds$iris_pp, na.rm = TRUE)

shapiro.test(ds$iris_h)
ggplot(ds, aes(x = iris_h)) + 
  geom_histogram(bins = 30) 
skewness(ds$iris_h, na.rm = TRUE)

shapiro.test(ds$ders_total)
ggplot(ds, aes(x = ders_total)) + 
  geom_histogram(bins = 30) 
skewness(ds$ders_total, na.rm = TRUE)

shapiro.test(ds$ders_goals)
ggplot(ds, aes(x = ders_goals)) + 
  geom_histogram(bins = 30) 
skewness(ds$ders_goals, na.rm = TRUE)

shapiro.test(ds$ders_awareness)
ggplot(ds, aes(x = ders_awareness)) + 
  geom_histogram(bins = 30) 
skewness(ds$ders_awareness, na.rm = TRUE)

shapiro.test(ds$ders_clarity)
ggplot(ds, aes(x = ders_clarity)) + 
  geom_histogram(bins = 30) 
skewness(ds$ders_clarity, na.rm = TRUE)

shapiro.test(ds$ders_strategies)
ggplot(ds, aes(x = ders_strategies)) + 
  geom_histogram(bins = 30) 
skewness(ds$ders_strategies, na.rm = TRUE)

shapiro.test(ds$ders_impulse)
ggplot(ds, aes(x = ders_impulse)) + 
  geom_histogram(bins = 30) 
skewness(ds$ders_impulse, na.rm = TRUE)

shapiro.test(ds$ders_nonaccept)
ggplot(ds, aes(x = ders_nonaccept)) + 
  geom_histogram(bins = 30) 
skewness(ds$ders_nonaccept, na.rm = TRUE)

shapiro.test(ds$cerq_reappraisal)
ggplot(ds, aes(x = cerq_reappraisal)) + 
  geom_histogram(bins = 30) 
skewness(ds$cerq_reappraisal, na.rm = TRUE)

shapiro.test(ds$cerq_perspective)
ggplot(ds, aes(x = cerq_perspective)) + 
  geom_histogram(bins = 30) 
skewness(ds$cerq_perspective, na.rm = TRUE)

shapiro.test(ds$sdt_autonomy_comp)
ggplot(ds, aes(x = sdt_autonomy_comp)) + 
  geom_histogram(bins = 30) 
skewness(ds$sdt_autonomy_comp, na.rm = TRUE)

shapiro.test(ds$sdt_competence_comp)
ggplot(ds, aes(x = sdt_competence_comp)) + 
  geom_histogram(bins = 30) 
skewness(ds$sdt_competence_comp, na.rm = TRUE)

shapiro.test(ds$sdt_relatedness_comp)
ggplot(ds, aes(x = sdt_relatedness_comp)) + 
  geom_histogram(bins = 30) 
skewness(ds$sdt_relatedness_comp, na.rm = TRUE)


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


df <- select(ds, c(iris_r:ppass_pc, eff_help:eff_control, sdt_autonomy_s:sdt_relatedness_comp, 
                   bnsr_autonomy:cerq_otherblame,ders_nonaccept:ders_total))

df <- select(ds, c(iris_cs, ppass_as, ppass_pc))
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

df <- select(ds, c(iris_r:iris_pp, sdt_autonomy_comp, sdt_competence_comp, sdt_relatedness_comp))
rcorr(as.matrix(df))
iris_sdt <- rcorr(as.matrix(df))
flatten_corr_matrix(iris_sdt$r, iris_sdt$P)

df <- select(ds, c(sdt_autonomy_comp, sdt_competence_comp, sdt_relatedness_comp, ders_nonaccept:ders_total))
rcorr(as.matrix(df))
ders_sdt <- rcorr(as.matrix(df))
flatten_corr_matrix(ders_sdt$r, ders_sdt$P)

df <- select(ds, c(sdt_autonomy_comp, sdt_competence_comp, sdt_relatedness_comp, ppass_as, ppass_pc))
rcorr(as.matrix(df))
ppass_sdt <- rcorr(as.matrix(df))
flatten_corr_matrix(ppass_sdt$r, ppass_sdt$P)


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
ds$iris_cs_centered <- center_scale(ds$iris_cs)

ds$eff_coping_centered <- center_scale(ds$eff_coping)
ds$eff_connect_centered <- center_scale(ds$eff_connect)
ds$eff_self_centered <- center_scale(ds$eff_self)
ds$eff_control_centered <- center_scale(ds$eff_control)
ds$eff_help_centered <- center_scale(ds$eff_help)
ds$eff_total_centered <- center_scale(ds$eff_total)


reg <- lm(ders_total ~ sdt_relatedness + sdt_competence, data = ds)
summary(reg)

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
reg <- lm(eff_coping_centered ~ as_centered + iris_cs_centered, data = ds)
summary(reg)
reg <- lm(eff_coping_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)
##------------

reg <- lm(eff_connect_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)

reg <- lm(eff_total_centered ~ as_centered + iris_cs_centered + csXas, data = ds)
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

#(wrong moderator)
#model<-lmres(eff_help~ppass_pc*iris_r_dicho, centered = c("ppass_pc", "iris_r_dicho"), data = ds)
#(S_slopes<-simpleSlope(model, pred = "ppass_pc", mod1 = "iris_r_dicho"))
#(Plot<-PlotSlope(S_slopes))

model1<-lmres(eff_help~ppass_pc*iris_r_dicho, centered = c("ppass_pc", "iris_r_dicho"), data = ds)
(S_slopes<-simpleSlope(model1, pred = "iris_r_dicho", mod1 = "ppass_pc"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Control (-1 SD)", 
                                       "High Parental Control (+1 SD)"),
                 namex = "IER Responsiveness", namey = "Perceived Helpfulness of IER"))

model2<-lmres(eff_coping~ppass_as*iris_cs, centered = c("ppass_as", "iris_cs"), data = ds)
(S_slopes<-simpleSlope(model2, pred = "iris_cs", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Autonomy Support (-1 SD)", 
                                     "High Parental Autonomy Support (+1 SD)"),
                 namex = "IER Cognitive Support", namey = "Perceived Emotional Coping"))

model3<-lmres(eff_control~ppass_as*iris_cs, centered = c("ppass_as", "iris_cs"), data = ds)
(S_slopes<-simpleSlope(model3, pred = "iris_cs", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Autonomy Support (-1 SD)", 
                                       "High Parental Autonomy Support (+1 SD)"),
                 namex = "IER Cognitive Support", namey = "Perceived Control Over Emotions"))

model4<-lmres(eff_control~ppass_pc*iris_cs, centered = c("ppass_pc", "iris_cs"), data = ds)
(S_slopes<-simpleSlope(model4, pred = "iris_cs", mod1 = "ppass_pc"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Control (-1 SD)", 
                                       "High Parental Control (+1 SD)"),
                 namex = "IER Cognitive Support", namey = "Perceived Control Over Emotions"))

SS_help_r_pc
SS_coping_cs_as
SS_control_cs_as
SS_control_cs_pc

##----------------------------------------------------------------------------

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

as_c <- ds$as_centered
cs_c <- ds$iris_cs_centered
summary(ds$as_centered, na.rm=T)
summary(ds$iris_cs_centered, na.rm=T)

sd(as_c, na.rm=T)
sd(cs_c, na.rm=T)
ds$eff_total <- rowMeans(subset(ds, select = c(eff_help:eff_control)), na.rm = T)

df <- select(ds, c(eff_coping, ppass_as, iris_cs, iris_r, iris_pp, iris_h))


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
ds$eff_coping_centered <- center_scale(ds$eff_coping)
ds$eff_connect_centered <- center_scale(ds$eff_connect)
ds$eff_self_centered <- center_scale(ds$eff_self)
ds$eff_control_centered <- center_scale(ds$eff_control)
ds$eff_help_centered <- center_scale(ds$eff_help)
ds$eff_total_centered <- center_scale(ds$eff_total)
ds$iris_cs_centered <- center_scale(ds$iris_cs)



ds$csXas <- ds$as_centered*ds$iris_cs_centered
reg <- lm(eff_coping_centered ~ as_centered + iris_cs_centered, data = ds)
summary(reg)
reg <- lm(eff_coping ~ as_centered + iris_cs_centered + csXas, data = ds)
summary(reg)

model_s<-lmres(eff_coping~ppass_as*iris_cs, centered = c("ppass_as", "iris_cs"), data = ds)
(S_slopes<-simpleSlope(model_s, pred = "iris_cs", mod1 = "ppass_as"))
(Plot<-PlotSlope(S_slopes, namemod = c("Low Parental Autonomy Support (-1 SD)", 
                                       "High Parental Autonomy Support (+1 SD)"),
                 namex = "IER Cognitive Support", namey = "Perceived Emotional Coping"))

summary(S_slopes)
S_slopes

model1 <- lm(eff_coping ~ as_centered + iris_cs_centered, data = ds)
(model2 <- lm(eff_coping ~ as_centered*iris_cs_centered, data = ds))

ds$iris_r_dicho <- as.factor(ds$iris_r_dicho)

(model3 <- lm(eff_coping ~ as_centered*iris_r_dicho, data = ds))
summary(model3)

(model4 <- lm(eff_connect ~ as_centered+iris_r_dicho, data = ds))
(model5 <- lm(eff_connect ~ as_centered*iris_r_dicho, data = ds))
anova(model4, model5)

summary(model5)

describe(ds)

stargazer(model1, model2, type="text", 
          column.labels = c("Main Effects", "Interaction"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) 

cs_sd <- c(mean(ds$iris_cs_centered, na.rm = T)-sd(ds$iris_cs_centered, na.rm = T),
           mean(ds$iris_cs_centered, na.rm = T),
           mean(ds$iris_cs_centered, na.rm = T)+sd(ds$iris_cs_centered, na.rm = T))
cs_sd <- round(cs_sd, 2)

as_sd <- c(mean(ds$as_centered, na.rm = T)-sd(ds$as_centered, na.rm = T),
           mean(ds$as_centered, na.rm = T),
           mean(ds$as_centered, na.rm = T)+sd(ds$as_centered, na.rm = T))

as_sd <- round(as_sd, 2)
as_sd

inter_sd <- effect(c("as_centered*iris_cs_centered"), model2,
                   xlevels=list(as_centered=c(-1.15, 1.15),
                                iris_cs_centered=c(-1.78, 1.78))) 
inter_sd <- as.data.frame(inter_sd)

inter_sd$as <-factor(inter_sd$as_centered,
                    levels=c(-1.15, 1.15),
                    labels=c("-1 SD below mean", "+1 SD above mean"))

inter_sd$cs<-factor(inter_sd$iris_cs_centered,
                    levels=c(-1.78, 1.78),
                    labels=c("Less Cognitive Support", "More Cognitive Support"))
coping_cs_as_SS <- ggplot(data = inter_sd, aes(x=cs, y=fit, group=as))+
  geom_line(size=1, aes(color=as))+ #Can adjust the thickness of your lines
  geom_point(aes(colour = as), size=2)+ #Can adjust the size of your points
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),fill="lightgrey",alpha=.6)+ #Can adjust your error bars
  ylab("Perceived Ability to Cope After IER")+ #Adds a label to the y-axis
  xlab("Cognitive Support from Parent (IER)")+ #Adds a label to the x-axis
  ggtitle("  IER & Autonomy Support Interaction")+
  theme_bw()+ #Removes the gray background 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.key = element_blank())+ #Removes the lines 
  scale_fill_grey() 

coping_cs_as_SS + labs(color = "Autonomy Support") + 
  theme(plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(color = "grey40", size=14, face="bold"),
        axis.title.y = element_text(color = "grey40", size=14, face="bold"))

anova(model1, model2)

rs1 <- summary(model1)$r.squared

rs2 <- summary(model2)$r.squared

library(interactions)
library(sandwich)
library(jtools)
library(vcov)
library(apaTables)


simpleslope <- sim_slopes(model = model2, 
           pred = "iris_cs_centered",
           modx = "as_centered")

ds$iris_r_dicho <- as.numeric(ds$iris_r_dicho)

simpleslope <- sim_slopes(model = model4, 
                          pred = "iris_r_dicho",
                          modx = "as_centered")
simpleslope

cov2cor(vcov(model2, complete = T))
sqrt(diag(vcov(model2)))

apa.cor.table(
  df,
  filename = "correlations_SAS.doc",
  table.number = NA,
  show.conf.interval = TRUE,
  show.sig.stars = TRUE,
  landscape = TRUE
)

apa.reg.table(model4)

summary(aov(eff_control ~ as_centered + iris_r_dicho, data = ds))


reg <- lm(eff_control~as_centered*iris_r_dicho,data=ds)
summary(reg)
emtrends(reg, ~ iris_r_dicho, var="as_centered")
emtrends(reg, pairwise ~ iris_r_dicho, var="as_centered")

##------------------------------------------------------------------------------

describe(ds$parent)
describe(ds$location)
describe(ds$seeking)
describe(ds$home)

t.test(eff_coping ~ seeking, data = ds)
wilcox.test(eff_coping ~ seeking, data = ds)
cor.test(ds$eff_coping, ds$seeking, method = "kendall")

t.test(eff_comp ~ seeking, data = ds)

t.test(ppass_as ~ seeking, data = ds)

ds$home_dicho <- dplyr::recode(ds$home, '3' = 2)
describe(ds$home_dicho)
t.test(eff_control ~ home_dicho, data = ds)

t.test(iris_cs ~ seeking, data = ds)
cor.test(ds$iris_cs, ds$seeking, method = "kendall")

#Combining phone calls with video chats
ds$location_dicho <- dplyr::recode(ds$location, '3' = 2)
describe(ds$location_dicho)
t.test(eff_coping ~ location_dicho, data = ds)
wilcox.test(eff_coping ~ location_dicho, data = ds)
t.test(iris_pp ~ location_dicho, data = ds)
wilcox.test(iris_pp ~ location_dicho, data = ds)

#Combining video chats with in-person (i.e., assuming participant can see parent's face)
ds$location_dicho2 <- dplyr::recode(ds$location, '3' = 1)
describe(ds$location_dicho2)
t.test(eff_coping ~ location_dicho2, data = ds)
wilcox.test(eff_coping ~ location_dicho2, data = ds)

ds$eff_comp <- rowMeans(subset(ds, select = c(eff_help, eff_coping, eff_control)), na.rm = T)
cor.test(ds$eff_comp, ds$ppass_as)

plot(ds$eff_comp, ds$ppass_as, main="Scatterplot",
     xlab="IER effectiveness", ylab="parental autonomy support", pch=20)

plot(ds$eff_comp, ds$ppass_pc, main="Scatterplot",
     xlab="IER effectiveness", ylab="parental control", pch=20)

plot(ds$eff_comp, ds$iris_pp, main="Scatterplot",
     xlab="IER effectiveness coping", ylab="IER physical presence", pch=20)

ds_in_person <- ds %>% 
  filter(location == "1")
ds_in_person

describe(ds_in_person)
describe(ds_in_person$iris_pp)

shapiro.test(ds$eff_comp)
ggplot(ds, aes(x = eff_comp)) + 
  geom_histogram(bins = 100) 
skewness(ds$eff_comp, na.rm = TRUE)

shapiro.test(ds_in_person$iris_pp)
ggplot(ds_in_person, aes(x = iris_pp)) + 
  geom_histogram(bins = 100) 
skewness(ds_in_person$iris_pp, na.rm = TRUE)

shapiro.test(ds_in_person$iris_r)
ggplot(ds_in_person, aes(x = iris_r)) + 
  geom_histogram(bins = 100) 
skewness(ds_in_person$iris_r, na.rm = TRUE)

shapiro.test(ds_in_person$iris_cs)
ggplot(ds_in_person, aes(x = iris_cs)) + 
  geom_histogram(bins = 100) 
skewness(ds_in_person$iris_cs, na.rm = TRUE)

cor.test(ds_in_person$eff_coping, ds_in_person$iris_pp)

plot(ds_in_person$eff_comp, ds_in_person$iris_pp, main="Scatterplot",
     xlab="IER effectiveness coping", ylab="IER physical presence", pch=20)

ds_parent <- ds %>% 
  filter(!parent == "3")
describe(ds_parent$parent)

t.test(eff_coping ~ parent, data = ds_parent)
wilcox.test(eff_coping ~ parent, data = ds_parent)

ds_dependent <- ds %>% 
  filter(!income == "8")
describe(ds_dependent$income)

t.test(income ~ dependent, data = ds_dependent)

summary(aov(eff_comp ~ ppass_as + seeking, data = ds))


summary(reg <- lm(eff_comp ~ ppass_as*seeking, data = ds))

library(emmeans)

emtrends(reg, pairwise ~ seeking, var="ppass_as")


describe(ds$parent)
