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

#install.packages("moments")
#install.packages("sjmisc")
#install.packages("pequod")
#install.packages("stargazer")
#install.packages("effects")

ds <- read_csv("data/pasier_data_cleaned.csv", col_names = T, na = "NA")
summary(ds)
describe(ds)
glimpse(ds)

ds$eff_total <- rowMeans(subset(ds, select = c(eff_help:eff_control)), na.rm = T)


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
ggqqplot(ds$eff_total)

shapiro.test(ds$eff_total)
ggplot(ds, aes(x = eff_total)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_total, na.rm = TRUE)

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
ds$eff_total_centered <- center_scale(ds$eff_total)
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

