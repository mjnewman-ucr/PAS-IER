library("tidyverse")
library("dplyr")
library("readr")
library("lubridate")
library("Hmisc")
library("DataExplorer")

ds <- read_csv("data/pasier_data_cleaned.csv", col_names = T, na = "9999")

##-----------------------------------------------------------------------------


#Creating a demographics dataset
demographics <- select(ds, c(ID, age, gender, s_orientation, relationship, race, home, dependent:income))



##-----------------------------------------------------------------------------
##DEMOGRAPHIC DESCRIPTIVES

summary(demographics)
describe(demographics)


ggplot(demographics, aes(x = age)) + 
  geom_histogram() + 
  xlab("age")

ggplot(demographics, aes(x = race)) + 
  geom_histogram() + 
  xlab("race")

ggplot(demographics, aes(x = income)) + 
  geom_histogram() + 
  xlab("income")

rcorr(as.matrix(demographics))

##-----------------------------------------------------------------------------


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


##-----------------------------------------------------------------------------
## Forgot to standardize stuff

df <- select(ds, c(age, gender, s_orientation, relationship, race, home, dependent:income, 
                   eff_help:eff_control, iris_r:ppass_pc, sdt_autonomy:sdt_relatedness, 
                   bnsr_autonomy:cerq_otherblame, ders_nonaccept:ders_total))

df_scaled <- as.data.frame(scale(df))

dfcor <- select(df_scaled, c(iris_r:iris_pp, eff_help:eff_control))
rcorr(as.matrix(dfcor))

dfcor <- select(df_scaled, c(ppass_as:ppass_pc, eff_help:eff_control))
rcorr(as.matrix(dfcor))
ppass_eff <- rcorr(as.matrix(dfcor))
flatten_corr_matrix(ppass_eff$r, ppass_eff$P)


df_scaled$irisrXas <- df_scaled$iris_r * df_scaled$ppass_as

reg_irisppass <- lm(eff_help ~ iris_r + ppass_as + irisrXas, data = df_scaled)
summary(reg_irisppass)


df <- select(df_scaled, c(iris_r:ppass_goals, home))
rcorr(as.matrix(df))
iris_ppass <- rcorr(as.matrix(df))
flatten_corr_matrix(iris_ppass$r, iris_ppass$P)
