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
library("lsr")
library("ggpubr")
library("rstatix")



ds <- read_csv("data/pasier_data_cleaned.csv", col_names = T, na = "NA")

ds$eff_total <- rowMeans(subset(ds, select = c(eff_help:eff_control)), na.rm = T)
ds$eff_comp <- rowMeans(subset(ds, select = c(eff_help, eff_coping, eff_control)), na.rm = T)

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

df <- select(ds, c(eff_comp, ppass_as, ppass_pc))
rcorr(as.matrix(df))
eff_ppass <- rcorr(as.matrix(df))
flatten_corr_matrix(eff_ppass$r, eff_ppass$P)


ds$seeking <- factor(ds$seeking,
                    levels = c(1,2),
                    labels = c("Yes", "No"))
describe(ds$seeking)

is.numeric(ds$ppass_as)

class(ds$seeking)
levels(ds$seeking)
#element1 = yes
#element2 = no
 
#> Model:
#> IER Effectiveness = b0 + b1PAS + b2NO_SEEKING + b3PAS*NO_SEEKING
#> 
#> Other idea:
#> Categorize the autonomy support but keep it all in one model instead of running
#> 3 t tests

t.test(eff_comp ~ seeking, data = ds)
cohensD(eff_comp ~ seeking, data = ds)
describe(ds$seeking)
summary(ds$seeking)

ds %>%
  group_by(seeking) %>%
  get_summary_stats(eff_comp, type = "mean_sd")

t.test(ppass_as ~ seeking, data = ds)

summary(reg <- lm(eff_comp ~ seeking, data = ds))

summary(reg <- lm(eff_comp ~ seeking + as_centered, data = ds))

summary(reg <- lm(eff_comp ~ seeking*as_centered, data = ds))


emtrends(reg, pairwise ~ seeking, var="as_centered")

(mylist <- list(as_centered=seq(-4,2.5,by=0.5),seeking=c("Yes","No")))
emmip(reg, seeking ~as_centered, at=mylist,CIs=TRUE)

ppass_a <- mean(ds$as_centered, na.rm = T) + sd(ds$as_centered, na.rm = T)
ppass_m <- mean(ds$as_centered, na.rm = T)
ppass_b <- mean(ds$as_centered, na.rm = T) - sd(ds$as_centered, na.rm = T)

(ppass_a_r <- round(ppass_a,1))
(ppass_m_r <- round(ppass_m,1))
(ppass_b_r <- round(ppass_b,1))

(mylist2 <- list(as_centered=c(ppass_a_r, ppass_m_r, ppass_b_r),seeking=c("Yes","No")))
emmip(reg, as_centered ~seeking, at=mylist2,CIs=TRUE)

summary(reg <- lm(eff_comp ~ seeking + as_centered, data = ds))
emmeans(reg, pairwise ~ as_centered, var="seeking", at = mylist2)

cor.test(ds$eff_comp, ds$ppass_as)

plot(ds$eff_comp, ds$ppass_as, main="Scatterplot",
     xlab="IER effectiveness", ylab="parental autonomy support", pch=20)

#> QUESTIONS FOR GRADQUANTS
#> 
#> 1. What does "NOTE: Results may be misleading due to involvement in interactions" mean?
#> 
#> 2. How can I assess whether levels of the +1sd slope ("Yes" "No") significantly differ?
#>  a. Use ANOVA to compare the means of the different groups 
#>  b. Or ANCOVA (little bit of regression for fun)
#>  c. Make parental AS categorical (based on SDs) and then run 3 ttests
#>    -This is what we ended up doing
#>
#> 3. Do I use "emtrends" to get the stats for the simple slopes?
#>
#>
#>



lm1 <- lm(eff_comp ~ ppass_as + seeking, data = ds)
summary(lm1)
#> This controls for the other variables
#> 
#> ANOVA (factoring parental autonomy support)

ds$autonomy_support_lvl <- NA

ds$autonomy_support_lvl[ds$as_centered < ppass_m] <- "low"

ds$autonomy_support_lvl[ds$as_centered > ppass_a] <- "high"

ds$autonomy_support_lvl[ds$as_centered > ppass_m & ds$as_centered < ppass_a] <- "med"

ds$autonomy_support_lvl

summary(reg <- lm(eff_comp ~ seeking*autonomy_support_lvl, data = ds))

#
aov_support_seeking_cat <- (aov(eff_comp ~ autonomy_support_lvl*seeking, data = ds))

summary(aov_support_seeking_cat)

TukeyHSD(aov_support_seeking_cat)

t.test(ds$eff_comp[ds$autonomy_support_lvl=="low"] ~ ds$seeking[ds$autonomy_support_lvl=="low"])
t.test(ds$eff_comp[ds$autonomy_support_lvl=="high"] ~ ds$seeking[ds$autonomy_support_lvl=="high"])
t.test(ds$eff_comp[ds$autonomy_support_lvl=="med"] ~ ds$seeking[ds$autonomy_support_lvl=="med"])

#> NOTES FROM GRADQUANT
#>

ds$as_s1 <- mean(ds$ppass_as, na.rm = T) + sd(ds$ppass_as, na.rm = T)
ds$as_s2 <- mean(ds$ppass_as, na.rm = T) - sd(ds$ppass_as, na.rm = T)

ds$d1 <- (ds$ppass_as - ds$as_s1) / sd(ds$ppass_as, na.rm = T)
ds$d2 <- (ds$ppass_as - ds$as_s2) / sd(ds$ppass_as, na.rm = T) 

t.test(ds$eff_comp[ds$d1] ~ ds$seeking)

summary(ds$as_centered)

cov2cor(vcov(reg, complete = T))
sqrt(diag(vcov(reg)))
vcov(reg)
