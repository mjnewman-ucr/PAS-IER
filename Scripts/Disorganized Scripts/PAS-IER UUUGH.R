##PAS-IER DATA CLEANING

#Loading the packages
library("tidyverse")
library("dplyr")
library("readr")
library("lubridate")
library("Hmisc")
library("DataExplorer")

#-------------------------------------------------------------------------------------------------------------

#READING IN DATA FILE

ds <- read_csv("data/pasier_mar12_2022.csv", col_names = T, na = "9999", 
               name_repair = tolower)
colnames <- as.vector(names(ds))
ds <- read_csv("data/pasier_mar12_2022.csv", col_names = colnames, na = "9999", 
               skip = 15, name_repair = tolower)


#-------------------------------------------------------------------------------------------------------------

#CLEANING/TIDYING DATA

#Cleaning the data
glimpse(ds)

#Observations:
#> 1. Some of the columns need to renamed (e.g., duration, id, etc)
#> 2. ID numbers look super weird

#Renaming columns
ds <- ds %>% rename(date = recordeddate,
                    duration = `duration (in seconds)`,
                    parent = ierc_3,
                    location = ierc_5,
                    seeking = ierc_6)

ds <- ds %>% mutate(ID = 8001:8195)

#Putting ID first and removing unnecessary columns
ds <- select(ds, c(ID, date, ierc_1a:income))
glimpse(ds)

#----------------------------------------------------------------------------------

## DEMOGRAPHICS

#For some odd reason, year of birth got recoded weirdly by qualtrics.

#> 1995 = 96
#> 1996 = 97
#> 1997 = 98
#> 1998 = 99
#> 1999 = 100
#> 2000 = 101
#> 2001 = 102
#> 2002 = 103
#> 2003 = 104
#> 2004 = 105
#> 2005 = 106

#Calculating age*
ds$`age#3_1` <- recode(ds$`age#3_1`, '96' = 1995L, '97' = 1996L, '98' = 1997L, '99' = 1998L, 
                       '100' = 1999L, '101' = 2000L, '102' = 2001L, '103' = 2002L, '104' = 2003L, 
                       '105' = 2004L, '106' = 2005L)
#*if this doesn't work, it's because a previously called package is masking the 
#*functionality. Try saving, quitting R, and then only running to code for this
#*script... or add dplyr:: in front of recode

ds <- unite(ds, dob, 'age#1_1':'age#3_1', sep = "-", remove = F, na.rm = T)


ds$date <- as_date(ds$date, format = '%d/%m/%y')

ds$dob <- as_date(ds$dob, format = '%m-%d-%Y')

ds$age <- as.period(interval(start = ds$dob, end = ds$date), unit = "year")
ds$age <- str_sub(ds$age, 1, 2) 

ds$age <- as.numeric(ds$age)

#Tidying race... recoding responses of multiple as 9
ds$race <- ifelse(str_length(ds$race) > 1, 9, ds$race)

ds$race <- factor(ds$race,
                  levels = c(1,2,3,4,5,6,8,9),
                  labels = c("Asian", "Black", "Latinx", "Pacific Islander", 
                             "White", "Middle Eastern", "Opt Out", "Multiple"))

#Recoding text demographics
describe(ds$s_orientation_6_text)
describe(ds$home_3_text)
describe(ds$home_who_9_text)
describe(ds$ierc_3_3_text)


#gender_5_text: all NAs

#s_orientation_6_text: "Queer", "Questioning"
#Note: for item "s_orientation" a response of 6 will now be "other"

#home_3_text:
#"Go back to visit every weekend"
#"Home and Dorm"
#"I live on campus in the dorms, but my real home I live with my mom Live with parent sometimes, live with roommates."
#"Sometimes with parents, sometimes on campus"
#"Live with parent sometimes, live with roommates."
#"when I am at home, yes"
#Note: for item "home" a response of 3 will now be "lives partially with parent(s)"

#home_who
#Mother 1
#Father 2
#Brother 3
#Sister 4
#Spouse or significant other 5
#Friend or roommate 6
#I live alone 7
#I am unhoused (e.g., shelters, cars, streets) 8
#Other: (free response) 9
#Note: I don't need this data yet, but may use it later 

#home_who_9_text
#College Dorm                
#Dorm                       
#Friends                     
#grandma                     
#Grandma                    
#Grandma
#Grandpa
#I just moved back to school 
#Relative Friend of Family  



#----------------------------------------------------------------------------------

#Calculating questionnaire subscales

#----------------------------------------------------------------------------------

#> IRIS: Interpersonal Regulation Interaction Scale (Swerdlow & Johnson, 2020)

#> iris_r = responsiveness (1, 4, 6, 8, 11, 13, 14, 17, 22, 25, 28)
#> iris_h = hostility (3, 7, 10, 18, 20, 23, 26)
#> iris_cs = cognitive support (2, 5, 9, 12, 15, 19, 27)
#> iris_pp = physical presence  (16, 21, 24)


ds$iris_r <- rowMeans(subset(ds, select = c(iris_1, iris_4, iris_6, iris_8, iris_11, 
                                            iris_13, iris_14, iris_17, iris_22, iris_25, 
                                            iris_28)), na.rm = T)

ds$iris_h <- rowMeans(subset(ds, select = c(iris_3, iris_7, iris_10, iris_18, 
                                            iris_20, iris_23, iris_26)), na.rm = T)

ds$iris_cs <- rowMeans(subset(ds, select = c(iris_2, iris_5, iris_9, iris_12, 
                                             iris_15, iris_19, iris_27)), na.rm = T)

ds$iris_pp <- rowMeans(subset(ds, select = c(iris_16, iris_21, iris_24)), na.rm = T)

#In case I ever was the sum of each scale
#ds <- ds %>% mutate(iris_r = iris_1 + iris_4 + iris_6 + iris_8 + iris_11 + iris_13 + iris_14 + iris_17 + iris_22 + iris_25 + iris_28)
#ds <- ds %>% mutate(iris_h = iris_3 + iris_7 + iris_10 + iris_18 + iris_20 + iris_23 + iris_26)
#ds <- ds %>% mutate(iris_cs = iris_2 + iris_5 + iris_9 + iris_12 + iris_15 + iris_19 +iris_27)
#ds <- ds %>% mutate(iris_pp = iris_16 + iris_21 + iris_24)

#----------------------------------------------------------------------------------

#Perceived effectiveness of IER (cite)

#Likert scale 1 to 7
#Note: I mislabeled the items in qualtrics, going straight from ier_eff3 to ier_eff5

#> Help (1) = Overall helpfulness
#> Self (2) = Feelings about self
#> Connect (3) = Connectedness with IER provider
#> Coping (5) = Ability to cope with the situation
#> Control (6) = Control over emotions

ds <- ds %>% rename(eff_help = ier_eff1,
                    eff_self = ier_eff2,
                    eff_connect = ier_eff3,
                    eff_coping = ier_eff5,
                    eff_control = ier_eff6)

describe(ds$eff_help)
describe(ds$eff_self)
describe(ds$eff_connect)
describe(ds$eff_coping)
describe(ds$eff_control)
#No missing data for ier effectiveness

ds$eff_total <- rowMeans(subset(ds, select = c(eff_help:eff_control)), na.rm = T)
ds$eff_comp <- rowMeans(subset(ds, select = c(eff_help, eff_coping, eff_control)), na.rm = T)


#----------------------------------------------------------------------------------

#> P-PASS: Perceived Parental Autonomy Support Scale (cite)
#> 
#> Likert scale of 1 - 7

#> Autonomy-Support Subscales
#> ppass_limits = Offering choice within certain limits: 1, 4, 8, 14 
#> ppass_reason = Explaining the reasons behind the demands, rules, and limits: 2, 9, 19, 23 
#> ppass_aware = Being aware of, accepting, and recognizing the child’s feelings: 7, 13, 16, 24 

#> Psychological Control Subscales
#> ppass_threat = Threatening to punish the child: 3, 10, 15, 20 
#> ppass_guilt = Inducing guilt: 6, 12, 18, 21 
#> ppass_goals = Encouraging performance goals: 5, 11, 17, 22

ds %>% select(ppass_1:ppass_24) %>% describe()

ds$ppass_limits <- rowMeans(subset(ds, select = c(ppass_1, ppass_4, ppass_8, ppass_14)), na.rm = T)
ds$ppass_reason <- rowMeans(subset(ds, select = c(ppass_2, ppass_9, ppass_19, ppass_23)), na.rm = T) 
ds$ppass_aware <- rowMeans(subset(ds, select = c(ppass_7, ppass_13, ppass_16, ppass_24)), na.rm = T) 
ds$ppass_threat <- rowMeans(subset(ds, select = c(ppass_3, ppass_10, ppass_15, ppass_20)), na.rm = T) 
ds$ppass_guilt <- rowMeans(subset(ds, select = c(ppass_6, ppass_12, ppass_18, ppass_21)), na.rm = T) 
ds$ppass_goals <- rowMeans(subset(ds, select = c(ppass_5, ppass_11, ppass_17, ppass_22)), na.rm = T) 

ds$ppass_as <- rowMeans(subset(ds, select = c(ppass_limits, ppass_reason, ppass_aware), na.rm = T))
ds$ppass_pc <- rowMeans(subset(ds, select = c(ppass_threat, ppass_guilt, ppass_goals), na.rm = T))

ds %>% select(ppass_as:ppass_pc) %>% describe()

#In case I ever was sums of scales
#ds <- ds %>% mutate(ppass_limits = ppass_1 + ppass_4 + ppass_8 + ppass_14)
#ds <- ds %>% mutate(ppass_reason = ppass_2 + ppass_9 + ppass_19 + ppass_23)
#ds <- ds %>% mutate(ppass_aware = ppass_7 + ppass_13 + ppass_16 + ppass_24) 
#ds <- ds %>% mutate(ppass_threat = ppass_3 + ppass_10 + ppass_15 + ppass_20) 
#ds <- ds %>% mutate(ppass_guilt = ppass_6 + ppass_12 + ppass_18 + ppass_21) 
#ds <- ds %>% mutate(ppass_perform = ppass_5 + ppass_11 + ppass_17 + ppass_22) 

#----------------------------------------------------------------------------------

#SDT: Basic Psychological Need Satisfaction and Frustration Scale (BPNSNF) (Chen et al., 2015)

#NOTE- I originally scored this completely wrong. Luckily, none of my original analyses 
#used this variable

#Scoring information:

#Autonomy satisfaction: items 1, 7, 13, 19
#Autonomy frustration: items 2, 8, 14, 20
#Competence satisfaction: items 5, 11, 17, 23
#Competence frustration: items 6, 12, 18, 24
#Relatedness satisfaction: items 3, 9, 15, 21
#Relatedness frustration: items 4, 10, 16, 22

ds$sdt_autonomy_s <- rowSums(subset(ds, select = c(sdt_1, sdt_7, sdt_13, sdt_19)), na.rm = T) 
ds$sdt_autonomy_f <- rowSums(subset(ds, select = c(sdt_2, sdt_8, sdt_14, sdt_20)), na.rm = T) 
ds <- ds %>% mutate(sdt_autonomy_fr = 24 - sdt_autonomy_f)
ds$sdt_autonomy_comp <- rowMeans(subset(ds, select = c(sdt_autonomy_s, sdt_autonomy_fr)), na.rm = T) 

ds$sdt_competence_s <- rowSums(subset(ds, select = c(sdt_5, sdt_11, sdt_17, sdt_23)), na.rm = T) 
ds$sdt_competence_f <- rowSums(subset(ds, select = c(sdt_6, sdt_12, sdt_18, sdt_24)), na.rm = T)
ds <- ds %>% mutate(sdt_competence_fr = 24 - sdt_competence_f)
ds$sdt_competence_comp <- rowMeans(subset(ds, select = c(sdt_competence_s, sdt_competence_fr)), na.rm = T) 

ds$sdt_relatedness_s <- rowSums(subset(ds, select = c(sdt_3, sdt_9, sdt_15, sdt_21)), na.rm = T) 
ds$sdt_relatedness_f <- rowSums(subset(ds, select = c(sdt_4, sdt_10, sdt_16, sdt_22)), na.rm = T) 
ds <- ds %>% mutate(sdt_relatedness_fr = 24 - sdt_relatedness_f)
ds$sdt_relatedness_comp <- rowMeans(subset(ds, select = c(sdt_relatedness_s, sdt_relatedness_fr)), na.rm = T) 

#----------------------------------------------------------------------------------

#BNSR: Basic Need Satisfaction in Relationships (cite)

#1 = not true at all ––– 7 = very true

#> Autonomy: 1, 5, 9r 
#> Competence: 2, 4r, 7 
#> Relatedness: 3, 6r, 8

ds <- ds %>% mutate(bnsr_9r = 8 - bnsr_9,
                    bnsr_4r = 8 - bnsr_4,
                    bnsr_6r = 8 - bnsr_6
)

ds$bnsr_autonomy <- rowMeans(subset(ds, select = c(bnsr_1, bnsr_5, bnsr_9r)), na.rm = T)
ds$bnsr_competence <- rowMeans(subset(ds, select = c(bnsr_2, bnsr_4r, bnsr_7)), na.rm = T)
ds$bnsr_relatedness <- rowMeans(subset(ds, select = c(bnsr_3, bnsr_6r, bnsr_8)), na.rm = T)

#----------------------------------------------------------------------------------

#CERQ: Cognitive Emotion Regulation Questionnaire (cite)

#> Self-blame: 4, 14
#> Acceptance: 1, 5
#> Rumination: 2, 6
#> Positive Refocusing: 7, 11
#> Refocus on Planning: 12, 15 
#> Positive Reappraisal: 3, 8 
#> Putting into Perspective: 13, 16 
#> Catastrophizing: 9, 17 
#> Other-blame: 10, 18 

ds %>% select(cerq_1:cerq_18) %>% describe()

ds$cerq_selfblame <- rowMeans(subset(ds, select = c(cerq_4, cerq_14)), na.rm = T)
ds$cerq_acceptance <- rowMeans(subset(ds, select = c(cerq_1, cerq_5)), na.rm = T)
ds$cerq_rumination <- rowMeans(subset(ds, select = c(cerq_2, cerq_6)), na.rm = T)
ds$cerq_refocusing <- rowMeans(subset(ds, select = c(cerq_7, cerq_11)), na.rm = T)
ds$cerq_planning <- rowMeans(subset(ds, select = c(cerq_12, cerq_15)), na.rm = T)
ds$cerq_reappraisal <- rowMeans(subset(ds, select = c(cerq_3, cerq_8)), na.rm = T)
ds$cerq_perspective <- rowMeans(subset(ds, select = c(cerq_13, cerq_16)), na.rm = T)
ds$cerq_catastrophizing <- rowMeans(subset(ds, select = c(cerq_9, cerq_17)), na.rm = T)
ds$cerq_otherblame <- rowMeans(subset(ds, select = c(cerq_10, cerq_18)), na.rm = T)

#----------------------------------------------------------------------------------

#DERS: Difficulties in Emotion Regulation (cite)

#Likert scale of 1-5 

#Nonacceptance of emotional responses (NONACCEPT): 11, 12, 21, 23, 25, 29
#Difficulty engaging in Goal-directed behavior (GOALS): 13, 18, 20r, 26, 33 
#Impulse control difficulties (IMPULSE): 3, 14, 19, 24r, 27, 32
#Lack of emotional awareness (AWARENESS): 2r, 6r, 8r, 10r, 17r, 34r 
#Limited access to emotion regulation strategies (STRATEGIES): 15, 16, 22r, 28, 30, 31, 35, 36
#Lack of emotional clarity (CLARITY): 1r, 4, 5, 7r, 9
#Total score: sum of all subscales

ders_reverse_score <- function(data, items, suffix = "r") {
  # loop over items
  for (item in items) {
    # create new column name
    new_col <- paste0(item, suffix)
    # reverse score item
    data[[new_col]] <- 6 - data[[item]]
  }
  return(data)
}


ds <- ders_reverse_score(ds, c("ders_20", "ders_24", "ders_2", "ders_6", "ders_8",
                               "ders_10", "ders_17", "ders_34", "ders_22", "ders_1",
                               "ders_7"))


#Old way I did it, without the function

#ds <- ds %>% mutate(ders_20r = 6 - ders_20,
#                    ders_24r = 6 - ders_24,
#                    ders_2r = 6 - ders_2,
#                    ders_6r = 6 - ders_6,
#                    ders_8r = 6 - ders_8,
#                    ders_10r = 6 - ders_10,
#                    ders_17r = 6 - ders_17,
#                   ders_34r = 6 - ders_34,
#                    ders_22r = 6 - ders_22,
#                    ders_1r = 6 - ders_1,
#                    ders_7r = 6 - ders_7
#)

ds$ders_nonaccept <- rowMeans(subset(ds, select = c(ders_11, ders_12, ders_21, 
                                                    ders_23, ders_25, ders_29)), na.rm = T)
ds$ders_goals <- rowMeans(subset(ds, select = c(ders_13, ders_18, ders_20r, ders_26, 
                                                ders_33)), na.rm = T)
ds$ders_impulse <- rowMeans(subset(ds, select = c(ders_3, ders_14, ders_19, ders_24r, 
                                                  ders_27, ders_32)), na.rm = T)
ds$ders_awareness <- rowMeans(subset(ds, select = c(ders_2r, ders_6r, ders_8r, 
                                                    ders_10r, ders_17r, ders_34r)), na.rm = T)
ds$ders_strategies <- rowMeans(subset(ds, select = c(ders_15, ders_16, ders_22r, 
                                                     ders_28, ders_30, ders_31, 
                                                     ders_35, ders_36)), na.rm = T)
ds$ders_clarity <- rowMeans(subset(ds, select = c(ders_1r, ders_4, ders_5, ders_7r, 
                                                  ders_9)), na.rm = T)

ds <- ds %>% mutate(ders_total = ders_nonaccept + ders_goals + ders_impulse + ders_awareness
                    + ders_strategies + ders_clarity) 

##-----------------------------------------------------------------------------
###WRITE A NEW CSV FILE WITH THIS TIDIED DATA ^^^

#new cleaned datafile, removing participants who reported "both" or "neither"

ds_drop<-subset(ds, ierc_3_3_text != "Both" & ierc_3_3_text != "both" 
                & ierc_3_3_text != "Both my parents" & ierc_3_3_text != "Neither" 
                & ierc_3_3_text != "My father & My mother" | is.na(ierc_3_3_text))

ds_drop<-subset(ds, ierc_3_3_text != "Both" & ierc_3_3_text != "both" 
                & ierc_3_3_text != "Both my parents" & ierc_3_3_text != "Neither" 
                & ierc_3_3_text != "It was initially from my mom, but then my dad joined us later on."
                & ierc_3_3_text != "My father & My mother" & ierc_3_3_text != "neither really, but more so from my father" 
                | is.na(ierc_3_3_text))

ds_drop<-subset(ds, ierc_3_3_text != is.na(ierc_3_3_text))
describe(ds_drop$ierc_2)

clean_ds <- select(ds_drop, c(ID, age, gender, s_orientation, relationship, race, home, dependent:income, 
                              parent, location, seeking, eff_help:eff_control, eff_comp, eff_total, iris_r:ppass_pc, 
                              sdt_autonomy_s:sdt_relatedness_comp, bnsr_autonomy:cerq_otherblame, 
                              ders_nonaccept:ders_total))
clean_ds <- select(clean_ds, c(sdt_autonomy_fr, sdt_competence_fr, sdt_relatedness_fr, ID:ders_total))
clean_ds <- select(clean_ds, ID:ders_total)


write_csv(clean_ds, "data/pasier_data_cleaned_updated.csv")


#> PAS-IER Analyses for ONLY IER responsiveness, cog support, and seeking
#> *basically, the variables in my manuscript



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
ds <- read_csv("data/pasier_data_cleaned_updated.csv", col_names = T, na = "NA")
summary(ds)
describe(ds)
glimpse(ds)


#SEEKING

#NOTE: If I want to run a correlation with seeking, it can't be a factor
ds$seeking_unfactored <- ds$seeking

ds$seeking <- factor(ds$seeking,
                     levels = c(1,2),
                     labels = c("Yes", "No"))

class(ds$seeking)

ds$seeking <- relevel(ds$seeking, ref="No") 

levels(ds$seeking)


#-------------------------------------------------------------------------------------------------------------
#DESCRIPTIVES

demos <- select(ds, c(age:location))
describe(demos)
summary(demos)
sd(demos$age, na.rm = T)

#-------------------------------------------------------------------------------------------------------------

#Centering variables for interaction terms later

ds$as_centered <- center_scale(ds$ppass_as)
ds$iris_cs_centered <- center_scale(ds$iris_cs)
ds$iris_r_centered <- center_scale(ds$iris_r)


#-------------------------------------------------------------------------------------------------------------

#Checking for multicollinearity
VIFmodel <- lm(eff_comp ~ seeking + ppass_as + iris_r + iris_cs, 
               data = ds)
vif(VIFmodel)

VIFmodel <- lm(eff_comp ~ seeking + ppass_as + iris_r, 
               data = ds)
vif(VIFmodel)

VIFmodel <- lm(eff_comp ~ seeking + ppass_as + iris_r, 
               data = ds)

vif(VIFmodel)

#okay, all good!

#Correlation matrices
ds_cor <- select(ds, c(iris_r, iris_cs, seeking_unfactored, ppass_as, eff_comp))
rcorr(as.matrix(ds_cor))
cor_mat <- rcorr(as.matrix(ds_cor))
flatten_corr_matrix(cor_mat$r, cor_mat$P)


#apa.cor.table(
#  ds_cor,
#  filename = "main_correlations.doc",
#  landscape = T)

ds_cor_demo <- select(ds, c(iris_r, iris_cs, seeking_unfactored, ppass_as, eff_comp,
                            c(age:relationship), c(home:location)))
rcorr(as.matrix(ds_cor_demo))
cor_mat <- rcorr(as.matrix(ds_cor_demo))
flatten_corr_matrix(cor_mat$r, cor_mat$P)


#Scatterplots of IER processes with IER effectiveness
plot(ds_cor$eff_comp, ds_cor$iris_cs, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER cognitive support", pch=20)
plot(ds_cor$eff_comp, ds_cor$iris_r, main="Scatterplot",
     xlab="IER effectiveness", ylab="IER responsiveness", pch=20)

#-------------------------------------------------------------------------------------------------------------

#t-test for IER effectiveness by IER seeking

(seeking_ftest <- var.test(eff_comp ~ seeking, data = ds))

t.test(eff_comp ~ seeking, data = ds)
cohens_d(eff_comp ~ seeking, data = ds, ci = T, conf.level = 0.95)
describe(ds$seeking)
summary(ds$seeking)
ds %>%
  group_by(seeking) %>%
  get_summary_stats(eff_comp, type = "mean_sd")

#t-test for parental autonomy support by IER seeking (just making sure)
t.test(ppass_as ~ seeking, data = ds)

#t-test for IER responsiveness by IER seeking (curious if youth who sought out 
#IER were more likely to get responsive support)
t.test(iris_r ~ seeking, data = ds)
#oooooh, interesting

t.test(iris_cs ~ seeking, data = ds)
#no sig difference for cognitive support

#-------------------------------------------------------------------------------------------------------------

#> Model:
#> IER Effectiveness = b0 + b1PAS + b2NO_SEEKING + b3PAS*NO_SEEKING

#Regressions for IER seeking with autonomy support as a moderator


summary(seeking_model1 <- lm(eff_comp ~ seeking + as_centered, data = ds))

summary(seeking_model2 <- lm(eff_comp ~ seeking*as_centered, data = ds))

apa.reg.table(
  seeking_model1,
  seeking_model2,
  filename = NA)

apa.reg.table(
  seeking_model1,
  seeking_model2,
  filename = "reg_solicitation.doc")

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
#> IER Effectiveness = b0 + b1PAS + b2IER_PROCESS + b3PAS*IER_PROCESS

#Regressions for IER processes with autonomy support as a moderator

#IER: Responsiveness *SIG*


summary(process_r_model <- lm(eff_comp ~ 1, data = ds))
summary(process_r_model1 <- lm(eff_comp ~ iris_r_centered + as_centered, data = ds))
summary(process_r_model2 <- lm(eff_comp ~ iris_r_centered*as_centered, data = ds))
summary(process_r_model3 <- lm(eff_comp ~ iris_r_centered + as_centered + seeking, 
                               data = ds))
summary(process_r_model4 <- lm(eff_comp ~ iris_r_centered*as_centered + seeking, data = ds))
summary(process_r_model5 <- lm(eff_comp ~ iris_r_centered + as_centered*seeking, data = ds))
summary(process_r_model6 <- lm(eff_comp ~ iris_r_centered*as_centered + 
                                 as_centered*seeking, data = ds))
summary(process_r_model7 <- lm(eff_comp ~ iris_r_centered*as_centered*seeking, data = ds))

apa.reg.table(
  process_r_model1,
  process_r_model2,
  filename = NA)

#apa.reg.table(
#  process_r_model1,
#  process_r_model2,
#  filename = "reg_responsiveness.doc")

#*this won't work until I deal with my missing data point
#okay, I removed participant 8134
anova(process_r_model, process_r_model1, process_r_model2)

#apa.reg.table(
#  process_r_model1,
#  process_r_model2,
#  filename = "responsivness_as_interaction_comp.doc")

#didn't need to do this
#iris_r_a <- mean(ds$iris_r_centered, na.rm = T) + sd(ds$iris_r_centered, na.rm = T)
#iris_r_m <- mean(ds$iris_r_centered, na.rm = T)
#iris_r_b <- mean(ds$iris_r_centered, na.rm = T) - sd(ds$iris_r_centered, na.rm = T)

#(iris_r_a_r <- round(iris_r_a,1))
#(iris_r_m_r <- round(iris_r_m,1))
#(iris_r_b_r <- round(iris_r_b,1))

#(process_r_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_r_centered = 
#                           c(iris_r_a_r, iris_r_b_r)))

(process_r_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_r_centered = 
                          seq(-5, 3, by = 1)))

emmip(process_r_model2, as_centered ~ iris_r_centered, at = process_r_list, CIs = TRUE)

(process_r_list_2 <- list(as_centered = c(ppass_a_r, ppass_b_r, 'sstest'), iris_r_centered = 
                            c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(process_r_model2, levels = process_r_list_2)

#for excel simple slope analyses
vcov(process_r_model2)

#What is I included IER cognitive support in the modeL
summary(process_rcs_model <- lm(eff_comp ~ 1, data = ds))
summary(process_rcs_model1 <- lm(eff_comp ~ iris_r_centered + iris_cs_centered + as_centered, data = ds))
summary(process_rcs_model2 <- lm(eff_comp ~ iris_r_centered*as_centered + iris_cs_centered, data = ds))

apa.reg.table(
  process_rcs_model1,
  process_rcs_model2,
  filename = NA)

#IER: Cognitive Support
summary(process_cs_model <- lm(eff_comp ~ 1, data = ds))
summary(process_cs_model1 <- lm(eff_comp ~ iris_cs_centered + as_centered, data = ds))
summary(process_cs_model2 <- lm(eff_comp ~ iris_cs_centered*as_centered, data = ds))
summary(process_cs_model3 <- lm(eff_comp ~ iris_cs_centered + as_centered + 
                                  seeking, data = ds))
summary(process_cs_model4 <- lm(eff_comp ~ iris_cs_centered*as_centered + 
                                  seeking, data = ds))
summary(process_cs_model5 <- lm(eff_comp ~ iris_cs_centered*as_centered + 
                                  seeking*as_centered, data = ds))
summary(process_cs_model6 <- lm(eff_comp ~ iris_cs_centered*as_centered*seeking, data = ds))

apa.reg.table(
  process_cs_model1,
  process_cs_model2,
  filename = NA)

#apa.reg.table(
#  process_cs_model1,
#  process_cs_model2,
#  filename = "reg_cognitive_support.doc")



(process_cs_list <- list(as_centered = c(ppass_a_r, ppass_b_r),iris_cs_centered = 
                           seq(-5, 3, by = 1)))

emmip(process_cs_model2, as_centered ~ iris_cs_centered, at = process_cs_list, CIs = TRUE)

(process_cs_list_2 <- list(as_centered = c(ppass_a_r, ppass_b_r, 'sstest'), iris_cs_centered = 
                             c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(process_cs_model2, levels = process_cs_list_2)

#for excel simple slope analyses
vcov(process_cs_model2)

#What if I looked at this interaction when autonomy support is explored +/- 2 SDs
ppass_a2 <- mean(ds$as_centered, na.rm = T) + 2*(sd(ds$as_centered, na.rm = T))
ppass_m2 <- mean(ds$as_centered, na.rm = T)
ppass_b2 <- mean(ds$as_centered, na.rm = T) - 2*(sd(ds$as_centered, na.rm = T))

(ppass_a2_r <- round(ppass_a2,1))
(ppass_m2_r <- round(ppass_m2,1))
(ppass_b2_r <- round(ppass_b2,1))

(process_cs_2_list <- list(as_centered = c(ppass_a2_r, ppass_b2_r),iris_cs_centered = 
                             seq(-5, 3, by = 1)))

emmip(process_cs_model2, as_centered ~ iris_cs_centered, at = process_cs_2_list, CIs = TRUE)

(process_cs_2_list_2 <- list(as_centered = c(ppass_a2_r, ppass_b2_r, 'sstest'), iris_cs_centered = 
                               c(seq(-5, 3, by = 1), 'sstest')))
simple_slopes(process_cs_model2, levels = process_cs_2_list_2)

#for excel simple slope analyses
vcov(process_cs_model2)

#What is I included IER repsonsiveness in the modeL
summary(process_csr_model <- lm(eff_comp ~ 1, data = ds))
summary(process_csr_model1 <- lm(eff_comp ~ iris_cs_centered + iris_r_centered + as_centered, data = ds))
summary(process_csr_model2 <- lm(eff_comp ~ iris_cs_centered*as_centered + iris_r_centered, data = ds))

apa.reg.table(
  process_csr_model1,
  process_csr_model2,
  filename = NA)

#-------------------------------------------------------------------------------------------------------------

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
