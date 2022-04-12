1##PAS-IER DATA CLEANING

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

#For some off reason, year of birth got recoded weirdly by qualtrics.

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
#*script

ds <- unite(ds, dob, 'age#1_1':'age#3_1', sep = "-", remove = F, na.rm = T)


ds$date <- as_date(ds$date, format = '%d/%m/%y')

ds$dob <- as_date(ds$dob, format = '%m-%d-%Y')

ds$age <- as.period(interval(start = ds$dob, end = ds$date), unit = "year")
ds$age <- str_sub(ds$age, 1, 2) 

ds$age <- as.numeric(ds$age)

#Tidying race... recoding responses of multiple as 9
ds$race <- ifelse(str_length(ds$race) > 1, 9, ds$race)

#Recoding text demographics
describe(ds$s_orientation_6_text)
describe(ds$home_3_text)
describe(ds$home_who_9_text)


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

ds$ppass_limits <- rowMeans(subset(ds, select = c(ppass_1, ppass_4, ppass_8, ppass_14)), na.rm = T)
ds$ppass_reason <- rowMeans(subset(ds, select = c(ppass_2, ppass_9, ppass_19, ppass_23)), na.rm = T) 
ds$ppass_aware <- rowMeans(subset(ds, select = c(ppass_7, ppass_13, ppass_16, ppass_24)), na.rm = T) 
ds$ppass_threat <- rowMeans(subset(ds, select = c(ppass_3, ppass_10, ppass_15, ppass_20)), na.rm = T) 
ds$ppass_guilt <- rowMeans(subset(ds, select = c(ppass_6, ppass_12, ppass_18, ppass_21)), na.rm = T) 
ds$ppass_goals <- rowMeans(subset(ds, select = c(ppass_5, ppass_11, ppass_17, ppass_22)), na.rm = T) 

ds$ppass_as <- rowMeans(subset(ds, select = c(ppass_limits, ppass_reason, ppass_aware), na.rm = T))
ds$ppass_pc <- rowMeans(subset(ds, select = c(ppass_threat, ppass_guilt, ppass_goals), na.rm = T))

#ds <- ds %>% mutate(ppass_limits = ppass_1 + ppass_4 + ppass_8 + ppass_14)
#ds <- ds %>% mutate(ppass_reason = ppass_2 + ppass_9 + ppass_19 + ppass_23)
#ds <- ds %>% mutate(ppass_aware = ppass_7 + ppass_13 + ppass_16 + ppass_24) 
#ds <- ds %>% mutate(ppass_threat = ppass_3 + ppass_10 + ppass_15 + ppass_20) 
#ds <- ds %>% mutate(ppass_guilt = ppass_6 + ppass_12 + ppass_18 + ppass_21) 
#ds <- ds %>% mutate(ppass_perform = ppass_5 + ppass_11 + ppass_17 + ppass_22) 

#----------------------------------------------------------------------------------

#SDT: Basic Psychological Need Satisfaction and Frustration Scale (BPNSNF) (cite)

#Likert scale of 1 - 7

#> Form three subscale scores, one for the degree to which the person experiences 
#> satisfaction of each of the three needs. 
#> 
#> To do that, you must first reverse score all items that are worded in a negative way 
#> (i.e., the items shown below with (r) following the items number). To reverse 
#> score an item, simply subtract the item response from 8. Thus, for example, 
#> a 2 would be converted to a 6. Once you have reverse scored the items, 
#> simply average the items on the relevant subscale. They are:

#> Autonomy: 1, 4r, 8, 11r, 14, 17, 20r
#> Competence: 3r, 5, 10, 13, 15r, 19r
#> Relatedness: 2, 6, 7r, 9, 12, 16r, 18r, 21

ds <- ds %>% mutate(sdt_4r = 8 - sdt_4,
                    sdt_11r = 8 - sdt_11,
                    sdt_20r = 8 - sdt_20,
                    sdt_3r = 8 - sdt_3,
                    sdt_15r = 8 - sdt_15,
                    sdt_19r = 8 - sdt_19,
                    sdt_7r = 8 - sdt_7,
                    sdt_16r = 8 - sdt_16,
                    sdt_18r = 8 - sdt_18
)

ds$sdt_autonomy <- rowMeans(subset(ds, select = c(sdt_1, sdt_4r, sdt_8, sdt_11r, 
                                                  sdt_14, sdt_17, sdt_20r)), na.rm = T) 

ds$sdt_competence <- rowMeans(subset(ds, select = c(sdt_3r, sdt_5, sdt_10, sdt_13, 
                                                    sdt_15r, sdt_19r)), na.rm = T) 

ds$sdt_relatedness <- rowMeans(subset(ds, select = c(sdt_2, sdt_6, sdt_7r, sdt_9, 
                                                     sdt_12, sdt_16r, sdt_18r, sdt_21)), na.rm = T) 

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

ds <- ds %>% mutate(ders_20r = 6 - ders_20,
                    ders_24r = 6 - ders_24,
                    ders_2r = 6 - ders_2,
                    ders_6r = 6 - ders_6,
                    ders_8r = 6 - ders_8,
                    ders_10r = 6 - ders_10,
                    ders_17r = 6 - ders_17,
                    ders_34r = 6 - ders_34,
                    ders_22r = 6 - ders_22,
                    ders_1r = 6 - ders_1,
                    ders_7r = 6 - ders_7
)

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

clean_ds <- select(ds, c(ID, age, gender, s_orientation, relationship, race, home, dependent:income, 
                         parent, location, seeking, eff_help:eff_control, iris_r:ppass_pc, 
                         sdt_autonomy:sdt_relatedness, bnsr_autonomy:cerq_otherblame, 
                         ders_nonaccept:ders_total))

write_csv(clean_ds, "data/pasier_data_cleaned.csv")


##-----------------------------------------------------------------------------
