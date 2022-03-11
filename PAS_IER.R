#> Description: This script is for my psyc259 workflow project. Since I don't have
#> data yet, I fabricated some on qualtrics (preview) 


#Loading the packages I expect I will need

library("tidyverse")
library("dplyr")
library("readr")

#> Before reading in the data file I did some basic cleaning (hardcoding, yikes)
#> of the csv file. I removed some weird columns/rows:
#> Rows: 2 & 3 (extra column info)
#> Columns: StartDate	EndDate	Status Progress Finished DistributionChannel	UserLanguage

#Messing around with the qualtRics package

#install.packages("qualtRics")

#Alrighty, seems like it's not possible with API access, but I've requested this from
#Emilyb and it's forthcoming


#-------------------------------------------------------------------------------------------------------------

#READING IN DATA FILE

ds <- read_csv("data/pasier_testing.csv", col_names = T, na = "9999", name_repair = tolower)
colnames <- as.vector(names(ds))
ds <- read_csv("data/pasier_testing.csv", col_names = colnames, na = "9999", skip = 15, name_repair = tolower)


#-------------------------------------------------------------------------------------------------------------

#CLEANING/TIDYING DATA

#Cleaning the data
glimpse(ds)

#Observations:
#> 1. Some of the columns need to renamed (e.g., duration, id, etc)
#> 2. ID numbers look super weird

#Renaming columns
ds <- ds %>% rename(date = recordeddate,
              duration = `duration (in seconds)`)

ds <- ds %>% mutate(ID = 8001:8132)

#Putting ID first and removing unnecessary columns
ds <- select(ds, c(ID, ierc_1a:income))
glimpse(ds)

#write_csv(ds, "cleaned_data_for_john.csv")

#Calculating questionnaire subscales

#> IRIS: Interpersonal Regulation Interaction Scale (Swerdlow & Johnson, 2020)

#> iris_r = responsiveness (1, 4, 6, 8, 11, 13, 14, 17, 22, 25, 28)
#> iris_h = hostility (3, 7, 10, 18, 20, 23, 26)
#> iris_cs = cognitive support (2, 5, 9, 12, 15, 19, 27)
#> iris_pp = physical presence  (16, 21, 24)

ds <- ds %>% mutate(iris_r = iris_1 + iris_4 + iris_6 + iris_8 + iris_11 + iris_13 + iris_14 + iris_17 + iris_22 + iris_25 + iris_28)
ds <- ds %>% mutate(iris_h = iris_3 + iris_7 + iris_10 + iris_18 + iris_20 + iris_23 + iris_26)
ds <- ds %>% mutate(iris_cs = iris_2 + iris_5 + iris_9 + iris_12 + iris_15 + iris_19 +iris_27)
ds <- ds %>% mutate(iris_pp = iris_16 + iris_21 + iris_24)

#> P-PASS: Perceived Parental Autonomy Support Scale (cite)

#> Autonomy-Support Subscales
#> ppass_limits = Offering choice within certain limits: 1, 4, 8, 14 
#> ppass_reason = Explaining the reasons behind the demands, rules, and limits: 2, 9, 19, 23 
#> ppass_aware = Being aware of, accepting, and recognizing the child’s feelings: 7, 13, 16, 24 

#> Psychological Control Subscales
#> ppass_threat = Threatening to punish the child: 3, 10, 15, 20 
#> ppass_guilt = Inducing guilt: 6, 12, 18, 21 
#> ppass_goals = Encouraging performance goals: 5, 11, 17, 22

ds <- ds %>% mutate(ppass_limits = ppass_1 + ppass_4 + ppass_8 + ppass_14)
ds <- ds %>% mutate(ppass_reason = ppass_2 + ppass_9 + ppass_19 + ppass_23)
ds <- ds %>% mutate(ppass_aware = ppass_7 + ppass_13 + ppass_16 + ppass_24) 
ds <- ds %>% mutate(ppass_threat = ppass_3 + ppass_10 + ppass_15 + ppass_20) 
ds <- ds %>% mutate(ppass_guilt = ppass_6 + ppass_12 + ppass_18 + ppass_21) 
ds <- ds %>% mutate(ppass_perform = ppass_5 + ppass_11 + ppass_17 + ppass_22) 

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

ds$cerq_selfblame
ds$cerq_acceptance
ds$cerq_rumination
ds$cerq_refocusing
ds$cerq_planning
ds$cerq_reappraisal
ds$cerq_perspective
ds$cerq_catastrophizing
ds$cerq_otherblame




