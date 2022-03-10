#> Description: This script is for my psyc259 workflow project. Since I don't have
#> data yet, I fabricated some on qualtrics (preview) 


#Loading the packages I expect I will need

library("tidyverse")
library("dplyr")
library("readr")

#> Before reading in the data file I did some basic cleaning (hardcoding, yikes)
#> of the csv file. I removed some weird columns/rows:
#> Rows: 2 & 3 (extra column info)
#> Columns: StartDate	EndDate	Status Progress DistributionChannel	UserLanguage

#Messing around with the qualtRics package

#install.packages("qualtRics")

#Alrighty, seems like it's not possible with API access, but I've requested this from
#Emilyb and it's forthcoming


#-------------------------------------------------------------------------------------------------------------

#READING IN DATA FILE

ds <- read_csv("PAS-IER fabricated data.csv", col_names = TRUE, na = "9999", name_repair = tolower)


#-------------------------------------------------------------------------------------------------------------

#CLEANING/TIDYING DATA

#Cleaning the data
glimpse(ds)

#Observations:
#> 1. Some of the columns need to renamed (e.g., duration, id, etc)
#> 2. ID numbers look super weird

#Renaming columns
ds <- ds %>% rename(ID = responseid,
              date = recordeddate,
              duration = `duration (in seconds)`,
              completed = finished)

#Putting ID first
ds <- select(ds, ID, everything())

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


