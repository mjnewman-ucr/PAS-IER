* Encoding: UTF-8.

*READING IN DATA 
    *data was cleaned in R

PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE="/Users/madelinenewman/GitHub/PAS-IER/data/pasier_data_cleaned.csv"
  /ENCODING='UTF8'
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /LEADINGSPACES IGNORE=YES
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
  ID AUTO
  age AUTO
  gender AUTO
  s_orientation AUTO
  relationship AUTO
  race AUTO
  home AUTO
  dependent AUTO
  first_gen AUTO
  income AUTO
  eff_help AUTO
  eff_self AUTO
  eff_connect AUTO
  eff_coping AUTO
  eff_control AUTO
  iris_r AUTO
  iris_h AUTO
  iris_cs AUTO
  iris_pp AUTO
  ppass_limits AUTO
  ppass_reason AUTO
  ppass_aware AUTO
  ppass_threat AUTO
  ppass_guilt AUTO
  ppass_goals AUTO
  ppass_as AUTO
  ppass_pc AUTO
  sdt_autonomy AUTO
  sdt_competence AUTO
  sdt_relatedness AUTO
  bnsr_autonomy AUTO
  bnsr_competence AUTO
  bnsr_relatedness AUTO
  cerq_selfblame AUTO
  cerq_acceptance AUTO
  cerq_rumination AUTO
  cerq_refocusing AUTO
  cerq_planning AUTO
  cerq_reappraisal AUTO
  cerq_perspective AUTO
  cerq_catastrophizing AUTO
  cerq_otherblame AUTO
  ders_nonaccept AUTO
  ders_goals AUTO
  ders_impulse AUTO
  ders_awareness AUTO
  ders_strategies AUTO
  ders_clarity AUTO
  ders_total AUTO
  /MAP.
RESTORE.
CACHE.
EXECUTE.
DATASET NAME DataSet2 WINDOW=FRONT.

*Assigning value labels to demographics

VALUE LABELS
gender
1 'Female'
2 'Male'
3 'Non-binary/Third gender'
4 'Prefer to self decsribe'
5 'Prefer to to say'.
EXECUTE. 

VALUE LABELS
s_orientation
1 'Straight/heterosexual'
2'Bisexual/pansexual'
3 'Gay/lesbian'
4 'Asexual'
5 'Prefer to self-describe'.
EXECUTE. 

VALUE LABELS
relationship
1 'Single'
2 'In a relationship'
3 'Married'
4 'Divorced'
5 'Widowed'.
EXECUTE. 

VALUE LABELS
race
1 'Asian'
2 'Black' 
3 'Latinx'
4 'Native Hawaiian/Pacific Islander'
5 'White'
6 'Middle Eastern/North African'
7 'American Indian/Alaskan Native'
8 'Prefer not to say'
9 'Multiracial'.
EXECUTE.

VALUE LABELS
home
1 'Yes'
2 'No'.
EXECUTE.

VALUE LABELS
dependent
1 'Yes'
2 'No'.
EXECUTE.

VALUE LABELS
first_gen
1 'Yes'
2 'No'.
EXECUTE.

VALUE LABELS
income
1 'Under $10,000'
2 '$10,000 to $19,000'
3 '$20,000 to $39,000'
4 '$40,000 to $59,000'
5 '$60,000 to $79,000'
6 '$80,000 to $100,000'
7 'Over $100,000'
8 'Prefer not to say'.
EXECUTE.


