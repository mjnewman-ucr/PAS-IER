describe(ds$iris_r)

shapiro.test(ds$ppass_as)
ggplot(ds, aes(x = ppass_as)) + 
  geom_histogram(bins = 30) + 
  xlab("autonomy support")
skewness(ds$ppass_as, na.rm = TRUE)

shapiro.test(ds$eff_connect)
ggplot(ds, aes(x = eff_connect)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_connect, na.rm = TRUE)

shapiro.test(ds$eff_self)
ggplot(ds, aes(x = eff_self)) + 
  geom_histogram(bins = 30)
skewness(ds$eff_self, na.rm = TRUE)

shapiro.test(ds$ppass_pc)
ggplot(ds, aes(x = ppass_pc)) + 
  geom_histogram(bins = 15) + 
  xlab("parental control")
skewness(ds$ppass_pc, na.rm = TRUE)


shapiro.test(ds$iris_r)
ggplot(ds, aes(x = iris_r)) + 
  geom_histogram(bins = 100) + 
  xlab("responsiveness")
skewness(ds$iris_r, na.rm = TRUE)

shapiro.test(ds$iris_cs)
ggplot(ds, aes(x = iris_cs)) + 
  geom_histogram(bins = 100) + 
  xlab("cog support")
skewness(ds$iris_cs, na.rm = TRUE)

shapiro.test(ds$iris_h)
ggplot(ds, aes(x = iris_h)) + 
  geom_histogram(bins = 100) + 
  xlab("hostility")
skewness(ds$iris_h, na.rm = TRUE)

shapiro.test(ds$iris_pp)
ggplot(ds, aes(x = iris_pp)) + 
  geom_histogram(bins = 100) + 
  xlab("pp")
skewness(ds$iris_pp, na.rm = TRUE)

shapiro.test(ds$ppass_goals)
ggplot(ds, aes(x = ppass_goals)) + 
  geom_histogram(bins = 100) + 
  xlab("ppass goals")
skewness(ds$ppass_goals, na.rm = TRUE)

shapiro.test(ds$ppass_threat)
ggplot(ds, aes(x = ppass_threat)) + 
  geom_histogram(bins = 100) + 
  xlab("ppass threat")
skewness(ds$ppass_threat, na.rm = TRUE)

shapiro.test(ds$ppass_guilt)

#Creating a demographics dataset
demographics <- select(ds, c(age, gender, s_orientation, relationship, race, home, dependent:income))



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

ggplot(ds, aes(x=iris_r, y=ppass_as)) + geom_point()
ggplot(ds, aes(x=iris_cs, y=ppass_as)) + geom_point()
ggplot(ds, aes(x=iris_h, y=ppass_as)) + geom_point()
ggplot(ds, aes(x=iris_pp, y=ppass_as)) + geom_point()
ggplot(ds, aes(x=iris_r, y=ppass_pc)) + geom_point()
ggplot(ds, aes(x=iris_cs, y=ppass_pc)) + geom_point()
ggplot(ds, aes(x=iris_h, y=ppass_pc)) + geom_point()
ggplot(ds, aes(x=iris_pp, y=ppass_pc)) + geom_point()
ggplot(ds, aes(x=iris_r, y=eff_help)) + geom_point()
ggplot(ds, aes(x=iris_cs, y=eff_help)) + geom_point()
ggplot(ds, aes(x=iris_h, y=eff_help)) + geom_point()
ggplot(ds, aes(x=iris_pp, y=eff_help)) + geom_point()
ggplot(ds, aes(x=iris_h, y=eff_self)) + geom_point()

#---------

#Standardized

df <- select(ds, c(age, gender, s_orientation, relationship, race, home, dependent:income, 
                   eff_help:eff_control, iris_r:ppass_pc, sdt_autonomy:sdt_relatedness, 
                   bnsr_autonomy:cerq_otherblame, ders_nonaccept:ders_total))

df_scaled <- as.data.frame(scale(df))

ggplot(df_scaled, aes(x=iris_r, y=eff_help)) + geom_point()
ggplot(df_scaled, aes(x=iris_r, y=ppass_as)) + geom_point() + 
  geom_point()+
  geom_smooth(method=lm)


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


##-----------------------------------------------------------------------------
#Messing around with transforming the data

ds$log_iris_r <- log10(ds$iris_r)
ds$log_iris_h <- log10(ds$iris_h)
ds$log_ppass_pc <- log10(ds$ppass_pc)
ds$sqrt_iris_r <- sqrt(ds$iris_r)
ds$cube_iris_r <- ds$iris_r^(1/3)
ds$exp_iris_h <- exp(ds$iris_h)
ds$exp_iris_r <- exp(ds$iris_r)

shapiro.test(ds$log_iris_h)
ggplot(ds, aes(x = log_iris_h)) + 
  geom_histogram(bins = 100) + 
  xlab("iris_h")
skewness(ds$log_iris_h, na.rm = TRUE)

shapiro.test(ds$exp_iris_r)
ggplot(ds, aes(x = exp_iris_h)) + 
  geom_histogram(bins = 100) + 
  xlab("iris_r")
skewness(ds$exp_iris_h, na.rm = TRUE)

shapiro.test(ds$log_ppass_pc)
ggplot(ds, aes(x = log_ppass_pc)) + 
  geom_histogram(bins = 100) + 
  xlab("parental control")
skewness(ds$log_ppass_pc, na.rm = TRUE)

ds$ppass_pc <- as.numeric(ds$ppass_pc)

ds$ppass_pc <- str_sub(ds$ppass_pc, 1, 4)