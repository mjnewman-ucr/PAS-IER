describe(ds$gender)
ds_demos <- ds %>% 
  filter(gender == "1" | gender =="2")
describe(ds_demos$gender)

describe(ds$home)
ds_demos <- ds %>% 
  filter(home == "1" | home =="2")
describe(ds_demos$home)


summary(lm(eff_comp ~ seeking*first_gen, data = ds))

summary(aov(eff_comp ~ first_gen, data = ds))

chisq.test(table(ds$parent, ds$first_gen))

cor.test(ds$eff_comp, ds$income, method = "spearman")

#gender

t.test(eff_comp ~ gender, data = ds_demos)

t.test(eff_comp ~ first_gen, data = ds)

t.test(eff_comp ~ parent, data = ds_demos)

t.test(eff_comp ~ seeking, data = ds)

ds_demos$gender <- factor(ds_demos$gender,
                     levels = c(1,2),
                     labels = c("Male", "Female"))

summary(lm(eff_comp ~ iris_cs_centered + iris_r_centered + as_centered + gender, data = ds_demos))

ds_demos$as_centered <- center_scale(ds_demos$ppass_as)
ds_demos$iris_cs_centered <- center_scale(ds_demos$iris_cs)
ds_demos$iris_r_centered <- center_scale(ds_demos$iris_r)

summary(test <- lm(eff_comp ~ iris_cs_centered + iris_r_centered*gender, data = ds_demos))

irisr_a <- mean(ds_demos$iris_r_centered, na.rm = T) + sd(ds_demos$iris_r_centered, na.rm = T)
irisr_m <- mean(ds_demos$iris_r_centered, na.rm = T)
irisr_b <- mean(ds_demos$iris_r_centered, na.rm = T) - sd(ds_demos$iris_r_centered, na.rm = T)

(irisr_a_r <- round(irisr_a,2))
(irisr_m_r <- round(irisr_m,2))
(irisr_b_r <- round(irisr_b,2))


(gender_list <- list(iris_r_centered = c(irisr_a_r, irisr_m_r, irisr_b_r), gender = c("Male", "Female")))
emmip(test, iris_r_centered ~ gender, at = gender_list, CIs = TRUE)

(gender_list_2 <- list(iris_r_centered= c(irisr_a_r, irisr_m_r, irisr_b_r, 'sstest'), 
                        gender = c("Male", "Female", 'sstest')))
simple_slopes(test, levels = gender_list_2)


ds_demos$latinx <- ifelse(ds_demos$race == "Latinx", 1, 0)

ds_demos$latinx <- factor(ds_demos$latinx,
                          levels = c(1,0),
                          labels = c("Yes", "No"))

ds_demos$latinx <- relevel(ds_demos$latinx, ref="No") 

ds_demos$latinx



t.test(eff_comp ~ latinx, data = ds_demos)

summary(lm(eff_comp ~ iris_cs_centered + iris_r_centered + gender + latinx, 
           data = ds_demos))

summary(lm(eff_comp ~ iris_cs_centered + iris_r_centered*gender + latinx, 
           data = ds_demos))

summary(lm(eff_comp ~ iris_cs_centered + iris_r_centered + latinx*gender, 
           data = ds_demos))

summary(lm(eff_comp ~ iris_r_centered + iris_cs_centered + as_centered + seeking + 
             latinx + first_gen, 
           data = ds_demos))

summary(lm(eff_comp ~ seeking*latinx, data = ds_demos))

mylogit <- glm(seeking ~ as_centered + gender + latinx, 
               data = ds_demos, family = "binomial")
summary(mylogit)

summary(mylogit <- glm(seeking ~ as_centered*gender, 
               data = ds_demos, family = "binomial"))

summary(mylogit <- glm(seeking ~ iris_r_centered*gender, 
                       data = ds_demos, family = "binomial"))

ds_demos$home <- factor(ds_demos$home,
                          levels = c(1,2),
                          labels = c("Yes", "No"))

summary(home_int <- lm(eff_comp ~ home*seeking, data = ds_demos))

(home_list <- list(home = c("Yes", "No"), 
                     seeking = c("Yes", "No")))

emmip(home_int, home ~ seeking, at = home_list, CIs = TRUE)

(home_list_2 <- list(home = c("Yes", "No", 'sstest'), 
                     seeking = c("Yes", "No", 'sstest')))
simple_slopes(home_int, levels = home_list_2)


interaction.plot(
  x.factor = ds_demos$home,
  trace.factor = ds_demos$seeking,
  response = ds_demos$eff_comp,
  fun = mean,
  ylab = "IER Effectiveness",
  xlab = "Home",
  trace.label = "Seeking",
  col = c("#0198f9", "#f95801"),
  lyt = 1,
  lwd = 3
)
