library(survival)
library(ggplot2)
library(ggfortify)

# if you don't have the packages above you can install them by uncommenting the code below
# install.packages(survival)
# install.packages(ggplot2)
# install.packages(ggfortify)

# Exercise 1 --------------------------------------------------------------

# For this section, we'll be using the ovarian dataset, which is from an RCT
# comparing two treatments for ovarian cancer. You can red a bit more about it,
# including a description of the variables, in the help pages, i.e.
?ovarian

# 1. Calculate the Kaplan-Meier estimates of survival by hand for the 13 women
# in the treatment group.
View(subset(ovarian, rx == 1))


# 2. Confirm your results using the `survfit()` function in `R`.
km <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
summary(km)

# 3. Calculate the Flemming-Harrington estimates of survival by hand for the 13 women in the treatment group.
View(subset(ovarian, rx == 1))

# 4. Confirm your results using the `survfit()` function in `R`. 
fh <- survfit(Surv(futime, fustat) ~ rx, data = ovarian, stype = 2)
summary(fh)

# 5. Plot both estimates for treatment and control groups with 95% confidence
# intervals (use `log-log` variance).
km <- survfit(Surv(futime, fustat) ~ rx, data = ovarian, conf.type = "log-log")
fh <- survfit(Surv(futime, fustat) ~ rx, data = ovarian, conf.type = "log-log", stype = 2)

autoplot(km)
autoplot(fh)

# 6. Conduct a log-rank test comparing the survival estimates across treatment
# and control. How do you interpret the results?
lrank <- survdiff(Surv(futime, fustat) ~ rx, data = ovarian, rho = 0)
lrank

# Exercise 2 --------------------------------------------------------------

# For this section, we'll be using the colon dataset, which is from an RCT
# comparing three treatments for colon cancer. You can red a bit more about it,
# including a description of the variables, in the help pages, i.e.
?survival::colon

# 1. Calculate the Kaplan-Meier estimates of survival for the treatment groups.
# Does the PH assumption seem reasonable here?
km <- survfit(Surv(time, status) ~ rx, data = colon, conf.type = "log-log")
autoplot(km)

# 2. Fit a Cox proportional hazards model for effect of treatment and adjusting
# for `age` and `sex`. Interpret the coefficients on the treatment terms.
cox <- coxph(Surv(time, status) ~ rx + sex + age, data = colon)
summary(cox)

# 3. Conduct Wald and likelihood ratio tests for the effect of treatment? What
# do you conclude?
cox.nested <- coxph(Surv(time, status) ~ sex + age, data = colon)
anova(cox, cox.nested)

# 4. Does the effect of treatment differ by `age` or `sex`? How do you know?
cox <- coxph(Surv(time, status) ~ rx * (sex + age), data = colon)
summary(cox)

# 5. What is the predicted survival for a male aged 50 and female aged 50 under
# the `Lev+5FU` treatment at time 1500?
df <- data.frame(
  rx = factor("Lev+5FU", levels = levels(colon$rx)),
  sex = c(1,0), 
  age = 50
)

cox.pred <- survfit(coxph(Surv(time, status) ~ rx * (sex + age), data = colon), newdata = df)
summary(cox.pred)

