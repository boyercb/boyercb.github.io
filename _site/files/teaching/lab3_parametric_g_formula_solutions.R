library(tidyverse)
library(gfoRmula)

# setting up the data -----------------------------------------------------

# The code below creates the frequency table, and then turns it into long format
# data. Read through the code carefully and run it.

# re-create the frequency table for homework 3
hw3_freq <- tribble(
  ~A_0, ~L_1, ~A_1, ~N, ~Y_1,
  0, 0, 0, 6000, 60,
  0, 0, 1, 2000, 60,
  0, 1, 0, 2000, 210,
  0, 1, 1, 6000, 210,
  1, 0, 0, 3000, 240,
  1, 0, 1, 1000, 240,
  1, 1, 0, 3000, 120,
  1, 1, 1, 9000, 120
)

# expand the frequency table to 1 row per person
# (first just assign everyone the average Y value)
dat <- uncount(hw3_freq, N) %>%
  # give those rows an id number
  rowid_to_column(var = "id") %>%
  # for each of the variables except for id, split it into two
  # rows, one for each time point
  pivot_longer(-id,
               names_to = c(".value", "time"),
               names_sep = "_"
  ) %>%
  # make sure that time is read as a number
  mutate(time = parse_number(time),
         # add some random error to Y 
         # (nrow(.) means a different value for each row of the dataset in use
         Y = Y + rnorm(nrow(.), 0, 10))


# create lagged variables
t1 <- dat %>%
  mutate(lag_A = lag(A)) %>%
  filter(time == 1)


# calculating the g-formula by hand ---------------------------------------

# fit models for covariate and outcome given past on t1
L_mod <- glm(L ~ lag_A, data = t1, family = binomial())
Y_mod <- glm(Y ~ A * lag_A * L, data = t1)

# create new data set with space for 10,000 simulated entries
nsim <- 10000
baseline_pop <- tibble(id = 1:nsim)

# fix intervention values in the new data set to be consistent with the regime
new_dat <- baseline_pop %>%
  mutate(lag_A = 1, A = 1)

# predict covariate means at time 1
new_dat$pL <- predict(L_mod, newdata = new_dat, type = "response")

# simulate covariate values at time 1
new_dat$L <- rbinom(nsim, 1, new_dat$pL)

# predict outcome at time 1 based on simulated covariates and fixed treatments
new_dat$Ey <- predict(Y_mod, newdata = new_dat, type = "response")

# take mean across all simulations to get g-formula estimate!
mean(new_dat$Ey)


# running the g-formula ---------------------------------------------------

# add your comments to the lines below
id <- "id"
time_name <- "time"
covnames <- c("L", "A")
outcome_name <- "Y"
# 
outcome_type <- "continuous_eof"
#
histories <- c(lagged)
#
histvars <- list(c("A"))
#
covtypes <- c("binary", "binary")
covparams <- list(covmodels = c(
  L ~ lag1_A,
  A ~ lag1_A * L
))
ymodel <- Y ~ A * L * lag1_A
#
intvars <- list("A", "A", "A", "A")
#
interventions <- list(
  list(c(static, c(0, 0))),
  list(c(static, c(0, 1))),
  list(c(static, c(1, 0))),
  list(c(static, c(1, 1)))
)
int_descript <- c(
  "0, 0",
  "0, 1",
  "1, 0",
  "1, 1"
)

# put it all together
# (the warning "obs_data was coerced to a data table" is fine!)
gform_res <- gformula(
  obs_data = dat,
  id = id,
  time_name = time_name,
  covnames = covnames,
  outcome_name = outcome_name,
  outcome_type = outcome_type,
  covtypes = covtypes,
  covparams = covparams,
  ymodel = ymodel,
  intvars = intvars,
  interventions = interventions,
  int_descript = int_descript,
  ref_int = 1,
  histories = histories,
  histvars = histvars,
  sim_data_b = TRUE,
  seed = 345636
)
# Use the following code to explore the results. (You can ignore Intervention 0,
# the natural course intervention, for now.) How many models were fit? How can
# you tell which of the data in the `sim_data` object was simulated or predicted
# from a model? Does these results match your answers to the earlier questions,
# and to Homework 3? Why or why not?
gform_res
gform_res$coeffs
gform_res$sim_data

