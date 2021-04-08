library(ggplot2)

# returns RRau for given RRuy and BF
bounding_func <- function(RRuy, BF) {
  (BF * (1 - RRuy)) / (BF - RRuy)
}

# set values to given and calculate e-values
RR <- 1.8
LCI <- 1.4
Eval <- RR + sqrt(RR * (RR - 1))
EvalCI <- LCI + sqrt(LCI * (LCI - 1))

# propose values of RRuy
RRuy <- seq(1, 10, by = .01)

# start off with empty vectors to be filled
RRau_RR <- rep(NA, length(RRuy))
RRau_LCI <- rep(NA, length(RRuy))

# run through all the RRuy values, calculating RRau for each
for (i in 1:length(RRuy)) {
  RRau_RR[i] <- bounding_func(RRuy[i], RR)
  RRau_LCI[i] <- bounding_func(RRuy[i], LCI)
}

# makde dataframes for each with identical colnames
bounds <-
  data.frame(RRau = RRau_RR, RRuy = RRuy, Bound = "Point Estimate")
CIbounds <-
  data.frame(RRau = RRau_LCI, RRuy = RRuy, Bound = "Lower Confidence Bound")

# put them together
plot_data <- rbind(bounds, CIbounds)

# we know the RRau has to be > 1
plot_data <- subset(plot_data, RRau > 1)

# make a dataframe to plot the points for the E-values
point_data <-
  data.frame(
    x = c(Eval, EvalCI),
    y = c(Eval, EvalCI),
    Bound = c("Point Estimate", "Lower Confidence Bound")
  )

# make labels for the points
label1 <- paste0("(", round(Eval, 2), ", ", round(Eval, 2), ")")
label2 <- paste0("(", round(EvalCI, 2), ", ", round(EvalCI, 2), ")")

ggplot(plot_data, aes(RRau, RRuy, group = Bound, col = Bound)) + geom_line() +
  geom_point(data = point_data, aes(x, y)) +
  annotate(
    "text",
    label = label1,
    x = Eval + 0.5,
    y = Eval + 0.3,
    size = 4
  ) +
  annotate(
    "text",
    label = label2,
    x = EvalCI + 0.5,
    y = EvalCI + 0.3,
    size = 4
  ) +
  ylab(expression(RR[UY])) + xlab(expression(RR[AU])) + ylim(1, 10) + xlim(1, 10) +
  scale_color_discrete(labels = c(
    expression(RR[AU] * RR[UY] * "/(" * RR[AU] * " + " * RR[UY] * " - 1) = 1.8"),
    expression(RR[AU] * RR[UY] * "/(" * RR[AU] *
                 " + " * RR[UY] * " - 1) = 1.4")
  )) +
  theme(legend.position = "bottom")