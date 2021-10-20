library(dplyr)
library(ggplot2)
library(MASS)

# the higher seeded team won games about 63% of the time (seeding, not just home court adv)
mean(po_dat_long$win)

# but this is quite variable across game numbers - home court advantage
po_dat_long %>%
  group_by(game_num) %>%
  summarize(p_win = mean(win, na.rm=T))

# according to elo, the higher seeded team should only have won about 58% of the time
# but they play about 54% of games at home (hc adv)
mean(po_dat_long$p_win)
mean(po_dat_long$is_home_team)

# the higher seeded team won the series about 75% of the time
mean(po_dat_wide$won_series)

# Elo difference is a strong predictor of outcome
x <- glm(win ~ elo_diff, data=po_dat_long, family="binomial")
x
summary(x)
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*-181))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*0))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*60))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*335))

# home court advantage still matters if we account for Elo - about 238 Elo points
x <- glm(win ~ elo_diff + is_home_team, data=po_dat_long, family="binomial")
x
summary(x)
1/(1+exp(-x$coefficients[1]-x$coefficients[3]*0))
1/(1+exp(-x$coefficients[1]-x$coefficients[3]*1))
# Elo has a higher coefficient at the series level than at the game level, as expected
x <- glm(win ~ elo_diff, data=po_dat_long, family="binomial")
x
x <- glm(won_series ~ elo_diff_game_1, data=po_dat_wide, family="binomial")
x

# How much advantage does 538's Elo model say the series home team typically has? About 60-40
po_dat_wide %>%
  ungroup() %>%
  summarise(p_win = mean(p_win_game_1))

###################

# prior probability distribution of series_win computed from negative binomial and game 1 p_win
# increase game win probability by 9% to align distribution mean with empirical series win freq
po_dat_wide1 <- po_dat_wide %>%
  mutate(prior_p_series_win = pnbinom(3,4,p_win_game_1*1.09))

# estimate beta distribution coefficients for series win prior
m <- MASS::fitdistr(po_dat_wide1$prior_p_series_win, dbeta,
                    start = list(shape1 = 1, shape2 = 1))
# beta estimates: 3.86, 1.24
alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]
# mean p(series_win) is 76%
alpha0/(alpha0+beta0)
# plot
ggplot(po_dat_wide1) +
  geom_histogram(aes(prior_p_series_win, y = ..density..), binwidth = .05) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  xlab("Series win probability")

# update: won game 1 (conditional series win prob: 83%)
cond <- sum(po_dat_wide1$win_game_1)
ws_cond <- sum(po_dat_wide1$win_game_1 & po_dat_wide1$won_series)
alpha1 <- alpha0 + ws_cond
beta1 <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha1, beta1), color = "blue",
                size = 1) +
  xlab("Series win probability")



# update: won game 2 (conditional series win prob: 84%)
cond <- sum(po_dat_wide1$win_game_2)
ws_cond <- sum(po_dat_wide1$win_game_2 & po_dat_wide1$won_series)
alpha2 <- alpha0 + ws_cond
beta2 <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha2, beta2), color = "blue",
                size = 1) +
  xlab("Series win probability")

# update: won game 3 (conditional series win prob: 91%)
cond <- sum(po_dat_wide1$win_game_3)
ws_cond <- sum(po_dat_wide1$win_game_3 & po_dat_wide1$won_series)
alpha3 <- alpha0 + ws_cond
beta3 <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha3, beta3), color = "blue",
                size = 1) +
  xlab("Series win probability")

# update: won game 4 (conditional series win prob: 92%)
cond <- sum(po_dat_wide1$win_game_4)
ws_cond <- sum(po_dat_wide1$win_game_4 & po_dat_wide1$won_series)
alpha4 <- alpha0 + ws_cond
beta4 <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha4, beta4), color = "blue",
                size = 1) +
  xlab("Series win probability")

ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha1, beta1), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha2, beta2), color = "blue",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha3, beta3), color = "green",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha4, beta4), color = "purple",
                size = 1) +
  xlab("Series win probability")

# update: lost game 1 (conditional series win prob: 54%)
cond <- sum(!po_dat_wide1$win_game_1)
ws_cond <- sum(!po_dat_wide1$win_game_1 & po_dat_wide1$won_series)
alpha1l <- alpha0 + ws_cond
beta1l <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha1, beta1), color = "blue",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alpha1l, beta1l), color = "green",
                size = 1) +
  xlab("Series win probability")

# update: WL (conditional series win prob: 60%)
cond <- sum(po_dat_wide1$win_game_1 & !po_dat_wide1$win_game_2)
ws_cond <- sum(po_dat_wide1$win_game_1 & !po_dat_wide1$win_game_2 & po_dat_wide1$won_series)
alphaWL <- alpha0 + ws_cond
betaWL <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaWL, betaWL), color = "blue",
                size = 1) +
  xlab("Series win probability")

# update: LW (conditional series win prob: 65%)
cond <- sum(!po_dat_wide1$win_game_1 & po_dat_wide1$win_game_2)
ws_cond <- sum(!po_dat_wide1$win_game_1 & po_dat_wide1$win_game_2 & po_dat_wide1$won_series)
alphaLW <- alpha0 + ws_cond
betaLW <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaWL, betaWL), color = "blue",
                size = 1) +
  xlab("Series win probability")

# update: WW (conditional series win prob: 92%)
cond <- sum(po_dat_wide1$win_game_1 & po_dat_wide1$win_game_2)
ws_cond <- sum(po_dat_wide1$win_game_1 & po_dat_wide1$win_game_2 & po_dat_wide1$won_series)
alphaWW <- alpha0 + ws_cond
betaWW <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaWW, betaWW), color = "blue",
                size = 1) +
  xlab("Series win probability")

# update: LL (conditional series win prob: 13%)
cond <- sum(!po_dat_wide1$win_game_1 & !po_dat_wide1$win_game_2)
ws_cond <- sum(!po_dat_wide1$win_game_1 & !po_dat_wide1$win_game_2 & po_dat_wide1$won_series)
alphaLL <- alpha0 + ws_cond
betaLL <- beta0 + (cond - ws_cond)
ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaLL, betaLL), color = "blue",
                size = 1) +
  xlab("Series win probability")

ggplot(po_dat_wide1) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "black",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaWL, betaWL), color = "red",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaLW, betaLW), color = "blue",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaWW, betaWW), color = "green",
                size = 1) +
  stat_function(fun = function(x) dbeta(x, alphaLL, betaLL), color = "purple",
                size = 1) +
  xlab("Series win probability")