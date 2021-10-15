set.seed(1234)

# series-wise dataset
po_series <- po_dat_long %>%
  filter(game_num == 1) %>%
  select(season, series, name, p_win, won_series) %>%
  unique() %>%
  mutate(is_home_team = name=="home_team") %>%
  group_by(season, series) %>%
  sample_n(1)

# select one outcome record per game (random team's perspective)
po_games <- po_dat_long %>%
  group_by(season, series, game_num) %>%
  sample_n(1) %>%
  mutate(is_home_team = name=="home_team")

# p(win | home team)
# the home team won games about 63% of the time (seeding, not just home court adv)
x <- glm(win ~ is_home_team, data=po_games, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]))
# the higher seeded team won the series about 75% of the time
x <- glm(won_series ~ is_home_team, data=po_series, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]))

# Elo-based p_win has a coefficient of approximately 1 in a linear regression
x <- glm(win ~ p_win, data=po_games, family="gaussian")
x
# also validates well in a logistic model
x <- glm(win ~ p_win, data=po_games, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.5))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.2))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.8))

# home court advantage still matters if we account for Elo
x <- glm(win ~ p_win + is_home_team, data=po_games, family="binomial")
x

# Elo (for game 1) at the series level is "more than linear" as expected
x <- glm(won_series ~ p_win, data=po_series, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.5))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.2))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.8))

# How much advantage does Elo say the series home team typically has? About 60-40
po_series %>%
  ungroup() %>%
  filter(is_home_team) %>%
  summarise(p_win = mean(p_win))

# work with wide data
po_dat_wide <- po_dat_long %>%
  group_by(season, series, team) %>%
  mutate(is_series_home_team = name[1]=="home_team") %>%  
  ungroup() %>%
  select(season, series, team, is_series_home_team, game_num, win, series_length, won_series, p_win) %>%
  pivot_wider(names_from = game_num, values_from = c(win, p_win), names_prefix = "game_") %>%
  group_by(series, season) %>%
  sample_n(1) 

# series home team wins about 75% of the time
x <- glm(won_series ~ is_series_home_team, data=po_dat_wide, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]))

# series win is "superlinear" in game 1 p_win
x <- glm(won_series ~ p_win_game_1, data=po_dat_wide, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.5))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.2))
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.8))

# How much advantage does Elo say the series home team typically has? About 60-40
x <- glm(p_win_game_1 ~ is_series_home_team, data=po_dat_wide, family="gaussian")

# game 1 winner wins series about 71% of the time
x <- glm(won_series ~ win_game_1, data=po_dat_wide, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]))

# game 2 winner wins series about 76% of the time
x <- glm(won_series ~ win_game_2, data=po_dat_wide, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]))

# game 3 winner wins series about 76% of the time
x <- glm(won_series ~ win_game_3, data=po_dat_wide, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]))

# games 1 and 2 seem more important than game 3
x <- glm(won_series ~ win_game_1 + win_game_2 + win_game_3, data=po_dat_wide, family="binomial")
x

# but this effect goes away when we control for being the home team
x <- glm(won_series ~ is_series_home_team + win_game_1 + win_game_2 + win_game_3, data=po_dat_wide, family="binomial")
x

# There is a benefit to being the series home team beyond Elo
x <- glm(won_series ~ p_win_game_1 + is_series_home_team, data=po_dat_wide, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.5 - x$coefficients[3]))

# combined effects
x <- glm(won_series ~ p_win_game_1 + is_series_home_team + win_game_1 + win_game_2 + win_game_3, data=po_dat_wide, family="binomial")
x
1/(1+exp(-x$coefficients[1]-x$coefficients[2]*.5 - x$coefficients[3] - x$coefficients[4] - x$coefficients[5] - x$coefficients[6]))
summary(x)

# interaction effects
x <- glm(won_series ~ 
           p_win_game_1 + 
           is_series_home_team + 
           win_game_1 + 
           win_game_2 + 
           win_game_3 + 
           is_series_home_team*win_game_1 +
           is_series_home_team*win_game_2 +
           is_series_home_team*win_game_3, 
         data=po_dat_wide, family="binomial")
summary(x)

x <- glm(won_series ~ 
           p_win_game_1 + 
           is_series_home_team + 
           win_game_1 + 
           win_game_2 + 
           win_game_3 + 
           win_game_1*win_game_2 +
           win_game_2*win_game_3 +
           win_game_1*win_game_3, 
         data=po_dat_wide, family="binomial")
summary(x)
