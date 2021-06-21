library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)

datdir <- "data"
datfiles <- list.files(datdir)

read_season <- function(str){
  year <- stringr::str_extract(str, '[0-9]+')
  read_csv(str) %>%
    mutate(season = as.numeric(year))
}


dat <- datfiles %>%
  paste0('data/', .) %>%
  map_df(read_season)

teams <- distinct(dat, season, team = away_team)

last_away <- dat %>% 
  group_by(season, away_team) %>%
  summarise(last_away_date = max(start_time)) %>%
  rename(team = away_team)

last_home <- dat %>% 
  group_by(season, home_team) %>%
  summarise(last_home_date = max(start_time)) %>%
  rename(team = home_team)

rs_ends <- teams %>%
  inner_join(last_away) %>%
  inner_join(last_home) %>%
  mutate(last_game = pmax(last_away_date, last_home_date)) %>%
  arrange(season, desc(last_game)) %>%
  group_by(season) %>%
  filter(row_number() == 17) %>%
  select(season, rs_end_date = last_game)

po_dat <- dat %>%
  inner_join(rs_ends) %>%
  group_by(season) %>%
  filter(start_time > rs_end_date) %>%
  mutate(team1 = pmin(away_team, home_team), team2 = pmax(away_team, home_team)) %>%
  mutate(game_winner = ifelse(away_team_score > home_team_score, away_team, home_team)) %>%
  mutate(team1_gamewin = game_winner == team1) 

po_dat_long <- dat %>%
  inner_join(rs_ends) %>%
  group_by(season) %>%
  filter(start_time > rs_end_date) %>% 
  mutate(away_team = word(away_team,-1)
         , home_team = word(home_team,-1)
         , series = paste0(pmin(home_team, away_team), '_',pmax(home_team, away_team))) %>%
  group_by(series, season) %>%
  arrange(series, start_time) %>%
  mutate(game_num = row_number()) %>%
  pivot_longer(cols = c(away_team, home_team), values_to = 'team') %>%
  mutate(win = if_else(name == 'away_team' & away_team_score > home_team_score |
                         name == 'home_team' & away_team_score < home_team_score , TRUE, FALSE)) %>%
  group_by(series, season, team) %>%
  mutate(nwins = sum(win)) %>%
  group_by(series, season) %>%
  mutate(series_length = max(game_num)
         , won_series = nwins > series_length/2)
  
po_dat_wide <- po_dat_long %>%
  select(season, series, team, game_num, win, series_length, won_series) %>%
  pivot_wider(names_from = game_num, values_from = win, names_prefix = "game_")

po_series <- po_dat %>%
  group_by(season, team1, team2) %>%
  summarise(ngames = n(), team1_nw = sum(team1_gamewin), 
            best_of = 2 * pmax(team1_nw, ngames - team1_nw) - 1, 
            team1_sw = team1_nw > best_of / 2)

po_dat2 <- po_dat %>%
  inner_join(po_series) %>%
  group_by(team1, team2) %>%
  mutate(gamenum = row_number())
  
#some exploratory analysis
po_dat_long %>%
  filter(series_length == 6, game_num == 1) %>%
  lm(data = ., won_series ~ win)

prepender <- function(string, prefix = "Game ") paste0(prefix, string)

plot_series_correlation <- function(data, n_games_series){
  data %>%
    filter(series_length == n_games_series, win) %>%
    ggplot(aes(x = win, fill = won_series, group = won_series)) +
    geom_bar(position = "fill") +
    facet_wrap(vars(game_num), nrow = 1, labeller = as_labeller(prepender)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    guides(fill = "none") +
    ylab('Proportion of series won') +
    scale_fill_manual(values = c('TRUE' = 'blue', 'FALSE' = 'NA'))
}


po_dat_long %>%
  plot_series_correlation(4)

po_dat_long %>%
  plot_series_correlation(5)

po_dat_long %>%
  plot_series_correlation(6)

po_dat_long %>%
  plot_series_correlation(7)

#basic linear probability model

po_dat_wide %>%
  filter(series_length == 4) %>%
  lm(data= ., won_series ~ game_1 + game_2 + game_3)

po_dat_wide %>%
  filter(series_length == 5) %>%
  lm(data= ., won_series ~ game_1 + game_2 + game_3 + game_4)

po_dat_wide %>%
  filter(series_length == 6) %>%
  lm(data= ., won_series ~ game_1 + game_2 + game_3 + game_4 + game_5)

po_dat_wide %>%
  filter(series_length == 7) %>%
  lm(data= ., won_series ~ game_1 + game_2 + game_3 + game_4 + game_5 + game_6)

