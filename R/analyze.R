library(dplyr)
library(purrr)
library(readr)

datdir <- "~/code/nba_playoffs_analysis/data"
datfiles <- list.files(datdir)
setwd(datdir)

dat <- datfiles %>%
  map(read_csv) %>%
  bind_rows(.id = "season")

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
po_series <- po_dat %>%
  group_by(season, team1, team2) %>%
  summarise(ngames = n(), team1_nw = sum(team1_gamewin), 
            best_of = 2 * pmax(team1_nw, ngames - team1_nw) - 1, 
            team1_sw = team1_nw > best_of / 2)
po_dat2 <- po_dat %>%
  inner_join(po_series) %>%
  group_by(team1, team2) %>%
  mutate(gamenum = row_number())
  