library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)

#install fivethirtyeightdata via github
#install.packages('fivethirtyeightdata', repos = 'https://fivethirtyeightdata.github.io/drat/', type = 'source')

team_mapping <- tribble(~team_short, ~team_long
                        ,'WAS', 'WASHINGTON WIZARDS'
                        ,'BOS', 'BOSTON CELTICS'
                        ,'CHO', 'CHARLOTTE HORNETS'
                        ,'CHI', 'CHICAGO BULLS'
                        ,'DEN', 'DENVER NUGGETS'
                        ,'GSW', 'GOLDEN STATE WARRIORS'
                        ,'LAL', 'LOS ANGELES LAKERS'
                        ,'NJN', 'NEW JERSEY NETS'
                        ,'ORL', 'ORLANDO MAGIC'
                        ,'PHO', 'PHOENIX SUNS'
                        ,'SAC', 'SACRAMENTO KINGS'
                        ,'SAS', 'SAN ANTONIO SPURS'
                        ,'SEA', 'SEATTLE SUPERSONICS'
                        ,'TOR', 'TORONTO RAPTORS'
                        ,'DET', 'DETROIT PISTONS'
                        ,'LAC', 'LOS ANGELES CLIPPERS'
                        ,'IND', 'INDIANA PACERS'
                        ,'HOU', 'HOUSTON ROCKETS'
                        ,'CLE', 'CLEVELAND CAVALIERS'
                        ,'UTA', 'UTAH JAZZ'
                        ,'NYK', 'NEW YORK KNICKS'
                        ,'MIN', 'MINNESOTA TIMBERWOLVES'
                        ,'MIL', 'MILWAUKEE BUCKS'
                        ,'MIA', 'MIAMI HEAT'
                        ,'POR', 'PORTLAND TRAIL BLAZERS'
                        ,'DAL', 'DALLAS MAVERICKS'
                        ,'ATL', 'ATLANTA HAWKS'
                        ,'PHI', 'PHILADELPHIA 76ERS'
                        ,'MEM', 'MEMPHIS GRIZZLIES'
                        ,'NOP', 'NEW ORLEANS PELICANS'
                        ,'CHO', 'CHARLOTTE BOBCATS'
                        ,'NOP', 'NEW ORLEANS HORNETS'
                        ,'OKC', 'OKLAHOMA CITY THUNDER'
                        ,'BRK', 'BROOKLYN NETS')

datdir <- "data"
datfiles <- list.files(datdir, '*.csv')

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

elo_long <- fivethirtyeightdata::nba_carmelo %>%
  select(date, season, elo1_pre, elo2_pre, team1, team2, playoff) %>% 
  filter(year(date) >= 1998) %>% #playoff game data goes up to 1998
  pivot_longer(c(team1, team2), values_to = 'team_short') %>% 
  mutate(elo_pre = if_else(name == 'team1', elo1_pre, elo2_pre), .keep = 'unused') 

po_dat <- dat %>%
  inner_join(rs_ends) %>%
  group_by(season) %>%
  mutate(start_time = start_time - hours(4)) %>% #convert GMT to EDT
  filter(start_time > rs_end_date) %>%
  mutate(team1 = pmin(away_team, home_team), team2 = pmax(away_team, home_team)) %>%
  mutate(game_winner = ifelse(away_team_score > home_team_score, away_team, home_team)) %>%
  mutate(team1_gamewin = game_winner == team1) 

po_dat_long <- dat %>%
  inner_join(rs_ends) %>%
  group_by(season) %>%
  mutate(start_time = start_time - hours(4) #convert GMT to EDT
         , date = as.Date(start_time)) %>% 
  filter(start_time > rs_end_date) %>%
  inner_join(team_mapping, by = c('away_team' = 'team_long'), suffix = c('', '.away')) %>%
  inner_join(team_mapping, by = c('home_team' = 'team_long'), suffix = c('.home', '.away')) %>%
  mutate(team_short.home = ifelse(team_short.home == "CHARLOTTE HORNETS" & season < 2016, "CHH", team_short.home)
         , team_short.away = ifelse(team_short.away == "CHARLOTTE HORNETS" & season < 2016, "CHH", team_short.away)) %>%
  inner_join(elo_long , by = c('team_short.home' = 'team_short', 'season', 'date')) %>%
  inner_join(elo_long , by = c('team_short.away' = 'team_short', 'season', 'date'), suffix = c('.away', '.home'))  %>%
  mutate(series = paste0(pmin(team_short.home, team_short.away), '_', pmax(team_short.home, team_short.away))) %>%
  group_by(series, season) %>%
  arrange(series, start_time) %>%
  mutate(game_num = row_number()) %>%
  select(-rs_end_date, -playoff.away, -playoff.home, -team_short.home, -team_short.away) %>%
  pivot_longer(cols = c(away_team, home_team), values_to = 'team') %>%
  mutate(win = if_else(name == 'away_team' & away_team_score > home_team_score |
                         name == 'home_team' & away_team_score < home_team_score , TRUE, FALSE)
         , team_elo = if_else(name == 'away_team', elo_pre.away, elo_pre.home)
         , opponent_elo = if_else(name == 'away_team', elo_pre.home, elo_pre.away)) %>%
  select(-elo_pre.away, -elo_pre.home) %>%
  group_by(series, season, team) %>%
  mutate(nwins = sum(win)) %>%
  group_by(series, season) %>%
  mutate(series_length = max(game_num)
         , won_series = nwins > series_length/2) %>%
  group_by(series, season) %>%
  filter(!any(won_series & nwins == 3)) %>%
  mutate(p_win = 1/(10^(-(team_elo-opponent_elo)/400) + 1),
         elo_diff = team_elo - opponent_elo) 

po_dat_long_high_seed_only <- po_dat_long %>%
  group_by(series, season) %>% arrange(series, season, game_num, rev(name)) %>% 
  filter(team == first(team)) %>%
  mutate(is_home_team = name=="home_team")

po_dat_wide <- po_dat_long %>%
  select(season, name, series, team, game_num, win, series_length, won_series, p_win, elo_diff) %>%
  pivot_wider(names_from = game_num, values_from = c(name, win, p_win, elo_diff), names_prefix = "game_") %>%
  group_by(series, season) %>%
  filter(name_game_1 == "home_team") %>%
  select(-starts_with("name_"))



saveRDS(po_dat_long, "data/po_dat_long.rds")
saveRDS(po_dat_long_high_seed_only, "data/po_dat_long_high_seed_only.rds")
saveRDS(po_dat_wide, "data/po_dat_wide.rds")

