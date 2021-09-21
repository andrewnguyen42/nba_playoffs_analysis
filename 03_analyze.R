library(tidyr)
library(ggplot2)
library(gamlss)
library(dplyr)
  
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

#basic linear probability

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



game7_series <- po_dat_wide %>%
  filter(series_length == 7)

game_2_model <- gamlss::gamlss(won_series ~ game_1 + game_2 
               , data = game7_series
               , link = NB())
mu_coefs <- game_2_model$mu.coefficients
game_2_model$sigma.coefficients

predict(game_2_model, newdata = tribble( ~game_1, ~game_2,  TRUE, TRUE))

fitted_distributions <- tribble(~id, ~fitted 
                         , 'WW', mu_coefs[1] +  mu_coefs[2] +  mu_coefs[3]
                         , 'WL', mu_coefs[1] +  mu_coefs[2]
                         , 'LW', mu_coefs[1] + mu_coefs[3]
                         , 'LL', mu_coefs[1] ) %>%
  mutate(sigma = exp(game_2_model$sigma.coefficients)
         , alpha = fitted/sigma
         , beta = (1-fitted)/sigma
         , posterior_probability = pbinom(q = 3, size = 7, prob = fitted, lower.tail = FALSE) ) 

#how prior distribution of win probability changes
fitted_distributions %>%
  crossing(x = seq(.01, .99, length.out = 100)) %>%
  mutate(prior_density = dbeta(x, alpha, beta)) %>%
  ggplot(aes(x = x, y = prior_density, colour = id)) +
  geom_line()

