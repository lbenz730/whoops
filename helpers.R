library(ggplot2)
library(ggridges)
library(viridis)

### Get opponent game id
get_opp_id <- function(data, i) {
  return(data$game_id[data$team == data$opponent[i] & data$opponent == data$team[i] & data$month == data$month[i] & data$day == data$day[i]])
}

### get team's conference
get_conf <- function(team) {
  return(confs$conference[confs$team == team])
}

### Compute Haromonic Mean
harmonic_mean <- function(a,b) {
  return(1/(mean(1/c(a, b))))
}

### Compute Game Excitement Index
compute_GEI <- function(data) {
  adj <- abs(min(powranks$YUSAG_Coefficient))
  team_strength <- powranks$YUSAG_Coefficient[powranks$Team == data$team] + adj + 1
  opp_strength <- powranks$YUSAG_Coefficient[powranks$Team == data$opponent] + adj + 1
  score <- abs(data$predscorediff)
  team_conf_rank <- by_conf$Conference_Rank[by_conf$Team == data$team]
  opp_conf_rank <- by_conf$Conference_Rank[by_conf$Team == data$opponent]
  return(20 * harmonic_mean(team_strength, opp_strength)/(10 + abs(team_strength - opp_strength)))
}

### Get Games for a certain date
get_games <- function(today, D, M, Y) {
  if(today == "T"){
    date<- unclass(as.POSIXlt(Sys.time()))
    Y <- 1900 + date$year
    M <- 1 + date$mon
    D <- date$mday
  }
  return(filter(y, day == D, month == M, year == Y))
}


### Update History (Weekly Basis) ### BUGGY
write_history <- function(update) {
  history <- suppressWarnings(read.csv("2.0_Files/History/2017_18_history.csv", as.is = T))
  if(update) {
    date<- unclass(as.POSIXlt(Sys.time()))
    Y <- 1900 + date$year
    M <- 1 + date$mon
    D <- date$mday
    
    ### Get one week from current date
    if((D <= 25 & M != 11 & M != 2) | (D <= 24 & M == 11) | (D <= 21 & M == 2)) {
      days <- D:(D + 6)
      months <- rep(M, 7)
      years <- rep(Y, 7)
    }else if(D > 25 & is.element(M, c(1,3))) {
      days <- c(D:31, 1:(D - 25))
      months <- c(rep(M, (31 - D)), rep(M + 1, (D - 24)))
      years <- rep(Y, 7)
    }else if(D > 25 & M == 12) {
      days <- c(D:31, 1:(D - 25))
      months <- c(rep(M, (31 - D)), rep(1, (D - 24)))
      years <- c(rep(Y, (31 - D)), rep(Y + 1, (D - 24)))
    }else if(D > 21 & M == 2){
      days <- c(D:28, 1:(D - 22))
      months <- c(rep(M, (28 - D)), rep(M + 1, (D - 21)))
      years <- rep(Y, 7)
    }else{
      days <- c(D:30, 1:(D - 24))
      months <- c(rep(M, (30 - D)), rep(M + 1, (D - 24)))
      years <- rep(Y, 7)
    }
    
    dates <- paste(months, days, years, sep = "_")
    tmp <- y %>% select(year, month, day, team, opponent, 
                        location, teamscore, oppscore, scorediff, 
                        predscorediff, GEI) %>%
      mutate(date = paste(month, day, year, sep = "_")) %>%
      filter(is.element(date, dates))
    
    history <- rbind(history, tmp[,-12])
    
    ### Get past week of Results
    if(D > 7) {
      days <- (D - 7):(D - 1) 
      months <- rep(M, 7)
      years <- rep(Y, 7)
    }else if(D <= 7 & M != 12 & M != 1 & M != 3) {
      days <- c((24 + D):31, 1:(D - 1))
      months <- c(rep((M - 1), (8 - D)), rep(M, (D - 1)))
      years <- rep(Y, 7)
    }else if(D <= 7 & M == 1) {
      days <- c((24 + D):31, 1:(D - 1))
      months <- c(rep(12, (8 - D)), rep(1, (D - 1)))
      years <- c(rep((Y - 1), (8 - D)), rep(Y, (D - 1)))
    }else if(D <= 7 & M == 12) {
      days <- c((23 + D):30, 1:(D - 1))
      months <- c(rep(12, (8 - D)), rep(1, (D - 1)))
      years <- rep(Y, 7)
    }else{
      days <- c((21 + D):28, 1:(D - 1))
      months <- c(rep(12, (8 - D)), rep(1, (D - 1)))
      years <- rep(Y, 7)
    }
    
    dates <- paste(months, days, years, sep = "_")
    tmp <- y %>% select(year, month, day, team, opponent, 
                        location, teamscore, oppscore, scorediff, 
                        predscorediff, GEI) %>%
      mutate(date = paste(month, day, year, sep = "_")) %>%
      filter(is.element(date, dates))
    
    history <- mutate(history, "date" = paste(month, day, year, sep = "_")) 
    history[is.element(history$date, dates),] <- history %>% 
      filter(is.element(date, dates)) %>%
      mutate(teamscore = tmp$teamscore, oppscore = tmp$oppscore, scorediff = tmp$scorediff)
    
    write.table(history[,-12], "2.0_Files/History/2017_18_history.csv", row.names = F, col.names = T, sep = ",")
  }
  return(history)
}

### Joy Plots
ivy_joy <- function(simresults) {
  n <- nrow(simresults)
  sims <- data.frame(team = rep(NA, 8*n),
                     wins = rep(NA, 8*n))
  
  for(i in 1:8) {
    sims$team[(n*i - n + 1):(n*i)] <- names(simresults)[i]
    sims$wins[(n*i - n + 1):(n*i)] <- floor(simresults[,i])
  }
  
  sims %>% mutate(team = reorder(team, wins, mean)) %>%
    ggplot(aes(x = wins, y = team, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3) + theme_ridges() + 
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "Conference Wins", option = "C") +
    theme(legend.position="right") +
    labs(y = "Team", x= "Ivy League Wins", 
         title = "Distribution of Ivy League Men's Basketball Conference Wins",
         subtitle = paste("Based on", n, "Simulations of Ivy League Season \n@recspecs730/@YaleSportsGroup"))
  
}

yusag_plot <- function(data){
  data %>% mutate(group = reorder(Conference, YUSAG_Coefficient, median)) %>%
    ggplot(aes(x = YUSAG_Coefficient, y = group, fill = ..x..)) + 
    geom_density_ridges_gradient(scale = 3) + theme_ridges() +
    scale_y_discrete(expand = c(0.01, 0)) + 
    scale_x_continuous(expand = c(0.01, 0)) +
    theme(legend.position="none") +
    scale_fill_viridis(name = "group", option = "C") +
    labs(y = "Conference", title = "NCAA Women's Basketball Power Rankings")
}


#### Conference Sims
conf_sim <- function(conf, nsims) {
  conf_teams <- unique(filter(confs, conference == conf) %>% select(team))$team
  results <- data.frame("team" = conf_teams,
                        "shared_title" = rep(0, length(conf_teams)),
                        "sole_title" = rep(0, length(conf_teams)),
                        "avg_wins" = rep(0, length(conf_teams)),
                        "avg_losses" = rep(0, length(conf_teams)))
  
  ### Sim Schedule
  schedule <- filter(y, conf_game, team_conf == conf, location == "H") %>%
    mutate(simwins = 0, opp_simwins = 0)
  
  sim_season <- rep(0, nrow(results))
  max_wins <- nrow(schedule) * 2 / nrow(results)
  
  for(i in 1:nsims) {
    print(paste("Sim #", i))
    rands <- runif(nrow(schedule))
    schedule$simwins <- ifelse(rands <= schedule$wins, 1, 0)
    schedule$opp_simwins <- abs(1 - schedule$simwins)
    for(j in 1:nrow(results)) {
      sim_season[j] <- sum(schedule$simwins[schedule$team == conf_teams[j]]) +
        sum(schedule$opp_simwins[schedule$opponent == conf_teams[j]])
    }
    
    results$avg_wins <- results$avg_wins + sim_season/nsims
    results$avg_losses <- results$avg_losses + (max_wins - sim_season)/nsims
    winners <- grep(max(sim_season), sim_season)
    results$shared_title[winners] <- results$shared_title[winners] + 1/nsims
    if(length(winners) == 1) {
      results$sole_title[winners] <- results$sole_title[winners] + 1/nsims
    }
  }
  return(results)
}

####### Ivy League Graphics
playoff_graphic <- function() {
  background_colors <- arrange(playoffs, desc(round(playoff_prob,1)), desc(seed1_prob)) %>%
    pull(Team) %>%
    sapply(., function(x) { ncaa_colors$primary_color[ncaa_colors$ncaa_name == x] })
  text_colors <- arrange(playoffs, desc(round(playoff_prob,1)), desc(seed1_prob)) %>%
    pull(Team) %>%
    sapply(., function(x) { ncaa_colors$secondary_color[ncaa_colors$ncaa_name == x] })
  text_colors[c("Brown", "Dartmouth", "Cornell")] <- "#FFFFFF"
  text_colors[c("Brown", "Dartmouth", "Cornell", "Harvard")] <- "#FFFFFF"
  tmp <- text_colors["Penn"]
  text_colors["Penn"] <- "red"
  background_colors["Penn"] <- tmp
  
  
  
  
  mutate(playoffs, auto_bid = case_when(
    auto_bid > 0.1 ~ auto_bid,
    playoff_prob > 0 ~ 0.1,
    T ~ 0)
  ) %>%
    arrange(desc(auto_bid), desc(playoff_prob)) %>%
    rename("Team" = Team,
           "Auto Bid" = auto_bid,
           "Playoff Probability" = playoff_prob,
           "1st Seed" = seed1_prob,
           "2nd Seed" = seed2_prob,
           "3rd Seed" = seed3_prob,
           "4th Seed" = seed4_prob) %>%
    kable(., align = "ccccccc") %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    row_spec(1, color = text_colors[1], background = background_colors[1], bold = T) %>%
    row_spec(2, color = text_colors[2], background = background_colors[2], bold = T) %>%
    row_spec(3, color = text_colors[3], background = background_colors[3], bold = T) %>%
    row_spec(4, color = text_colors[4], background = background_colors[4], bold = T) %>%
    row_spec(5, color = text_colors[5], background = background_colors[5], bold = T) %>%
    row_spec(6, color = text_colors[6], background = background_colors[6], bold = T) %>%
    row_spec(7, color = text_colors[7], background = background_colors[7], bold = T) %>%
    row_spec(8, color = text_colors[8], background = background_colors[8], bold = T) %>%
    row_spec(0, bold = T, font_size = 16) %>%
    add_header_above(c("Ivy League Women's Basketball Playoff Odds" = 7), bold = T)
}

psf_graphic <- function() {
  background_colors <- arrange(playoffs, desc(playoff_prob), desc(auto_bid)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$primary_color[ncaa_colors$ncaa_name == x] })
  text_colors <- arrange(playoffs, desc(auto_bid), desc(playoff_prob), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$secondary_color[ncaa_colors$ncaa_name == x] })
  text_colors[c("Brown", "Dartmouth", "Cornell", "Harvard")] <- "#FFFFFF"
  
  arrange(psf_results, desc(psf)) %>%
    mutate(home = cell_spec(home, 
                            color = text_colors[home], 
                            background = background_colors[home],
                            bold = T),
           away = cell_spec(away, 
                            color = text_colors[away], 
                            background = background_colors[away],
                            bold = T)
    ) %>%
    mutate(psf = cell_spec(
      psf, color = "white", bold = T,
      background = spec_color(psf, end = 0.9, option = "C", direction = -1)
    )) %>%
    mutate(auto_bid_sf = cell_spec(
      auto_bid_sf, color = "white", bold = T,
      background = spec_color(auto_bid_sf, end = 0.9, option = "C", direction = -1)
    )) %>%
    select(-winner) %>%
    rename("Home" = home,
           "Away" = away,
           "Playoff Swing Factor" = psf,
           "Auto Bid Swing Factor" = auto_bid_sf) %>%
    kable(., escape = F, align = "ccccccc") %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    row_spec(0, bold = T, font_size = 16) %>%
    add_header_above(c("Ivy League Women's Basketball Playoff Importance" = 5), bold = T)
}