#############################  Read CSVs #######################################
library(dplyr)
source("helpers.R")
source("powerrankings.R")
source("Ivy_Sims.R")
source("tourney_sim.R")
confs <- read.csv("conferences.csv", as.is = T)
y <- read.csv("Results/NCAA_WHoops_Results_12_24_2018.csv", as.is = T)
########################  Data Cleaning ########################################
y <- y %>%
  mutate(scorediff = teamscore - oppscore, 
         season_id = "2018-19", game_id = NA, opp_game_id = NA, 
         team_conf = NA, opp_conf = NA, conf_game = NA, GEI = NA) %>%
  filter(D1 == 2)
teams <- unique(y$team)

### Game IDs
for(i in 1:length(teams)) {
  y[y$team == teams[i],] <- y %>%
    filter(team == teams[i]) %>%
    mutate(game_id = seq(1, sum(team == teams[i]), 1))
}

### Opp Game IDs
for(i in 1:nrow(y)) {
  y$opp_game_id[i] <- get_opp_id(y, i)[1]
}

### Confs
for(i in 1:length(teams)) {
  y$team_conf[y$team == teams[i]] <- get_conf(teams[i])
  y$opp_conf[y$opponent == teams[i]] <- get_conf(teams[i])
}
y$conf_game <- y$team_conf == y$opp_conf
y$reg_season <- (y$month < 3 | y$month >= 11) | (y$month == 3 & y$day <= 10)

################################# Set Weights ##################################
y$weights <- 0
for(i in 1:nrow(y)) {
  w_team <- 1 - (max(c(0, y$game_id[y$team == y$team[i] & !is.na(y$scorediff)])) - y$game_id[i])/
    max(c(0, y$game_id[y$team == y$team[i] & !is.na(y$scorediff)]))
  w_opponent <- 1 - (max(c(0, y$game_id[y$team == y$opponent[i] & !is.na(y$scorediff)])) - y$opp_game_id[i])/
    max(c(1, y$game_id[y$team == y$opponent[i] & !is.na(y$scorediff)]))
  rr <- mean(c(w_team, w_opponent))
  y$weights[i] <- 1/(1 + (0.5^(5 * rr)) * exp(-rr))
}   

############################### Create Model ###################################
lm.hoops <- lm(scorediff ~ team + opponent + location, weights = weights, data = y) 

######################## Point Spread to Win Percentage Model #################
y$predscorediff <- round(predict(lm.hoops, newdata = y), 1)
y$wins[y$scorediff > 0] <- 1
y$wins[y$scorediff < 0] <- 0
glm.pointspread <- glm(wins ~ predscorediff, data = y, family=binomial) 
summary(glm.pointspread)
y$wins[is.na(y$wins)] <- 
  round(predict(glm.pointspread, newdata = y[is.na(y$wins),], type = "response"), 3)

################################ Power Rankings ################################
powranks <- pr_compute()
yusag_plot(powranks)

################################ Ivy Sims ######################################
playoffs <- ivy.sim(nsims = 5000)
simresults <- fast.sim(nsims = 20000)
psf_results <- psf(nsims = 500, year = 2018, months = c(2,2), days = c(23,24))
write.csv(simresults, "Predictions/womens_simresults.csv", row.names = F)

### Win Plots
par(mfrow = c(2,4))
colors <- c("brown", "skyblue", "red", "forestgreen", "firebrick4", "maroon", "orange", "navy")
for(i in 1:8) {
  hist(simresults[,i], xlab = "Conference Wins", col = colors[i], main = names(simresults)[i],
       xlim = c(0, 14))
}
winmat <- apply(simresults, 1, sort, decreasing = T)

table(simresults$Princeton)/20000


ivy <- c("Princeton", "Penn", "Harvard", "Yale")
tourney_sim(ivy, seeds = 1:4, byes = 0, double_byes = 0, hca = NA, 1000)

vec <- c("Harvard", "Yale", "N")
z <- predict(lm.hoops, data.frame("team" = vec[1],"opponent" = vec[2],"location" = vec[3]))
predict(glm.pointspread, data.frame("predscorediff" = z), type = "response")
z
