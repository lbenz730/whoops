pr_compute <- function() {
  avg <- mean(lm.hoops$coefficients[2:351])
  opp_avg <- mean(lm.hoops$coefficients[352:701])
  
  powerrankings <- data.frame(Team = rep(NA, length(teams)),
                              Conference = rep(NA, length(teams)),
                              YUSAG_Coefficient = rep(NA, length(teams)))
  powerrankings[1, ] <- c(teams[1], "Southland", lm.hoops$coefficients[1] - mean(avg, abs(opp_avg)))
  
  
  ### Get YUSAG Coefficients
  for(i in 2:(length(teams))) {
    teamcoef <- 
      lm.hoops$coefficients[paste("team", teams[i], sep = "")] - mean(avg, abs(opp_avg))
    opponentcoef <- 
      lm.hoops$coefficients[paste("opponent", teams[i], sep = "")] + mean(avg, abs(opp_avg))
    tmp <- c(teams[i], get_conf(teams[i]), round((teamcoef - opponentcoef)/2, 2))
    powerrankings[i, ] <- tmp
  }
  powerrankings$YUSAG_Coefficient <- round(as.numeric(powerrankings$YUSAG_Coefficient),2)
  
  ### Return w/out sorting by conference
  powerrankings <- powerrankings[order(powerrankings$YUSAG_Coefficient, decreasing = T), ]
  powerrankings$rank <- seq(1, 351, 1)
  write.table(powerrankings, "Powerrankings.csv", row.names = F, col.names = T, sep = ",")
  return(powerrankings)
  
}

