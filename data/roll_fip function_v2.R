roll_fip <- function (x, r=NULL, rl_excl="N") {
  if(rl_excl == "Y"){  #Determines if the rolling sum should be broken out by batter stance
      x <- x %>%       #Removing stand from dataset
        #  select(-stand) %>%
        group_by(game_date, pitcher,p_throws, home_team,league, away_team, p_team, p_league, inning_topbot, game_year) %>%
        summarise(BF = sum(BF)
                  ,uBB = sum(uBB)
                  ,HBP = sum(HBP)
                  ,X1B = sum(X1B)
                  ,X2B = sum(X2B)
                  ,X3B = sum(X3B)
                  ,HR = sum(HR)
                  ,SH = sum(SH)
                  ,SO = sum(SO)
                  ,FB = sum(FB)
                  ,FB_HR = sum(FB_HR)
                  ,AB = sum(AB)
                  ,PF = mean(PF)
                  ,IP = mean(IP)) %>%
        arrange(pitcher, game_date)
      
      
    
     if(missing(r)){    #Determining how long the rolling sum is. If null then sums season
        df <- x %>% group_by(pitcher, game_year) %>%
          mutate(BF = cumsum(BF)
                 ,uBB = cumsum(uBB)
                 ,HBP = cumsum(HBP)
                 ,X1B = cumsum(X1B)
                 ,X2B = cumsum(X2B)
                 ,X3B = cumsum(X3B)
                 ,HR = cumsum(HR)
                 ,SH = cumsum(SH)
                 ,SO = cumsum(SO)
                 ,FB = cumsum(FB)
                 ,FB_HR = cumsum(FB_HR)
                 ,AB = cumsum(AB)
                 ,PF = cummean(PF)
                 ,IP = cumsum(IP)) %>%
          select(game_date, pitcher,p_throws, home_team,league, away_team, p_team, p_league, inning_topbot, BF, uBB, HBP,
                 X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB, PF, IP, game_year) %>%
          mutate(HR_FB_rate = FB_HR/FB)
      }
      
      else { #Command for is parameter r is not NULL
        rolling_sum <- function(d){
          rollapplyr(d,r,sum,partial=TRUE)
        }
        
        rolling_avg <- function(d){
          rollapplyr(d,r,mean,partial=TRUE)
        }
      
        df <- x %>% group_by(pitcher, game_year) %>%
          mutate(BF = rolling_sum(BF)
                 ,uBB = rolling_sum(uBB)
                 ,HBP = rolling_sum(HBP)
                 ,X1B = rolling_sum(X1B)
                 ,X2B = rolling_sum(X2B)
                 ,X3B = rolling_sum(X3B)
                 ,HR = rolling_sum(HR)
                 ,SH = rolling_sum(SH)
                 ,SO = rolling_sum(SO)
                 ,FB = rolling_sum(FB)
                 ,FB_HR = rolling_sum(FB_HR)
                 ,AB = rolling_sum(AB)
                 ,PF = rolling_avg(PF)
                 ,IP = rolling_sum(IP)) %>%
         select(game_date, pitcher, p_throws, home_team,league, away_team, p_team, p_league, inning_topbot, BF, uBB, HBP,
                 X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB,PF, IP, game_year) %>%
          mutate(HR_FB_rate = FB_HR/FB)
      }
  }  #End of command for if rl_excl == "Y"
  
  
  else { #beginning of formatting data if batter stance remains: rl_excl == "N"
    x <- x %>% arrange(pitcher, stand, game_date)
    
    if(missing(r)){
      df <- x %>% group_by(pitcher,game_year, stand) %>%
        mutate(BF = cumsum(BF)
               ,uBB = cumsum(uBB)
               ,HBP = cumsum(HBP)
               ,X1B = cumsum(X1B)
               ,X2B = cumsum(X2B)
               ,X3B = cumsum(X3B)
               ,HR = cumsum(HR)
               ,SH = cumsum(SH)
               ,SO = cumsum(SO)
               ,FB = cumsum(FB)
               ,FB_HR = cumsum(FB_HR)
               ,AB = cumsum(AB)
               ,PF = cummean(PF)
               ,IP = cumsum(IP)) %>%
        select(game_date, pitcher,  p_throws, home_team,league, away_team, p_team, p_league, inning_topbot, stand, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB,PF, IP, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    } else {
      rolling_sum <- function(d){
        rollapplyr(d,r,sum,partial=TRUE)
      }
      
      rolling_avg <- function(d){
        rollapplyr(d,r,mean,partial=TRUE)
      }
      
      df <- x %>% group_by(pitcher,game_year, stand) %>%
        mutate(BF = rolling_sum(BF)
               ,uBB = rolling_sum(uBB)
               ,HBP = rolling_sum(HBP)
               ,X1B = rolling_sum(X1B)
               ,X2B = rolling_sum(X2B)
               ,X3B = rolling_sum(X3B)
               ,HR = rolling_sum(HR)
               ,SH = rolling_sum(SH)
               ,SO = rolling_sum(SO)
               ,FB = rolling_sum(FB)
               ,FB_HR = rolling_sum(FB_HR)
               ,AB = rolling_sum(AB)
               ,PF = rolling_avg(PF)
               ,IP = rolling_sum(IP)) %>%
        select(game_date, pitcher, p_throws, home_team,league, away_team, p_team, p_league, inning_topbot, stand, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB, PF, IP, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
      }
    
  }
  
  df$game_year <- as.character(df$game_year)
  
  
  if (!exists("guts_table")) {
    
    guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
    guts_table <- guts_table %>% html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
      html_table(fill = TRUE)
    guts_table <- as.data.frame(guts_table)[-(1:2), (1:14)]
    names(guts_table) <- c("game_year", "lg_woba", "woba_scale", 
                           "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", 
                           "runCS", "lg_r_pa", "lg_r_w", "cFIP")
    for (i in c(2:ncol(guts_table))) {
      guts_table[, i] <- as.numeric(as.character(guts_table[, 
                                                            i]))
    }
    assign("guts_table", guts_table, envir = .GlobalEnv)
  }
  df_join <- left_join(df, guts_table, by = "game_year")
  df_join$FIP <- round(((((13 * df_join$HR) + (3 * (df_join$uBB + 
                                                      df_join$HBP)) - (2 * df_join$SO))/df_join$IP) + df_join$cFIP), 
                       2)
  df_join$wOBA_against <- round((((df_join$wBB * df_join$uBB) + 
                                    (df_join$wHBP * df_join$HBP) + (df_join$w1B * df_join$X1B) + 
                                    (df_join$w2B * df_join$X2B) + (df_join$w3B * df_join$X3B) + 
                                    (df_join$wHR * df_join$HR))/(df_join$BF)), 3)
  df_join$wOBA_CON_against <- round((((df_join$w1B * df_join$X1B) + 
                                        (df_join$w2B * df_join$X2B) + (df_join$w3B * df_join$X3B) + 
                                        (df_join$wHR * df_join$HR))/(df_join$AB - df_join$SO)), 
                                    3)
  df_join <- arrange_(df_join, ~desc(wOBA_against))
  x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", 
                             "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", 
                             "lg_r_pa", "lg_r_w", "cFIP")
  df_join <- df_join[!x]
  df_join
}

saveRDS(roll_fip, "data/roll_fip.rds")


roll_woba <- function (x, r=NULL, rl_excl="N") {
  if(rl_excl == "Y"){  #Determines if the rolling sum should be broken out by pitcher handedness
    x <- x %>%       #Removing p_throws from dataset
      #  select(-stand) %>%
      group_by(game_date, batter, game_year) %>%
      summarise(BF = sum(BF)
                ,uBB = sum(uBB)
                ,HBP = sum(HBP)
                ,X1B = sum(X1B)
                ,X2B = sum(X2B)
                ,X3B = sum(X3B)
                ,HR = sum(HR)
                ,SH = sum(SH)
                ,SO = sum(SO)
                ,FB = sum(FB)
                ,FB_HR = sum(FB_HR)
                ,AB = sum(AB)
                ,PF = mean(PF)) %>%
      arrange(batter, game_date)
    
    
    
    if(missing(r)){    #Determining how long the rolling sum is. If null then sums season
      df <- x %>% group_by(batter, game_year) %>%
        mutate(BF = cumsum(BF)
               ,uBB = cumsum(uBB)
               ,HBP = cumsum(HBP)
               ,X1B = cumsum(X1B)
               ,X2B = cumsum(X2B)
               ,X3B = cumsum(X3B)
               ,HR = cumsum(HR)
               ,SH = cumsum(SH)
               ,SO = cumsum(SO)
               ,FB = cumsum(FB)
               ,FB_HR = cumsum(FB_HR)
               ,AB = cumsum(AB)
               ,PF = cummean(PF)) %>%
        select(game_date, batter, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB, PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    }
    
    else { #Command for is parameter r is not NULL
      rolling_sum <- function(d){
        rollapplyr(d,r,sum,partial=TRUE)
      }
      
      rolling_avg <- function(d){
        rollapplyr(d,r,mean,partial=TRUE)
      }
      
      df <- x %>% group_by(batter, game_year) %>%
        mutate(BF = rolling_sum(BF)
               ,uBB = rolling_sum(uBB)
               ,HBP = rolling_sum(HBP)
               ,X1B = rolling_sum(X1B)
               ,X2B = rolling_sum(X2B)
               ,X3B = rolling_sum(X3B)
               ,HR = rolling_sum(HR)
               ,SH = rolling_sum(SH)
               ,SO = rolling_sum(SO)
               ,FB = rolling_sum(FB)
               ,FB_HR = rolling_sum(FB_HR)
               ,AB = rolling_sum(AB)
               ,PF = rolling_avg(PF)) %>%
        select(game_date,batter, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB,PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    }
  }  #End of command for if rl_excl == "Y"
  
  
  else { #beginning of formatting data if batter stance remains: rl_excl == "N"
    x <- x %>% arrange(batter, p_throws, game_date)
    
    if(missing(r)){
      df <- x %>% group_by(batter,game_year, p_throws) %>%
        mutate(BF = cumsum(BF)
               ,uBB = cumsum(uBB)
               ,HBP = cumsum(HBP)
               ,X1B = cumsum(X1B)
               ,X2B = cumsum(X2B)
               ,X3B = cumsum(X3B)
               ,HR = cumsum(HR)
               ,SH = cumsum(SH)
               ,SO = cumsum(SO)
               ,FB = cumsum(FB)
               ,FB_HR = cumsum(FB_HR)
               ,AB = cumsum(AB)
               ,PF = cummean(PF)) %>%
        select(game_date, batter,  p_throws, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB,PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    } else {
      rolling_sum <- function(d){
        rollapplyr(d,r,sum,partial=TRUE)
      }
      
      rolling_avg <- function(d){
        rollapplyr(d,r,mean,partial=TRUE)
      }
      
      df <- x %>% group_by(batter, game_year, p_throws) %>%
        mutate(BF = rolling_sum(BF)
               ,uBB = rolling_sum(uBB)
               ,HBP = rolling_sum(HBP)
               ,X1B = rolling_sum(X1B)
               ,X2B = rolling_sum(X2B)
               ,X3B = rolling_sum(X3B)
               ,HR = rolling_sum(HR)
               ,SH = rolling_sum(SH)
               ,SO = rolling_sum(SO)
               ,FB = rolling_sum(FB)
               ,FB_HR = rolling_sum(FB_HR)
               ,AB = rolling_sum(AB)
               ,PF = rolling_avg(PF)) %>%
        select(game_date,batter, p_throws, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB, PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    }
    
  }
  
  df$game_year <- as.character(df$game_year)
  
  
  if (!exists("guts_table")) {
    
    guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
    guts_table <- guts_table %>% html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
      html_table(fill = TRUE)
    guts_table <- as.data.frame(guts_table)[-(1:2), (1:14)]
    names(guts_table) <- c("game_year", "lg_woba", "woba_scale", 
                           "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", 
                           "runCS", "lg_r_pa", "lg_r_w", "cFIP")
    for (i in c(2:ncol(guts_table))) {
      guts_table[, i] <- as.numeric(as.character(guts_table[, 
                                                            i]))
    }
    assign("guts_table", guts_table, envir = .GlobalEnv)
  }
  df_join <- left_join(df, guts_table, by = "game_year")
  df_join$wOBA <- round((((df_join$wBB * df_join$uBB) + (df_join$wHBP * 
                                                           df_join$HBP) + (df_join$w1B * df_join$X1B) + (df_join$w2B * 
                                                                                                           df_join$X2B) + (df_join$w3B * df_join$X3B) + (df_join$wHR * 
                                                                                                                                                           df_join$HR))/(df_join$AB + df_join$uBB + df_join$HBP + 
                                                                                                                                                                           df_join$SH)), 3)
  df_join$wOBA_CON <- round((((df_join$w1B * df_join$X1B) + 
                                (df_join$w2B * df_join$X2B) + (df_join$w3B * df_join$X3B) + 
                                (df_join$wHR * df_join$HR))/(df_join$AB - df_join$SO)), 
                            3)
  df_join <- df_join %>% dplyr::arrange(dplyr::desc(.data$wOBA))
  x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", 
                             "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", 
                             "lg_r_pa", "lg_r_w", "cFIP")
  df_join <- df_join[!x]
  return(df_join)
}

saveRDS(roll_woba, "data/roll_woba.rds")

roll_lg_woba <- function (x, r=NULL, rl_excl="N") {
  if(rl_excl == "Y"){  #Determines if the rolling sum should be broken out by pitcher handedness
    x <- x %>%       #Removing p_throws from dataset
      #  select(-stand) %>%
      group_by(game_date, game_year) %>%
      summarise(BF = sum(BF)
                ,uBB = sum(uBB)
                ,HBP = sum(HBP)
                ,X1B = sum(X1B)
                ,X2B = sum(X2B)
                ,X3B = sum(X3B)
                ,HR = sum(HR)
                ,SH = sum(SH)
                ,SO = sum(SO)
                ,FB = sum(FB)
                ,FB_HR = sum(FB_HR)
                ,AB = sum(AB)
                ,PF = mean(PF)) %>%
      arrange(game_date)
    
    
    
    if(missing(r)){    #Determining how long the rolling sum is. If null then sums season
      df <- x %>% group_by(game_year) %>%
        mutate(BF = cumsum(BF)
               ,uBB = cumsum(uBB)
               ,HBP = cumsum(HBP)
               ,X1B = cumsum(X1B)
               ,X2B = cumsum(X2B)
               ,X3B = cumsum(X3B)
               ,HR = cumsum(HR)
               ,SH = cumsum(SH)
               ,SO = cumsum(SO)
               ,FB = cumsum(FB)
               ,FB_HR = cumsum(FB_HR)
               ,AB = cumsum(AB)
               ,PF = cummean(PF)) %>%
        select(game_date, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB, PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    }
    
    else { #Command for is parameter r is not NULL
      rolling_sum <- function(d){
        rollapplyr(d,r,sum,partial=TRUE)
      }
      
      rolling_avg <- function(d){
        rollapplyr(d,r,mean,partial=TRUE)
      }
      
      df <- x %>% group_by(game_year) %>%
        mutate(BF = rolling_sum(BF)
               ,uBB = rolling_sum(uBB)
               ,HBP = rolling_sum(HBP)
               ,X1B = rolling_sum(X1B)
               ,X2B = rolling_sum(X2B)
               ,X3B = rolling_sum(X3B)
               ,HR = rolling_sum(HR)
               ,SH = rolling_sum(SH)
               ,SO = rolling_sum(SO)
               ,FB = rolling_sum(FB)
               ,FB_HR = rolling_sum(FB_HR)
               ,AB = rolling_sum(AB)
               ,PF = rolling_avg(PF)) %>%
        select(game_date, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB,PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    }
  }  #End of command for if rl_excl == "Y"
  
  
  else { #beginning of formatting data if batter stance remains: rl_excl == "N"
    x <- x %>% arrange(p_throws, game_date)
    
    if(missing(r)){
      df <- x %>% group_by(game_year, p_throws) %>%
        mutate(BF = cumsum(BF)
               ,uBB = cumsum(uBB)
               ,HBP = cumsum(HBP)
               ,X1B = cumsum(X1B)
               ,X2B = cumsum(X2B)
               ,X3B = cumsum(X3B)
               ,HR = cumsum(HR)
               ,SH = cumsum(SH)
               ,SO = cumsum(SO)
               ,FB = cumsum(FB)
               ,FB_HR = cumsum(FB_HR)
               ,AB = cumsum(AB)
               ,PF = cummean(PF)) %>%
        select(game_date, p_throws, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB,PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    } else {
      rolling_sum <- function(d){
        rollapplyr(d,r,sum,partial=TRUE)
      }
      
      rolling_avg <- function(d){
        rollapplyr(d,r,mean,partial=TRUE)
      }
      
      df <- x %>% group_by(game_year, p_throws) %>%
        mutate(BF = rolling_sum(BF)
               ,uBB = rolling_sum(uBB)
               ,HBP = rolling_sum(HBP)
               ,X1B = rolling_sum(X1B)
               ,X2B = rolling_sum(X2B)
               ,X3B = rolling_sum(X3B)
               ,HR = rolling_sum(HR)
               ,SH = rolling_sum(SH)
               ,SO = rolling_sum(SO)
               ,FB = rolling_sum(FB)
               ,FB_HR = rolling_sum(FB_HR)
               ,AB = rolling_sum(AB)
               ,PF = rolling_avg(PF)) %>%
        select(game_date,p_throws, BF, uBB, HBP,
               X1B, X2B, X3B, HR, SH, SO, FB, FB_HR, AB, PF, game_year) %>%
        mutate(HR_FB_rate = FB_HR/FB)
    }
    
  }
  
  df$game_year <- as.character(df$game_year)
  
  
  if (!exists("guts_table")) {
    
    guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
    guts_table <- guts_table %>% html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
      html_table(fill = TRUE)
    guts_table <- as.data.frame(guts_table)[-(1:2), (1:14)]
    names(guts_table) <- c("game_year", "lg_woba", "woba_scale", 
                           "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", 
                           "runCS", "lg_r_pa", "lg_r_w", "cFIP")
    for (i in c(2:ncol(guts_table))) {
      guts_table[, i] <- as.numeric(as.character(guts_table[, 
                                                            i]))
    }
    assign("guts_table", guts_table, envir = .GlobalEnv)
  }
  df_join <- left_join(df, guts_table, by = "game_year")
  df_join$wOBA <- round((((df_join$wBB * df_join$uBB) + (df_join$wHBP * 
                                                           df_join$HBP) + (df_join$w1B * df_join$X1B) + (df_join$w2B * 
                                                                                                           df_join$X2B) + (df_join$w3B * df_join$X3B) + (df_join$wHR * 
                                                                                                                                                           df_join$HR))/(df_join$AB + df_join$uBB + df_join$HBP + 
                                                                                                                                                                           df_join$SH)), 3)
  df_join$wOBA_CON <- round((((df_join$w1B * df_join$X1B) + 
                                (df_join$w2B * df_join$X2B) + (df_join$w3B * df_join$X3B) + 
                                (df_join$wHR * df_join$HR))/(df_join$AB - df_join$SO)), 
                            3)
  df_join <- df_join %>% dplyr::arrange(dplyr::desc(.data$wOBA))
  x <- names(df_join) %in% c("wBB", 
                             "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", 
                             "lg_r_pa", "lg_r_w", "cFIP")
  df_join <- df_join[!x]
  return(df_join)
}

saveRDS(roll_lg_woba, "data/roll_lg_woba.rds")
