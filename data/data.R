###################################
# Sean Trainor                    #
# http://github.com/seanjtrainor  #
###################################

#READ IN NEEDED PACKAGES

usePackage <- function(p) {
  if(!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


listPackages <- c("baseballr", "dplyr", "RcppRoll", "zoo", "lubridate", "tidyverse",
                  "tree", "caret", "randomForest", "rvest", "ggalt", "remotes", "gbm", 
                  "glmnet", "shiny", "shinythemes", "tibble", "shinydashboard")

sapply(listPackages, usePackage)

readRDS("data/scrape_statcast_savant_v2.rds")
readRDS("data/season_pbp.rds")
readRDS("data/roll_fip.rds")
readRDS("data/roll_woba.rds")
readRDS("data/roll_lg_woba.rds")

################################
# Input data needed for model
################################

pbp <- rbind(season_pbp(2022),season_pbp(2021), season_pbp(2020), season_pbp(2019), season_pbp(2018))

#Set variables for different out situations to create IP field

two_out_ie_events <- c('caught_stealing_2b', 'caught_stealing_3b', 'field_out', 'fielders_choice', 'fielders_choice_out',
                       'force_out', 'strikeout', 'other_out')

single_out_events <- c('caught_stealing_2b', 'caught_stealing_3b', 'field_out', 'fielders_choice', 'fielders_choice_out',
                       'force_out', 'strikeout', 'sac_bunt', 'sac_fly')

double_out_events <- c('double_play', 'grounded_into_double_play', 'strikeout_double_play')

three_out_events <- c('triple_play')

#Bring in Chadwick to get different playerids (baseball savant, bref, fangraphs, etc)

chadwick_player_lu_table <- subset(chadwick_player_lu(), !is.na(key_mlbam)) %>%
  dplyr::select(key_mlbam,key_fangraphs, name_last, name_first) %>%
  dplyr::mutate(hitter_name = paste(name_first, name_last))

#bring in sprint speeds

sprint.speed18 <- read.csv("~/data/sprint_speed_18.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2018")

sprint.speed19 <- read.csv("~/data/sprint_speed_19.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2019")

sprint.speed20 <- read.csv("~/data/sprint_speed_20.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2020")

sprint.speed21 <- read.csv("~/data/sprint_speed_21.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2021")

sprint.speed22 <- read.csv("~/data/sprint_speed_22.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2022")

sprint.speed <- rbind(sprint.speed18, sprint.speed19, sprint.speed20, sprint.speed21, sprint.speed22) #use to train model

sprint_max <- sprint.speed %>%                    #use as input for prediction
  group_by(player_id) %>%
  filter(game_year == max(game_year)) %>%
  select(-c(game_year))%>%
  left_join(chadwick_player_lu_table, by = c("player_id" = "key_mlbam")) %>%
  select(-c(key_fangraphs, name_last, name_first)) %>%
  rename(player_name = hitter_name)

rm(sprint.speed18, sprint.speed19, sprint.speed20, sprint.speed21, sprint.speed22)

#Create innings pitched field

innings <- pbp %>% group_by(game_date,game_year,pitcher,p_throws, home_team,away_team,p_team, p_league, inning_topbot) %>%
  arrange(game_date,pitcher, at_bat_number, pitch_number) %>%
  summarise(innings = last(inning),
            last_events = last(events),
            outs = last(outs_when_up))%>%
  mutate(IP = ifelse(outs==2 && last_events %in% two_out_ie_events, as.numeric(innings), 
                     ifelse(outs==1 && last_events %in% double_out_events, as.numeric(innings),
                            ifelse(outs==0 && last_events == 'triple_play', as.numeric(innings),
                                   ifelse(last_events %in% single_out_events,(as.numeric(innings)-1)+((as.numeric(outs)+1)/3),
                                          ifelse(last_events %in% double_out_events,(as.numeric(innings)-1)+((as.numeric(outs)+2)/3),
                                                 (as.numeric(innings)-1)+((as.numeric(outs)/3)))))))) %>%
  select(game_date,game_year, pitcher, p_throws, home_team, away_team, p_team, p_league, inning_topbot, IP)


###################################################
#Pitch summary
################################################

pitch_sum <- pbp %>% group_by(game_date,game_year, pitcher, p_throws, home_team,league, away_team, p_team, p_league, inning_topbot,stand) %>%
  summarise(BF = n_distinct(at_bat_number),
            uBB = sum(events == "walk", na.rm = TRUE),
            HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
            X1B = sum(events == "single", na.rm = TRUE),
            X2B = sum(events == "double", na.rm = TRUE),
            X3B = sum(events == "triple", na.rm = TRUE),
            HR = sum(events == "home_run", na.rm = TRUE),
            SH = sum(substr(events,1,3) == "sac", na.rm = TRUE),
            SO = sum(substr(events,1,9) == "strikeout", na.rm = TRUE),
            FB = sum(bb_type == "fly_ball", na.rm = TRUE),
            GB = sum(bb_type == "ground_ball", na.rm = TRUE),
            FB_HR = sum(events == "home_run" & bb_type == "fly_ball", na.rm = TRUE),
            PF = mean(PF)) %>%
  mutate(AB = BF - (uBB + HBP + SH)
         #     ,HR_FB_rate = FB_HR/FB
  ) %>%
  left_join(innings, by = c("game_date","game_year", "pitcher", "p_throws", "home_team", "away_team", "p_team", "p_league", "inning_topbot")) %>%
  arrange(pitcher, stand, game_date)

pitch_cum_r <- roll_fip(pitch_sum) %>%
  filter(stand == "R") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
  mutate(cum_p_k_rate_r = SO/AB,
         cum_p_bb_rate_r = uBB/AB) %>%
  rename(FIP_cum_r = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))

pitch_cum_l <- roll_fip(pitch_sum) %>%
  filter(stand == "L") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
  mutate(cum_p_k_rate_l = SO/AB,
         cum_p_bb_rate_l = uBB/AB) %>%
  rename(FIP_cum_l = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))


pitch_cum_tot <- roll_fip(pitch_sum, rl_excl = "Y") %>%
  arrange(pitcher, game_date) %>%
  select("game_date", "game_year", "pitcher", "AB", "uBB", "SO", "FIP") %>%
  mutate(cum_p_k_rate = SO/AB,
         cum_p_bb_rate = uBB/AB) %>%
  rename(FIP_cum_tot = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO"))

pitch_rol_r <- roll_fip(pitch_sum, 5) %>%
  filter(stand == "R") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date", "game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
  mutate(rol_p_k_rate_r = SO/AB,
         rol_p_bb_rate_r = uBB/AB) %>%
  rename(FIP_rol_r = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))

pitch_rol_l <- roll_fip(pitch_sum, 5) %>%
  filter(stand == "L") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
  mutate(rol_p_k_rate_l = SO/AB,
         rol_p_bb_rate_l = uBB/AB) %>%
  rename(FIP_rol_l = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))


pitch_rol_tot <- roll_fip(pitch_sum, 5, rl_excl = "Y") %>%
  arrange(pitcher, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP") %>%
  mutate(rol_p_k_rate = SO/AB,
         rol_p_bb_rate = uBB/AB) %>%
  rename(FIP_rol_tot = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO"))

#pitch all is used for training
pitch_all <- pitch_cum_tot %>%
  left_join(pitch_cum_l, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_cum_r, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_rol_tot, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_rol_l, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_rol_r, by = c("game_date", "pitcher", "game_year")) %>%
  distinct() %>%
  group_by(pitcher, game_year) %>%
  mutate(real_dt = lag(game_date))

#pitch_max used for prediction
pitch_max <- pitch_all %>% 
  group_by(pitcher) %>%
  filter(game_date == max(game_date)) %>%
  select(-c(game_year, game_date, real_dt)) %>%
  left_join(chadwick_player_lu_table, by = c("pitcher" = "key_mlbam")) %>%
  select(-c(key_fangraphs, name_last, name_first)) %>%
  rename(pitcher_name = hitter_name)

#p_throw is used for prediction
p_throw <- pbp %>% group_by(pitcher, p_throws) %>%
  summarise(cnt = n_distinct(at_bat_number)) %>%
  filter(cnt == max(cnt)) %>%
  select(-cnt)

rm(pitch_cum_tot, pitch_cum_l, pitch_cum_r, pitch_rol_tot, pitch_rol_l, pitch_rol_r)

###################################################
#League Batting Summary
################################################

lg_bat_sum <- pbp %>% group_by(game_date,game_year, p_throws) %>%
  summarise(BF = n_distinct(game_pk, at_bat_number),
            uBB = sum(events == "walk", na.rm = TRUE),
            HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
            X1B = sum(events == "single", na.rm = TRUE),
            X2B = sum(events == "double", na.rm = TRUE),
            X3B = sum(events == "triple", na.rm = TRUE),
            HR = sum(events == "home_run", na.rm = TRUE),
            SH = sum(substr(events,1,3) == "sac", na.rm = TRUE),
            SO = sum(substr(events,1,9) == "strikeout", na.rm = TRUE),
            FB = sum(bb_type == "fly_ball", na.rm = TRUE),
            GB = sum(bb_type == "ground_ball", na.rm = TRUE),
            FB_HR = sum(events == "home_run" & bb_type == "fly_ball", na.rm = TRUE),
            PF = mean(PF)) %>%
  mutate(AB = BF - (uBB + HBP + SH)
         #     ,HR_FB_rate = FB_HR/FB
  ) %>%
  #  left_join(innings, by = c("game_date", "pitcher", "p_throws", "home_team", "away_team", "p_team", "p_league", "inning_topbot")) %>%
  arrange(p_throws, game_date)


bat_cum_r_lg <- roll_lg_woba(lg_bat_sum) %>%
  filter(p_throws == "R") %>%
  arrange(p_throws, game_date) %>%
  select("game_date","game_year","BF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_cum_r_lg = wOBA,
         BF_cum_r_lg = BF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_cum_l_lg <- roll_lg_woba(lg_bat_sum) %>%
  filter(p_throws == "L") %>%
  arrange(p_throws, game_date) %>%
  select("game_date","game_year","BF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_cum_l_lg = wOBA,
         BF_cum_l_lg = BF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_cum_tot_lg <- roll_lg_woba(lg_bat_sum, rl_excl = "Y") %>%
  arrange(game_date) %>%
  select("game_date", "game_year","BF", "AB", "uBB", "SO", "wOBA", "woba_scale", "lg_woba") %>%
  rename(wOBA_cum_tot_lg = wOBA,
         BF_cum_tot_lg = BF) %>%
  select(-c("AB", "uBB", "SO"))

bat_rol_r_lg <- roll_lg_woba(lg_bat_sum, 15) %>%
  filter(p_throws == "R") %>%
  arrange(p_throws, game_date) %>%
  select("game_date","game_year","BF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_rol_r_lg = wOBA,
         BF_rol_r_lg = BF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_rol_l_lg <- roll_lg_woba(lg_bat_sum, 15) %>%
  filter(p_throws == "L") %>%
  arrange(p_throws, game_date) %>%
  select("game_date", "game_year", "AB","BF", "uBB", "SO", "wOBA", "p_throws") %>%
  rename(wOBA_rol_l_lg = wOBA,
         BF_rol_l_lg = BF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_rol_tot_lg <- roll_lg_woba(lg_bat_sum, 15, rl_excl = "Y") %>%
  arrange(game_date) %>%
  select("game_date", "game_year","BF", "AB", "uBB", "SO", "wOBA") %>%
  rename(wOBA_rol_tot_lg = wOBA,
         BF_rol_tot_lg = BF) %>%
  select(-c("AB", "uBB", "SO"))

bat_all_lg <- bat_cum_tot_lg %>%
  left_join(bat_cum_l_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_cum_r_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_rol_tot_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_rol_l_lg, by = c("game_date", "game_year")) %>%
  left_join(bat_rol_r_lg, by = c("game_date", "game_year")) %>%
  distinct() %>%
  group_by(game_year) 

tot_score <- pbp %>%
  group_by(game_pk,home_team, away_team, game_date, game_year) %>%
  summarise(away_score = max(away_score),
            home_score = max(home_score)) %>%
  mutate(tot_score = away_score + home_score) %>%
  ungroup() %>%
  group_by(game_date, game_year) %>%
  summarise(tot_score = sum(tot_score)) %>%
  ungroup() %>%
  arrange(game_year, game_date) %>%
  group_by(game_year) %>%
  mutate(rol_score = rollapplyr(tot_score,15,sum, partial=TRUE)
         ,cum_score = cumsum(tot_score)) %>%
  select(-tot_score)

tot_score$game_year <- as.character(tot_score$game_year)

bat_all_lg_fin <- bat_all_lg %>%
  left_join(tot_score, by = c("game_date", "game_year")) %>%
  mutate(wRC_cum_tot_lg = (((wOBA_cum_tot_lg - lg_woba)/woba_scale) + (cum_score/BF_cum_tot_lg))*BF_cum_tot_lg,
         wRC_cum_l_lg = (((wOBA_cum_l_lg - lg_woba)/woba_scale) + (cum_score/BF_cum_tot_lg))*BF_cum_l_lg,
         wRC_cum_r_lg = (((wOBA_cum_r_lg - lg_woba)/woba_scale) + (cum_score/BF_cum_tot_lg))*BF_cum_r_lg,
         wRC_rol_tot_lg = (((wOBA_rol_tot_lg - lg_woba)/woba_scale) + (cum_score/BF_cum_tot_lg))*BF_rol_tot_lg,
         wRC_rol_l_lg = (((wOBA_rol_l_lg - lg_woba)/woba_scale) + (cum_score/BF_cum_tot_lg))*BF_rol_l_lg,
         wRC_rol_r_lg = (((wOBA_rol_r_lg - lg_woba)/woba_scale) + (cum_score/BF_cum_tot_lg))*BF_rol_r_lg)

rm(bat_cum_tot_lg, bat_cum_l_lg, bat_cum_r_lg, bat_rol_tot_lg, bat_rol_l_lg, bat_rol_r_lg,
   tot_score, bat_all_lg)


###################################################
#Batting summary
################################################

bat_sum <- pbp %>% group_by(game_date,game_year, batter, p_throws) %>%
  summarise(BF = n_distinct(at_bat_number),
            uBB = sum(events == "walk", na.rm = TRUE),
            HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
            X1B = sum(events == "single", na.rm = TRUE),
            X2B = sum(events == "double", na.rm = TRUE),
            X3B = sum(events == "triple", na.rm = TRUE),
            HR = sum(events == "home_run", na.rm = TRUE),
            SH = sum(substr(events,1,3) == "sac", na.rm = TRUE),
            SO = sum(substr(events,1,9) == "strikeout", na.rm = TRUE),
            FB = sum(bb_type == "fly_ball", na.rm = TRUE),
            GB = sum(bb_type == "ground_ball", na.rm = TRUE),
            FB_HR = sum(events == "home_run" & bb_type == "fly_ball", na.rm = TRUE),
            PF = mean(PF)) %>%
  mutate(AB = BF - (uBB + HBP + SH)
         #     ,HR_FB_rate = FB_HR/FB
  ) %>%
  #  left_join(innings, by = c("game_date", "pitcher", "p_throws", "home_team", "away_team", "p_team", "p_league", "inning_topbot")) %>%
  arrange(batter, p_throws, game_date)

bat_cum_r <- roll_woba(bat_sum) %>%
  filter(p_throws == "R") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter","BF","PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(cum_b_k_rate_r = SO/AB,
         cum_b_bb_rate_r = uBB/AB) %>%
  rename(wOBA_cum_r = wOBA,
         BF_cum_r = BF,
         PF_cum_r = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_cum_l <- roll_woba(bat_sum) %>%
  filter(p_throws == "L") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter","BF","PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(cum_b_k_rate_l = SO/AB,
         cum_b_bb_rate_l = uBB/AB) %>%
  rename(wOBA_cum_l = wOBA,
         BF_cum_l = BF,
         PF_cum_l = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_cum_tot <- roll_woba(bat_sum, rl_excl = "Y") %>%
  arrange(batter, game_date) %>%
  select("game_date", "game_year", "batter","BF", "PF", "AB", "uBB", "SO", "wOBA") %>%
  mutate(cum_b_k_rate = SO/AB,
         cum_b_bb_rate = uBB/AB) %>%
  rename(wOBA_cum_tot = wOBA,
         BF_cum_tot = BF,
         PF_cum_tot = PF) %>%
  select(-c("AB", "uBB", "SO"))

bat_rol_r <- roll_woba(bat_sum, 15) %>%
  filter(p_throws == "R") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter", "BF", "PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(rol_b_k_rate_r = SO/AB,
         rol_b_bb_rate_r = uBB/AB) %>%
  rename(wOBA_rol_r = wOBA,
         BF_rol_r = BF,
         PF_rol_r = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_rol_l <- roll_woba(bat_sum, 15) %>%
  filter(p_throws == "L") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date", "game_year", "batter", "BF", "PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(rol_b_k_rate_l = SO/AB,
         rol_b_bb_rate_l = uBB/AB) %>%
  rename(wOBA_rol_l = wOBA,
         BF_rol_l = BF,
         PF_rol_l = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_rol_tot <- roll_woba(bat_sum, 15, rl_excl = "Y") %>%
  arrange(batter, game_date) %>%
  select("game_date", "game_year", "batter", "BF", "PF", "AB", "uBB", "SO", "wOBA") %>%
  mutate(rol_b_k_rate = SO/AB,
         rol_b_bb_rate = uBB/AB) %>%
  rename(wOBA_rol_tot = wOBA,
         BF_rol_tot = BF,
         PF_rol_tot = PF) %>%
  select(-c("AB", "uBB", "SO"))

#bat_all is used for training
bat_all <- bat_cum_tot %>%
  left_join(bat_cum_l, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_cum_r, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_tot, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_l, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_r, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_all_lg_fin, by = c("game_date", "game_year")) %>%
  distinct() %>%
  group_by(batter, game_year) %>%
  mutate(real_dt = lag(game_date),
         wRAA_cum_tot = ((wOBA_cum_tot - wOBA_cum_tot_lg)/woba_scale)*BF_cum_tot,
         wRAA_cum_l = ((wOBA_cum_l - wOBA_cum_l_lg)/woba_scale)*BF_cum_l,
         wRAA_cum_r = ((wOBA_cum_r - wOBA_cum_r_lg)/woba_scale)*BF_cum_r,
         wRAA_rol_tot = ((wOBA_rol_tot - wOBA_rol_tot_lg)/woba_scale)*BF_rol_tot,
         wRAA_rol_l = ((wOBA_rol_l - wOBA_rol_l_lg)/woba_scale)*BF_rol_l,
         wRAA_rol_r = ((wOBA_rol_r - wOBA_rol_r_lg)/woba_scale)*BF_rol_r,
         wRAA_cum_tot_pa = wRAA_cum_tot/BF_cum_tot,
         wRC_cum_tot = (((wRAA_cum_tot/BF_cum_tot + (cum_score/BF_cum_tot_lg))+((cum_score/BF_cum_tot_lg)-((PF_cum_tot/100)*(cum_score/BF_cum_tot_lg))))/(wRC_cum_tot_lg/BF_cum_tot_lg))*100,
         wRC_cum_l = (((wRAA_cum_l/BF_cum_l + (cum_score/BF_cum_tot_lg))+((cum_score/BF_cum_tot_lg)-((PF_cum_l/100)*(cum_score/BF_cum_tot_lg))))/(wRC_cum_l_lg/BF_cum_l_lg))*100,
         wRC_cum_r = (((wRAA_cum_r/BF_cum_r + (cum_score/BF_cum_tot_lg))+((cum_score/BF_cum_tot_lg)-((PF_cum_r/100)*(cum_score/BF_cum_tot_lg))))/(wRC_cum_r_lg/BF_cum_r_lg))*100,
         wRC_rol_tot = (((wRAA_rol_tot/BF_rol_tot + (rol_score/BF_rol_tot_lg))+((rol_score/BF_rol_tot_lg)-((PF_rol_tot/100)*(rol_score/BF_rol_tot_lg))))/(wRC_rol_tot_lg/BF_rol_tot_lg))*100,
         wRC_rol_l = (((wRAA_rol_l/BF_rol_l + (rol_score/BF_rol_tot_lg))+((rol_score/BF_rol_tot_lg)-((PF_rol_l/100)*(rol_score/BF_rol_tot_lg))))/(wRC_rol_l_lg/BF_rol_l_lg))*100,
         wRC_rol_r = (((wRAA_rol_r/BF_rol_r + (rol_score/BF_rol_tot_lg))+((rol_score/BF_rol_tot_lg)-((PF_rol_r/100)*(rol_score/BF_rol_tot_lg))))/(wRC_rol_r_lg/BF_rol_r_lg))*100) %>%
  select(-c(PF_cum_tot, wOBA_cum_tot, PF_cum_l, wOBA_cum_l, PF_cum_r, wOBA_cum_r, PF_rol_tot,
            wOBA_rol_tot, PF_rol_l, wOBA_rol_l, PF_rol_r, wOBA_rol_r, BF_cum_tot_lg, wOBA_cum_tot_lg,
            woba_scale, lg_woba, BF_cum_l_lg, wOBA_cum_l_lg, BF_cum_r_lg, wOBA_cum_r_lg, BF_rol_tot_lg,
            wOBA_rol_tot_lg, BF_rol_l_lg, wOBA_rol_l_lg, BF_rol_r_lg, wOBA_rol_r_lg, rol_score, cum_score,
            wRC_cum_tot_lg, wRC_cum_l_lg, wRC_cum_r_lg, wRC_rol_tot_lg, wRC_rol_l_lg, wRC_rol_r_lg,
            wRAA_cum_tot, wRAA_cum_l, wRAA_cum_r, wRAA_rol_tot, wRAA_rol_l, wRAA_rol_r, wRAA_cum_tot_pa))

#bat_max used for prediction
bat_max <- bat_all %>%
  group_by(batter) %>%
  filter(game_date == max(game_date)) %>%
  select(-c(game_date, game_year, real_dt)) %>%
  left_join(chadwick_player_lu_table, by = c("batter" = "key_mlbam")) %>%
  select(-c(key_fangraphs, name_last, name_first)) 

#stand_bat used for prediction
stand_bat <- pbp %>% group_by(batter, p_throws, stand) %>%
  summarise(cnt = n_distinct(at_bat_number)) %>%
  filter(cnt == max(cnt)) %>%
  select(-cnt)

rm(bat_cum_tot, bat_cum_l, bat_cum_r, bat_rol_tot, bat_rol_l, bat_rol_r)

###########################################################
#Final score of every half inning - used for training
###########################################################

fin_score <- pbp %>% select(game_date,game_year, game_pk, inning, inning_topbot, bat_score) %>%
  group_by(game_date,game_year, game_pk, inning, inning_topbot) %>%
  summarise(end_score = max(bat_score))

###############################################
# Bring in on deck - used for training
###############################################

b_order <- pbp %>%
  select(game_pk, game_date,game_year, batter,inning, inning_topbot, at_bat_number) %>%
  arrange(game_pk, inning_topbot, at_bat_number) %>%
  distinct() %>%
  mutate(on_deck = lead(batter))

###########################################################
#Situations for training
###########################################################

sit_all_test <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter((outs_when_up == 1 & !is.na(on_1b) & is.na(on_2b) & is.na(on_3b)) |
           (outs_when_up == 2 & is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) |
           (outs_when_up == 0 & !is.na(on_1b) & is.na(on_2b) & is.na(on_3b)) |
           (outs_when_up == 1 & is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) |
           (outs_when_up == 0 & is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) |
           (outs_when_up == 1 & is.na(on_1b) & is.na(on_2b) & !is.na(on_3b)) |
           (outs_when_up == 0 & !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) |
           (outs_when_up == 1 & is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b))
         ) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_1b" = "player_id", "game_year.y" = "game_year")) %>%
  left_join(sprint.speed, by=c("on_2b" = "player_id", "game_year.y" = "game_year")) %>%
  left_join(sprint.speed, by=c("on_3b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y")) %>%
  rename(sprint_speed_bat = sprint_speed.x,
         sprint_speed_1b = sprint_speed.y,
         sprint_speed_2b = sprint_speed.x.x,
         sprint_speed_3b = sprint_speed.y.y)

input_df <- function(df){
  
  df1 <- df %>%
    filter(stand == "R" & p_throws == "R") %>%
    select(-c("wRC_cum_l.x", "cum_b_k_rate_l.x", "cum_b_bb_rate_l.x", "wRC_cum_l.y", "cum_b_k_rate_l.y", "cum_b_bb_rate_l.y",
              "wRC_rol_l.x", "rol_b_k_rate_l.x", "rol_b_bb_rate_l.x", "wRC_rol_l.y", "rol_b_k_rate_l.y", "rol_b_bb_rate_l.y",
              "FIP_rol_l", "rol_p_k_rate_l", "rol_p_bb_rate_l", "FIP_cum_l", "cum_p_k_rate_l", "cum_p_bb_rate_l", "BF_rol_l.x",
              "BF_rol_l.y", "BF_cum_l.x", "BF_cum_l.y")) %>%
    rename(wRC_cum_hand = wRC_cum_r.x,
           cum_b_k_rate_hand = cum_b_k_rate_r.x,
           cum_b_bb_rate_hand = cum_b_bb_rate_r.x,
           wRC_cum_hand_od = wRC_cum_r.y,
           cum_b_k_rate_hand_od = cum_b_k_rate_r.y,
           cum_b_bb_rate_hand_od = cum_b_bb_rate_r.y,
           wRC_rol_hand = wRC_rol_r.x,
           rol_b_k_rate_hand = rol_b_k_rate_r.x,
           rol_b_bb_rate_hand = rol_b_bb_rate_r.x,
           wRC_rol_hand_od = wRC_rol_r.y,
           rol_b_k_rate_hand_od = rol_b_k_rate_r.y,
           rol_b_bb_rate_hand_od = rol_b_bb_rate_r.y,
           FIP_rol_hand = FIP_rol_r,
           rol_p_k_rate_hand = rol_p_k_rate_r,
           rol_p_bb_rate_hand = rol_p_bb_rate_r,
           FIP_cum_hand = FIP_cum_r,
           cum_p_k_rate_hand = cum_p_k_rate_r,
           cum_p_bb_rate_hand = cum_p_bb_rate_r,
           BF_cum_hand = BF_cum_r.x,
           BF_rol_hand = BF_rol_r.x,
           BF_cum_hand_od = BF_cum_r.y,
           BF_rol_hand_od = BF_rol_r.y)
  
  df2 <- df %>%
    filter(stand == "L" & p_throws == "L") %>%
    select(-c("wRC_cum_r.x", "cum_b_k_rate_r.x", "cum_b_bb_rate_r.x", "wRC_cum_r.y", "cum_b_k_rate_r.y", "cum_b_bb_rate_r.y",
              "wRC_rol_r.x", "rol_b_k_rate_r.x", "rol_b_bb_rate_r.x", "wRC_rol_r.y", "rol_b_k_rate_r.y", "rol_b_bb_rate_r.y",
              "FIP_rol_r", "rol_p_k_rate_r", "rol_p_bb_rate_r", "FIP_cum_r", "cum_p_k_rate_r", "cum_p_bb_rate_r", "BF_rol_r.x",
              "BF_rol_r.y", "BF_cum_r.x", "BF_cum_r.y")) %>%
    rename(wRC_cum_hand = wRC_cum_l.x,
           cum_b_k_rate_hand = cum_b_k_rate_l.x,
           cum_b_bb_rate_hand = cum_b_bb_rate_l.x,
           wRC_cum_hand_od = wRC_cum_l.y,
           cum_b_k_rate_hand_od = cum_b_k_rate_l.y,
           cum_b_bb_rate_hand_od = cum_b_bb_rate_l.y,
           wRC_rol_hand = wRC_rol_l.x,
           rol_b_k_rate_hand = rol_b_k_rate_l.x,
           rol_b_bb_rate_hand = rol_b_bb_rate_l.x,
           wRC_rol_hand_od = wRC_rol_l.y,
           rol_b_k_rate_hand_od = rol_b_k_rate_l.y,
           rol_b_bb_rate_hand_od = rol_b_bb_rate_l.y,
           FIP_rol_hand = FIP_rol_l,
           rol_p_k_rate_hand = rol_p_k_rate_l,
           rol_p_bb_rate_hand = rol_p_bb_rate_l,
           FIP_cum_hand = FIP_cum_l,
           cum_p_k_rate_hand = cum_p_k_rate_l,
           cum_p_bb_rate_hand = cum_p_bb_rate_l,
           BF_cum_hand = BF_cum_l.x,
           BF_rol_hand = BF_rol_l.x,
           BF_cum_hand_od = BF_cum_l.y,
           BF_rol_hand_od = BF_rol_l.y)
  
  df3 <- df %>%
    filter(stand == "R" & p_throws == "L") %>%
    select(-c("wRC_cum_r.x", "cum_b_k_rate_r.x", "cum_b_bb_rate_r.x", "wRC_cum_r.y", "cum_b_k_rate_r.y", "cum_b_bb_rate_r.y",
              "wRC_rol_r.x", "rol_b_k_rate_r.x", "rol_b_bb_rate_r.x", "wRC_rol_r.y", "rol_b_k_rate_r.y", "rol_b_bb_rate_r.y",
              "FIP_rol_l", "rol_p_k_rate_l", "rol_p_bb_rate_l", "FIP_cum_l", "cum_p_k_rate_l", "cum_p_bb_rate_l", "BF_rol_r.x",
              "BF_rol_r.y", "BF_cum_r.x", "BF_cum_r.y")) %>%
    rename(wRC_cum_hand = wRC_cum_l.x,
           cum_b_k_rate_hand = cum_b_k_rate_l.x,
           cum_b_bb_rate_hand = cum_b_bb_rate_l.x,
           wRC_cum_hand_od = wRC_cum_l.y,
           cum_b_k_rate_hand_od = cum_b_k_rate_l.y,
           cum_b_bb_rate_hand_od = cum_b_bb_rate_l.y,
           wRC_rol_hand = wRC_rol_l.x,
           rol_b_k_rate_hand = rol_b_k_rate_l.x,
           rol_b_bb_rate_hand = rol_b_bb_rate_l.x,
           wRC_rol_hand_od = wRC_rol_l.y,
           rol_b_k_rate_hand_od = rol_b_k_rate_l.y,
           rol_b_bb_rate_hand_od = rol_b_bb_rate_l.y,
           FIP_rol_hand = FIP_rol_r,
           rol_p_k_rate_hand = rol_p_k_rate_r,
           rol_p_bb_rate_hand = rol_p_bb_rate_r,
           FIP_cum_hand = FIP_cum_r,
           cum_p_k_rate_hand = cum_p_k_rate_r,
           cum_p_bb_rate_hand = cum_p_bb_rate_r,
           BF_cum_hand = BF_cum_l.x,
           BF_rol_hand = BF_rol_l.x,
           BF_cum_hand_od = BF_cum_l.y,
           BF_rol_hand_od = BF_rol_l.y)
  
  df4 <- df %>%
    filter(stand == "L" & p_throws == "R") %>%
    select(-c("wRC_cum_l.x", "cum_b_k_rate_l.x", "cum_b_bb_rate_l.x", "wRC_cum_l.y", "cum_b_k_rate_l.y", "cum_b_bb_rate_l.y",
              "wRC_rol_l.x", "rol_b_k_rate_l.x", "rol_b_bb_rate_l.x", "wRC_rol_l.y", "rol_b_k_rate_l.y", "rol_b_bb_rate_l.y",
              "FIP_rol_r", "rol_p_k_rate_r", "rol_p_bb_rate_r", "FIP_cum_r", "cum_p_k_rate_r", "cum_p_bb_rate_r", "BF_rol_l.x",
              "BF_rol_l.y", "BF_cum_l.x", "BF_cum_l.y")) %>%
    rename(wRC_cum_hand = wRC_cum_r.x,
           cum_b_k_rate_hand = cum_b_k_rate_r.x,
           cum_b_bb_rate_hand = cum_b_bb_rate_r.x,
           wRC_cum_hand_od = wRC_cum_r.y,
           cum_b_k_rate_hand_od = cum_b_k_rate_r.y,
           cum_b_bb_rate_hand_od = cum_b_bb_rate_r.y,
           wRC_rol_hand = wRC_rol_r.x,
           rol_b_k_rate_hand = rol_b_k_rate_r.x,
           rol_b_bb_rate_hand = rol_b_bb_rate_r.x,
           wRC_rol_hand_od = wRC_rol_r.y,
           rol_b_k_rate_hand_od = rol_b_k_rate_r.y,
           rol_b_bb_rate_hand_od = rol_b_bb_rate_r.y,
           FIP_rol_hand = FIP_rol_l,
           rol_p_k_rate_hand = rol_p_k_rate_l,
           rol_p_bb_rate_hand = rol_p_bb_rate_l,
           FIP_cum_hand = FIP_cum_l,
           cum_p_k_rate_hand = cum_p_k_rate_l,
           cum_p_bb_rate_hand = cum_p_bb_rate_l,
           BF_cum_hand = BF_cum_r.x,
           BF_rol_hand = BF_rol_r.x,
           BF_cum_hand_od = BF_cum_r.y,
           BF_rol_hand_od = BF_rol_r.y)
  
  df_all <- rbind(df1, df2, df3, df4)
  
  return(df_all)
}

sit_all_test2 <- input_df(sit_all_test) %>%
  select(-c("game_date", "game_pk", "at_bat_number", "batter", "pitcher", "inning", "inning_topbot", "stand",
            "p_throws", "on_3b", "on_2b", "on_1b", "bat_score", "end_score", "on_deck"))

sit_all_comp <- sit_all_test2[complete.cases(sit_all_test2[1:48]),]

saveRDS(sit_all_comp, "training_data.rds")
saveRDS(pitch_max, "data/pitch_max.rds")
saveRDS(bat_max, "data/bat_max.rds")
saveRDS(sprint_max, "data/sprint_max.rds")
saveRDS(stand_bat, "data/stand_bat.rds")
saveRDS(p_throw, "data/p_throw.rds")
saveRDS(input_df, "data/input_df.rds")

bat_names <- as.list(bat_max$hitter_name)
pit_names <- as.list(pitch_max$pitcher_name)
run_names <- as.list(sprint_max$player_name)


