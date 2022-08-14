##########################################
# Sean Trainor                           #
# http://github.com/seanjtrainor/shiftr  #
##########################################

#READ IN NEEDED PACKAGES

usePackage <- function(p) {
  if(!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


listPackages <- c("baseballr", "dplyr", "RcppRoll", "zoo", "lubridate", "tidyverse",
                  "tree", "caret", "randomForest", "rvest", "ggalt", "remotes", "gbm", 
                  "glmnet", "shiny", "shinythemes", "tibble", "shinydashboard", "xgboost",
                  "mlr", "ggalt")

sapply(listPackages, usePackage)

# install.packages("devtools")
devtools::install_github("camdenk/mlbplotR")

scrape_statcast_savant_v2 <- readRDS("data/scrape_statcast_savant_v2.rds")
season_pbp <- readRDS("data/season_pbp.rds")
#readRDS("data/roll_fip.rds")
#readRDS("data/roll_woba.rds")
#readRDS("data/roll_lg_woba.rds")

################################
# Input data needed for model
################################

pbp <- rbind((season_pbp(2022) %>%
                  subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))),
             (season_pbp(2021) %>%
                  subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))), 
             (season_pbp(2020) %>%
                  subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))), 
             (season_pbp(2019) %>%
                  subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))), 
             (season_pbp(2018) %>%
                  subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))))

#Bring in Chadwick to get different playerids (baseball savant, bref, fangraphs, etc)

chadwick_player_lu_table <- subset(chadwick_player_lu(), !is.na(key_mlbam)) %>%
  dplyr::select(key_mlbam,key_fangraphs, name_last, name_first) %>%
  dplyr::mutate(hitter_name = paste(name_first, name_last))

#bring in sprint speeds

sprint.speed18 <- read.csv("~/Desktop/baseball/shiftr/data/sprint_speed_18.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2018")

sprint.speed19 <- read.csv("~/Desktop/baseball/shiftr/data/sprint_speed_19.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2019")

sprint.speed20 <- read.csv("~/Desktop/baseball/shiftr/data/sprint_speed_20.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2020")

sprint.speed21 <- read.csv("~/Desktop/baseball/shiftr/data/sprint_speed_21.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2021")

sprint.speed22 <- read.csv("~/Desktop/baseball/shiftr/data/sprint_speed_22.csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2022")

sprint.speed <- rbind(sprint.speed18, sprint.speed19, sprint.speed20, sprint.speed21, sprint.speed22) #use to train model

pbp$game_year <- as.character(pbp$game_year)

bip <- pbp %>%
  dplyr::mutate(location_x = hc_x - 125.42,
                location_y = 198.27 - hc_y) %>%
  dplyr::left_join(chadwick_player_lu_table, by =c("batter" = "key_mlbam")) %>%
  dplyr::left_join((chadwick_player_lu_table %>%
                      select(key_mlbam, hitter_name) %>%
                      rename(pitcher_name = hitter_name)), 
                   by = c("pitcher" = "key_mlbam")) %>%
  dplyr::left_join(sprint.speed, by = c("batter" = "player_id", "game_year" = "game_year")) %>%
  dplyr::mutate(p_team = ifelse(inning_topbot == "Bot", away_team, home_team),
                h_team = ifelse(inning_topbot == "Bot", home_team, away_team)) 

bip_ <- bip %>% dplyr::mutate(dis_org = sqrt((location_x - 0)^2 + (location_y - 0)^2),
                              dis_oth = sqrt((location_x - 0)^2 + (location_y - 150)^2),
                              radians = acos((150^2 + dis_org^2 - dis_oth^2)/(2*150*dis_org)),
                              angle_ = (180*radians)/pi,
                              angle = ifelse(location_x == 0, 0, ifelse(location_x < 0, angle_ * -1, angle_)),
                              hit_loc = ifelse((angle<0 & stand == "R") | (angle > 0 & stand == "L"), "pull", "oppo"),
                              shift = ifelse(if_fielding_alignment != "Standard", 1, 0),
                              hit = ifelse(woba_value > 0, 1, 0))

bip_fin <- bip_ %>% dplyr::select(woba_value,hit, shift, launch_speed, angle, stand, launch_angle
                                  ,hitter_name,pitcher_name, p_team, h_team, outs_when_up, on_3b, on_2b, on_1b,game_year,sprint_speed) %>%
  dplyr::mutate(on1 = ifelse(is.na(on_1b), 0,1),
                on2 = ifelse(is.na(on_2b), 0,1),
                on3 = ifelse(is.na(on_3b), 0,1)) %>%
  dplyr::select(-c(on_3b, on_2b, on_1b))

bip_fin$sprint_speed <- replace(bip_fin$sprint_speed, is.na(bip_fin$sprint_speed), 27.22)


bip_comp <- bip_fin[complete.cases(bip_fin),]



