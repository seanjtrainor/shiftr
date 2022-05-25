usePackage <- function(p) {
  if(!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

listPackages <- c("baseballr", "dplyr", "RcppRoll", "zoo", "lubridate", "tidyverse",
                  "tree", "caret", "randomForest", "rvest", "ggalt")

sapply(listPackages, usePackage)

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
install.packages("remotes")
remotes::install_github("camdenk/mlbplotR")
pacman::p_load_current_gh("BillPetti/baseballr")

#chadwick_test <- head(chadwick_player_lu())

chadwick_player_lu_table <- subset(chadwick_player_lu(), !is.na(key_mlbam)) %>%
  dplyr::select(key_mlbam,key_fangraphs, name_last, name_first) %>%
  dplyr::mutate(hitter_name = paste(name_first, name_last))

sprint.speed19 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (1).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = 2019)

sprint.speed20 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (2).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = 2020)

sprint.speed21 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (3).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = 2021)

sprint.speed <- rbind(sprint.speed19, sprint.speed20, sprint.speed21)

#chadwick2 <- chadwick_player_lu_table %>%
#  left_join(sprint.speed, by=c("key_mlbam" = "player_id"))

#mlb_all <- rbind(mlb_all21_2, mlb_all20_2, mlb_all19_2)


mlb_all <- rbind((season_pbp(2019) %>%
                    subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))),
                 (season_pbp(2020) %>%
                    subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))),
                 (season_pbp(2021) %>%
                    subset(type == "X" & (bb_type == "ground_ball" | bb_type == "line_drive") & hit_distance_sc < 224 & !is.na(hc_x) & !is.na(hc_y) & !is.na(if_fielding_alignment) & !is.na(launch_speed))))

bip <- mlb_all %>%
  dplyr::mutate(location_x = hc_x - 125.42,
         location_y = 198.27 - hc_y) %>%
  dplyr::left_join(chadwick_player_lu_table, by =c("batter" = "key_mlbam")) %>%
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

#train <- sample(1:nrow(bip_), nrow(bip_)*.8)

#test <- as.factor(bip_[-train,]$woba_value)

#ntree <- 250

#y.train <- as.factor(bip_[train,]$woba_value)
#y.test <- as.factor(bip_[-train,]$woba_value)

#train.error <- rep(0, ntree)
#test.error <- rep(0, ntree)

# for (i in 1:ntree){
#   bip.rf2 <- randomForest(as.factor(woba_value) ~ shift + launch_speed + angle +
#                            stand + launch_angle, data = bip_, subset = train, ntree = i, importance = TRUE)
#   
#   yhat.train <- predict(bip.rf2, newdata = bip_[train,]) 
#   train.error[i] <- (table(y.train,yhat.train)[1,1] + table(y.train,yhat.train)[2,2] + table(y.train,yhat.train)[3,3] +
#                        table(y.train,yhat.train)[4,4])/length(y.train)
#   
#   yhat.test <- predict(bip.rf2, newdata = bip_[-train, ]) 
#   test.error[i] <- (table(y.test,yhat.test)[1,1] + table(y.test,yhat.test)[2,2] + table(y.test,yhat.test)[3,3] +
#                       table(y.test,yhat.test)[4,4])/length(y.test)
# }

#plot(test.error)

bip_fin <- bip_ %>% dplyr::select(woba_value,hit, shift, launch_speed, angle, stand, launch_angle
                           ,hitter_name, p_team, h_team, outs_when_up, on_3b, on_2b, on_1b,game_year,sprint_speed) %>%
  dplyr::mutate(on1 = ifelse(is.na(on_1b), 0,1),
         on2 = ifelse(is.na(on_2b), 0,1),
         on3 = ifelse(is.na(on_3b), 0,1)) %>%
  dplyr::select(-c(on_3b, on_2b, on_1b))

bip_fin$sprint_speed <- replace(bip_fin$sprint_speed, is.na(bip_fin$sprint_speed), 27.22)


bip_comp <- bip_fin[complete.cases(bip_fin),]


#####################################################
# Fine Tuning a BABIP Model
#####################################################

train.babip <- sample(1:nrow(bip_comp), nrow(bip_comp)*.8)

test.babip <- as.factor(bip_comp[-train.babip,]$hit)

ntree <- 250

y.train.babip <- as.factor(bip_comp[train.babip,]$hit)
y.test.babip <- as.factor(bip_comp[-train.babip,]$hit)

train.error.babip <- rep(0, ntree)
test.error.babip <- rep(0, ntree)

for (i in 1:ntree){
  babip.rf2 <- randomForest(as.factor(hit) ~ shift + launch_speed + angle + stand
                           + launch_angle + sprint_speed, data = bip_comp, subset = train.babip, ntree = i, importance = TRUE)
  
  yhat.train.babip <- predict(babip.rf2, newdata = bip_comp[train.babip,]) 
  train.error.babip[i] <- (table(y.train.babip,yhat.train.babip)[1,1] + table(y.train.babip,yhat.train.babip)[2,2])/length(y.train.babip)
  
  yhat.test.babip <- predict(babip.rf2, newdata = bip_comp[-train.babip, ]) 
  test.error.babip[i] <- (table(y.test.babip,yhat.test.babip)[1,1] + table(y.test.babip,yhat.test.babip)[2,2])/length(y.test.babip)
}


#This is the final model. We found that the optimal model has 224 trees

babip.rf <- randomForest(as.factor(hit) ~ shift + launch_speed + angle +
                         stand + launch_angle + sprint_speed, data = bip_comp, ntree = 224, importance = TRUE)

#Simulating 2021 batted balls if there was no shift
babip_comp_test <- dplyr::filter(bip_comp, game_year == "2021")
babip_comp_test$shift_real <- babip_comp_test$shift
babip_comp_test$shift <- 0

babip.pred.no.shift <- predict(babip.rf, babip_comp_test, type = "prob")[,2]
babip.pred2 <- predict(babip.rf, dplyr::filter(bip_comp, game_year == "2021"), type = "prob")[,2]

babip_comp_test$prediction.no.shift = as.numeric(babip.pred.no.shift)
babip_comp_test$prediction2 = as.numeric(babip.pred2)

# babip_comp_test2 <- babip_comp_test %>%
#   mutate(pred.no.shift = ifelse(prediction.no.shift == 1, 0,1),
#          pred2 = ifelse(prediction2 == 1, 0, 1))

babip_comp_sum <- babip_comp_test %>% dplyr::select(h_team,shift_real, hit, prediction.no.shift, prediction2) %>%
  dplyr::group_by(h_team) %>%
  dplyr::summarise(ab = dplyr::n(),
            shifts = sum(shift_real),
            hits = sum(hit),
            pa_hits = sum(prediction2),
            pred.hits.no.shift = sum(prediction.no.shift)) %>%
  dplyr::mutate(real_babip = hits/ab,
         shift_pct = shifts/ab,
         ex_babip = pa_hits/ab,
         pred.babip.no.shift = pred.hits.no.shift/ab,
         diff = pred.babip.no.shift - ex_babip) %>%
  dplyr::arrange(desc(diff)) %>%
  dplyr::filter(ab >= 50)

teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
  dplyr::filter(!team_primary_abbr %in% c("AL", "NL", "MLB")) %>% 
  dplyr::mutate(
    a = rep(1:6, 5),
    b = sort(rep(1:5, 6), decreasing = TRUE),
    alpha = ifelse(grepl("A", team_primary_abbr), 1, 0.75), # Keep alpha == 1 for teams that have an "A"
    color = ifelse(grepl("E", team_primary_abbr), "b/w", NA) # Set teams that have an "E" to black & white
  )

babip_comp_sum2 <- babip_comp_sum %>%
  left_join(teams_colors_logos, by = c("h_team" = "team_primary_abbr"))

babip_comp_sum2 %>% 
  ggplot2::ggplot(aes(x= ex_babip, y=pred.babip.no.shift)) +
  mlbplotR::geom_mlb_logos(aes(team_savant_abbr =h_team), width = .075, alpha = .7) +
  ggplot2::labs(
    title = "2021: xBABIP with Shift vs. xBABIP with no Shift",
    x = "xBABIP with Shift",
    y = "xBABIP with no Shift"
  ) +
  ggplot2::geom_smooth(method = lm, se=FALSE)

#look at defense

babip_comp_sum_def <- babip_comp_test %>% dplyr::select(p_team,shift_real, hit, prediction.no.shift, prediction2) %>%
  dplyr::group_by(p_team) %>%
  dplyr::summarise(ab = dplyr::n(),
                   shifts = sum(shift_real),
                   hits = sum(hit),
                   pa_hits = sum(prediction2),
                   pred.hits.no.shift = sum(prediction.no.shift)) %>%
  dplyr::mutate(real_babip = hits/ab,
                shift_pct = as.numeric(shifts/ab),
                ex_babip = pa_hits/ab,
                pred.babip.no.shift = pred.hits.no.shift/ab,
                diff = pred.babip.no.shift - ex_babip) %>%
  dplyr::arrange(desc(diff)) 

teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
  dplyr::filter(!team_primary_abbr %in% c("AL", "NL", "MLB")) %>% 
  dplyr::mutate(
    a = rep(1:6, 5),
    b = sort(rep(1:5, 6), decreasing = TRUE),
    alpha = ifelse(grepl("A", team_primary_abbr), 1, 0.75), # Keep alpha == 1 for teams that have an "A"
    color = ifelse(grepl("E", team_primary_abbr), "b/w", NA) # Set teams that have an "E" to black & white
  )

babip_comp_sum_def2 <- babip_comp_sum_def %>%
  left_join(teams_colors_logos, by = c("p_team" = "team_primary_abbr"))

babip_comp_sum_def2 %>% 
  ggplot2::ggplot(aes(x= ex_babip, y=pred.babip.no.shift)) +
  mlbplotR::geom_mlb_logos(aes(team_savant_abbr =p_team, size = "shift_pct"), width = .075, alpha = .7) +
#  ggplot2::geom_point(aes(size = "shift_pct")) +
  ggplot2::labs(
    title = "2021: xBABIP with Shift vs. xBABIP with no Shift",
    x = "xBABIP with Shift",
    y = "xBABIP with no Shift"
  ) +
  ggplot2::geom_smooth(method = lm, se=FALSE)

babip_comp_sum_def2 %>% ggplot2::ggplot(aes(x=ex_babip, xend=pred.babip.no.shift, y = p_team, group = p_team)) +
  ggalt::geom_dumbbell(aes(color = p_team),colour_x = "#a3c4dc", colour_xend = "#0e668b") +
  mlbplotR::scale_color_mlb(type = "primary") +
  mlbplotR::scale_fill_mlb(alpha = .4) +
  ggplot2::labs(x = "xBABIP",
                y=NULL,
                title="xBABIP with Shift vs No Shift") +
  ggplot2::theme(
    axis.text.y = mlbplotR::element_mlb_logo()
  )
  

#try different approach

x <- bip_comp[,c(1,3:16)]
y <- bip_comp[,2]

control <- trainControl(method="repeatedcv", number = 10, repeats=3, search="random")
seed <- 6876
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry = mtry)
rf_random <- train(as.factor(hit) ~ shift + launch_speed + angle +
                      stand + launch_angle, data = bip_comp, method = "rf", metric=metric,
                    tuneLength = 15, trControl = control)
print(rf_random)

control.grid <- trainControl(method="repeatedcv", number = 10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry = c(1:15))
rf_grid <- train(as.factor(hit) ~ shift + launch_speed + angle +
                     stand + launch_angle + sprint_speed, data = bip_comp, method = "rf", metric=metric,
                   tuneGrid = tunegrid, trControl = control.grid)
print(rf_grid)


table(y.test.babip, yhat.boost.df$pred)








