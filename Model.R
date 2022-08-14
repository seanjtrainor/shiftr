###################################
# Sean Trainor                    #
# http://github.com/seanjtrainor  #
###################################

###################################
#Training an XGBoost Model
###################################

set.seed(3434)

splits <- createDataPartition(bip_comp$hit, p = .8, list = F)
train <- bip_comp[splits, ]
test <- bip_comp[-splits, ]

#split train and validation set
train_id <- createDataPartition(train$hit, p = .8, list = F)
training <- train[train_id,]
validation <- train[-train_id,]

#define predictor and response variables in training set
train_x = data.matrix(training[, c(3:7,13)])
train_y = training$hit

#define predictor and response variables in validation set
valid_x = data.matrix(validation[, c(3:7,13)])
valid_y = validation$hit

#define predictor and response variables in testing set
test_x = data.matrix(test[, c(3:7,13)])
test_y = test$hit

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_valid = xgb.DMatrix(data = valid_x, label = valid_y)
xgb_test = xgb.DMatrix(data = test_x)

#define watchlist
watchlist = list(train=xgb_train, valid=xgb_valid)

########################################
#Fine Tune the Model
#########################################

# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(206)
for (iter in 1:50){
  param <- list(booster = "gbtree",
                objective = "binary:logistic",
                max_depth = sample(3:10, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)

# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
  set.seed(20)
  mdcv <- xgb.train(data=xgb_train,
                    booster = "gbtree",
                    objective = "binary:logistic",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    nrounds= 1000,
                    eval_metric = "error",
                    early_stopping_rounds= 30,
                    print_every_n = 100,
                    watchlist = watchlist
  )
  lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$valid_error))
  lowest_error_list[[row]] <- lowest_error
}

lowest_error_df <- do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

randomsearch <- as.data.frame(randomsearch) %>%
  rename(val_acc = `1 - min(mdcv$evaluation_log$valid_error)`) %>%
  arrange(-val_acc)

# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$valid_error)`)


mdcv <- xgb.train(data=xgb_train,
                  booster = "gbtree",
                  objective = "binary:logistic",
                  max_depth = randomsearch[1,]$max_depth,
                  eta =randomsearch[1,]$eta,
                  subsample = randomsearch[1,]$subsample,
                  colsample_bytree = randomsearch[1,]$colsample_bytree,
                  min_child_weight = randomsearch[1,]$min_child_weight,
                  nrounds= 1000,
                  eval_metric = "error",
            #      early_stopping_rounds= 30,
          #        print_every_n = 100,
                  watchlist = watchlist
)

bip_comp_no_shift <- bip_comp
bip_comp_no_shift$shift <- 0
bip_comp_no_shift_mat <- xgb.DMatrix(data=data.matrix(bip_comp_no_shift[, c(3:7,13)]))

pred.no.shift <- predict(mdcv, bip_comp_no_shift_mat, type="pred")

bip_comp_shift <- bip_comp
bip_comp_shift_mat <- xgb.DMatrix(data=data.matrix(bip_comp_shift[, c(3:7,13)]))
pred.shift <- predict(mdcv, bip_comp_shift_mat, type="pred")

bip_comp_pred <- bip_comp
bip_comp_pred$xBA <- pred.shift
bip_comp_pred$xBA.no.shift <- pred.no.shift

teams_colors_logos <- mlbplotR::load_mlb_teams() %>% 
  dplyr::filter(!team_abbr %in% c("AL", "NL", "MLB")) %>% 
  dplyr::mutate(
    a = rep(1:6, 5),
    b = sort(rep(1:5, 6), decreasing = TRUE),
    alpha = ifelse(grepl("A", team_abbr), 1, 0.75), # Keep alpha == 1 for teams that have an "A"
    color = ifelse(grepl("E", team_abbr), "b/w", NA) # Set teams that have an "E" to black & white
  )

babip_comp_sum <- bip_comp_pred %>% dplyr::select(h_team,game_year,shift, hit, xBA, xBA.no.shift) %>%
  dplyr::group_by(game_year,h_team) %>%
  dplyr::summarise(ab = dplyr::n(),
                   shifts = sum(shift),
                   hits = sum(hit),
                   xHits = sum(xBA),
                   xHits.no.shift = sum(xBA.no.shift)) %>%
  dplyr::mutate(BABIP = hits/ab,
                shift_pct = shifts/ab,
                xBABIP = xHits/ab,
                xBABIP.no.shift = xHits.no.shift/ab,
                diff = xBABIP.no.shift - xBABIP) %>%
  dplyr::arrange(desc(diff)) %>%
#  dplyr::filter(ab >= 50)  %>%
  left_join(teams_colors_logos, by = c("h_team" = "team_abbr"))

babip_comp_sum %>% filter(game_year == 2021) %>%
  ggplot2::ggplot(aes(x=xBABIP, xend=xBABIP.no.shift, y = h_team, group = h_team)) +
  ggalt::geom_dumbbell(aes(color = h_team),colour_x = "#a3c4dc", colour_xend = "#0e668b",size=2,
                       dot_guide = TRUE, dot_guide_size = .25) +
  mlbplotR::scale_color_mlb(type = "primary") +
  mlbplotR::scale_fill_mlb(alpha = .4) +
  ggplot2::labs(x = "xBABIP",
                y=NULL,
                title="xBABIP with Shift vs No Shift") +
  ggplot2::theme(
    axis.text.y = mlbplotR::element_mlb_logo(size = .65),
    panel.background = element_rect(fill = "azure"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "darkslategray3")
  ) 

babip_comp_sum %>% filter(game_year == 2021) %>%
  ggplot2::ggplot(aes(x=xBABIP, xend=xBABIP.no.shift, y = h_team, group = h_team)) +
  ggalt::geom_dumbbell(aes(color = h_team),colour_x = "#a3c4dc", colour_xend = team_color,size=2,
                       dot_guide = TRUE, dot_guide_size = .25) +
  mlbplotR::scale_color_mlb(type = "primary") +
  mlbplotR::scale_fill_mlb(alpha = .4) +
  ggplot2::labs(x = "xBABIP",
                y=NULL,
                title="xBABIP with Shift vs No Shift") +
  ggplot2::theme(
    axis.text.y = mlbplotR::element_mlb_logo(size = .65),
    panel.background = element_rect(fill = "azure"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "darkslategray3")
  ) 

babip_comp_sum_p <- bip_comp_pred %>% dplyr::select(p_team,game_year,shift, hit, xBA, xBA.no.shift) %>%
  dplyr::group_by(game_year,p_team) %>%
  dplyr::summarise(ab = dplyr::n(),
                   shifts = sum(shift),
                   hits = sum(hit),
                   xHits = sum(xBA),
                   xHits.no.shift = sum(xBA.no.shift)) %>%
  dplyr::mutate(BABIP = hits/ab,
                shift_pct = shifts/ab,
                xBABIP = xHits/ab,
                xBABIP.no.shift = xHits.no.shift/ab,
                diff = xBABIP.no.shift - xBABIP) %>%
  dplyr::arrange(desc(diff)) %>%
  #  dplyr::filter(ab >= 50)  %>%
  left_join(teams_colors_logos, by = c("p_team" = "team_abbr"))

saveRDS(babip_comp_sum, "app/data/babip_comp_sum.rds")
saveRDS(babip_comp_sum_p, "app/data/babip_comp_sum_p.rds")
