
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





