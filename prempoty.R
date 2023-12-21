library(worldfootballR)
library(dplyr)
library(car)
library(corrplot)
library(gt)
total_stats <- data.frame()

for(year in 2018:2024) {
  norm_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "standard", team_or_player = "player")
  pos_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "possession", team_or_player = "player")
  shoot_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "shooting", team_or_player = "player")
  pass_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "passing", team_or_player = "player")
  def_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "defense", team_or_player = "player")
  gca_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "gca", team_or_player = "player")
  
  stats <- inner_join(norm_stats, shoot_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, pass_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, pos_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, def_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, gca_stats, by = c("Player", "Squad"))  
  
  stats <- stats %>%
    group_by(Squad) %>%
    filter(Pos.x != "GK", Comp.x == "Premier League", MP_Playing >= 0.75 * max(MP_Playing), !is.na(SoT_percent_Standard)) %>%
    ungroup() %>%
    mutate(season = year) %>%
    select(season, team = Squad, comp = Comp.x, player = Player, pos = Pos.x, goals = Gls_Standard, npxg = npxG_Expected.x, progdist = PrgDist_Total, cmp = Cmp_Total, ast = Ast.x, xag = xAG, key = KP, finalthird = Final_Third, progpass = PrgP, touches = Touches_Touches, attake = Att_Take, succtake = Succ_percent_Take, progdistcarr = PrgDist_Carries, progcarr = PrgC_Carries, finalthirdcarr = Final_Third_Carries, progrec = PrgR_Receiving, tkls = Tkl_Tackles, attchal = Att_Challenges, blks = Blocks_Blocks, int = Int, sca90 = SCA90_SCA, gca90 = GCA90_GCA)
  
  stats[is.na(stats)] <- 0
  
  league_stats <- fb_season_team_stats("ENG", "M", year, "1st", "league_table") %>%
    select(team = Squad, team_pts = Pts)
  
  stats <- left_join(stats, league_stats, by = "team")
  
  total_stats <- rbind(total_stats, stats)
}

total_stats <- total_stats %>%
  mutate(poty = 0)

total_stats$poty[which(total_stats$season == 2018 & total_stats$player == "Mohamed Salah")] <- 1
total_stats$poty[which(total_stats$season == 2019 & total_stats$player == "Virgil van Dijk")] <- 1
total_stats$poty[which(total_stats$season == 2020 & total_stats$player == "Kevin De Bruyne")] <- 1
total_stats$poty[which(total_stats$season == 2021 & total_stats$player == "Rúben Dias")] <- 1
total_stats$poty[which(total_stats$season == 2022 & total_stats$player == "Kevin De Bruyne")] <- 1
total_stats$poty[which(total_stats$season == 2023 & total_stats$player == "Erling Haaland")] <- 1

stats_train <- total_stats %>%
  filter(season != 2024)

stats_test <- total_stats %>%
  filter(season == 2024)

new_stats_train <- stats_train %>%
  select(-cmp, -finalthird, -progpass, -touches, -finalthirdcarr, -progrec, -succtake, -attchal, -gca90, -progcarr)

new_stats_test <- stats_test %>%
  select(-cmp, -finalthird, -progpass, -touches, -finalthirdcarr, -progrec, -succtake, -attchal, -gca90, -progcarr)

poty_reg <- glm(poty ~ ., data = new_stats_train[, c(length(new_stats_train), 6:(length(new_stats_train) - 1))], family = "binomial")

new_stats_train <- new_stats_train %>%
  ungroup() %>%
  mutate(prediction = predict(poty_reg, new_stats_train, type = "response")) %>%
  group_by(season) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(poty == 1, "WON", "")) %>%
  ungroup() 

new_stats_test <- new_stats_test %>%
  ungroup() %>%
  mutate(prediction = predict(poty_reg, new_stats_test, type = "response")) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  ungroup() 

final_data_top15_train <- new_stats_train %>%
  group_by(season) %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(season, player, team, pos, award_prob, award_won) %>%
  ungroup()

final_data_top15_2024 <- new_stats_test %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(player, team, pos, award_prob)

subfolder_path <- "poty/"
dir.create(subfolder_path, showWarnings = FALSE)

szns <- unique(final_data_top15_train$season)

for (szn in szns) {
  per_year <- final_data_top15_train %>%
    filter(season == szn)
  table <- per_year %>% gt() %>% 
    cols_align(
      align = "center",
      columns = c(season, player, team, pos, award_prob, award_won)
    ) %>%
    data_color(
      columns = award_prob,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    cols_label(
      season = md("**Season**"),
      player = md("**Player**"),
      team = md("**Team**"),
      pos = md("**Position**"),
      award_prob = md("**POTY Probability**"),
      award_won = md("**POTY Result**")
    ) 
  filename <- paste0(szn, "poty.png")
  gtsave(table, file.path(subfolder_path, filename))
}

table_2024 <- final_data_top15_2024 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, team, pos, award_prob)
  ) %>%
  data_color(
    columns = award_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    pos = md("**Position**"),
    award_prob = md("**POTY Probability**"),
  ) %>%
  tab_header(
    title = md("**2023-24 Premier League POTY Probability**"),
    subtitle = "Based on Prem POTY Data from 2017/18 - 2022/23"
  )
gtsave(table_2024, "poty/2024poty.png")
