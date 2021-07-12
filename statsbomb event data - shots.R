# full data dictionary here http://statsbomb.com/wp-content/uploads/2019/12/Using-StatsBomb-Data-In-R.pdf

# what's working:
# * shots: scatterplot dist to goal x xg, color body part, facet position
# * bar charts of avg xg by player country, position, team country

# still to try
# * shot map with left/right foot colored dots to see if more likely to shoot with on-wing foot?
# * bar charts of number of shots, goals by minute
# * avgxg and dist to goal by team ahead, behind or tie?
# * avg xg by winning & losing team
# * avg xg by number of passes?

library(tidyverse)
library(StatsBombR)
library(tidylog)
library(ggsoccer)
library(viridis)
library(ggforce)


allcomps <- FreeCompetitions()
glimpse(allcomps)

allcomps %>%
  count(competition_name, season_name)


allmatches <- FreeMatches(allcomps)

allmatches <- allclean(allmatches)

glimpse(allmatches)

glimpse(allmatches$home_team.managers)

allmatches %>%
  filter(competition.competition_name == "Champions League") %>%
  select(season.season_name, competition_stage.name) %>%
  arrange(season.season_name)


allmatches %>%
  filter(competition.competition_name == "Champions League") %>%
  count(home_team.home_team_name, away_team.away_team_name, match_date) %>%
  arrange(match_date)

# match data for ucl finals
uclfinal_c <- allcomps %>%
  filter(competition_name == "Champions League")
glimpse(uclfinal_c)

uclfinal_m <- FreeMatches(uclfinal_c)
glimpse(uclfinal_m)

uclfinal_ta <- uclfinal_m %>%
  select(team.id = away_team.away_team_id, team_country = away_team.country.name)

uclfinal_th <- uclfinal_m %>%
  select(team.id = home_team.home_team_id, team_country = away_team.country.name)

uclfinal_t <- rbind(uclfinal_ta, uclfinal_th) %>%
  distinct(team.id, .keep_all = T)

# gets event data for matches
uclfinal_e <- StatsBombFreeEvents(MatchesDF = uclfinal_m, Parallel = T)

  # cleans data, expands lists
uclfinal_e <- allclean(uclfinal_e)
uclfinal_e <- uclfinal_e %>%
  arrange(match_id, period, minute, second)

glimpse(uclfinal_e)

# save event file so don't have to rebuild
saveRDS(uclfinal_e, "data/uclfinal_event.rds")

uclfinal_e %>%
  select(match_id, period, minute, second, team.name, possession_team.name, type.name,
         player.name, position.name,
         location.x, location.y, pass.length) %>%
  view()

uclfinal_e %>%
  count(type.name) %>%
  view()

uclfinal_e %>%
  select(match_id, period, minute, second, team.name, possession_team.name, type.name,
         possession, duration,
         ElapsedTime, StartOfPossession, TimeInPoss, TimeToPossEnd) %>%
  View()

### get lineup, player position data, minutes played
 # position data
uclfinal_p <- get.positioncategory(uclfinal_e) %>%
  rename(player_id = player.id) %>%
  filter(!is.na(player_id))
glimpse(uclfinal_p)

# minutes played
uclfinal_mp <- get.minutesplayed(uclfinal_e) %>%
  rename(player_id = player.id)
glimpse(uclfinal_mp)

# select match info
uclfinal_m2 <- uclfinal_m %>%
  select(match_id, match_date, season = season.season_name,
         home_score, away_score)


# lineup - join with position & minutes & match info
uclfinal_lu1 <- StatsBombFreeLineups(MatchesDF = uclfinal_m, Parallel = T)
uclfinal_lu <- cleanlineups(uclfinal_lu1) %>%
  mutate(player_nickname = ifelse(is.na(player_nickname), player_name, player_nickname)) %>%
  left_join(uclfinal_p) %>%
  left_join(uclfinal_mp) %>%
  left_join(uclfinal_m2)


uclfinal_lu <- uclfinal_lu %>%
  left_join(uclfinal_p)

glimpse(uclfinal_lu)

# save lineup so don't have to rebuild
saveRDS(uclfinal_lu, "data/uclfinal_lineup.rds")


uclfinal_lu %>%
  count(TypicalPosition)

### shot info...do a goals timeline, shots from longest distance, avg/med dist, type
  # get player data
uclfinal_lu_ext <- uclfinal_lu %>%
  select(player.id = player_id, player_nickname, jersey_number, TypicalPosition,
         player_country = country.name) %>%
  filter(jersey_number > 0) %>%
  group_by(player.id) %>%
  slice_min(jersey_number, with_ties = FALSE)

glimpse(uclfinal_lu_ext)

uclfinal_lu_ext %>%
  count(player_nickname, jersey_number) %>%
  arrange(player_nickname, jersey_number) %>%
  view()

# top-line match data (date, teams)
glimpse(uclfinal_m)

library(glue)
uclfinal_m_ext <- uclfinal_m %>%
  mutate(season = season.season_name) %>%
  mutate(teams = glue("{home_team.home_team_name} vs {away_team.away_team_name}")) %>%
  mutate(score = glue("{home_score} - {away_score}")) %>%
  select(match_id, season, match_date, teams, score)
glimpse(uclfinal_m_ext)

uclfinal_m_ext %>%
  count(teams, season) %>%
  arrange(season)

glimpse(uclfinal_t)
# get shot data from events, match with player data
uclfinal_shots <- uclfinal_e %>%
  filter(type.name == "Shot") %>%
  mutate(period = as.integer(period)) %>%
  mutate(halfminute = case_when(period == 1L ~ minute,
                                period == 2L ~ (minute - 45L),
                                period == 3L ~ (minute - 90L),
                                period == 4L ~ (minute - 105L),
                                TRUE ~ minute)) %>%
  mutate(shot.body_part2 = case_when(shot.body_part.name %in% c("Left Foot", "Right Foot") ~ "Foot",
                                     TRUE ~ shot.body_part.name)) %>%
  select(match_id, period, minute, second, halfminute, timestamp, duration,
         team.id, team.name, player.id, player.name, position.name,
         shot.type.name, shot.body_part.name, shot.body_part2,
         shot.first_time, shot.aerial_won, shot.open_goal,
         shot.deflected, shot.technique.name, shot.outcome.name,
         shot.statsbomb_xg, location.x, location.y, shot.end_location.x, shot.end_location.y, shot.end_location.z,
         play_pattern.name, DistToGoal:avevelocity) %>%
  left_join(uclfinal_lu_ext) %>%
  left_join(uclfinal_t) %>%
  left_join(uclfinal_m_ext)

glimpse(uclfinal_shots)

## save shots so no need to rebuild
saveRDS(uclfinal_shots, "data/uclfinal_shots.rds")


uclfinal_shots %>%
  filter(period > 2) %>%
  count(period, minute, halfminute) %>%
  view()

uclfinal_shots %>%
  count(shot.type.name, play_pattern.name)


uclfinal_shots %>%
  filter(period < 3) %>%
  filter(shot.type.name != "Penalty") %>%
  ggplot(aes(x = halfminute, y = shot.statsbomb_xg)) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ period)

uclfinal_shots %>%
  filter(period %in% c(3, 4)) %>%
  filter(shot.type.name != "Penalty") %>%
  ggplot(aes(x = halfminute, y = shot.statsbomb_xg)) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ period)

uclfinal_shots %>%
  filter(shot.type.name != "Penalty") %>%
  filter(DistToGoal < 50) %>%
  ggplot(aes(x = DistToGoal, y = shot.statsbomb_xg, color = TypicalPosition)) +
  geom_point()

uclfinal_shots %>%
  filter(shot.type.name != "Penalty") %>%
  filter(DistToGoal < 50) %>%
  ggplot(aes(x = DistToGoal, y = shot.statsbomb_xg, color = shot.body_part2)) +
  geom_point() +
  facet_wrap(~ TypicalPosition)


uclfinal_shots %>%
  group_by(period) %>%
  summarise(avgxg = mean(shot.statsbomb_xg),
            n=n()) %>%
  ungroup()

uclfinal_shots %>%
  group_by(team.name) %>%
  summarise(avgxg = mean(shot.statsbomb_xg),
            n=n()) %>%
  ungroup() %>%
  arrange(desc(avgxg))

uclfinal_shots %>%
  filter(shot.type.name != "Penalty") %>%
  group_by(TypicalPosition) %>%
  summarise(avgxg = mean(shot.statsbomb_xg),
            n=n()) %>%
  ungroup() %>%
  arrange(desc(avgxg))

uclfinal_shots %>%
  filter(shot.type.name != "Penalty") %>%
  group_by(player_country) %>%
  summarise(avgxg = mean(shot.statsbomb_xg),
            n=n()) %>%
  ungroup() %>%
  filter(n > 4) %>%
  arrange(desc(avgxg)) %>%
  view()

uclfinal_shots %>%
  filter(shot.type.name != "Penalty") %>%
  group_by(team_country) %>%
  summarise(avgxg = mean(shot.statsbomb_xg),
            n=n()) %>%
  ungroup() %>%
  arrange(desc(avgxg))

uclfinal_shots %>%
  filter(shot.type.name != "Penalty") %>%
  group_by(shot.technique.name) %>%
  summarise(avgxg = mean(shot.statsbomb_xg),
            n=n()) %>%
  ungroup() %>%
  arrange(desc(avgxg))



uclfinal_shots %>%
  filter(period < 3) %>%
  filter(shot.type.name != "Penalty") %>%
  ggplot(aes(x = halfminute, y = shot.statsbomb_xg, color = period)) +
  geom_point()

  ## scatterplot
