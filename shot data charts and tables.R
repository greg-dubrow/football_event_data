## chart protoyping for shot data
# what's working:
# * shots: scatterplot dist to goal x xg, color body part, facet position
# * bar charts of avg xg by player country, position, team country

# still to try
# * shot map with left/right foot colored dots to see if more likely to shoot with on-wing foot?
# * bar charts of number of shots, goals by minute

library(tidyverse)
library(StatsBombR)
library(tidylog)
library(ggsoccer)
library(viridis)
library(ggforce)
library(patchwork)

## read in data files - might not need event & lineup data if shots correctly constructed
uclfinal_shots <- readRDS("data/uclfinal_shots.rds")

uclfinal_event <- readRDS("data/uclfinal_event.rds")
uclfinal_lineup <- readRDS("data/uclfinal_lineup.rds")


glimpse(uclfinal_shots)

uclfinal_event %>%
	count(match_id)

uclfinal_shots %>%
	count(shot.outcome.name)

# utility to get colors being used
gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]}
n = 2
cols = gg_color_hue(n)


# total & avg number of shots per game - bar chart
uclfinal_shots %>%
	filter(period < 3) %>%
	count(season,period) %>%
	group_by(period) %>%
	summarise(avg_shot_half = mean(n))

# overall and by game shot outcomes bar charts

uclfinal_shots %>%
	count(shot.statsbomb_xg) %>%
	view()


xgbymin1 <-
uclfinal_shots %>%
	filter(period == 1) %>%
	filter(shot.type.name != "Penalty") %>%
	mutate(goal = ifelse(shot.outcome.name == "Goal", "Goal", "No Goal")) %>%
	ggplot(aes(x = minute, y = shot.statsbomb_xg, color = goal)) +
	geom_point() +
	geom_smooth() +
	annotate("text", x = 5, y = 0.77, label = "1st half") +
	scale_x_continuous(limits = c(0, 50),
		breaks = seq(0, 45, by = 5)) +
	scale_y_continuous(limits = c(-0.025, 0.8)) +
	labs(x = "", y = "") +
	theme_minimal() +
	theme(legend.position = "none")

xgbymin2 <-
	uclfinal_shots %>%
	filter(period == 2) %>%
	filter(shot.type.name != "Penalty") %>%
	mutate(goal = ifelse(shot.outcome.name == "Goal", "Goal", "No Goal")) %>%
	ggplot(aes(x = minute, y = shot.statsbomb_xg, color = goal)) +
	geom_point() +
	geom_smooth() +
	annotate("text", x = 92, y = 0.77, label = "2nd half") +
	scale_x_continuous(limits = c(45, 100),
										 breaks = seq(45, 90, by = 5)) +
	scale_y_continuous(limits = c(-0.025, 0.8), labels = NULL) +
	labs(x = "", y = "")+
	theme_minimal() +
	theme(legend.position = "none")


#"#F8766D" "#00BFC4"
xgbymin <- xgbymin1 + xgbymin2 +
	plot_annotation(title = "xG Value by Minute", subtitle = "Goals in orange",
										theme = theme(plot.subtitle = element_text(colour = "#F8766D")))



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


