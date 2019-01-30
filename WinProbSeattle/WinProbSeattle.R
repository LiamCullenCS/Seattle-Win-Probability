library(nflscrapR)

week_16_games <- scrape_game_ids(2018, weeks = 16)

week_16_games %>% 
	pander::pander()

library(tidyverse)

sea_vs_kc_pbp <- week_16_games %>%
	filter(home_team == "SEA") %>%
	pull(game_id) %>%
	scrape_json_play_by_play()

library(teamcolors)

nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
sea_color <- nfl_teamcolors %>% 
	filter(name == "Seattle Seahawks") %>%
	pull(primary)
kc_color <- nfl_teamcolors %>%
	filter(name == "Kansas City Chiefs") %>%
	pull(primary)

sea_vs_kc_pbp %>%
	filter(!is.na(home_wp),
		 !is.na(away_wp)) %>%
	dplyr::select(game_seconds_remaining,
				home_wp,
				away_wp) %>%
	gather(team, wpa, -game_seconds_remaining) %>%
	ggplot(aes(x = game_seconds_remaining, y = wpa, color = team))+
	geom_line(size = 2) +
	geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") + 
	scale_color_manual(labels = c("KC", "SEA"),
				values = c(kc_color, sea_color),
				guide = FALSE) + 
	scale_x_reverse(breaks = seq(0, 3600, 300))+
	annotate("text", x = 3000, y = .85, label = "SEA", color = sea_color, size = 8)+
	annotate("text", x = 3000, y = .15, label = "KC", color = kc_color, size = 8)+
	geom_vline(xintercept = 900, linetype = "dashed", black) +
	geom_vline(xintercept = 1800, linetype = "dashed", black) +
	geom_vline(xintercept = 2700, linetype = "dashed", black) +
	geom_vline(xintercept = 0, linetype = "dashed", black)+
	labs(
		x = "Time Remaining (seconds)",
		y = "Win Probability", 
		title = "Week 16 Win Probability Chart",
		subtitle = "Seahawks vs Chiefs",
		caption = "Data from nflscrapR"
	) + theme_bw()
