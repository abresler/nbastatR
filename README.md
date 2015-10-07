# nbastatR
NBA Stats API Wrapper for R, formal description coming soon!!

This package is in it's extreme infancy but it should work if you have the necassary packages installed.  Formal vigentte's are coming but you please explore around.  Here is some sample code that should work for you.

```{r}
devtools::install_github("abresler/nbastatR")
library("nbastatR")
get_nba_days_scores("10/06/15")
get_nba_franchise_data(return_franchises = c('all'))
get_nba_players_ids(active_only = F)
get_nba_teams_seasons_roster(team = 'Nets', include_coaches = F)
get_team_season_shot_data(team = "Nets", year_data = 2015)
plot_nba_team_season_bokeh_shotchart(team = "Nets", year_roster = 2016,
                                          year_data = 2016, plot_hex = F,
                                          use_shot_zone_side = T,
                                          season_type = c('Pre Season'))
plot_nba_player_bokeh_shotchart(player = "Brook Lopez", plot_hex = F, vs_conference = 'East',
                                          year_season_end = 2015)
```
