# nbastatR
An interface for professional basketball data in R.  Data sources include, but are not limited to: NBA Stats API, Basketball Insiders, Basketball-Reference, HoopsHype, and RealGM.  Overtime additional data sources will be added.


## Installation
```r
devtools::install_github("abresler/nbastatR")
library("nbastatR") # note requires a bunch of other packages which are listed in the import
```

### Relaunch Coming Soon

* Documentation too


## Other Functionality
```r
get_nba_franchise_data(return_franchises = "all")
get_nba_team_stat_table(year.season_start = 2015,
                        period = 4,
                        division_against = "Atlantic",
                        outcome = "W")
get_nba_player_injuries(filter_returning_today = T)

```

## NBA Draft Functions
```r
get_year_draft_combine(combine_year = 2009, return_message = T)
get_all_draft_combines(combine_years = 2000:2015) #draft combines
get(draft_years = 1960:2015) # drafts
```

## Salary Cap Data Example

```r
all_salaries <- 
  get_all_team_salaries()

non_guaranteed_players <- 
  all_salaries %>% 
  dplyr::filter(id.season %in% c("2015-16", "2016-17"),
                is.non.guaranteed == T)

non_guaranteed_players %>% 
  View()

team_2017_non_guaranteed_players <- 
  non_guaranteed_players %>% 
  group_by(id.season, team) %>% 
  summarise(players = n(),
            total.non_guaranteed.salary = sum(value, na.rm = T) %>% formattable::currency(digits = 0)
            ) %>% 
  ungroup() %>% 
  arrange(desc(total.non_guaranteed.salary))

team_2017_non_guaranteed_players %>% 
  View()
```


## Mock Draft
```r
draft_2016 <- 
  get_nba_draftnet_year_mock_draft(draft_year = 2016)

mocks_09_15 <- 
  get_nba_draftnet_years_mock_draft(draft_years = 2009:2015)
```
