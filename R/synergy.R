dictionary_synergy_categories <-
  function() {
    data_frame(nameSynergy = c("Transition", "Isolation", "PRBallHandler", "PRRollman", "Postup", "Spotup", "Handoff", "Cut", "OffScreen", "OffRebound", "Misc"),
               nameTable = c("Transition", "Isolation", "Pick and Roll Ball Handler", "Pick and Roll Rollman", "Post Up", "Spot Up", "Handoff", "Cut", "OffScreen", "Off Rebound", "Misc"))
  }

get_synergy_category <-
  function(year_season_start = 2017,
         result_type = "player",
         season_type = "Regular Season",
         category = "transition",
         type = "offensive"
         ) {

    slug_season_type <-
      case_when(season_type %>% str_to_lower() %>% str_detect("regular") ~ "REG",
                TRUE ~ "Post")
    json_url <-
      glue::glue(
      "https://stats-prod.nba.com/wp-json/statscms/v1/synergy/{result_type}/?category={category}&season={year_season_start}&seasonType={slug_season_type}&names={type}&limit=10000"
    ) %>%
      as.character()

    json <-
      json_url %>%
      curl_json_to_vector()

    data <-
      json$results %>% as_data_frame()
  }
