# slugs -------------------------------------------------------------------

dictionary_boxscore_slugs <-
  function(){
    data_frame(nameSlug = c("traditional", "advanced", "scoring","misc", "usage", "four factors",
                            "hustle", "tracking", "winprob", "defense", "matchups"),
               slugNBA = c("boxscoretraditionalv2","boxscoreadvancedv2", "boxscorescoringv2", "boxscoremiscv2",
                           "boxscoreusagev2", "boxscorefourfactorsv2", "hustlestatsboxscore",
                           "boxscoreplayertrackv2", "winprobabilitypbp", "boxscoredefensive",
                           "boxscorematchups"),
               slugBase = c("traditional", "advanced", "scoring", "misc", "usage", "fourfactors",
                            "hustlestats", "playertrack", "winprob", "defense", "matchups")) %>%
      mutate(typeStatsCall = "boxscore")
  }


.get_box_score_type <-
  function(game_id = 21700865,
           league = "NBA",
           result_type = "player",
           boxscore = "traditional",
           return_message = T,
           ...) {

    type_slug <- str_to_lower(result_type)

    table_id <-
      case_when(type_slug %>% str_detect("player")~ 1,
               TRUE ~ 2)

    league_slug <- case_when(league %>% str_to_upper() == "WNBA" ~ "10",
                             league %>% str_to_upper() == "GLEAGUE" ~ "20",
                             TRUE ~ "00")

    if (boxscore %>% str_to_lower() %>% str_detect("hustle")) {
      table_id <- 2
    }

    if (league == "WNBA") {
      table_id <- case_when(result_type == "player" ~ 2,
                            TRUE ~ 3)
    }

    df_box_slugs <-
      dictionary_boxscore_slugs()

    box_score_slug <-
      df_box_slugs %>%
      filter(nameSlug %>% str_detect(boxscore %>% str_to_lower())) %>%
      pull(slugNBA)


    game_slug <-
      pad_id(game_id)

    json_url <-
      make_url(
        datatype = box_score_slug,
        GameID = game_slug,
        LeagueID = league_slug,
        StartPeriod = 0,
        EndPeriod = 12,
        StartRange = 0,
        EndRange = 12,
        RangeType = 1
      )

    if (return_message) {
      glue::glue("Getting {league} {result_type} {boxscore} box score for game {game_id}") %>% message()
    }

    if (league == "WNBA") {

      json_url <- case_when(
        boxscore %>% str_to_lower() == "advanced" ~ glue::glue(
          "https://stats.nba.com/stats/wnbaadvancedboxscore?GameID={game_id}"
        ),
        TRUE ~ glue::glue(
          "http://data.wnba.com/data/5s/v2015/json/mobile_teams/wnba/2018/scores/gamedetail/{game_id}_gamedetail.json"
        )
      ) %>%
        URLencode() %>%
        as.character()
    }

    json <-
      json_url  %>%
      curl_json_to_vector()

    if (names(json) %>% str_detect( "g") %>% sum(na.rm = T) > 0) {
      json_data <- json$g
      df_classes <-
        json_data %>% map_df(class) %>%
        gather(column, class)

      base_cols <-
        df_classes %>%
        filter(!class == "list") %>%
        pull(column)

      df_base <-
        json_data[base_cols] %>% flatten_df()

      df_base <-
        df_base %>%
        purrr::set_names(names(df_base) %>%
        resolve_nba_names()) %>%
        munge_nba_data() %>%
        mutate(dateGame = lubridate::ymd(dateGame))

      list_cols <-
        df_classes %>%
        filter(class == "list") %>%
        pull(column)

      list_cols <-
        list_cols[list_cols %in% c("vls", "hls")]

      all_data <-
        list_cols %>%
        map_df(function(col){
          d <-
            json_data[[col]]
          if (col == "offs"){
            d <- d %>% flatten_df()

            d <-
              d %>%
              purrr::set_names(names(d) %>% resolve_nba_names()) %>%
              unite(nameOfficial, nameFirst, nameLast, sep = " ") %>%
              mutate(idGame = game_id) %>%
              rename(numberJerseyOfficial = numberJersey) %>%
              munge_nba_data()
            return(d)
          }
            df_base <-
              d[c("ta", "tn", "tid", "tc")] %>%
              flatten_df()

            df_base <-
              df_base %>%
              purrr::set_names(names(df_base) %>% resolve_nba_names()) %>%
              unite(nameTeam, cityTeam, teamName, sep = " ") %>%
              mutate(idGame = game_id)

            df_stats <-
              d$pstsg %>% as_data_frame()

            df_stats <-
              df_stats %>%
              purrr::set_names(names(df_stats) %>% resolve_nba_names()) %>%
              munge_nba_data() %>%
              unite(namePlayer, nameFirst, nameLast, sep = " ") %>%
              mutate(idGame = game_id)

            df_stats <-
              df_stats %>%
              left_join(df_base) %>%
              select(one_of(names(df_base)), everything()) %>%
              suppressMessages() %>%
              rename(slugPositionStarter = slugPosition)

            df_stats



        })

      df_officials <-
        json_data$offs %>% flatten_df() %>%
        purrr::set_names(c("nameFirst", "nameLast", "numberJerseyRef")) %>%
        unite(nameReferee, nameFirst, nameLast, sep = " ") %>%
        mutate(idGame = game_id)

      all_data <-
        all_data %>%
        left_join(df_officials) %>%
        left_join(df_base) %>%
        select(one_of(names(df_base)), everything()) %>%
        suppressMessages()

      data <-
        all_data %>%
        mutate(slugLeague = league) %>%
        nest(-c(idGame, slugLeague), .key = 'dataBoxScore') %>%
        mutate(typeResult = result_type,
               typeBoxScore = boxscore) %>%
        mutate(cols = dataBoxScore %>% map_dbl(ncol)) %>%
        filter(cols > 1) %>%
        select(-cols) %>%
        select(typeBoxScore, typeResult, everything())
      return(data)
    }

    data <-
      json$resultSets$rowSet[[table_id]] %>%
      as_data_frame()

    if (data %>% nrow() == 0) {
      return(invisible())
    }

    json_names <-
      json$resultSets$headers[[table_id]]

    actual_names <-
      json_names %>% resolve_nba_names()

    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      munge_nba_data()


    if (table_id == 2 &
        data %>% tibble::has_name("nameTeam") &
        league %>% str_to_upper() == "NBA") {
      data <-
        data %>%
        munge_nba_data() %>%
        unite(nameTeam,
              cityTeam,
              teamName,
              sep =  " ",
              remove = F)
    }

    if (boxscore %>% str_to_lower() %>% str_detect("defense")) {
      names(data)[names(data) %in% c(
        "fgm",
        "fga",
        "pctFG",
        "fg3m",
        "fg3a",
        "pctFG3",
        "fgmContested",
        "fgaContested",
        "pctFGContested",
        "fgm3mConested",
        "fgm3Contested",
        "pctFG3M",
        "fg2m",
        "fg2a",
        "ast",
        "ftm"
      )] <-
        names(data)[names(data) %in% c(
          "fgm",
          "fga",
          "pctFG",
          "fg3m",
          "fg3a",
          "pctFG3",
          "fgmContested",
          "fgaContested",
          "pctFGContested",
          "fgm3mConested",
          "fgm3Contested",
          "pctFG3M",
          "fg2m",
          "fg2a",
          "ast",
          "ftm"
        )] %>% str_c(., "Allowed")

      if (data %>% tibble::has_name("tov")) {
        data <-
          data %>%
          dplyr::rename(tovForced = tov)
      }

      if (data %>% tibble::has_name("possessions")) {
        data <-
          data %>%
          dplyr::rename(possessionsDefense = possessions)
      }
    }
    if (league == "WNBA" && table_id == 2) {
      df_team <-
        json$resultSets$rowSet[[3]] %>%
        as_data_frame()


      json_names <-
        json$resultSets$headers[[3]]

      actual_names <-
        json_names %>% resolve_nba_names()

      df_team <-
        df_team %>%
        purrr::set_names(actual_names) %>%
        munge_nba_data()

      data <-
        data %>%
        left_join(
          df_team %>% unite(nameTeam, nameCityTeam, teamName, sep = " ") %>%
            select(idTeam, nameTeam, slugTeam, locationGame)
        ) %>%
        select(-one_of(c(
          "typeBoxScore", "typeDataSet", "orderBoxScore"
        ))) %>%
        select(idGame, slugTeam, nameTeam, locationGame, everything()) %>%
        mutate(isStarter = !is.na(groupPosition)) %>%
        suppressMessages()

    }


    if (boxscore %>% str_to_lower() %>% str_detect("matchup")) {
      data <-
        data %>%
        unite(nameTeamOffense, cityTeamOffense, nicknameTeamOffense, sep = " ") %>%
        unite(nameTeamDefense, cityTeamDefense, nicknameTeamDefense, sep = " ") %>%
        dplyr::rename(pfShootingDrawn = pfShootingCommitted)
    }

    data <-
      data %>%
      mutate(slugLeague = league) %>%
      nest(-c(idGame, slugLeague), .key = 'dataBoxScore') %>%
      mutate(typeResult = result_type,
             typeBoxScore = boxscore) %>%
      mutate(cols = dataBoxScore %>% map_dbl(ncol)) %>%
      filter(cols > 1) %>%
      select(-cols) %>%
      select(typeBoxScore, typeResult, everything())
    gc()
    data
  }

#' NBA box scores
#'
#' Acquires specified box score type by game ID
#'
#' @param game_ids vector of game ids
#' @param box_score_types vector of box score types options include \itemize{
#' \item traditional
#' \item advanced
#' \item scoring
#' \item misc
#' \item usage
#' \item four factors
#' \item tracking
#' \item defense
#' \item matchups
#' \item hustle
#' }
#' @param join_data if \code{TRUE} joins the underlying table data
#' @param result_types vector of result types options include \itemize{
#' \item team - Team statistics
#' \item player - player
#' }
#' @param assign_to_environment if \code{TRUE} assigns a data frame for each table to the environment
#' starting with data
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame}
#' @family game
#' @export
#' @import dplyr curl stringr lubridate readr magrittr tidyr httr purrr jsonlite
#' @importFrom glue glue
#' @examples
#' get_games_box_scores(game_ids = c(21700002, 21700003), box_score_types = c("Traditional", "Advanced", "Scoring", "Misc", "Usage", "Four Factors", "Tracking"), result_types = c("player", "team"), join_data = TRUE, assign_to_environment = TRUE, return_message = TRUE)

get_games_box_scores <-
  function(game_ids = NULL,
           league = "NBA",
           box_score_types = c("Traditional", "Advanced", "Scoring","Misc", "Usage", "Four Factors",
                               "hustle", "tracking"),
           result_types = c("player", "team"),
           join_data = TRUE,
           assign_to_environment = TRUE,
           return_message = TRUE) {
    if (game_ids %>% purrr::is_null()) {
      stop("Please enter game ids")
    }

    if (league == "WNBA") {
      box_score_types <- str_to_lower(box_score_types)
      result_types <- "player"
      box_score_types <- box_score_types[box_score_types %in% c("traditional", "advanced")]
    }

    input_df <-
      expand.grid(game_id = game_ids,
                  result_type = result_types,
                  boxscore = box_score_types,
                  stringsAsFactors = F) %>%
      dplyr::as_data_frame()



    get_box_score_type_safe <-
      purrr::possibly(.get_box_score_type, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <-
          input_df %>% slice(x)

        df_row %$%
          .get_box_score_type(
            game_id = game_id,
            result_type = result_type,
            boxscore = boxscore,
            league = league,
            return_message = return_message
          )
      })

    if (join_data  &&
        league == "NBA") {
      results <-
        all_data$typeResult %>% unique()

      all_data <-
        results %>%
        map_df(function(result) {
          df_results <-
            all_data %>%
            filter(typeResult == result)
          tables <-
            df_results$typeBoxScore %>% unique()
          all_tables <-
            tables %>%
            map(function(table) {
              data <-
                df_results %>%
                filter(typeBoxScore == table) %>%
                select(idGame, dataBoxScore) %>%
                unnest()


              if (table == "Usage") {
                data <-
                  data %>%
                  dplyr::select(-one_of("pctUSG"))
              }

              if (table == "Tracking") {
                data <-
                  data %>%
                  dplyr::select(-one_of("pctFG"))
              }

              if (table == "Four Factors") {
                data <-
                  data %>%
                  dplyr::select(-one_of(c(
                    "pctOREB", "pctTOVTeam", "pctEFG"
                  )))
              }
              data
            })

          all_tables <-
            all_tables %>%
            purrr::reduce(left_join) %>%
            suppressMessages()

          if (result == "player") {
            all_tables <-
              all_tables %>%
              mutate(isStarter = !groupStartPosition %>% is.na()) %>%
              dplyr::select(idGame:groupStartPosition, isStarter, everything())
          }
          data_frame(typeResult = result,
                     dataBoxScore = list(all_tables))
        })

      if (assign_to_environment) {
        all_data$typeResult %>%
          walk(function(result) {
            table_name <-
              glue::glue("dataBoxScore{str_to_title(result)}{str_to_upper(league)}")
            df_table <-
              all_data %>%
              filter(typeResult == result) %>%
              select(-typeResult) %>%
              unnest()

            assign(x = table_name,
                   value = df_table,
                   envir = .GlobalEnv)
          })
      }
    } else {
      if (assign_to_environment) {
        results <-
          all_data$typeResult %>%
          unique()
        results %>%
          walk(function(result) {
            df_tables <-
              all_data %>%
              filter(typeResult == result) %>%
              select(-typeResult)
            data <-
              df_tables$typeBoxScore %>%
              unique() %>%
              map(function(type) {
                df_table <-
                  df_tables %>%
                  filter(typeBoxScore == type) %>%
                  tidyr::unnest(.drop = T) %>%
                  select(-typeBoxScore)
                type_slug <-
                  type %>% str_split("\\ ") %>% flatten_chr() %>%
                  str_to_title() %>% str_c(collapse = "")

                table_name <-
                  glue::glue('dataBoxScore{type_slug}{str_to_title(result)}{str_to_upper(league)}') %>% as.character()

                assign(x = table_name, df_table, envir = .GlobalEnv)
              })
          })
      }
    }
    all_data
  }
