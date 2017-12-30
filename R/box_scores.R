# slugs -------------------------------------------------------------------

dictionary_boxscore_slugs <-
  function(){
    data_frame(nameSlug = c("traditional", "advanced", "scoring","misc", "usage", "four factors",
                            "hustle", "tracking", "winprob"),
               slugNBA = c("boxscoretraditionalv2","boxscoreadvancedv2", "boxscorescoringv2", "boxscoremiscv2",
                           "boxscoreusagev2", "boxscorefourfactorsv2", "hustlestatsboxscore",
                           "boxscoreplayertrackv2", "winprobabilitypbp"),
               slugBase = c("traditional", "advanced", "scoring", "misc", "usage", "fourfactors",
                            "hustlestats", "playertrack", "winprob")) %>%
      mutate(typeStatsCall = "boxscore")
  }


get_box_score_type <-
  function(game_id = 21601112,
           result_type = "player",
           boxscore = "traditional",
           return_message = T,
           ...) {

    type_slug <- str_to_lower(result_type)

    table_id <-
      case_when(type_slug %>% str_detect("player")~ 1,
               TRUE ~ 2)

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
        StartPeriod = 0,
        EndPeriod = 12,
        StartRange = 0,
        EndRange = 12,
        RangeType = 1
      )

    if (return_message) {
      glue::glue("Getting {result_type} {boxscore} box score for game {game_id}") %>% message()
    }

    json <-
      json_url  %>%
      curl_json_to_vector()

    data <-
      json$resultSets$rowSet[[table_id]] %>%
      as_data_frame()

    json_names <-
      json$resultSets$headers[[table_id]]

    actual_names <-
      json_names %>% resolve_nba_names()

    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      munge_nba_data()

    if (table_id == 2) {
      data <-
        data %>%
        munge_nba_data() %>%
        unite(nameTeam,
              cityTeam,
              teamName,
              sep =  " ",
              remove = F)
    }

    data <-
      data %>%
      nest(-c(idGame), .key = 'dataBoxScore') %>%
      mutate(typeResult = result_type,
             typeBoxScore = boxscore) %>%
      mutate(cols = dataBoxScore %>% map_dbl(ncol)) %>%
      filter(cols > 1) %>%
      select(-cols) %>%
      select(typeBoxScore, typeResult, everything())

    data
  }

#' Get boxscores by Game ID
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
#' }
#' @param result_types vector of result types options include \itemize{
#' \item team - Team statistics
#' \item player - player
#' }
#' @param assign_to_environment if \code{TRUE} assigns a data frame for each table to the environment
#' starting with data
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr curl stringr lubridate readr magrittr tidyr httr purrr jsonlite
#' @importFrom glue glue
#' @examples
#' get_games_play_by_play(game_ids = c(21700002, 21700003), box_score_types = c("traditional", "advanced", "scoring","misc", "usage", "four factors", "tracking"), result_types = c("player", "team"), nest_data = FALSE, assign_to_environment = TRUE, return_message = TRUE)
get_games_box_scores <-
  function(game_ids = c(21700002, 21700003),
           box_score_types = c("traditional", "advanced", "scoring","misc", "usage", "four factors",
                               "hustle", "tracking"),
           result_types = c("player", "team"),
           assign_to_environment = TRUE,
           return_message = TRUE) {
    if (game_ids %>% purrr::is_null()) {
      stop("Please enter game ids")
    }
    input_df <-
      expand.grid(game_id = game_ids,
                  result_type = result_types,
                  boxscore = box_score_types,
                  stringsAsFactors = F) %>%
      dplyr::as_data_frame()



    get_box_score_type_safe <-
      purrr::possibly(get_box_score_type, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
      df_row <-
        input_df %>% slice(x)

      df_row %$%
        get_box_score_type_safe(
          game_id = game_id,
          result_type = result_type,
          boxscore = boxscore,
          return_message = return_message
        )
    })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(idGame), .key = 'dataPlayByPlay')
    }


    if (assign_to_environment) {
      results <-
        all_data$typeResult %>%
        unique()
    results %>%
      walk(function(result){
        df_tables <-
          all_data %>%
          filter(typeResult == result) %>%
          select(-typeResult)
        data <-
          df_tables$typeBoxScore %>%
          unique() %>%
          map(function(type){
            df_table <-
              df_tables %>%
              filter(typeBoxScore == type) %>%
              tidyr::unnest(.drop = T) %>%
              select(-typeBoxScore)

            table_name <-
              glue::glue('data{str_to_title(result)}{str_to_title(type)}BoxScore') %>% as.character()

            assign(x = table_name, df_table, envir = .GlobalEnv)
          })
      })
    }
    all_data
  }
