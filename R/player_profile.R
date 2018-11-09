

# awards ------------------------------------------------------------------


.get_player_award <-
  function(player_id = 76003,
           return_message = T) {
    url <-
      glue::glue("https://stats.nba.com/stats/playerawards/?playerId={player_id}") %>%
      as.character()

    json <-
      url  %>%
      curl_json_to_vector()

    data <-
      json$resultSets$rowSet[[1]] %>%
      data.frame(stringsAsFactors = F) %>%
      dplyr::as_data_frame()

    if (data %>% ncol() == 0) {
      return(invisible())
    }
    data <-
      data %>%
      purrr::set_names(
        c(
          "idPlayer",
          "nameFirst",
          "nameLast",
          "nameTeam",
          "nameAward",
          "numberTeamAward",
          "slugSeason",
          "dateMonthAward",
          "dateWeekAward",
          "idTeam",
          "typeItem",
          "sponsorAward",
          "slugAward",
          "otherAward"
        )
      ) %>%
      tidyr::unite(namePlayer, nameFirst, nameLast, sep = " ")

    data <-
      data %>%
      mutate_at(c("idPlayer", "numberTeamAward", "idTeam"),
                funs(. %>% as.numeric())) %>%
      suppressWarnings() %>%
      mutate(
        dateMonthAward = lubridate::mdy(dateMonthAward),
        dateWeekAward = readr::parse_datetime(dateWeekAward) %>% as.Date()
      ) %>%
      arrange(slugSeason) %>%
      remove_na_columns()

    if (return_message) {
      glue::glue("Acquired {nrow(data)} awards for {data$namePlayer %>% unique()}") %>% cat(fill = T)
    }
    data
  }


#' NBA players awards
#'
#' Gets NBA awards for
#' specified players
#'
#' @param players vector of players
#' @param player_ids vector of player ids
#' @param nest_data if \code{TRUE} returns nested data_frame
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame}
#' @family player
#' @family awards
#' @export
#'
#' @examples
#' players_awards(players = c( "Charles Oakley", "Gary Melchionni"),
#' player_ids = c(893, 76375),
#' return_message = T,
#'  nest_data = F)

players_awards <-
  function(players =  NULL,
           player_ids = NULL,
           nest_data = F,
           return_message = TRUE) {
    assign_nba_players()
    ids <-
      nba_player_ids(player_ids = player_ids,
                          players = players)
    get_player_award_safe <-
      purrr::possibly(.get_player_award, data_frame())

    all_data <-
      ids %>%
      future_map_dfr(function(id) {
        get_player_award_safe(player_id = id, return_message = return_message)
      })
    if (all_data %>% tibble::has_name("datetimePublished")) {
    all_data <-
      all_data %>%
      arrange(datetimePublished)
    }

    all_data <-
      all_data %>%
      left_join(df_nba_player_dict %>% dplyr::select(idPlayer, dplyr::matches("url"))) %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate(nameAwardFull = ifelse(
        numberTeamAward %>% is.na(),
        nameAward,
        str_c(nameAward, numberTeamAward, sep =  " ")
      )) %>%
      dplyr::select(idPlayer:nameAward, nameAwardFull, everything())

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(
          -c(
            idPlayer,
            nameTeam,
            namePlayer,
            urlPlayerActionPhoto,
            urlPlayerStats,
            urlPlayerThumbnail,
            urlPlayerHeadshot
          ),
          .key = 'dataPlayerAwards'
        ) %>%
        mutate(countAwards = dataPlayerAwards %>% map_dbl(nrow))
    }
    all_data
  }

# bios --------------------------------------------------------------------


.get_player_bio <-
  function(player_id = 101127,
           return_message = T) {
    url <-
      glue::glue("https://data.nba.net/json/bios/player_{player_id}.json") %>%
      as.character()


    json <-
      url  %>%
      curl() %>%
      jsonlite::fromJSON(simplifyVector = T)

    data <-
      json$Bio %>% flatten_df()


    data <-
      data %>%
      purrr::set_names(
        c(
          "idPlayer",
          "typeResult",
          "nameDisplay",
          "htmlPlayerBio",
          "nameCollge",
          "nameHighSchool",
          "nameTwitter",
          "labelOther",
          "textOther"
        )
      ) %>%
      mutate(idPlayer = idPlayer %>% as.numeric()) %>%
      mutate(textBio = htmlPlayerBio %>% map_chr(function(x) {
        x %>% read_html() %>% html_text() %>% str_trim()
      })) %>%
      dplyr::select(-htmlPlayerBio) %>%
      tidyr::separate(nameDisplay,
                      into = c("nameLast", "nameFirst"),
                      sep = "\\, ") %>%
      tidyr::unite(namePlayer, nameFirst, nameLast, sep = " ") %>%
      mutate_if(is.character,
                funs(ifelse(. == "", NA_character_, .))) %>%
      remove_na_columns() %>%
      dplyr::select(idPlayer, namePlayer, everything())

    if (return_message) {
      glue::glue("Acquired {data$namePlayer} 2013-14 bio") %>% cat(fill = T)
    }


    data

  }

#' NBA.com bios
#'
#' Player biographies.
#'
#' Seems to only work for the 2013-14 season
#'
#' @param players vector of players
#' @param player_ids  vector of player ids
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} returns nested data_frame
#'
#' @return a `data_frame`
#' @export
#' @family biography
#' @family players
#' @import dplyr curl purrr jsonlite tidyr readr
#' @importFrom glue glue
#' @examples
#' players_bios(players = c("Carmelo Anthony", "Joe Johnson"))
players_bios <-
  function(players = NULL,
           player_ids = NULL,
           nest_data = F,
           return_message = TRUE) {
    ids <-
      nba_player_ids(player_ids = player_ids,
                          players = players)
    get_player_bio_safe <-
      purrr::possibly(.get_player_bio, data_frame())

    all_data <-
      ids %>%
      future_map_dfr(function(id) {
        get_player_bio_safe(player_id = id, return_message = return_message)
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(idPlayer, namePlayer, typeResult), .key = dataBio)
    }

    all_data

  }


# profiles ----------------------------------------------------------------


.get_player_profile <-
  function(player_id = 1628378,
           return_message = T) {
    if (player_id %>% purrr::is_null()) {
      stop("Pleas enter player id")
    }
    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    player <-
      df_nba_player_dict %>%
      filter(idPlayer == player_id) %>%
      pull(namePlayer)

    if (return_message) {
      glue::glue("Acquiring {player} NBA player profile") %>% cat(fill = T)
    }

    url_json <-
      glue::glue(
        'https://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID={player_id}'
      ) %>%
      as.character()
    ## Build URL
    json <-
      curl(url_json) %>%
      fromJSON(simplifyVector = T)

    table_length <-
      json$resultSets$rowSet %>% length()

    all_data <-
      1:table_length %>%
      future_map_dfr(function(table_id) {
        table_name <-
          json$resultSets$name[table_id]

        df_table <-
          json %>%
          nba_json_to_df(table_id = table_id) %>%
          mutate(numberTable = table_id) %>%
          select(numberTable, everything())

        df_table <-
          df_table %>%
          suppressMessages() %>%
          select(-numberTable)

        if (!df_table %>% tibble::has_name("idPlayer")) {
          df_table <-
            df_table %>%
            mutate(idPlayer = player_id)
        }
        df_table <-
          df_table %>%
          mutate(nameTable = table_name,
                 namePlayer = player) %>%
          select(nameTable, idPlayer, namePlayer, everything())


        df_table <-
          df_table %>%
          dplyr::select(-one_of("idLeague")) %>%
          dplyr::select(-dplyr::matches("Group")) %>%
          nest(-c(nameTable, idPlayer, namePlayer),
               .key = 'dataTable') %>%
          suppressWarnings()

        df_table
      }) %>%
      mutate(urlNBAAPI = url_json)

    all_data
  }

#' NBA.com player profiles
#'
#' Returns NBA player profiles for specified
#' players.
#'
#' @param player_ids numeric vector of player IDs
#' @param players character vector of player names
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE}
#'
#' @return a `data_frame`
#' @export
#' @import dplyr curl purrr jsonlite tidyr readr
#' @importFrom glue glue
#' @examples
#' player_profiles(player_ids = c(203500, 1628384),
#' players = c("Michael Jordan", "Caris LeVert", "Jarrett Allen"),
#' nest_data = FALSE,
#' return_message = TRUE)
player_profiles <- function(players = NULL,
                                     player_ids = NULL,
                                     nest_data = F,
                                     return_message = TRUE) {
  if (player_ids %>% purrr::is_null() &&
      players %>% purrr::is_null()) {
    stop("Please enter players of player ids")
  }

  player_ids <-
    nba_player_ids(player_ids = player_ids, players = players)
  get_player_profile_safe <-
    purrr::possibly(.get_player_profile, data_frame())
  all_data <-
    player_ids %>%
    future_map_dfr(function(player_id) {
      get_player_profile_safe(player_id = player_id)
    })
  tables <- all_data$nameTable %>% unique()
  tables <- tables[!tables %in% "AvailableSeasons"]

  data <-
    tables %>%
    future_map(function(table) {
      all_data %>%
        filter(nameTable == table) %>%
        select(-nameTable) %>%
        tidyr::unnest()
    })

  all_data <-
    data %>%
    purrr::reduce(left_join) %>%
    mutate(heightInches = heightInches %>% map_dbl(height_in_inches)) %>%
    dplyr::select(
      one_of(
        "idPlayer",
        "namePlayer",
        "datetimeBirth",
        "numberJersey",
        "idTeam",
        "teamName",
        "slugTeam",
        "cityTeam",
        "slugPlayer",
        "yearSeasonFirst",
        "yearSeasonLast",
        "yearDraft",
        "numberRound",
        "numberOverallPick",
        "slugSeason",
        "nameSchool",
        "nameOrganizationFrom",
        "heightInches",
        "weightLBS",
        "countSeasonsPlayed",
        "pts",
        "ast",
        "treb",
        "countAllStarGames",
        "ratioPIE",
        "urlNBAAPI",
        "nameFirst",
        "nameLast",
        "namePlayerLastFirst",
        "namePlayerAbbr"
      ),
      everything()
    ) %>%
    suppressMessages() %>%
    suppressWarnings()

  assign_nba_players()

  all_data <-
    all_data %>%
    left_join(df_dict_nba_players %>% dplyr::select(idPlayer, dplyr::matches("url"))) %>%
    suppressMessages()

  if (all_data %>% tibble::has_name("datetimeBirth")) {
    all_data <-
      all_data %>%
      dplyr::rename(dateBirth = datetimeBirth)
  }

  if (nest_data) {
    all_data <-
      all_data %>%
      nest(-c(idPlayer, namePlayer), .key = dataPlayerProfiles)
  }

  all_data
}
