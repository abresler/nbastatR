
# rotoworld ---------------------------------------------------------------

.get_player_rotowire_news <-
  function(player_id = 201935,
           return_message = TRUE,
           results = 50) {
    json_url <-
      glue::glue(
        "https://stats-prod.nba.com/wp-json/statscms/v1/rotowire/player/?playerId={player_id}&limit={results}&offset=0"
      ) %>%
      as.character()

    json <-
      json_url %>%
      curl_json_to_vector()

    data <-
      json$PlayerRotowires %>%
      as_tibble()

    actual_names <- names(data) %>% resolve_nba_names()
    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      tidyr::unite(namePlayer, nameFirst, nameLast, sep = " ") %>%
      mutate_at(c("idUpdate", "idPlayer", "idRotoWorld", "dateISO", "numberPriority"),
                funs(. %>% as.numeric())) %>%
      mutate_at(c("datetimePublished", "datetimeUpdatedLast"),
                funs(. %>% lubridate::mdy_hms())) %>%
      mutate(isInjured = !slugInjured %>% str_detect("NO")) %>%
      dplyr::select(idPlayer, namePlayer, slugTeam, codeTeam, datetimePublished, articleHeadline, everything()) %>%
      mutate_if(is.character,
                funs(ifelse(. == "", NA_character_, .)))

    if (return_message) {
      glue::glue("Acquired {nrow(data)} Roto Wire articles for {data$namePlayer %>% unique()}") %>% cat(fill = T)
    }
    data
  }

#' Players RotoWire news
#'
#' Returns rotowire news for specified
#' players.
#'
#' @param players vector of player_names
#' @param player_ids vector of player names
#' @param nest_data if \code{TRUE} returns a nested data frame
#' @param results integer of results
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a `tibble`
#' @export
#' @family news
#' @import dplyr curl readr lubridate purrr jsonlite tidyr
#' @importFrom glue glue
#' @examples
#' players_rotowire(players = c( "Jarrett Allen", "DeMarre Carroll", "Allen Crabbe"))
players_rotowire <-
  function(players =  NULL,
           player_ids = NULL,
           nest_data = F,
           results = 50,
           return_message = TRUE) {
    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }
    ids <-
      nba_player_ids(player_ids = player_ids,
                          players = players)
    get_player_rotowire_news_safe <-
      purrr::possibly(.get_player_rotowire_news, tibble())

    all_data <-
      ids %>%
      future_map_dfr(function(id) {
        get_player_rotowire_news_safe(player_id = id, return_message = return_message, results = results)
      })

    all_data <-
      all_data %>%
      arrange(datetimePublished)




    all_data <-
      all_data %>%
      left_join(
        df_nba_player_dict %>% dplyr::select(nameTeam, idPlayer, dplyr::matches("url"))
      ) %>%
      suppressMessages()

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
          .key = dataRotoWireArticles
        ) %>%
        mutate(countArticles = dataRotoWireArticles %>% map_dbl(nrow))
    }
    all_data
  }

#' Teams Rotowire news
#'
#' Returns roto wire news for specified teams.
#'
#' @param teams vector of teams
#' @param team_ids vector
#' @param all_active_teams if `TRUE` searches all active teams
#' @param nest_data if `TRUE` returns nested data frame
#' @param results numeric vector of results
#' @param return_message if `TRUE` returns a message
#'
#' @return a `tibble`
#' @family news
#' @export
#' @import dplyr curl readr lubridate purrr jsonlite tidyr
#' @importFrom glue glue
#' @examples
#' teams_rotowire(teams = "Brooklyn Nets")
  teams_rotowire <-
  function(teams = NULL,
           team_ids = NULL,
           all_active_teams = F,
           nest_data = F,
           results = 50,
           return_message = TRUE) {
    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    assign_nba_teams()
    team_ids <-
      nba_teams_ids(teams = teams,
                        team_ids = team_ids,
                        all_active_teams = all_active_teams)

    ids <-
      df_nba_player_dict %>%
      filter(idTeam %in% team_ids) %>%
      pull(idPlayer) %>%
      unique()

    all_data <- get_players_rotowire(player_ids = ids, nest_data = F, results = results)

    all_data <-
      all_data %>%
      arrange(desc(datetimePublished))

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
          .key = dataRotoWireArticles
        ) %>%
        mutate(countArticles = dataRotoWireArticles %>% map_dbl(nrow)) %>%
        arrange(nameTeam, namePlayer)
    }
    all_data

  }

# transactions ------------------------------------------------------------


.nba_transactions_historic <-
  function() {
    json <-
      "http://stats.nba.com/feeds/NBAPlayerTransactions-559107/json.js" %>%
      curl_json_to_vector()

    data <-
      json$ListItems %>%
      as_tibble() %>%
      purrr::set_names(c("title", "descriptionTransaction", "idTeam", "nameTeamFrom",
                         "idPlayer", "dateTransaction", "idTransaction", "meta")) %>%
      mutate_at(c("idPlayer", "idTransaction", "idTeam"),
                funs(. %>% as.numeric())) %>%
      mutate(dateTransaction = lubridate::mdy(dateTransaction)) %>%
      select(-one_of("title", "nameTeamFrom", "meta")) %>%
      suppressWarnings()

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }

    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }
    data <-
      data %>%
      mutate(
        idPlayer = ifelse(idPlayer == 0 , NA, idPlayer),
        yearTransaction = lubridate::year(dateTransaction),
        monthTransaction = lubridate::month(dateTransaction),
        hasDraftPick = descriptionTransaction %>% str_detect("draft"),
        typeTransaction = case_when(
          descriptionTransaction %>% str_to_lower() %>% str_detect("trade") ~ "Trade",
          descriptionTransaction %>% str_to_lower() %>% str_detect("sign") ~ "Signing",
          descriptionTransaction %>% str_to_lower() %>% str_detect("waive") ~ "Waive",
          descriptionTransaction %>% str_to_lower() %>% str_detect("claimed") ~ "AwardOnWaivers"
        )
      ) %>%
      left_join(df_nba_player_dict %>% dplyr::select(idPlayer, namePlayer)) %>%
      left_join(df_nba_team_dict %>% dplyr::select(idTeam, nameTeam)) %>%
      suppressMessages()
    data


  }

#' NBA transactions since 2012
#'
#' Acquires all NBA transactions since 2012
#'
#' @return a `tibble`
#' @export
#' @family transactions
#' @import dplyr purrr curl jsonlite readr lubridate tidyr tibble
#' @examples
#' transactions()
transactions <-
  function(include_history = T) {
    json <-
      "http://stats.nba.com/js/data/playermovement/NBA_Player_Movement.json" %>%
      curl_json_to_vector()

    data <-
      json$NBA_Player_Movement$rows %>%
      as_tibble()

    json_names <- json$NBA_Player_Movement$columns$Name
    actual_names <- json_names %>% resolve_nba_names()
    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }

    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    data <-
      data %>%
      purrr::set_names(actual_names)

    data <-
      data %>%
      tidyr::separate(sortGroup,
                      into = c("remove", "idTransaction"),
                      sep = "\\ ") %>%
      mutate(
        dateTransaction = readr::parse_datetime(dateTransaction) %>% as.Date(),
        idTeamFrom = ifelse(idTeamFrom == 0, NA, idTeamFrom),
        idPlayer = ifelse(idPlayer == 0 , NA, idPlayer),
        yearTransaction = lubridate::year(dateTransaction),
        monthTransaction = lubridate::month(dateTransaction),
        hasDraftPick = descriptionTransaction %>% str_detect("draft"),
        idTransaction = idTransaction %>% as.numeric()
      ) %>%
      left_join(df_nba_player_dict %>% dplyr::select(idPlayer, namePlayer)) %>%
      left_join(df_nba_team_dict %>% dplyr::select(idTeam, nameTeam)) %>%
      left_join(df_nba_team_dict %>% dplyr::select(idTeamFrom = idTeam, nameTeamFrom = nameTeam)) %>%
      dplyr::select(-one_of("remove")) %>%
      suppressMessages()

    data <-
      data %>%
      bind_rows(    .nba_transactions_historic())

    data <-
      data %>%
      dplyr::select(yearTransaction, monthTransaction,
                    dateTransaction, idTransaction, descriptionTransaction,
                    dplyr::matches("name|id"), everything()) %>%
      arrange(desc(dateTransaction)) %>%
      distinct()

    data

  }


# beyond_the_numbers ------------------------------------------------------

#'  NBA.com Beyond The Numbers Articles
#'
#'  Returns articles from beyond the numbers
#'
#' @param count_articles numeric vector of counts
#'
#' @return a \code{tibble}
#' @family news
#' @export
#' @import dplyr curl jsonlite rvest xml2 purrr stringr lubridate readr
#' @importFrom glue glue
#' @examples
#'\dontrun{
#' beyond_the_numbers(count_articles = 10)
#' }

beyond_the_numbers <-
  function(count_articles = 50) {
    if (count_articles > 500){
      stop("Articles can't exceed 500")
    }

    url <-
      glue::glue("https://stats-prod.nba.com/wp-json/statscms/v1/type/beyondthenumber/?limit={count_articles}&offset=0") %>%
      as.character()

    data <-
      url %>%
      jsonlite::fromJSON(simplifyVector = T)
    url_article <- data$posts$meta %>% flatten_chr()

    df <-
      data$posts[1:5] %>% as_tibble() %>%
      purrr::set_names(c("idArticle", "titleArticle", "datetimeArticle", "htmlContent", "urlImage")) %>%
      mutate(urlArticle = url_article,
             titleArticle = ifelse(titleArticle == "", NA, titleArticle),
             datetimeArticle = datetimeArticle %>% lubridate::ymd_hms()) %>%
      select(datetimeArticle, everything())

   df <-
     df %>%
      mutate(textArticle = htmlContent %>% map_chr(function(x) {
        x %>% read_html() %>% html_text() %>% str_trim()
      })) %>%
      dplyr::select(-htmlContent)

   df
  }

# daily video --------------------------------------------------------------
# http://api.nba.net/0/league/collection/47b76848-028b-4536-9c9c-37379d209639
