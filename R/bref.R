# gdeltr2::load_needed_packages(c('formattable', 'dplyr', 'stringr', 'httr', 'rvest', 'purrr', 'tidyr', 'lubridate', 'curl', 'readr', 'stringi'))
# dicitonaries --------------------------------------------------------------

get_bref_name_df <- function() {
  data_frame(
    nameBREF = c(
      "idSeason",
      "yearSeason",
      "Rk",
      "idPlayer",
      "Player",
      "Pos",
      "Age",
      "Tm",
      "G",
      "GS",
      "MP",
      "FG",
      "FGA",
      "FG%",
      "3P",
      "3PA",
      "3P%",
      "2P",
      "2PA",
      "2P%",
      "eFG%",
      "FT",
      "FTA",
      "FT%",
      "ORB",
      "DRB",
      "TRB",
      "AST",
      "STL",
      "BLK",
      "TOV",
      "PF",
      "PS/G",
      "urlData",
      "PTS",
      "PER",
      "TS.",
      "X3PAr",
      "FTr",
      "ORB.",
      "DRB.",
      "TRB.",
      "AST.",
      "STL.",
      "BLK.",
      "TOV.",
      "USG.",
      "OWS",
      "DWS",
      "WS",
      "WS.48",
      "OBPM",
      "DBPM",
      "BPM",
      "VORP",
      "FG.",
      "X3P",
      "X3PA",
      "X3P.",
      "X2P",
      "X2PA",
      "X2P.",
      "eFG.",
      "FT.",
      "PS.G"

    ),
    nameActual = c(
      "idSeason",
      "yearSeason",
      "idRank",
      "idPlayer",
      "namePlayer",
      "idPosition",
      "agePlayer",
      "slugTeamBREF",
      "countGames",
      "countGamesStarted",
      "MinutesPlayed",
      "FGM",
      "FGA",
      "pctFG",
      "FG3M",
      "FG3A",
      "pctFG3",
      "FG2M",
      "FG2A",
      "pctFG3",
      "pctEFG",
      "FTM",
      "FTA",
      "pctFT",
      "ORB",
      "DRB",
      "TRB",
      "AST",
      "STL",
      "BLK",
      "TOV",
      "PF",
      "pergamePTS",
      "urlData",
      "PTS",
      "ratioPER",
      "pctTrueShooting",
      "pct3PRate",
      "pctFTRate",
      "pctORB",
      "pctDRB",
      "pctTRB",
      "pctAST",
      "pctSTL",
      "pctBLK",
      "pctTOV",
      "pctUSG",
      "ratioOWS",
      "ratioDWS",
      "ratioWS",
      "ratioWSPer48",
      "ratioOBPM",
      "ratioDBPM",
      "ratioBPM",
      "ratioVORP",
      "pctFG",
      "FG3M",
      "FG3A",
      "pctFG3",
      "FG2M",
      "FG2A",
      "pctFG2",
      "pctEFG",
      "pctFT",
      "pergamePTS"
    )
  )
}




# all_nba -----------------------------------------------------------------


#' All NBA teams
#'
#' @param return_message
#'
#' @return
#' @export
#' @import httr rvest xml2 dplyr purrr stringr readr
#' @examples
get_data_all_nba_teams <-
  function(return_message = TRUE) {
    page <-
      "http://www.basketball-reference.com/awards/all_league.html" %>%
      read_html()

    df <-
      page %>%
      html_table(fill = TRUE) %>%
      .[[1]] %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df() %>%
      dplyr::rename(idSeason = Season,
                    idLeague = Lg,
                    classAllNBA = Tm) %>%
      filter(!idSeason == '') %>%
      tidyr::gather(value,
                    namePlayerPosition,
                    -c(idSeason, idLeague, classAllNBA)) %>%
      select(-value) %>%
      tidyr::separate(
        idSeason,
        into = c('yearSeasonStart', 'remove'),
        sep = '\\-',
        remove = F
      ) %>%
      dplyr::select(-remove) %>%
      mutate(
        yearSeasonStart = yearSeasonStart %>% as.numeric(),
        yearSeason = yearSeasonStart + 1,
        numberAllNBATeam = classAllNBA %>% readr::parse_number(),
        namePlayer =
          ifelse(
            yearSeasonStart >= 1955,
            namePlayerPosition %>% substr(start = 1, stop = nchar(namePlayerPosition) - 2) %>% str_trim(),
            namePlayerPosition
          ),
        groupPosition = ifelse(
          yearSeasonStart >= 1955,
          namePlayerPosition %>% substr(
            start = nchar(namePlayerPosition) - 1,
            stop = nchar(namePlayerPosition)
          ) %>% str_trim(),
          NA
        )
      ) %>%
      tidyr::separate(namePlayer,
                      into = c('namePlayer', 'namePlayer2'),
                      sep = '\\, ') %>%
      select(idLeague,
             idSeason,
             yearSeason,
             groupPosition,
             numberAllNBATeam,
             namePlayer) %>%
      filter(idLeague == "NBA") %>%
      arrange(desc(yearSeason), numberAllNBATeam) %>%
      suppressMessages() %>%
      suppressWarnings()

    player_names <-
      page %>%
      html_nodes(css = 'td a') %>%
      html_text()

    slugs <-
      page %>%
      html_nodes(css = 'td a') %>%
      html_attr(name = 'href')

    df_people <-
      1:length(slugs) %>%
      map_df(function(x) {
        namePlayer <-
          player_names[[x]]
        urlPlayerBREF <-
          str_c('http://www.basketball-reference.com', slugs[[x]], collapse = '')
        is_player <-
          slugs[[x]] %>% str_detect('/players/')

        if (is_player) {
          idPlayer <-
            slugs[[x]] %>% str_replace_all('.html', '') %>% str_split('/players/') %>% flatten_chr() %>% .[[2]] %>% str_split('/') %>% flatten_chr() %>% .[[2]]
        } else {
          idPlayer <-
            slugs[[x]] %>% str_replace_all('.html', '') %>% str_split('/leagues/') %>% flatten_chr() %>% .[[2]] %>% str_split('_') %>% flatten_chr() %>% .[[1]]
        }
        data_frame(idPlayer, namePlayer, urlPlayerBREF)
      }) %>%
      distinct() %>%
      mutate_all(str_trim)

    df_people <-
      df_people %>%
      select(namePlayer, idPlayer, urlPlayerBREF) %>%
      distinct() %>%
      filter(!idPlayer %>% is.na())

    df <-
      df %>%
      mutate(namePlayer = namePlayer %>% str_trim()) %>%
      left_join(df_people) %>%
      suppressMessages() %>%
      mutate(
        idPlayerSeason = list(idPlayer, '_', yearSeason) %>% purrr::reduce(paste0),
        isAllNBA = TRUE
      )

    df <-
      df %>%
      mutate(
        isAllNBA1 = numberAllNBATeam == 1,
        isAllNBA2 = numberAllNBATeam == 2,
        isAllNBA3 = numberAllNBATeam == 3
      )

    if (return_message) {
      list(
        "Returned All-NBA teams from ",
        df$yearSeason %>% min() %>% unique(),
        ' to ',
        df$yearSeason %>% max() %>% unique()
      ) %>% purrr::reduce(paste0)
    }

    return(df)
  }


# parse_player_page -------------------------------------------------------


parse_player_page <-
  function(url = "http://www.basketball-reference.com/players/g/gillke01.html") {
    page <-
      url %>%
      curl() %>%
      read_html()
  }


# parse_page_season --------------------------------------------------------------

generate_years_urls <-
  function(table = "per_game", years = 1951:2017) {
    tables <-
      c('per_game', 'advanced', 'totals', 'per_minute', 'per_poss')

    if (!table %in% tables) {
      stop(str_c("Tables can only be: " , tables %>% paste(collapse = ', ')))
    }

    urls <-
      list('http://www.basketball-reference.com/leagues/NBA_',
           years,
           '_',
           table,
           '.html') %>%
      purrr::reduce(paste0)
    return(urls)
  }

parse_player_season <-
  function(url = "http://www.basketball-reference.com/leagues/NBA_2017_per_game.html",
           return_message = TRUE) {
    page <-
      url %>%
      read_html()

    url_df <- url %>% httr::parse_url() %>% flatten_df()

    url_path <-
      url_df$path %>% str_replace_all(".html|leagues/NBA_", '')

    year_season_end <-
      url_path %>% str_split('\\_') %>% flatten_chr() %>% .[[1]] %>% readr::parse_number()

    name_slug <-
      url_path %>%
      map_chr(function(x) {
        parts <-
          x %>% str_split('\\_') %>% flatten_chr()

        parts[2:length(parts)] %>% str_c(collapse = '')
      })

    id_season <-
      list(year_season_end - 1, '-', year_season_end %>% str_sub(3, 4)) %>%
      purrr::reduce(paste0)

    players <-
      page %>%
      html_nodes('th+ .left a') %>%
      html_text()

    player_id <-
      page %>%
      html_nodes('th+ .left a') %>%
      html_attr('href') %>%
      str_replace_all('/players/', '')

    player_ids <-
      player_id %>%
      map_chr(function(x) {
        x %>%
          str_replace_all('.html', '') %>%
          str_split('/') %>%
          flatten_chr() %>%
          .[[2]]
      })

    df_players <-
      data_frame(idPlayer = player_ids, namePlayer = players) %>%
      distinct()

    df <-
      page %>%
      html_table() %>%
      .[[1]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      tbl_df() %>%
      dplyr::select(-matches("Var"))

    df <-
      df %>%
      mutate_at(df %>% select(-c(Tm, Player, Pos)) %>% names(),
                funs(. %>% as.numeric())) %>%
      filter(!Rk %>% is.na()) %>%
      suppressWarnings() %>%
      dplyr::select(-matches("Rk"))

    df_names <-
      get_bref_name_df()

    bref_names <-
      names(df)

    actual_names <-
      1:length(bref_names) %>%
      map_chr(function(x) {
        actual <-
          df_names %>%
          filter(nameBREF == bref_names[x]) %>%
          .$nameActual

        if (actual == 'MinutesPlayed') {
          if (!name_slug == 'pergame') {
            actual <-
              str_c('total', actual)
          } else {
            actual <-
              str_c('pergame', actual)
          }
          return(actual)
        }

        if (actual %>% substr(1, 1) %in% LETTERS) {
          actual <-
            str_c(name_slug, actual)
        }
        return(actual)
      })


    df <-
      df %>%
      purrr::set_names(actual_names) %>%
      mutate(
        isHOFPlayer = namePlayer %>% str_detect('\\*'),
        namePlayer = namePlayer %>% str_replace_all('\\*', '')
      ) %>%
      left_join(df_players) %>%
      distinct() %>%
      suppressMessages() %>%
      mutate(idSeason = id_season,
             yearSeason = year_season_end,
             urlData = url) %>%
      dplyr::select(idSeason, yearSeason,
                    idPlayer, everything())

    df <-
      df %>%
      mutate_at(df %>% dplyr::select(matches("pct")) %>% names(),
                funs(ifelse(. >= 1, . / 100, .)))

    if (return_message) {
      list("Parsed ", url) %>%
        purrr::reduce(paste0) %>%
        message()
    }
    gc()
    closeAllConnections()
    return(df)
  }

# player seasons -----------------------------------------------------------------


get_data_bref_player_seasons <-
  function(years = 1980:2017,
           table = "advanced",
           only_totals = TRUE,
           return_message = TRUE) {
    urls <-
      generate_years_urls(table = table, years = years)
    parse_player_season_safe <-
      purrr::possibly(parse_player_season, data_frame())
    all_data <-
      urls %>%
      map_df(function(x) {
        parse_player_season_safe(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      dplyr::select(-matches("urlData")) %>%
      tidyr::unite(idPlayerSeason, idPlayer, yearSeason, remove = F)

    all_data <-
      all_data %>%
      arrange(yearSeason)

    if (only_totals) {
      all_data <-
        all_data %>%
        group_by(yearSeason, idPlayer) %>%
        filter(countGames == max(countGames)) %>%
        ungroup()
    }
    gc()
    return(all_data)
  }


#' Basketball Reference Player Season Tables
#'
#' @param tables player table \itemize{
#' \item \code{totals}: Totals
#' \item \code{'per_game'}: Per game
#' \item \code{advanced}: Advanced
#' \item \code{per_minute}: Per 36 minutes
#' \item \code{per_poss}: Per Possesion
#' }
#' @param include_all_nba if \code{TRUE} include all_nba teams
#' @param years vector of years 1951 to current season
#' @param only_totals if \code{TRUE} returns only a player's total statistics
#' @param nest_data if \code{TRUE} returns a nested data frame
#' @param return_message  if \code{TRUE} returns a message
#'
#' @return
#' @export
#' @import curl dplyr tidyr httr xml2 rvest tidyr stringr purrr readr
#' @examples
get_data_bref_players_seasons <-
  function(years = 2017,
           tables = c('advanced', 'totals'),
           include_all_nba = TRUE,
           only_totals = TRUE,
           nest_data = FALSE,
           return_message = TRUE) {
    all_data <-
      tables %>%
      purrr::map(function(x) {
        get_data_bref_player_seasons(
          table = x,
          years = years,
          only_totals = only_totals,
          return_message = return_message
        )
      })

    all_data <-
      all_data %>%
      purrr::reduce(left_join)

    all_data <-
      all_data %>%
      mutate(
        groupPosition = ifelse(
          idPosition %>% nchar() == 1,
          idPosition,
          idPosition %>% substr(2, 2)
        ),
        isHOFPlayer = ifelse(!isHOFPlayer %>% is.na(), TRUE, FALSE)
      ) %>%
      mutate(groupPosition = ifelse(groupPosition == "-", substr(idPosition, 1, 1), groupPosition)) %>%
      dplyr::select(idSeason:namePlayer, groupPosition, everything())

    if (include_all_nba) {
      df_all_nba <-
        get_data_all_nba_teams(return_message = return_message)

      all_data <-
        all_data %>%
        left_join(df_all_nba %>% dplyr::select(-c(idLeague, groupPosition, urlPlayerBREF))) %>%
        distinct()

      all_data <-
        all_data %>%
        tidyr::replace_na(
          list(
            numberAllNBATeam = 0,
            isAllNBA = FALSE,
            isAllNBA1 = FALSE,
            isAllNBA2 = FALSE,
            isAllNBA3 = FALSE
          )
        )
    }

    all_data <-
      all_data %>%
      mutate(urlPlayerBREF = list('http://www.basketball-reference.com/players/', idPlayer %>% substr(start = 1,stop = 1), '/', idPlayer, '.html') %>% purrr::reduce(paste0))

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(idSeason, yearSeason), .key = 'dataSeasonPlayer')
    }

    return(all_data)
  }

