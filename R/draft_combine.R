

parse_out_set <-
  function(data, set_column = "setSpot15CornerLeftCollege") {
    df_set <-
      data %>%
      select(one_of(set_column)) %>%
      distinct()

    all_data <-
      df_set %>% pull() %>%
      map_df(function(x) {
        if (x %>% is.na()) {
          return(data_frame(UQ(set_column) := x))
        }
        names_set <-
          c(
            set_column,
            glue::glue("{set_column}Made"),
            glue::glue("{set_column}Attempted"),
            glue::glue("{set_column}Pct")
          )
        values <-
          x %>% str_split("\\-") %>% flatten_chr() %>% as.numeric()

        data_frame(
          X1 = x,
          X2 = values[1],
          X3 = values[2],
          X4 = X2 / X3
        ) %>%
          purrr::set_names(c(names_set))
      })

    data %>%
      left_join(all_data)

  }

get_shot_pct <- function(x) {
  shots <-
    x %>%
    str_split('\\-') %>%
    unlist %>%
    as.numeric()

  shot.pct <-
    shots[1] / shots[2]

  return(shot.pct)

}
get_year_draft_combine <-
  function(combine_year = 2014,
           return_message = T) {
    if (combine_year < 2000) {
      stopifnot("Sorry data starts in the 2000-2001 season")
    }

    if (return_message) {
      glue::glue("Acquiring {combine_year} NBA Draft Combine Data") %>% message()
    }
    slugSeason <- generate_season_slug(season = combine_year)
    url <-
      glue::glue(
        "http://stats.nba.com/stats/draftcombinestats?LeagueID=00&SeasonYear={slugSeason}"
      ) %>%
      as.character()


    json <-
      url %>%
      curl() %>%
      readr::read_lines() %>%
      fromJSON(simplifyDataFrame = T)


    data <-
      json$resultSets$rowSet %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df()

    headers <-
      json$resultSets$headers %>% flatten_chr()

    actual_names <-  headers %>% resolve_nba_names()

    data <-
      data %>%
      purrr::set_names(actual_names)

    num_names <-
      actual_names[actual_names %>% str_detect("pct|Inches|^id[A-Z]|time|weight|reps")]

    data <-
      data %>%
      mutate_at(num_names,
                funs(. %>% as.character() %>% readr::parse_number())) %>%
      dplyr::rename(slugPosition = groupPosition)

    if (actual_names[actual_names %>% str_detect("set")] %>% length() > 0) {
      data <-
        actual_names[actual_names %>% str_detect("set")] %>%
        map(function(set) {
          parse_out_set(data = data, set_column = set)
        }) %>%
        suppressMessages()

      data <-
        data %>%
        purrr::reduce(left_join) %>%
        suppressMessages()
    }

    data <-
      data %>%
      mutate(yearCombine = combine_year) %>%
      select(yearCombine, everything()) %>%
      remove_na_columns()

    data
  }

#' NBA draft combine data
#'
#' Acquires NBA draft combine data
#'
#' @param years vector of draft years
#' @param return_message if \code{TRUE} return message
#' @param nest_data if \code{TRUE} returns nested data_frame
#'
#' @return a \code{data_frame()}
#' @family draft
#' @family player
#' @export
#' @import dplyr stringr curl jsonlite lubridate purrr tidyr rlang
#' @importFrom glue glue
#' @examples
#' get_years_draft_combines(years = c(2001:2018),
#' nest_data = T)

get_years_draft_combines <-
  function(years = NULL,
           return_message = T,
           nest_data = F) {
    if (years %>% purrr::is_null()) {
      stop("Please enter combine years")
    }
    get_year_draft_combine_safe <-
      purrr::possibly(get_year_draft_combine, data_frame())

    all_data <-
      years %>%
      map_df(function(combine_year) {
        get_year_draft_combine_safe(combine_year = combine_year,
                                    return_message = return_message)
      }) %>%
      select(-yearSeasonStart)

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-yearCombine, .key = 'dataCombine') %>%
        mutate(countPlayers = dataCombine %>% map_dbl(nrow)) %>%
        select(yearCombine, countPlayers, dataCombine)
    }

    all_data

  }
