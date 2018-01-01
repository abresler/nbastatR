make_url <- function(datatype = NULL,
                     SeasonType = "",
                     LeagueID = "",
                     Season = "",
                     IsOnlyCurrentSeason = "",
                     PlayerID = "",
                     TeamID = "",
                     GameID = "",
                     ContextMeasure = "",
                     PlayerPosition = "",
                     DateFrom = "",
                     DateTo = "",
                     GameSegment = "",
                     LastNGames = "",
                     Location = "",
                     Month = "",
                     OpponentTeamID = "",
                     Outcome = "",
                     SeasonSegment = "",
                     VSConference = "",
                     VSDivision = "",
                     RookieYear = "",
                     Period = "",
                     StartPeriod = "",
                     EndPeriod = "",
                     StartRange = "",
                     EndRange = "",
                     RangeType = "",
                     runType = "") {
  prefix <- paste0("http://stats.nba.com/stats/", datatype, "?")
  info <- list(
    SeasonType = SeasonType,
    LeagueID = LeagueID,
    Season = Season,
    IsOnlyCurrentSeason = IsOnlyCurrentSeason,
    PlayerID = PlayerID,
    TeamID = TeamID,
    GameID = GameID,
    ContextMeasure = ContextMeasure,
    PlayerPosition = PlayerPosition,
    DateFrom = DateFrom,
    DateTo = DateTo,
    GameSegment = GameSegment,
    LastNGames = LastNGames,
    Location = Location,
    Month = Month,
    OpponentTeamID = OpponentTeamID,
    Outcome = Outcome,
    SeasonSegment = SeasonSegment,
    VSConference = VSConference,
    VSDivision = VSDivision,
    RookieYear = RookieYear,
    Period = Period,
    StartPeriod = StartPeriod,
    EndPeriod = EndPeriod,
    StartRange = StartRange,
    EndRange = EndRange,
    RangeType = RangeType,
    runType = runType
  )

  info_str <- paste0(names(info), "=", unlist(info), sep = "&", collapse = "")
  str_len <- nchar(info_str)
  info_str <- substr(info_str, 1, str_len - 1)
  url_str <- paste0(prefix, info_str)
  return(url_str)
}


# names -------------------------------------------------------------------


fix_name <- function(data, fix_name = "Weeks12") {
  names(data) <-
    names(data) %>%
    str_replace_all(fix_name, glue::glue("\\.{fix_name}"))
  data
}


#' Fix data frame's names
#'
#' @param data \code{data_frame}
#' @param slugs vector of slugs
#'
#' @return
#' @export
#' @import stringr
#' @importFrom glue glue
#' @examples
fix_names <-
  function(data, slugs = c("FD", "DK")) {

    slugs <- slugs %>% unique()

    for(slug in slugs) {
      data <-
        data %>% fix_name(fix_name = slug)
    }
    data
  }


# tidy --------------------------------------------------------------------



#' Gather a data frame
#'
#' @param data a \code{data_frame}
#' @param numeric_ids vector of numeric ids
#' @param use_logical_keys if \code{TRUE} uses logicals as eys
#' @param use_factor_keys if \code{TRUE} uses factors as a key
#' @param use_date_keys if \code{TRUE} uses dates as a key
#' @param variable_name variable column name
#' @param unite_columns if not \code{NULL} \code{list} \itemize{
#' \item new_column : new column name
#' \item column_1 : first column to unite
#' \item column_2 : second column to unite
#' \item sep : seperator
#' }
#' @param seperate_columns if not \code{NULL} \code{list} \itemize{
#' \item column : column to seperate
#' \item new_column_1 : new_column 1
#' \item new_column_2 : new_column 2
#' \item sep : seperator
#' }
#' @param unite_columns if not \code{NULL} \code{list} \itemize{
#' \item new_column : new column name
#' \item column_1 : first column to unite
#' \item column_2 : second column to unite
#' \item sep : seperator
#' }
#' @param seperate_columns if not \code{NULL} \code{list} \itemize{
#' \item column : column to seperate
#' \item new_column_1 : new_column 1
#' \item new_column_2 : new_column 2
#' \item sep : seperator
#' }
#' @return
#' @export
#' @import dplyr stringr
#' @importFrom rlang UQ
#' @importFrom tidyr gather
#' @importFrom purrr is_null
#' @examples
gather_data <-
  function(data,
           variable_name = 'item',
           numeric_ids = c("^id"),
           use_logical_keys = TRUE,
           use_factor_keys = TRUE,
           unite_columns = NULL,
           seperate_columns = NULL,
           use_date_keys = FALSE,
           remove_na = TRUE) {

    gather_cols <- c()

    char_names <-
      data %>% select_if(is.character) %>% names()

    gather_cols <-
      gather_cols %>% append(char_names)


    if (!numeric_ids %>% purrr::is_null()){
      numeric_names <-
        numeric_ids %>% str_c(collapse = "|")
      base_numerics <-
        data %>% dplyr::select(matches(numeric_names)) %>% names()

      gather_cols <-
        base_numerics %>%
        append(gather_cols)
    }

    use_logical <- data %>% get_data_classes() %>% filter(class == "logical") %>% nrow() > 0 && use_logical_keys

    if (use_logical) {
      logical_cols <-
        data %>% select_if(is.logical) %>% names()

      gather_cols <-
        gather_cols %>%
        append(logical_cols)
    }

    use_factor <- data %>% get_data_classes() %>% filter(class %>%  str_detect("factor")) %>% nrow() > 0 && use_factor_keys

    if (use_factor) {
      factor_cols <-
        data %>% select_if(is.factor) %>% names()

      gather_cols <-
        gather_cols %>%
        append(factor_cols)
    }

    use_date <- data %>% get_data_classes() %>% filter(class %>%  str_detect("date")) %>% nrow() > 0 && use_date_keys

    if (use_date) {
      date_cols <-
        data %>% get_data_classes() %>% filter(class %>% str_detect("date")) %>% pull(column)

      gather_cols <-
        gather_cols %>%
        append(factor_cols)
    }


    data <-
      data %>%
      gather(rlang::UQ(variable_name), value, -gather_cols)

    if (!unite_columns %>% purrr::is_null()) {
      df_unite <- unite_columns %>% flatten_df()
      data <-
        data %>%
        unite(col = rlang::UQ(df_unite$new_column), df_unite$column_1, df_unite$column_2, sep = df_unite$sep) %>%
        suppressWarnings()
    }

    if (!seperate_columns %>% purrr::is_null()) {
      df_sep <-
        seperate_columns %>% flatten_df()
      data <-
        data %>%
        separate(col = rlang::UQ(df_sep$column), into = c(df_sep$new_column_1, df_sep$new_column_2), sep = df_sep$sep) %>%
        suppressWarnings()
    }

    if (remove_na) {
      data <-
        data %>%
        filter(!value %>% is.na())
    }


    data
  }


#' Spread gathered data frame
#'
#' @param data a \code{data_frame}
#' @param variable_name name of variable vector
#' @param value_name name of value vector
#' @param perserve_order if \code{TRUE} perserve order
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom tidyr spread
#' @examples
spread_data <-
  function(data, variable_name = "item", value_name = "value", perserve_order = TRUE,
           unite_columns = NULL,
           seperate_columns = NULL) {

    if (!unite_columns %>% purrr::is_null()) {
      df_unite <- unite_columns %>% flatten_df()
      data <-
        data %>%
        unite(col = rlang::UQ(df_unite$new_column), df_unite$column_1, df_unite$column_2, sep = df_unite$sep)
    }

    if (!seperate_columns %>% purrr::is_null()) {
      df_sep <-
        seperate_columns %>% flatten_df()
      data <-
        data %>%
        separate(col = rlang::UQ(df_sep$column), into = c(df_sep$new_column_1, df_sep$new_column_2), sep = df_sep$sep) %>%
        suppressMessages()
    }

    base_cols <- data %>% dplyr::select(-one_of(c(variable_name, value_name))) %>% names()

    variables <- data %>% pull(variable_name) %>% unique()

    col_order <-
      c(base_cols, variables)

    data <-
      data %>%
      spread(variable_name, value_name)

    if (perserve_order) {
      data <-
        data %>% dplyr::select(one_of(col_order))
    }
    data
  }

remove_na_columns <-
  function(data) {
    data %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))
  }

get_data_classes <- function(data) {
  df_classes <-
    data %>%
    map(class) %>%
    as_data_frame() %>%
    gather(column,class) %>%
    mutate(idColumn = 1:n()) %>%
    select(idColumn, everything()) %>%
    mutate(isNested = class %>% str_detect("list|data.frame|tbl|data_frame|data"))
  has_nested <- df_classes %>% filter(isNested) %>% nrow() >0

  if (has_nested) {
    nested_cols <- df_classes %>% filter(isNested) %>% pull(idColumn)

    df_nested_cols <-
      nested_cols %>%
      map_df(function(x) {
        df_wide <- data %>%
          select(x) %>%
          purrr::set_names("listColumn") %>%
          mutate(nrow = listColumn %>% map_dbl(length)) %>%
          count(countZero = nrow == 0) %>%
          mutate(pctZero = n /sum(n),
                 idColumn = x) %>%
          select(idColumn, everything()) %>%
          gather(item, value, -c(idColumn, countZero)) %>%
          unite(item, item, countZero, sep = "") %>%
          spread(item, value)

        if (df_wide %>% tibble::has_name("pctZeroTRUE")) {
          df_wide <-
            df_wide %>%
            mutate(removeColumn = if_else(pctZeroTRUE == 1, TRUE, FALSE),
                   isMessedList = if_else(pctZeroTRUE > 0 && !pctZeroTRUE == 1, T, F)
            )
        } else {
          df_wide <-
            df_wide %>%
            mutate(removeColumn = F,
                   isMessedList = F)
        }

      })

    df_classes <-
      df_classes %>%
      left_join(df_nested_cols) %>%
      mutate_if(is_logical,
                funs(ifelse(. %>% is.na(), FALSE, .))) %>%
      mutate_if(is_double,
                funs(ifelse(. %>% is.na(), 0, .))) %>%
      suppressMessages()
  }

  df_classes
}


# gets --------------------------------------------------------------------


get.json_data <-
  function(url, use_read_lines = TRUE, is_data_frame = F, is_flattened = T) {
    if (use_read_lines) {
      data <-
        url %>%
        readr::read_lines() %>%
        jsonlite::fromJSON(flatten = is_flattened, simplifyDataFrame = is_data_frame)
      return(data)
    }

    url %>%
      jsonlite::fromJSON(flatten = is_flattened, simplifyDataFrame = is_data_frame)

  }



# normalizing -------------------------------------------------------------
#' Summarise data per minute
#'
#' @param data
#' @param id_columns
#' @param scale_columns
#'
#' @return
#' @export
#' @import dplyr stringr purrr
#' @importFrom glue glue
#' @examples
summarise_per_minute <-
  function(data,
           id_columns = c("idPlayerSeason"),
           scale_columns = c("pts", "fg", "ast", "tov", "blk", "stl", "drb", "trb", "orb", "ft", "pf", "countLayupsShooting", "countDunks", "hlf")) {
    cols_to_match <-
      glue::glue("^{scale_columns}") %>%
      str_c(collapse = "|")

    data <-
      data %>%
      dplyr::select(-one_of("minutes")) %>%
      suppressWarnings()

    min_var <-
      data %>% select(matches("^min|^minutes")) %>% names() %>% .[[1]]

    min_totals <- data %>% pull(min_var)

    munge_cols <-
      data %>% dplyr::select(matches(cols_to_match)) %>% names()
    data <-
      data %>%
      dplyr::select(one_of(c(id_columns, min_var, munge_cols)))

    data <-
      data %>%
      mutate(minutes := min_totals) %>%
      dplyr::select(-one_of(min_var)) %>%
      dplyr::select(one_of("minutes", id_columns), everything()) %>%
      mutate_at(munge_cols,
                funs(. / minutes))

    names(data) <-
      names(data) %>% str_replace_all("Totals|Advanced|PerGame|PerPossesion|Per36", "")

    names(data)[names(data) %>% str_detect(cols_to_match)] <-
      names(data)[names(data) %>% str_detect(cols_to_match)] %>%
      str_c("PerMinute", sep = '')

    data <-
      data %>%
      select(one_of(c(id_columns, "minutes")), everything())
    data
  }


#' Summarise data per minute
#'
#' @param data
#' @param id_columns
#' @param scale_columns
#'
#' @return
#' @export
#' @import dplyr stringr purrr rlang
#' @importFrom glue glue
#' @examples
scale_per_minute <-
  function(data,
           scale_columns = c("pts", "fg", "ast", "tov", "blk", "stl", "drb", "trb", "orb", "ft", "pf", "countLayupsShooting", "countDunks", "hlf")) {
    cols_to_match <-
      glue::glue("^{scale_columns}") %>%
      str_c(collapse = "|")

    is_team <-
      names(data) %>% str_detect("urlBREFTeamData") %>% sum(na.rm = T) > 0

    min_var <-
      data %>% select(matches("^min|^minutes")) %>% names() %>% .[[1]]


    min_totals <-
      data %>% pull(min_var)

    munge_cols <-
      data %>% dplyr::select(matches(cols_to_match)) %>% names()

    data <-
      data %>%
      mutate(minutes := min_totals)

    if (!min_var == "minutes") {
      data <-
        data %>%
        dplyr::select(-one_of(min_var)) %>%
        suppressMessages()
    }
    data <-
      data %>%
      dplyr::select(one_of("minutes"), everything()) %>%
      mutate_at(munge_cols,
                funs(. / minutes))
    if (!is_team) {
    names(data) <-
      names(data) %>% str_replace_all("Totals|Advanced|PerGame|PerPossesion|Per36", "")
    }

    names(data)[names(data) %>% str_detect(cols_to_match)] <-
      names(data)[names(data) %>% str_detect(cols_to_match)] %>%
      str_c("PerMinute", sep = '')

    start_vars <- names(data)[names(data) %>% str_detect("^name|year|^id|slug|group")]

    data <-
      data %>%
      dplyr::select(one_of(c(start_vars, "minutes")), everything())

    data
  }

# other -------------------------------------------------------------------

remove_zero_sum_cols <-
  function(data) {
    data %>% select(which(colSums(. != 0) > 0))
  }

height_in_inches <-
  function(height) {
    height_ft_in <-
      height %>%
      stringr::str_split("-") %>%
      flatten_chr() %>%
      as.numeric()
    height_in <-
      height_ft_in[1] * 12 + height_ft_in[2]
    return(height_in)
  }


clean_to_stem <- function(x) {
  x <-
    x %>%
    str_replace('\\ ', '\\+') %>%
    str_replace('\\/', '\\2F') %>%
    str_replace("\\'", '%27')

  return(x)

}

