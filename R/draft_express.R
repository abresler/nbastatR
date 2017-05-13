
# http://www.draftexpress.com/nba-pre-draft-measurements
parse_to_inches <-
  function(x) {
    values <-
      x %>%
      str_split("'") %>%
      flatten_chr()

    portion_feet <-
      values[[1]] %>% readr::parse_number() * 12

    inches <- values[[2]] %>% readr::parse_number()

    plus_inches <-
      case_when(
        values %>% str_detect("¾") ~ .75,
        values %>% str_detect("¼") ~ .25,
        values %>% str_detect("½") ~ .5,
        TRUE ~ 0) %>%
      max()

    portion_feet + inches + plus_inches
  }

get_draft_urls <- function(year = NULL) {

  if (year %>% purrr::is_null()) {
    url <-
      "http://www.draftexpress.com/nba-pre-draft-measurements/all/all/all"
  } else {
    url <-
      glue::glue("http://www.draftexpress.com/nba-pre-draft-measurements/{year}/all/all/all")
  }

  max_numbers <-
    url %>%
    as.character() %>%
    read_html() %>%
    html_nodes('.pagination a') %>%
    html_text() %>%
    str_trim()


  if (max_numbers %>% length() == 0) {
    max_numbers <- 1
  }

  max_numbers <-
    max_numbers %>% readr::parse_number() %>% suppressWarnings()


  max_numbers <- max_numbers %>% max(na.rm = T) %>% .[[1]]
  if (max_numbers > 50) {
    max_numbers <- 1
  }

  all_numbers <-
    1:max_numbers

  urls <-
    glue::glue("{url}/{all_numbers}")

  urls
}

parse_page_data <-
  function(url = "http://www.draftexpress.com/nba-pre-draft-measurements/all/all/all/all/33",
           return_message = TRUE) {
   if (return_message) {
     glue::glue("Parsing {url}") %>%
       message()
   }
    page <-
     url %>%
     read_html()

   players <-
      page %>%
      html_nodes(css = '.key') %>%
      html_text() %>%
      str_trim()

    url_players <-
      page %>%
      html_nodes(css = '.key') %>%
      html_nodes('a') %>%
      html_attr('href') %>%
      str_c('http://www.draftexpress.com', .)


    df_players <-
      data_frame(namePlayer = players,
               urlPlayerDraftExpress = url_players) %>%
      mutate(idRow = 1:n())

    df_values <-
      2:18 %>%
      map_df(function(x){
        attribute <-
          glue("td:nth-child({x})")
        values <-
          page %>% html_nodes(attribute) %>% html_text() %>% str_trim()

        values <-
          values %>% str_replace_all('\\-', '')

        data_frame(idColumn = x, values) %>%
          mutate(idRow = 1:n())
      })

    df_items <-
      data_frame(idColumn = 2:18,
               nameColumn = c('yearDraft', 'numberDraftPick',
                              'heightNoShoesInches', 'heightShoesInches', 'wingspanInches',
                              'standingreachInches', 'verticalMaxInches', 'verticalMaxReachInches',
                              'verticalNoStepInches', 'verticalNoStepReachInches', 'weightLBS', 'pctBodyFat',
                              'lengthHandInches', 'widthHandInches', 'countBenchReps',
                              'speedAgility', 'speedSprint'))

    df_values <-
      df_values %>%
      left_join(df_items) %>%
      dplyr::select(-idColumn) %>%
      spread(nameColumn, values) %>%
      dplyr::select(one_of(df_items$nameColumn)) %>%
      suppressMessages()
    parse_to_inches_safe <-
      purrr::possibly(parse_to_inches, NA)

    df_values <-
      df_values %>%
      mutate_at(
        c(
          'heightNoShoesInches',
          'heightShoesInches',
          'wingspanInches',
          'standingreachInches',
          'verticalMaxReachInches',
          'verticalNoStepReachInches'
        ),
        funs((. %>% map_dbl(
          parse_to_inches_safe
        )))
      ) %>%
      mutate_at(
        c(
          'yearDraft',
          'numberDraftPick',
          'weightLBS',
          'pctBodyFat',
          'countBenchReps',
          'speedAgility',
          'speedSprint',
          'verticalMaxInches',
          'verticalNoStepInches',
          'lengthHandInches',
          'widthHandInches'
        ),
        funs(readr::parse_number)) %>%
        mutate(pctBodyFat = pctBodyFat /100)

    df_players %>%
      left_join(df_values %>% mutate(idRow = 1:n())) %>%
      select(-idRow) %>%
      dplyr::select(namePlayer, yearDraft, everything()) %>%
      suppressMessages() %>%
      mutate(urlPage = url)
  }

parse_draft_pages <-
  function(urls,
           return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res){
      parse_page_data_safe <-
        purrr::possibly(parse_page_data, data_frame())
      page_url <-
        res$url

      data <-
        page_url %>%
        parse_page_data_safe(return_message = return_message)

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      data_frame()
    }
    urls %>%
      walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    closeAllConnections()
    df
  }

parse_de_id <- function(x = "http://www.draftexpress.com/profile/Scottie-Pippen-3959/") {
  numbers <-
    x %>%
    str_replace_all("http://www.draftexpress.com/profile/",'') %>%
    str_split('\\-') %>%
    flatten_chr() %>%
    readr::parse_number() %>%
    suppressWarnings()

  numbers[length(numbers)]
}

#' Draft Express Measurements
#' @param year draft years - if \code{NULL} all draft years
#' @param return_unique_players if \code{TRUE} returns players last data
#' @param return_message
#'
#' @return
#' @export
#' @import purrr glue dplyr curl rvest tidyr dplyr stringr readr
#' @examples
get_data_draft_express_measurements <-
  function(years = 1987:2017,
           return_unique_players = FALSE,
           return_message = TRUE) {
    get_draft_urls_safe <-
      purrr::possibly(get_draft_urls, NA)

    if (years %>% purrr::is_null()){
      urls <-
      get_draft_urls(year = NULL)
    } else {
      urls <- years %>%
        map_df(function(x){
          urls <- get_draft_urls_safe(year = x )
          data_frame(yearData = x, urlData = urls)
        }) %>%
        .$urlData
    }

    parse_pages_data_safe <-
      purrr::possibly(parse_draft_pages, data_frame())

    all_data <-
      urls %>%
      map_df(function(x) {
        parse_pages_data_safe(url = x, return_message = T)
      })

    max_year <- all_data$yearDraft %>% max(na.rm = T)

    is_predraft <-
      all_data %>%
      filter(yearDraft == max_year) %>%
      filter(!numberDraftPick %>% is.na()) %>%
      nrow() == 0

    if (is_predraft) {
      all_data <-
        all_data %>%
        filter(yearDraft < max_year) %>%
        mutate(isDrafted = !numberDraftPick %>% is.na()) %>%
        select(yearDraft,
               namePlayer,
               isDrafted,
               numberDraftPick,
               everything()) %>%
        bind_rows(all_data %>% filter(yearDraft == max_year)) %>%
        arrange(yearDraft)
    } else {
      all_data <-
        all_data %>%
        mutate(isDrafted = !numberDraftPick %>% is.na()) %>%
        select(yearDraft,
               namePlayer,
               isDrafted,
               numberDraftPick,
               everything()) %>%
        arrange(yearDraft)
    }
    parse_de_id_safe <-
      purrr::possibly(parse_de_id, 0)

    all_data <-
      all_data %>%
      mutate(idPlayerDE = urlPlayerDraftExpress %>% map_dbl(parse_de_id_safe)) %>%
      dplyr::select(yearDraft, namePlayer, idPlayerDE, isDrafted, everything())

    all_data <-
      all_data %>%
      mutate(
        namePlayer = namePlayer %>% str_replace_all('_ ', ' ') %>% str_trim(),
        isFirstRoundPick = numberDraftPick <= 30,
        isSecondRoundPick = numberDraftPick > 30,
        isLotteryPick = numberDraftPick <= 14,
        isTop5Pick =  numberDraftPick <= 5,
        isNumber1Pick =  numberDraftPick == 1
      ) %>%
      distinct()


    if (return_unique_players) {
      all_data <-
        all_data %>%
        group_by(idPlayerDE) %>%
        filter(yearDraft == max(yearDraft))
    }

    all_data
  }
