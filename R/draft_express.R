
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

get_draft_urls <- function(url) {
  page <-
    url %>%
    as.character() %>%
    read_html()

  max_numbers <-
    page %>%
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
#' Draft Express Combine Sources
#'
#' @return
#' @export
#' @import dplyr
#' @examples
get_data_draft_express_combine_sources <-
  function() {
    df_sources <-
      data_frame(
        nameSource = c(
          "All",
          "Vince Carter Camp",
          "USA Pan Am Team",
          "USA Basketball",
          "UA All-American Camp",
          "Reebok Breakout",
          "Portsmouth",
          "Paul Pierce Camp",
          "PG Skills Acad",
          "Official Team",
          "Official College Team",
          "Nike Skills Academy",
          "Nike Elite 100",
          "Nike Basketball Academy",
          "Newspaper",
          "Nets Workout",
          "NBA Top 100 Camp",
          "NBA Pre-Draft Camp",
          "NBA Draft Combine",
          "LeBron James Camp",
          "Kevin Durant Camp",
          "Hoop Summit",
          "Eurocamp",
          "Elite 24",
          "Deron Williams Camp",
          "D-League Elite Camp",
          "Clippers Workout",
          "Biosteel All-Canadian Game",
          "Big Man Skills Acad",
          "Amare Stoudemire Camp"
        ),
        slugSource = c(
          "all",
          "Vince+Carter+Camp",
          "USA+Pan+Am+Team",
          "USA+Basketball",
          "UA+All-American+Camp",
          "Reebok+Breakout",
          "Portsmouth",
          "Paul+Pierce+Camp",
          "PG+Skills+Acad",
          "Official+Team",
          "Official+College+Team",
          "Nike+Skills+Academy",
          "Nike+Elite+100",
          "Nike+Basketball+Academy",
          "Newspaper",
          "Nets+Workout",
          "NBA+Top+100+Camp",
          "NBA+Pre-Draft+Camp",
          "NBA+Draft+Combine",
          "LeBron+James+Camp",
          "Kevin+Durant+Camp",
          "Hoop+Summit",
          "Eurocamp",
          "Elite+24",
          "Deron+Williams+Camp",
          "D-League+Elite+Camp",
          "Clippers+Workout",
          "Biosteel+All-Canadian+Game",
          "Big+Man+Skills+Acad",
          "Amare+Stoudemire+Camp"
        )
      )
    df_sources
  }


generate_base_de_urls <-
  function(year = 2016,
           source = "NBA Draft Combine",
           position = NULL) {
    if (year %>% purrr::is_null()) {
      year_slug <- 'all'
    } else {
      year_slug <-
        year
    }

    if (position %>% purrr::is_null()) {
      position_slug <- 'all'
    } else {
      positions <- c("ALL","C", "PF", "SF", "SG", "PG")
      position_options <- positions %>% str_c(collapse = '\n')
      if (!position %>% str_to_upper() %in% positions) {
        stop(glue::glue("Sorry sources can only be:\n{position_options}"))
      }
      position_slug <-
        position %>% str_to_lower()
    }

    if (source %>% purrr::is_null()) {
      source <- 'All'
    }
    df_sources <-
      get_data_draft_express_combine_sources()
    sources <- df_sources$nameSource %>% str_to_lower()
    if (!source %>% str_to_lower() %in% sources) {
      source_options <- df_sources$nameSource %>% str_c(collapse = '\n')
      stop(glue::glue("Sorry sources can only be:\n{source_options}"))
    }
    source_search <-
      source %>% str_to_lower()

    slug_source <-
      df_sources %>%
      mutate(nameSource = nameSource %>% str_to_lower()) %>%
      filter(nameSource %in% source_search) %>%
      .$slugSource

    url <-
      glue::glue(
        'http://www.draftexpress.com/nba-pre-draft-measurements/{year_slug}/{slug_source}/{position_slug}/all'
      )
    urls <-
      url %>%
      get_draft_urls()

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
      mutate(urlSearch = url)
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
#' @param year draft years - if \code{NULL} or \code{ALL} all draft years
#' @param sources sources to search \itemize{
#' \item NULL - All Sources
#' \item All - All sources
#' \item Vince Carter Camp
#' \item USA Pan Am Team
#' \item USA Basketball
#' \item UA All-American Camp
#' \item Reebok Breakout
#' \item Portsmouth
#' \item Paul Pierce Camp
#' \item PG Skills Acad
#' \item Official Team
#' \item Official College Team
#' \item Nike Skills Academy
#' \item Nike Elite 100
#' \item Nike Basketball Academy
#' \item Newspaper
#' \item Nets Workout
#' \item NBA Top 100 Camp
#' \item NBA Pre-Draft Camp
#' \item NBA Draft Combine
#' \item LeBron James Camp
#' \item Kevin Durant Camp
#' \item Hoop Summit
#' \item Eurocamp
#' \item Elite 24
#' \item Deron Williams Camp
#' \item D-League Elite Camp
#' \item Clippers Workout
#' \item Biosteel All-Canadian Game
#' \item Big Man Skills Acad
#' \item Amare Stoudemire Camp
#' }
#' @param positions - positions to return \itemize{
#' \item NULL - All positions
#' \item All - All
#' \item PG - Point Guards
#' \item SG - Shooting Guards
#' \item SF - Small Forwards
#' \item PF - Power Forwards
#' \item C - Centers
#' }
#' @param return_unique_players if \code{TRUE} returns players last data
#' @param return_message
#' @references \href{http://www.draftexpress.com/}{Draft Express}
#'
#' @return
#' @export
#' @import purrr glue dplyr curl rvest tidyr dplyr stringr readr
#' @examples
get_data_draft_express_measurements <-
  function(years = 1987:2017,
           sources = NULL,
           positions = "all",
           return_unique_players = FALSE,
           return_message = TRUE) {

    if (sources %>% purrr::is_null()) {
      sources <- 'all'
    }

    if (years %>% purrr::is_null()) {
      years <- 'all'
    }

    if (positions %>% purrr::is_null()) {
      positions <- 'all'
    }
    generate_base_de_urls_safe <-
      purrr::possibly(generate_base_de_urls, data_frame())

    df_options <-
      expand.grid(yearData = years,
                sourceData = sources,
                idPosition = positions,
                stringsAsFactors = FALSE) %>%
      tbl_df()

    df_search_urls <-
      1:nrow(df_options) %>%
      map_df(function(x){
        year_search <-
          df_options$yearData[[x]]
        source_search <-
          df_options$sourceData[[x]]
        position_search <-
          df_options$idPosition[[x]]
        search_url <- generate_base_de_urls_safe(year = year_search,
                                            source = source_search,
                                            position = position_search)
      data_frame(urlSearch = search_url,
                 positionSearch = position_search,
                 yearSearch = year_search,
                 sourceSearch = source_search)
      })

    parse_pages_data_safe <-
      purrr::possibly(parse_draft_pages, data_frame())

    urls <- df_search_urls$urlSearch

    all_data <-
      urls %>%
      map_df(function(x) {
        parse_pages_data_safe(url = x, return_message = T)
      }) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      tidyr::separate(namePlayer, into = c('namePlayer', 'detailPlayer'), sep = '\\(') %>%
      mutate(detailPlayer = detailPlayer %>% str_replace_all('\\)','')) %>%
      mutate_if(is.character,str_trim) %>%
      suppressWarnings()

    max_year <-
      all_data$yearDraft %>% max(na.rm = T)

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
        isFirstRoundPick = ifelse(isDrafted, numberDraftPick <= 30, FALSE),
        isSecondRoundPick = ifelse(isDrafted, numberDraftPick > 30, FALSE),
        isLotteryPick = ifelse(isDrafted, numberDraftPick <= 14, FALSE),
        isTop5Pick =  ifelse(isDrafted, numberDraftPick <= 5, FALSE),
        isNumber1Pick =  ifelse(isDrafted, numberDraftPick == 1, FALSE)
      ) %>%
      distinct() %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    all_data <-
      all_data %>%
      left_join(df_search_urls) %>%
      suppressMessages()


    if (return_unique_players) {
      all_data <-
        all_data %>%
        group_by(idPlayerDE) %>%
        filter(yearDraft == max(yearDraft)) %>%
        ungroup()
    }

    all_data
  }
