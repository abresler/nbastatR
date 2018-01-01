dictionary_organization_types <- function() {
  data <-
    data_frame(
      typeOrganizationFrom = c(NA, "College/University", "High School", "Other Team/Club"),
      slugOrganizationTypeFrom = c("UNK", "COL", "HS", "PRO")
    )
  data
}

get_nba_draft_year <-
  function(draft_year = 2015,
           return_message = T) {
    if (draft_year <= 1948) {
      stop("Drafts start in 1948")
    }
    url <-
      glue::glue(
        "http://stats.nba.com/stats/drafthistory?College=&LeagueID=00&OverallPick=&RoundNum=&RoundPick=&Season={draft_year}"
      )


    json <-
      curl::curl(url) %>%
      fromJSON(simplifyVector = T)

    table_length <-
      json$resultSets$rowSet %>% length()

    all_data <-
      1:table_length %>%
      map_df(function(table_id) {
        # table_id %>% message()
        table_name <-
          json$resultSets$name[table_id]

        df_table <-
          json %>%
          nba_json_to_df(table_id = table_id)

        df_table %>%
          dplyr::rename(yearDraft = yearSeasonStart) %>%
          tidyr::unite(nameTeam, cityTeam, teamName, sep = " ", remove = F) %>%
          select(yearDraft, numberPickOverall, numberRound, numberRoundPick, everything())
      })

    if (return_message) {
      glue::glue("Acquired {draft_year} NBA Draft data") %>%
        message()
    }

    closeAllConnections()

    all_data

  }

#' Get NBA Drafts
#'
#' Acquires NBA draft data
#'
#' @param draft_years \code{vector} of draft years
#' @param return_message if \code{TRUE} returns message
#' @param nest_data if \code{TRUE} returns nested data frame
#'
#' @return
#' @export
#' @import dplyr stringr jsonlite purrr tidyr curl
#' @importFrom glue glue
#' @examples
#' get_nba_drafts(draft_years = 2000:2018, nest_data = FALSE, return_message = TRUE)

get_nba_drafts <-
  function(draft_years = 1947:2018,
           nest_data = F,
           return_message = T) {
    get_nba_draft_year_safe <-
      purrr::possibly(get_nba_draft_year, data_frame())

    all_data <-
      draft_years %>%
      map_df(function(draft_year){
        get_nba_draft_year_safe(draft_year = draft_year,
                                return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(dictionary_organization_types()) %>%
      suppressMessages()


    df_foreign <-
      all_data %>%
      filter(slugOrganizationTypeFrom == "PRO") %>%
      tidyr::separate(
        nameOrganizationFrom,
        sep = "\\(",
        into = c("nameOrganizationFrom", "locationOrganizationFrom")
      ) %>%
      mutate(locationOrganizationFrom = locationOrganizationFrom %>% str_replace_all("\\)", "")) %>%
      mutate_if(is.character, str_trim) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      filter(!slugOrganizationTypeFrom == "PRO") %>%
      bind_rows(df_foreign) %>%
      arrange(desc(yearDraft), numberPickOverall)

    if (nest_data) {
      all_data <-
        all_data %>%
        tidyr::nest(-c(yearDraft), .key = 'dataDraft')
    }

    all_data
  }
