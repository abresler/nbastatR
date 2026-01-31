.get_player_names <-
  memoise(function(){
  data <-
    tibble(
    namePlayer = c("J.J. Barea", "Amare Stoudemire", "C.J. Watson", "Nene Hilario", "Wes Johnson", "A.J. Hammons",
                   "C.J. Wilcox"),
    nameNBA = c("Jose Juan Barea", "Amar'e Stoudemire", "CJ Watson", "Nene",
                "Wesley Johnson", "AJ Hammons",
                "CJ Wilcox")

  )
  return(data)

})


nba_injuries <-
  function(filter_returning_today = T) {
  url <-
    'http://www.rotoworld.com/teams/injuries/nba/all/'

  page <-
    url %>%
    read_html()

  players <-
    page %>%
    html_nodes('td:nth-child(1) a') %>%
    html_text()

  injury <-
    page %>%
    html_nodes('td:nth-child(2) span') %>%
    html_text()

  injuryTimeline <-
    page %>%
    html_nodes('td:nth-child(2) .impact') %>%
    html_text() %>%
    str_to_lower() %>%
    str_trim()

  dateInformation <-
    page %>%
    html_nodes('td:nth-child(2) .date') %>%
    html_text() %>%
    paste0(", ",
           Sys.Date() %>% year()
           ) %>%
    mdy() %>%
    as.Date()

  dateInjury <-
    page %>%
    html_nodes('tr+ tr td:nth-child(5)') %>%
    html_text() %>%
    paste0(", ",
           Sys.Date() %>% year()
    ) %>%
    mdy() %>%
    as.Date()

  injuryDetail <-
    page %>%
    html_nodes('.report') %>%
    html_text()

  injury_data <-
    tibble(dateInformation,
               namePlayer = players,
               dateInjury,
               injury,
               injuryTimeline,
               injuryDetail)

  player_ids <-
    nba_players()

  player_missing_df <-
    .get_player_names()

  injury_data <-
    injury_data %>%
    left_join(player_missing_df) %>%
    mutate(namePlayer = ifelse(nameNBA %>% is.na(), namePlayer, nameNBA)) %>%
    dplyr::select(-nameNBA) %>%
    suppressMessages()

  injury_data <-
    injury_data %>%
    left_join(
      player_ids %>%
        dplyr::select(
          namePlayer,
          slugTeam,
          urlPlayerHeadshot,
          idPlayer,
          urlPlayerActionPhoto
        )
    ) %>%
    dplyr::filter(!is.na(slugTeam)) %>%
    dplyr::select(slugTeam, namePlayer, everything()) %>%
    arrange(slugTeam) %>%
    suppressMessages()

  injury_data <-
    injury_data %>%
    mutate(isGTD = ifelse(injuryTimeline == 'day-to-day', T, F)) %>%
    dplyr::select(slugTeam, namePlayer, isGTD, everything())

  injury_data <-
    injury_data %>%
    separate(injuryTimeline, into = c('remove','dateReturn'),sep = 'target',
             remove = F) %>%
    dplyr::select(-remove) %>%
    mutate(dateReturn = dateReturn %>% gsub('\\ing','',.),
           dateReturn = dateReturn %>% gsub('\\.','',.) %>% str_trim(),
           dateReturn = ifelse(!dateReturn %>% is.na() & dateReturn %>% nchar < 7,
                                yes = dateReturn %>%
                                  paste0(", ",
                                         Sys.Date() %>% year()
                                  ) %>%
                                  mdy() %>%
                                  as.Date() %>% as.character, dateReturn)
           ) %>%
    suppressWarnings()

  if (filter_returning_today){
    active_players <-
      injury_data %>%
      dplyr::filter(
        dateReturn == Sys.Date() %>% as.character() |
          dateReturn == Sys.Date() %>% weekdays() %>% str_to_lower()
      ) %>%
      .$namePlayer
    if (active_players %>% length() >0 ) {
      injury_data <-
        injury_data %>%
        dplyr::filter(!namePlayer %in% active_players)
    }

  }
  injury_data
}

# ============================================================================
# INJURY SEVERITY SCORING (Added for fantasy sports)
# ============================================================================

#' Injury Status Severity Levels
#'
#' Named vector mapping injury status to severity scores (0-100).
#' Higher scores indicate greater likelihood of missing games.
#'
#' @export
#' @family injuries
INJURY_SEVERITY <- c(
  "OUT" = 100,
  "DOUBTFUL" = 80,
  "QUESTIONABLE" = 50,
  "PROBABLE" = 20,
  "AVAILABLE" = 0,

  "DAY-TO-DAY" = 40,
  "GTD" = 40
)

#' Score Injury Severity
#'
#' Converts injury status strings to numeric severity scores.
#'
#' @param status character vector of injury status strings
#'
#' @return numeric vector of severity scores (0-100)
#' @export
#' @family injuries
#'
#' @examples
#' score_injury_severity(c("OUT", "QUESTIONABLE", "PROBABLE"))
score_injury_severity <- function(status) {
  status <- toupper(status)

  vapply(status, function(s) {
    if (is.na(s) || s == "" || s == "NA") return(0)

    # Exact match
    if (s %in% names(INJURY_SEVERITY)) return(INJURY_SEVERITY[[s]])

    # Partial matches
    if (grepl("OUT", s)) return(100)
    if (grepl("DOUBT", s)) return(80)
    if (grepl("QUESTION", s)) return(50)
    if (grepl("PROB", s)) return(20)
    if (grepl("GTD|GAME.?TIME", s)) return(40)
    if (grepl("DAY", s)) return(40)

    NA_real_
  }, numeric(1), USE.NAMES = FALSE)
}

#' Categorize Injury Severity
#'
#' Converts severity scores to human-readable categories.
#'
#' @param score numeric vector of severity scores
#'
#' @return character vector of categories
#' @export
#' @family injuries
#'
#' @examples
#' categorize_injury_severity(c(100, 75, 45, 15, 0))
categorize_injury_severity <- function(score) {
  dplyr::case_when(
    is.na(score) ~ "UNKNOWN",
    score >= 80 ~ "CRITICAL",
    score >= 50 ~ "HIGH",
    score >= 25 ~ "MEDIUM",
    score > 0 ~ "LOW",
    TRUE ~ "NONE"
  )
}

#' Add Fantasy Impact to Injury Data
#'
#' Enriches injury data with severity scores and fantasy recommendations.
#'
#' @param injury_data tibble from nba_injuries()
#'
#' @return tibble with additional columns for severity and fantasy impact
#' @export
#' @family injuries
#'
#' @examples
#' \dontrun{
#' injuries <- nba_injuries()
#' injuries_enriched <- add_fantasy_impact(injuries)
#' }
add_fantasy_impact <- function(injury_data) {
  if (nrow(injury_data) == 0) return(injury_data)

  injury_data %>%
    dplyr::mutate(
      severityScore = dplyr::case_when(
        .data$isGTD ~ 40,
        grepl("out", .data$injuryTimeline, ignore.case = TRUE) ~ 100,
        grepl("doubt", .data$injuryTimeline, ignore.case = TRUE) ~ 80,
        grepl("question", .data$injuryTimeline, ignore.case = TRUE) ~ 50,
        TRUE ~ 30
      ),
      severityCategory = categorize_injury_severity(.data$severityScore),
      fantasyImpact = dplyr::case_when(
        .data$severityScore >= 80 ~ "BENCH_IMMEDIATELY",
        .data$severityScore >= 50 ~ "CONSIDER_BENCHING",
        .data$severityScore >= 25 ~ "MONITOR_CLOSELY",
        TRUE ~ "LIKELY_PLAYING"
      ),
      recommendedAction = dplyr::case_when(
        .data$severityScore >= 80 ~ "Find replacement; player likely to miss game",
        .data$severityScore >= 50 ~ "Have backup ready; game-time decision likely",
        .data$severityScore >= 25 ~ "Monitor news; may play with limitations",
        TRUE ~ "Expected to play; no action needed"
      )
    )
}
