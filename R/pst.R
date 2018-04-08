# http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search

fix_double_name <-
  function(x) {
    if (x %>% is.na()) {
      return(x)
    }
    count_slashes <-x %>% str_count("/")
    if (count_slashes == 0) {
      return(x)
    }
    actual_name <- x %>% str_split("/") %>% flatten_chr() %>% str_trim() %>% .[[2]]
    actual_name
  }

munge_data <-
  function(data) {

    data <-
      data %>%
      separate(nicknameTeam,
               into = c("nicknameTeam", "slugLeague"),
               sep = "\\(") %>%
      mutate(
        slugLeague = slugLeague %>% str_replace("\\)", ""),
        slugLeague = if_else(slugLeague %>% is.na(), "NBA", slugLeague),
        isTrade = descriptionTransaction %>% str_detect("trade"),
        isDraftPick = descriptionTransaction %>% str_detect("round pick"),
        isHiredExec = descriptionTransaction %>% str_detect("hire"),
        isPromotedExec = descriptionTransaction %>% str_detect("promote"),
        isLostFreeAgent = descriptionTransaction %>% str_detect("lost free agent"),
        isSignedFreeAgent = descriptionTransaction %>% str_detect("signed"),
        isWaived = descriptionTransaction %>% str_detect("waived"),
        isActivedFromIL = descriptionTransaction %>% str_detect("activated from IL"),
        isIRPlacement = descriptionTransaction %>% str_detect("placed on IR"),
        isILPlacement = descriptionTransaction %>% str_detect("placed on IL"),
        isArrested = descriptionTransaction %>% str_detect("arrest"),
        iSurgery = descriptionTransaction %>% str_detect("Surgery"),
        isFine = descriptionTransaction %>% str_detect("fined"),
        isDNP = descriptionTransaction %>% str_detect("DNP"),
        isLostExpansionDraft = descriptionTransaction %>% str_detect("lost in expansion draft"),
        isDTD = descriptionTransaction %>% str_detect("DTD")
      ) %>%
      mutate_if(is.character,
                funs(str_trim)) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate(namePlayerAcquired = namePlayerAcquired %>% map_chr(fix_double_name),
             namePlayerRelinquished = namePlayerRelinquished %>% map_chr(fix_double_name))

   data
  }

# generate ----------------------------------------------------------------

generate_pst_url <-
  function(person = NULL,
         team = NULL,
         date_from = NULL,
         date_to = NULL,
         include_trades = T,
         include_injury_list_movement = T,
         include_g_league_movement = T,
         include_injury_missed_games = T,
         include_personal_missed_games = T,
         include_discipline = T,
         include_criminal_incidents = T
) {

    if (person %>% purrr::is_null()) {
      entity_slug <- ''
    } else {
      entity_slug <-
        person %>% URLencode()
    }
    if (team %>% purrr::is_null()) {
      team_slug <- ''
    } else {
      team_slug <-
        team %>% URLencode()
    }

    if (date_from %>% purrr::is_null()) {
      date_f_slug <- ''
    } else {
      date_f_slug <- lubridate::ymd(date_from)
    }

    if (date_to %>% purrr::is_null()) {
      date_t_slug <- ''
    } else {
      date_t_slug <- lubridate::ymd(date_to)
    }

    player_m_slug <-
      case_when(include_trades == T ~ "yes",
                TRUE ~ "no")


    il_slug <-
      case_when(include_injury_list_movement == T ~ "yes",
                TRUE ~ "no")

    gl_slug <-
      case_when(include_g_league_movement == T ~ "yes",
                TRUE ~ "no")
    injury_slug  <-
      case_when(include_injury_missed_games == T ~ "yes",
                TRUE ~ "no")
    personal_slug <-
      case_when(include_personal_missed_games == T ~ "yes",
                TRUE ~ "no")
    disc_slug  <-
      case_when(include_discipline == T ~ "yes",
                TRUE ~ "no")
    legal_slug <-
      case_when(include_criminal_incidents == T ~ "yes",
                TRUE ~ "no")



  base_url <- "http://www.prosportstransactions.com/basketball/Search/SearchResults.php?"
  url <-
    glue::glue("{base_url}Player={entity_slug}&Team={team_slug}&BeginDate={date_f_slug}&EndDate={date_t_slug}&PlayerMovementChkBx={player_m_slug}&ILChkBx={il_slug}&NBADLChkBx={gl_slug}&InjuriesChkBx={injury_slug}&PersonalChkBx={personal_slug}&DisciplinaryChkBx={disc_slug}&LegalChkBx={legal_slug}&Submit=Search") %>% as.character()
  url
}

# parse -------------------------------------------------------------------
parse.pst.page <-
    function(url = "http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search&start=110050") {
    page <-
      url %>%
        read_page()
    df_items <-
      data_frame(idNode = 1:5,
               item = c("dateTransaction","nicknameTeam",
                        "namePlayerAcquired", "namePlayerRelinquished",
                        "descriptionTransaction"))
   data <-
     1:5 %>%
      map_df(function(x){
        value <-
          page %>%
          html_nodes(css = glue::glue(".center td:nth-child({x})") %>% as.character()) %>%
          html_text() %>%
          str_trim()

        value <- value[2:length(value)]

        value <- case_when(value == "" ~ NA_character_,
                  TRUE ~ value)
        if (x %in% 3:5) {
        value <-
         value %>%
          map_chr(function(v){
            v %>%
              str_split(". ") %>%
              flatten_chr() %>%
              discard(~.x == "") %>% str_c(collapse = " | ")
          })
        }

        item <-
          df_items %>% filter(idNode == x) %>% pull(item)


        data_frame(item, value) %>%
          mutate(idRow = 1:n())
      })

   data <-
     data %>%
     spread(item, value) %>%
     select(-idRow) %>%
     select(one_of(df_items$item)) %>%
     mutate(dateTransaction = dateTransaction %>% lubridate::ymd(),
            urlPST = url)
   data
    }

parse_pst_urls <-
  function(urls = c("http://www.prosportstransactions.com/basketball/SearchSearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search&start=97350",
                    "http://www.prosportstransactions.com/basketball/SearchSearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search&start=2025",
                    "http://www.prosportstransactions.com/basketball/SearchSearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search&start=106175",
                    "http://www.prosportstransactions.com/basketball/SearchSearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search&start=22675",
                    "http://www.prosportstransactions.com/basketball/SearchSearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search&start=46650"
  ),
           return_message = T) {
    df <-
      data_frame()

    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      parse.pst.page.safe <-
        purrr::possibly(parse.pst.page, data_frame())

      all_data <-
        parse.pst.page(url = url)


      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

get_pst_result_url_df <-
  function(url = "http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&PlayerMovementChkBx=yes&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes&LegalChkBx=yes&Submit=Search&start=110050") {
    page <-
      url %>%
      read_page()
    pages <-
      page %>%
      html_nodes('.center+ table td:nth-child(3) a') %>%
      html_text() %>%
      readr::parse_number()

    if (pages %>% length() == 0) {
      return(data_frame(idPage = 1,
                 urlPST = url))
    }

    urls <-
      page %>%
      html_nodes('.center+ table td:nth-child(3) a') %>%
      html_attr('href') %>%
      str_c("http://www.prosportstransactions.com/basketball/Search/", .)

    data_frame(idPage = pages,
               urlPST = urls)
  }

#' ProSports NBA transactions
#'
#' Returns data matching specified
#' parameters
#'
#' @param person if not \code{NULL} vector of person name
#' @param team if not \code{NULL} vector of team name
#' @param date_from if not \code{NULL} vector of dates
#' @param date_to if not \code{NULL} vector of person name
#' @param include_trades if \code{TRUE} includes trades
#' @param include_injury_list_movement if \code{TRUE} includes injury list
#' @param include_g_league_movement if \code{TRUE} includes g-league movement
#' @param include_injury_missed_games if \code{TRUE} includes injury missed games
#' @param include_personal_missed_games if \code{TRUE} include personal missed games
#' @param include_discipline if \code{TRUE} include disciple
#' @param include_criminal_incidents if \code{TRUE} include criminal incidents
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' get_nba_pst_transaction(person = NULL, team = "Nets")
get_nba_pst_transaction <-
  function(person = "Jarrett Jack",
           team = NULL,
           date_from = NULL,
           date_to = NULL,
           include_trades = T,
           include_injury_list_movement = T,
           include_g_league_movement = T,
           include_injury_missed_games = T,
           include_personal_missed_games = T,
           include_discipline = T,
           include_criminal_incidents = T,
           return_message = T) {
    url <-
      generate_pst_url(person = person,
                     team = team,
                     date_from = date_from,
                     date_to = date_to,
                     include_trades = include_trades,
                     include_injury_list_movement = include_injury_list_movement,
                     include_g_league_movement = include_g_league_movement,
                     include_injury_missed_games = include_injury_missed_games,
                     include_personal_missed_games = include_personal_missed_games,
                     include_discipline = include_discipline,
                     include_criminal_incidents = include_criminal_incidents)
    df_urls <-
      get_pst_result_url_df(url = url)
    data <-
      parse_pst_urls(urls = df_urls$urlPST, return_message = return_message)

    data %>%
      munge_data()
  }
