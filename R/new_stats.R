
curl_json_to_vector <-
  function(url = "https://data.nba.net/prod/v1/2017/coaches.json") {
  json <-
    url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)

  json

}

# utils -------------------------------------------------------------------
remove_na_columns <-
  function(data) {
    data %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))
  }

generate_team_season_logo <-
  function(season = 1987, slug_team = "SAC") {

    slug_season <-
      generate_season_slug(season = season)

    url <-
      glue::glue("http://stats.nba.com/media/img/teams/logos/season/{slug_season}/{slug_team}_logo.svg") %>%
      as.character()

    url
  }

generate_team_seasons_logos_data <-
  function(seasons = 1951:2018, slug_teams = "SAC") {
    input_df <-
      expand.grid(season = seasons,
                slug_team = slug_teams,
                stringsAsFactors = F) %>%
      as_data_frame()
    generate_team_season_logo_safe <-
      purrr::possibly(generate_team_season_logo, NULL)

    1:nrow(input_df) %>%
      map_df(function(x){
        df_row <-
          input_df %>% slice(x)
        season <- df_row$season
        slug_team <- df_row$slug_team
        url <-
          generate_team_season_logo(season = season, slug_team = slug_team)

        if (url %>% length() == 0) {
          return(invisible())
        }

        data_frame(yearSeason = season, slugTeam = slug_team, urlTeamLogoSeason = url)

      })
  }


parse.nba.json_data <-
  function(url = "http://stats.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=") {
    json_data <- url %>%
      get.json_data(use_read_lines = TRUE, is_data_frame = TRUE, is_flattened = TRUE)

    js_names <-
      names(json_data)[!names(json_data) %in% "resource"]

    table_name <-
      json_data$resultSets$name


    df_parameters <-
      data_frame(item = json_data[["parameters"]] %>% names(),
                 value = json_data[["parameters"]] %>% as.character()) %>%
      mutate(value = ifelse(value == "NULL", NA, value)) %>%
      filter(!value %>% is.na(),
             !value == "0")



    names_nba <-
      json_data$resultSets$headers %>% flatten_chr()

    ##
    names_actual <- names_nba

    data <-
      json_data$resultSets$rowSet %>%
      data.frame(stringsAsFactors = F) %>%
      dplyr::as_data_frame() %>%
      purrr::set_names(names_actual)




  }

generate.nba_api.items <-
  function(nba_slug = "MeasureType",
           param_slug = "(Base)|(Advanced)|(Misc)|(Four Factors)|(Scoring)|(Opponent)|(Usage)",
           allow_blank = FALSE) {

    values <-
      param_slug %>%
      str_replace_all("\\^|\\$|\\?", '') %>%
      str_split("\\|") %>%
      flatten_chr() %>%
      str_replace_all("\\(|\\)", "")

    data_frame(apiItem = nba_slug, values, isBlankAllowed = allow_blank) %>%
      mutate_if(is.numeric,
                as.character())
  }






# Table_headers -----------------------------------------------------------



generate.nba_slugs.definitions <-
  function(table_slugs = c(
    "traditional",
    "advanced",
    "scoring",
    "misc",
    "usage",
    "fourfactors",
    "hustlestats",
    "playertrack"
  ),
  table_type = "game") {
    urls <-
      glue::glue("http://stats.nba.com/templates/angular/views/{table_type}/{table_type}-{table_slugs}.html") %>%
      as.character()

    data_frame(typeTable = table_type,
               slugBase = table_slugs,
               urlHeaderTable = urls)
  }

get.nba_api_parameters <-
  function(url = "http://stats.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=1996-97&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=") {
  slug_nba <- url %>%
    str_replace_all("http://stats.nba.com/stats/", "") %>%
    str_split("\\?") %>%
    flatten_chr() %>%
    .[[1]]

  api_options <-
    url %>%
    get.json_data() %>%
    .$parameters %>%
    names()
  gc()

  data_frame(slugBase = slug_nba,
             parameterOption = api_options,
             urlNBAStatsAPI = url)
}

parse.nba_headers.definitions <-
  function(url = "http://stats.nba.com/templates/angular/views/game/game-playertracking.html") {
    page <-
      url %>%
      read_html()

    name_nba <-
      page %>%
      html_nodes("dt") %>%
      html_text()

    description_nba <-
      page %>%
      html_nodes("dd") %>%
      html_text()

    data_frame(
      nameNBA = name_nba,
      descriptionItem = description_nba,
      urlHeaderTable = url
    )

  }

get.nba_headers <-
  function(table_type = "game",
           table_slugs = c(
    "traditional",
    "advanced",
    "scoring",
    "misc",
    "usage",
    "fourfactors",
    "hustlestats",
    "playertrack"
  )) {
    df_urls <-
      generate.nba_slugs.definitions(table_type = table_type, table_slugs = table_slugs)

    df_tables <- df_urls$urlHeaderTable %>%
      map_df(function(x){
        parse.nba_headers.definitions(url = x)
      })

    df_tables %>%
      left_join(df_urls) %>%
      dplyr::select(typeTable, slugBase, everything()) %>%
      suppressMessages()
  }


# base --------------------------------------------------------------------

#' NBA current season schedule
#'
#' Get NBA schedule for most current season
#'
#' @return a `data_frame`
#' @export
#'
#' @import curl jsonlite readr purrr stringr lubridate
#' @examples
#' get_nba_current_season_schedule()
get_nba_current_season_schedule <-
  function() {
  json <-
    "https://data.nba.net/prod/v2/2017/schedule.json" %>%
  curl_json_to_vector()

  json_data <- json$league$standard

  df_season_games <-
    json_data[!json_data %>% names() %in% c("period", "nugget", "hTeam", "vTeam", "watch", "playoffs")] %>%
    dplyr::as_data_frame()

  df_season_games <-
    df_season_games %>%
    purrr::set_names(c("slugGame", "idStageGame", "slugGameCode", "idGameStatus",
                       "hasExtendedStatus", "isUnknownStartTime", "datetimeGame", "dateSlugGame",
                       "timeEasternGame", "hasBuzzerBeater", "tags")) %>%
    select(-one_of("tags")) %>%
    tidyr::separate(slugGameCode,
                    into = c("idGame", "slugTeams"),
                    sep = "/")
  season <- df_season_games$idGame[[1]] %>% substr(1,4) %>% as.numeric() + 1

  df_season_games <-
    df_season_games %>%
    mutate(
      yearSeason = season,
      idGame = idGame %>% as.numeric(),
      slugTeamHome = slugTeams %>% substr(4, 6),
      slugTeamAway = slugTeams %>% substr(1, 3)
    ) %>%
    mutate(
      idGame = slugGame %>% as.numeric(),
      urlNBAGameBook = glue::glue(
        "https://data.nba.net/prod/v1/{dateSlugGame}/{dateSlugGame}_Book.pdf"
      ) %>% as.character(),
      datetimeGame = readr::parse_datetime(datetimeGame),
      dateGame = lubridate::ymd(dateSlugGame)
    ) %>%
    mutate(idRow = 1:n()) %>%
    select(idGame, everything()) %>%
    select(yearSeason, dateGame, slugTeamAway, slugTeamHome, everything())

  df_periods <-
    json_data$period %>%
    as_data_frame() %>%
    purrr::set_names(c("quarterMaxPlayed", "idSeasonType", "maxQuartersRegular")) %>%
    mutate(
      hasOvertime = quarterMaxPlayed > 4,
      countOTQuarters =  quarterMaxPlayed - maxQuartersRegular,
      isComplete = !quarterMaxPlayed == 1
    ) %>%
    mutate(idRow = 1:n())

  df_descriptions <-
    data_frame(descriptionGame = json_data$nugget$text) %>%
    mutate(idRow = 1:n()) %>%
    mutate_all(funs(ifelse(. == "", NA, .)))

  df_home <-
    json_data$hTeam %>% flatten() %>% dplyr::as_data_frame() %>%
    purrr::set_names(c('idTeamHome', 'scoreHome', 'isWinnerHome', 'isLoserHome')) %>%
    mutate_all(as.numeric) %>%
    mutate(idRow = 1:n()) %>%
    left_join(get_nba_teams() %>% select(idTeamHome = idTeam, nameTeamHome = nameTeam)) %>%
    select(idTeamHome, nameTeamHome, everything()) %>%
    mutate(idRow = 1:n()) %>%
    suppressMessages()

  df_away <-
    json_data$vTeam %>% flatten() %>% dplyr::as_data_frame() %>%
    purrr::set_names(c('idTeamAway', 'scoreAway', 'isWinnerAway', 'isLoserAway')) %>%
    mutate_all(as.numeric) %>%
    mutate(idRow = 1:n()) %>%
    left_join(get_nba_teams() %>% select(idTeamAway = idTeam, nameTeamAway = nameTeam)) %>%
    select(idTeamAway, nameTeamAway, everything()) %>%
    mutate(idRow = 1:n()) %>%
    suppressMessages()

  data <-
    list(df_season_games, df_periods, df_home, df_away, df_descriptions) %>%
    purrr::reduce(left_join) %>%
    suppressMessages() %>%
    select(-idRow) %>%
    dplyr::select(idSeasonType, dateGame, timeEasternGame, idGame, everything())

  data
}

#' NBA active coaching staffs
#'
#' Gets active coaching staff information for all
#' NBA teams
#'
#' @return a \code{data_frames}
#' @export
#' @import tidyr curl jsonlite dplyr stringr
#' @family Current data
#' @family Coaching
#' @family Roster information
#' @examples
#' get_coaching_staffs()
get_coaching_staffs <-
  function() {
    json <- "https://data.nba.net/prod/v1/2017/coaches.json" %>%
      curl() %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = T)

    data <-
      json$league %>% flatten_df() %>%
      purrr::set_names(c("nameFirst", "nameLast",
                         "isHeadCoach", "idCoach",
                         "idTeam", "numberSort",
                         "nameCollegeCoach")) %>%
      mutate_at(c("idCoach", "numberSort", "idTeam"),
                funs(. %>% as.numeric())) %>%
      select(-numberSort)

    data <-
      data %>%
      left_join(get_nba_teams() %>% select(nameTeam, idTeam)) %>%
      tidyr::unite(nameCoach, nameFirst, nameLast, sep = " ") %>%
      select(nameTeam, everything()) %>%
      arrange(nameTeam) %>%
      suppressMessages()

    data

  }

get_dictionary_todays_parameters <-
  function() {
    df <-
      "https://data.nba.net/10s/prod/v3/today.json" %>%
      get.json_data(use_read_lines = T, is_data_frame = T) %>%
      flatten_df() %>%
      gather(item, value) %>%
      mutate(hasSlash = value %>% str_detect("/"),
             urlNBA = ifelse(hasSlash, str_c("https://data.nba.net", value), ""))
    df
  }


parse_for_players <-
  function(json) {
    df_players <-
      json$data$players %>%
      dplyr::as_data_frame() %>%
      purrr::set_names(
        c(
          "idPlayer",
          "namePlayerLastFirst",
          "isActive",
          "yearSeasonFirst",
          "yearSeasonLast",
          "idTeam",
          "hasGamesPlayedFlag"
        )
      ) %>%
      mutate_at(
        c(
          "idPlayer",
          "isActive",
          "idTeam",
          "yearSeasonLast",
          "yearSeasonFirst"
        ),
        funs(. %>% as.integer())
      ) %>%
      mutate(
        isActive = as.logical(isActive),
        countSeasons = (yearSeasonLast - yearSeasonFirst)
      )

    most_recent <-
      df_players %>% pull(yearSeasonFirst) %>% max(na.rm = T)

    df_players <-
      df_players %>%
      mutate(
        hasGamesPlayedFlag = ifelse(hasGamesPlayedFlag == "Y", TRUE, FALSE),
        idTeam = ifelse(idTeam == 0, NA, idTeam),
        isRookie = ifelse(countSeasons == 0 &
                            yearSeasonFirst == most_recent, TRUE, FALSE),
        urlPlayerStats = glue::glue("http://stats.nba.com/player/{idPlayer}") %>% as.character(),
        urlPlayerThumbnail = glue::glue(
          "http://stats.nba.com/media/players/230x185/{idPlayer}.png"
        ) %>% as.character(),
        urlPlayerHeadshot = glue::glue(
            "https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{idPlayer}.png"
          ) %>% as.character()
        ) %>%
      mutate(
        urlPlayerActionPhoto = ifelse(isRookie, "http://stats.nba.com/media/img/league/nba-headshot-fallback.png",
                                      glue::glue("http://stats.nba.com/media/players/700/{idPlayer}.png")) %>% as.character())

    df_players <-
      df_players %>%
      mutate(NP = namePlayerLastFirst %>% sub("\\,", "\\:",.)) %>%
      tidyr::separate(NP, into = c("namePlayerLast", "namePlayerFirst"), sep = "\\:") %>%
      mutate(namePlayer = ifelse(namePlayerFirst %>% is.na(), namePlayerLast,
                                 str_c(namePlayerFirst, namePlayerLast, sep = " ")) %>% str_trim()) %>%
      dplyr::select(idPlayer, namePlayer, everything()) %>%
      suppressWarnings()

    df_players <-
      df_players %>%
      mutate(urlPlayerThumbnail = if_else(yearSeasonFirst >= 2017, urlPlayerHeadshot, urlPlayerThumbnail))


    df_players
  }

parse_for_seasons_data <-
  function(json) {
    json_seasons <-
      json$data$seasons

    seasons <-
      1:length(json_seasons) %>%
      map_df(function(x){
        row <-
          json_seasons[[x]]

        values <-
          row[1:5] %>%
          flatten_chr()

        items <-
          c("nameParameter", "slugLeague", "yearDataEnd", "idLeagueSeasonType", "yearDataStart")

        data_frame(items, values) %>%
          mutate(values = values %>% as.character()) %>%
          tidyr::spread(items, values)
      })

    seasons %>%
      mutate_all(as.character) %>%
      mutate_at(c( "yearDataEnd", "idLeagueSeasonType", "yearDataStart"),
                funs(. %>% as.numeric()))
  }

parse_for_teams <-
  function(json) {
    json_teams <-
      json$data$teams

    df_teams <-
      1:length(json_teams) %>%
      map_df(function(x) {
        values <-
          json_teams[[x]] %>%
          purrr::map_chr(function(z) {
            if (z %>% length() == 0) {
              return(NA)
            }
            z %>% str_c(collapse = ", ")
          })

        items <-
          str_c("V", 1:length(values))

        data_frame(items, values) %>%
          tidyr::spread(items, values)

      }) %>%
      dplyr::select(one_of(
        c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10",
          "V11")
      )) %>%
      purrr::set_names(c("idTeam", "slugTeam", "teamName", "cityTeam", "teamNameFull",
                         "idConference", "idDivision", "isNonNBATeam", "yearPlayedLast", "idLeague",
                         "colorsTeam")) %>%
      mutate_at(c("idTeam",
                "idConference", "idDivision", "isNonNBATeam", "yearPlayedLast", "idLeague"),
                funs(. %>% as.integer())) %>%
      mutate(nameTeam = str_c(cityTeam, teamNameFull, sep = " ")) %>%
      mutate_if(is.character,
                funs(ifelse(. == "", NA, .))) %>%
      dplyr::select(nameTeam, everything())

    df_teams <-
      df_teams %>%
      mutate(nameTeam = nameTeam %>% str_replace_all("LA Clippers", "Los Angeles Clippers"),
               urlThumbnailTeam = if_else(isNonNBATeam == 0,
                                        glue::glue("http://stats.nba.com/media/img/teams/logos/{slugTeam}_logo.svg") %>% as.character(),
                                        "http://stats.nba.com/media/img/teams/logos/NBA_logo.svg"
                                        ))
    df_teams

  }

#' NBA team dictionary
#'
#' Returns team dictionary
#'
#' @return a `data_frame`
#' @export
#' @importFrom readr read_lines
#' @importFrom glue glue
#' @import dplyr jsonlite stringr tidyr purrr
#' @examples
#' get_nba_teams()
get_nba_teams <-
  function() {
    url <- "http://stats.nba.com/js/data/ptsd/stats_ptsd.js"
    json <-
      url %>%
      readr::read_lines() %>%
      str_replace_all("var stats_ptsd =|\\;", "") %>%
      jsonlite::fromJSON(flatten = TRUE,
                         simplifyDataFrame = TRUE)

    df_teams <-
      json %>%
      parse_for_teams()
    df_teams
  }

#' NBA teams seasons
#'
#'
#'
#' @return a \code{data_frame()}
#' @export
#'
#' @examples
#' get_nba_teams_seasons()
get_nba_teams_seasons <- function() {
  json <- "http://stats.nba.com/stats/commonteamyears/?leagueId=00" %>%
    curl_json_to_vector()
  actual_names <-
    json$resultSets$headers[[1]] %>%
    resolve_nba_names()

  data <-
    json$resultSets$rowSet[[1]] %>%
    as_data_frame() %>%
    purrr::set_names(actual_names) %>%
    munge_nba_data() %>%
    mutate(isActiveTeam = yearSeasonLast == max(yearSeasonLast)) %>%
    select(isActiveTeam, everything())
  data
}

#' NBA stats API parameters, teams and items
#'
#'
#'
#' @return a `data_frame`
#' @export
#' @import readr jsonlite dplyr purrr tibble tidyr stringr
#' @examples
#' get_nba_stats_api_items()
get_nba_stats_api_items <-
  function(){
    url <- "http://stats.nba.com/js/data/ptsd/stats_ptsd.js"
    json <-
      url %>%
      readr::read_lines() %>%
      str_replace_all("var stats_ptsd =|\\;", "") %>%
      jsonlite::fromJSON(flatten = TRUE,
                         simplifyDataFrame = TRUE)

    df_players <-
      json %>%
      parse_for_players() %>%
      assign(x = "df_dict_nba_players", value = ., envir = .GlobalEnv)

    df_tables <-
      json %>%
      parse_for_seasons_data()
    assign(x = "df_dict_nba_parameters", value = df_tables, envir = .GlobalEnv)

    df_teams <-
      json %>%
      parse_for_teams() %>%
      assign(x = "df_dict_nba_teams", value = ., envir = .GlobalEnv)

    data_frame(
      nameTable = c("Players", "Teams", "API Parameters"),
      dataTable = list(df_players, df_teams, df_tables)
    )
  }

#' NBA player dictionary
#'
#'
#' @return a \code{data_frame}
#' @export
#'
#' @import dplyr purrr jsonlite curl stringr lubridate
#' @importFrom glue glue
#' @importFrom readr read_lines
#' @examples
#' get_nba_players()
get_nba_players <-
  function() {
    con <-
      "http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&Season=2017-18" %>%
      curl()

    json <-
      con %>%
      read_lines() %>%
      jsonlite::fromJSON(flatten = TRUE, simplifyDataFrame = T)

    data <-
      json$resultSets$rowSet[[1]] %>%
      dplyr::as_data_frame()

    names_nba <-
      json$resultSets$headers %>%
      flatten_chr()

    df_names <-
      dictionary_nba_names()

    actual_names <-
      names_nba %>%
      map_chr(function(x) {
        df_names %>% filter(nameNBA == x) %>% pull(nameActual)
      })
    df_players <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate_at(
        c(
          "idPlayer",
          "idTeam",
          "isActive",
          "yearSeasonLast",
          "yearSeasonFirst"
        ),
        funs(. %>% as.integer())
      ) %>%
      mutate_if(is.character,
                funs(ifelse(. == "", NA, .))) %>%
      mutate(
        isActive = as.logical(isActive),
        countSeasons = (yearSeasonLast - yearSeasonFirst)
      )

    most_recent <-
      df_players %>% pull(yearSeasonFirst) %>% max(na.rm = T)


    df_players <-
      df_players %>%
      mutate(
        hasGamesPlayedFlag = ifelse(hasGamesPlayedFlag == "Y", TRUE, FALSE),
        idTeam = ifelse(idTeam == 0, NA, idTeam),
        isRookie = ifelse(countSeasons == 0 &
                            yearSeasonFirst == most_recent, TRUE, FALSE),
        urlPlayerStats = glue::glue("http://stats.nba.com/player/{idPlayer}") %>% as.character(),
        urlPlayerThumbnail = glue::glue(
          "http://stats.nba.com/media/players/230x185/{idPlayer}.png"
        ) %>% as.character(),
        urlPlayerHeadshot = glue::glue(
          "https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{idPlayer}.png"
        ) %>% as.character()
      ) %>%
      mutate(
        urlPlayerActionPhoto = ifelse(
          isRookie,
          "http://stats.nba.com/media/img/league/nba-headshot-fallback.png",
          glue::glue("http://stats.nba.com/media/players/700/{idPlayer}.png") %>% as.character()
        )
      )

    df_players <-
      df_players %>%
      mutate(NP = namePlayerLastFirst %>% sub("\\,", "\\:", .)) %>%
      tidyr::separate(NP,
                      into = c("namePlayerLast", "namePlayerFirst"),
                      sep = "\\:") %>%
      mutate(namePlayer = ifelse(
        namePlayerFirst %>% is.na(),
        namePlayerLast,
        str_c(namePlayerFirst, namePlayerLast, sep = " ")
      ) %>% str_trim()) %>%
      dplyr::select(idPlayer, namePlayer, everything()) %>%
      suppressWarnings()

    df_players <-
      df_players %>%
      mutate(
        urlPlayerThumbnail = if_else(yearSeasonFirst >= 2017, urlPlayerHeadshot, urlPlayerThumbnail)
      )


    df_players <- df_players %>%
      mutate(namePlayer = case_when(idPlayer == 203318 ~ "Glen Rice Jr.",
                                    TRUE ~ namePlayer))

    df_players <-
      df_players %>%
      mutate(nameTeam = ifelse(idTeam %>% is.na(), NA, str_c(cityTeam, teamName, sep =  " "))) %>%
      select(idPlayer, nameTeam, everything())

    df_players %>%
      select(isActive,
             isRookie,
             namePlayer,
             idPlayer,
             countSeasons,
             everything())
  }

# games -------------------------------------------------------------------


## game detail : https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/2017/scores/gamedetail/0021700136_gamedetail.json

## winprob:  https://stats.nba.com/stats/winprobabilitypbp?GameID=0021700106&RunType=each+second
## infographic: https://stats.nba.com/stats/infographicfanduelplayer?GameID=0021700106

## lead tracker
### https://data.nba.net/data/10s/prod/v1/20171105/0021700137_lead_tracker_1.json -- 1 through 4

### full play by play
### https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/2017/scores/pbp/0021700136_full_pbp.json

## Summary
### http://stats.nba.com/stats/boxscoresummaryv2?GameID=0021700136

## images
## https://api.nba.net/0/league/video?games=0021700136&count=36&accessToken=nbainternal%7C3830242580404678b2552bbdd03b73ee

### Box-score
## advanced : http://stats.nba.com/stats/boxscoreadvancedv2?EndPeriod=10&EndRange=28800&GameID=0021700136&RangeType=0&Season=2017-18&SeasonType=Regular+Season&StartPeriod=1&StartRange=0
## scoring : http://stats.nba.com/stats/boxscorescoringv2?EndPeriod=10&EndRange=28800&GameID=0021700136&RangeType=0&Season=2017-18&SeasonType=Regular+Season&StartPeriod=1&StartRange=0

### Game Book
## http://www.nba.com/data/html/nbacom/2017/gameinfo/20171105/0021700136_Book.pdf


