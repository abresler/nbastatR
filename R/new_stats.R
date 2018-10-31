.generate_url_reference <-
  function() {
    user_agents <-
      c("Mozilla/5.0 (Linux; Android 7.0; SM-G892A Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/60.0.3112.107 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 7.0; SM-G930VC Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/58.0.3029.83 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0.1; SM-G935S Build/MMB29K; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/55.0.2883.91 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0.1; SM-G920V Build/MMB29K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.98 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 5.1.1; SM-G928X Build/LMY47X) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.83 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0.1; Nexus 6P Build/MMB29P) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.83 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 7.1.1; G8231 Build/41.2.A.0.219; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/59.0.3071.125 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0.1; E6653 Build/32.2.A.0.253) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.98 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0; HTC One X10 Build/MRA58K; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/61.0.3163.98 Mobile Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0; HTC One M9 Build/MRA58K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.98 Mobile Safari/537.36",
        "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A372 Safari/604.1",
        "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.34 (KHTML, like Gecko) Version/11.0 Mobile/15A5341f Safari/604.1",
        "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A5370a Safari/604.1",
        "Mozilla/5.0 (iPhone9,3; U; CPU iPhone OS 10_0_1 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) Version/10.0 Mobile/14A403 Safari/602.1",
        "Mozilla/5.0 (iPhone9,4; U; CPU iPhone OS 10_0_1 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) Version/10.0 Mobile/14A403 Safari/602.1",
        "Mozilla/5.0 (Apple-iPhone7C2/1202.466; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like Gecko) Version/3.0 Mobile/1A543 Safari/419.3",
        "Mozilla/5.0 (Windows Phone 10.0; Android 6.0.1; Microsoft; RM-1152) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Mobile Safari/537.36 Edge/15.15254",
        "Mozilla/5.0 (Windows Phone 10.0; Android 4.2.1; Microsoft; RM-1127_16056) AppleWebKit/537.36(KHTML, like Gecko) Chrome/42.0.2311.135 Mobile Safari/537.36 Edge/12.10536",
        "Mozilla/5.0 (Windows Phone 10.0; Android 4.2.1; Microsoft; Lumia 950) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2486.0 Mobile Safari/537.36 Edge/13.10586",
        "Mozilla/5.0 (Linux; Android 7.0; Pixel C Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/52.0.2743.98 Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0.1; SGP771 Build/32.2.A.0.253; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/52.0.2743.98 Safari/537.36",
        "Mozilla/5.0 (Linux; Android 6.0.1; SHIELD Tablet K1 Build/MRA58K; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/55.0.2883.91 Safari/537.36",
        "Mozilla/5.0 (Linux; Android 7.0; SM-T827R4 Build/NRD90M) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.116 Safari/537.36",
        "Mozilla/5.0 (Linux; Android 5.0.2; SAMSUNG SM-T550 Build/LRX22G) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/3.3 Chrome/38.0.2125.102 Safari/537.36",
        "Mozilla/5.0 (Linux; Android 4.4.3; KFTHWI Build/KTU84M) AppleWebKit/537.36 (KHTML, like Gecko) Silk/47.1.79 like Chrome/47.0.2526.80 Safari/537.36",
        "Mozilla/5.0 (Linux; Android 5.0.2; LG-V410/V41020c Build/LRX22G) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/34.0.1847.118 Safari/537.36",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246",
        "Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36",
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9",
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
        "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:15.0) Gecko/20100101 Firefox/15.0.1",
        "Mozilla/5.0 (CrKey armv7l 1.5.16041) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.0 Safari/537.36",
        "Roku4640X/DVP-7.70 (297.70E04154A)", "Mozilla/5.0 (Linux; U; Android 4.2.2; he-il; NEO-X5-116A Build/JDQ39) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Safari/534.30",
        "Mozilla/5.0 (Linux; Android 5.1; AFTS Build/LMY47O) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/41.99900.2250.0242 Safari/537.36",
        "Dalvik/2.1.0 (Linux; U; Android 6.0.1; Nexus Player Build/MMB29T)",
        "AppleTV6,2/11.1", "AppleTV5,3/9.1.1", "Mozilla/5.0 (Nintendo WiiU) AppleWebKit/536.30 (KHTML, like Gecko) NX/3.0.4.2.12 NintendoBrowser/4.3.1.11264.US",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64; XBOX_ONE_ED) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.79 Safari/537.36 Edge/14.14393",
        "Mozilla/5.0 (Windows Phone 10.0; Android 4.2.1; Xbox; Xbox One) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2486.0 Mobile Safari/537.36 Edge/13.10586",
        "Mozilla/5.0 (PlayStation 4 3.11) AppleWebKit/537.73 (KHTML, like Gecko)",
        "Mozilla/5.0 (PlayStation Vita 3.61) AppleWebKit/537.73 (KHTML, like Gecko) Silk/3.2",
        "Mozilla/5.0 (Nintendo 3DS; U; ; en) Version/1.7412.EU", "Mozilla/5.0 (compatible; Googlebot/2.1; +https://www.google.com/bot.html)",
        "Mozilla/5.0 (compatible; bingbot/2.0; +https://www.bing.com/bingbot.htm)",
        "Mozilla/5.0 (compatible; Yahoo! Slurp; https://help.yahoo.com/help/us/ysearch/slurp)",
        "Mozilla/5.0 (X11; U; Linux armv7l like Android; en-us) AppleWebKit/531.2+ (KHTML, like Gecko) Version/5.0 Safari/533.2+ Kindle/3.0+",
        "Mozilla/5.0 (Linux; U; en-US) AppleWebKit/528.5+ (KHTML, like Gecko, Safari/528.5+) Version/4.0 Kindle/3.0 (screen 600x800; rotate)"
      )



    user_agent <-
      user_agents[!user_agents %>% str_detect("bot|slurp")] %>%
      sample(1)

    tl_domain <-
      c('.com', '.gov', '.org') %>%
      sample(1)

    word_length <-
      8:15

    words <-
      word_length %>% sample(1)

    domain_slug <-
      1:words %>%
      map_chr(function(x) {
        sample(letters, 1)
      }) %>%
      paste0(collapse = '')

    url <-
      list('https://', domain_slug, tl_domain) %>%
      purrr::reduce(paste0)
    df <-
      data_frame(urlReferer = url,
                 userAgent = user_agent)
    df
  }

.nba_headers <-
  function() {
    structure(list("close", "no-cache", "no-cache", "1", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.67 Safari/537.36",
                   "1", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
                   "gzip, deflate, br", "en-US,en;q=0.9"), .Names = c("Connection",
                                                                      "Pragma", "Cache-Control", "Upgrade-Insecure-Requests", "User-Agent",
                                                                      "DNT", "Accept", "Accept-Encoding", "Accept-Language"))

  }
.get_slug_year <-
  function() {
    current_date <- Sys.Date()
    current_year <- lubridate::year(current_date)
    current_month <- lubridate::month(current_date)
    slug_year <- dplyr::case_when(
      current_month >= 10 ~  current_year,
      TRUE ~ current_year -1
    )
    slug_year
  }

curl_json_to_vector <-
  function(url = "https://data.nba.net/prod/v1/2017/coaches.json") {
    json <-
      curl::curl(url = url) %>%
      read_lines() %>%
      fromJSON(simplifyVector = T)

    json

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
      glue::glue("https://stats.nba.com/media/img/teams/logos/season/{slug_season}/{slug_team}_logo.svg") %>%
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
      future_map_dfr(function(x){
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
  function(url = "https://stats.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=") {
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



.generate.nba_slugs.definitions <-
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
      glue::glue("https://stats.nba.com/templates/angular/views/{table_type}/{table_type}-{table_slugs}.html") %>%
      as.character()

    data_frame(typeTable = table_type,
               slugBase = table_slugs,
               urlHeaderTable = urls)
  }

.get.nba_api_parameters <-
  function(url = "https://stats.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=1996-97&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=") {
  slug_nba <- url %>%
    str_replace_all("https://stats.nba.com/stats/", "") %>%
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

.parse.nba_headers.definitions <-
  function(url = "https://stats.nba.com/templates/angular/views/game/game-playertracking.html") {
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
      .generate.nba_slugs.definitions(table_type = table_type, table_slugs = table_slugs)

    df_tables <- df_urls$urlHeaderTable %>%
      future_map_dfr(function(x){
        .parse.nba_headers.definitions(url = x)
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
#' current_schedule()
current_schedule <-
  function() {
    slug_year <-
      .get_slug_year()
    json <-
    glue::glue("https://data.nba.net/prod/v2/{slug_year}/schedule.json") %>%
      as.character() %>%
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
    left_join(nba_teams() %>% select(idTeamHome = idTeam, nameTeamHome = nameTeam)) %>%
    select(idTeamHome, nameTeamHome, everything()) %>%
    mutate(idRow = 1:n()) %>%
    suppressMessages()

  df_away <-
    json_data$vTeam %>% flatten() %>% dplyr::as_data_frame() %>%
    purrr::set_names(c('idTeamAway', 'scoreAway', 'isWinnerAway', 'isLoserAway')) %>%
    mutate_all(as.numeric) %>%
    mutate(idRow = 1:n()) %>%
    left_join(nba_teams() %>% select(idTeamAway = idTeam, nameTeamAway = nameTeam)) %>%
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
#' coaching_staffs()
coaching_staffs <-
  function() {
    slug_year <-
      .get_slug_year()
    json <-
      glue::glue("https://data.nba.net/prod/v1/{slug_year}/coaches.json") %>%
      as.character() %>%
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
      left_join(nba_teams() %>% select(nameTeam, idTeam)) %>%
      tidyr::unite(nameCoach, nameFirst, nameLast, sep = " ") %>%
      select(nameTeam, everything()) %>%
      arrange(nameTeam) %>%
      suppressMessages()

    data

  }

.nbastats_api_parameters <-
  function(api_version = 3) {
    df <-
      glue::glue("https://data.nba.net/10s/prod/v{api_version}/today.json") %>%
      as.character() %>%
      get.json_data(use_read_lines = T, is_data_frame = T) %>%
      flatten_df() %>%
      gather(item, value) %>%
      mutate(hasSlash = value %>% str_detect("/"),
             urlNBA = ifelse(hasSlash, str_c("https://data.nba.net", value), ""),
             versionAPI = api_version) %>%
      select(versionAPI, everything())

    df <-
      df %>%
      mutate(urlNBA = ifelse(urlNBA == "", NA, urlNBA))

    df
  }

#' NBA Stats API Parameters
#'
#' @param api_versions \itemize{
#' \item 1  - V1
#' \item 2 - V2
#' \item 3 - V3
#'
#' }
#'
#' @return \code{data_frame()}
#' @export
#'
#' @examples
#' nbastats_api_parameters(1:3)
nbastats_api_parameters <-
  function(api_versions = 1:3) {
    .nbastats_api_parameters_safe <-
      purrr::possibly(.nbastats_api_parameters, data_frame())
    api_versions %>%
      future_map_dfr(function(api_version){
        .nbastats_api_parameters_safe(api_version = api_version)
      })
  }


.parse_for_players <-
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
        urlPlayerStats = glue::glue("https://stats.nba.com/player/{idPlayer}") %>% as.character(),
        urlPlayerThumbnail = glue::glue(
          "https://stats.nba.com/media/players/230x185/{idPlayer}.png"
        ) %>% as.character(),
        urlPlayerHeadshot = glue::glue(
            "https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{idPlayer}.png"
          ) %>% as.character()
        ) %>%
      mutate(
        urlPlayerActionPhoto = ifelse(isRookie, "https://stats.nba.com/media/img/league/nba-headshot-fallback.png",
                                      glue::glue("https://stats.nba.com/media/players/700/{idPlayer}.png")) %>% as.character())

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
      seq_along(json_seasons) %>%
      future_map_dfr(function(x){
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

.parse_for_teams <-
  function(json) {
    json_teams <-
      json$data$teams

    df_teams <-
      seq_along(json_teams) %>%
      future_map_dfr(function(x) {
        values <-
          json_teams[[x]] %>%
          purrr::map_chr(function(z) {
            if (z %>% length() == 0) {
              return(NA)
            }
            z %>% str_c(collapse = ", ")
          })

        items <-
          str_c("V", seq_along(values))

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
                                        glue::glue("https://stats.nba.com/media/img/teams/logos/{slugTeam}_logo.svg") %>% as.character(),
                                        "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg"
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
#' nba_teams()
nba_teams <-
  function() {
    url <- "https://stats.nba.com/js/data/ptsd/stats_ptsd.js"
    json <-
      url %>%
      readr::read_lines() %>%
      str_replace_all("var stats_ptsd =|\\;", "") %>%
      jsonlite::fromJSON(flatten = TRUE,
                         simplifyDataFrame = TRUE)

    df_teams <-
      json %>%
      .parse_for_teams()
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
#' nba_teams_seasons()
nba_teams_seasons <- function() {
  json <- "https://stats.nba.com/stats/commonteamyears/?leagueId=00" %>%
    curl_json_to_vector()
  actual_names <-
    json$resultSets$headers[[1]] %>%
    resolve_nba_names()

  data <-
    json$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = F) %>%
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
#' nba_stats_api_items()
nba_stats_api_items <-
  function(){
    url <- "https://stats.nba.com/js/data/ptsd/stats_ptsd.js"
    json <-
      url %>%
      readr::read_lines() %>%
      str_replace_all("var stats_ptsd =|\\;", "") %>%
      jsonlite::fromJSON(flatten = TRUE,
                         simplifyDataFrame = TRUE)

    df_players <-
      json %>%
      .parse_for_players() %>%
      assign(x = "df_dict_nba_players", value = ., envir = .GlobalEnv)

    df_tables <-
      json %>%
      parse_for_seasons_data()
    assign(x = "df_dict_nba_parameters", value = df_tables, envir = .GlobalEnv)

    df_teams <-
      json %>%
      .parse_for_teams() %>%
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
#' nba_players()
nba_players <-
  function() {
    con <-
      "https://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&Season=2018-19" %>%
      curl()

    json <-
      con %>%
      read_lines() %>%
      jsonlite::fromJSON(flatten = TRUE, simplifyDataFrame = T)
    names_nba <-
      json$resultSets$headers %>%
      flatten_chr()

    data <-
      json$resultSets$rowSet[[1]] %>%
      data.frame(stringsAsFactors = F) %>%
      as_data_frame()


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
        urlPlayerStats = glue::glue("https://stats.nba.com/player/{idPlayer}") %>% as.character(),
        urlPlayerThumbnail = glue::glue(
          "https://stats.nba.com/media/players/230x185/{idPlayer}.png"
        ) %>% as.character(),
        urlPlayerHeadshot = glue::glue(
          "https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{idPlayer}.png"
        ) %>% as.character()
      ) %>%
      mutate(
        urlPlayerActionPhoto = ifelse(
          isRookie,
          "https://stats.nba.com/media/img/league/nba-headshot-fallback.png",
          glue::glue("https://stats.nba.com/media/players/700/{idPlayer}.png") %>% as.character()
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
### https://stats.nba.com/stats/boxscoresummaryv2?GameID=0021700136

## images
## https://api.nba.net/0/league/video?games=0021700136&count=36&accessToken=nbainternal%7C3830242580404678b2552bbdd03b73ee

### Box-score
## advanced : https://stats.nba.com/stats/boxscoreadvancedv2?EndPeriod=10&EndRange=28800&GameID=0021700136&RangeType=0&Season=2017-18&SeasonType=Regular+Season&StartPeriod=1&StartRange=0
## scoring : https://stats.nba.com/stats/boxscorescoringv2?EndPeriod=10&EndRange=28800&GameID=0021700136&RangeType=0&Season=2017-18&SeasonType=Regular+Season&StartPeriod=1&StartRange=0

### Game Book
## https://www.nba.com/data/html/nbacom/2017/gameinfo/20171105/0021700136_Book.pdf


