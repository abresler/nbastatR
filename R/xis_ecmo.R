.curl_chinazi <-
  function(url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=P&SeasonType=Regular%20Season&Sorter=DATE",
           timeout_seconds = 60) {

    headers <- c(
      `Host` = "stats.nba.com",
      `Connection` = "keep-alive",
      `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
      `Accept` = "application/json, text/plain, */*",
      `Accept-Language` = "en-US,en;q=0.9",
      `Accept-Encoding` = "gzip, deflate, br",
      `x-nba-stats-origin` = "stats",
      `x-nba-stats-token` = "true",
      `Origin` = "https://www.nba.com",
      `Sec-Fetch-Site` = "same-site",
      `Sec-Fetch-Mode` = "cors",
      `Sec-Fetch-Dest` = "empty",
      `Referer` = "https://www.nba.com/"
    )

    res <-
      httr::GET(url,
                httr::add_headers(.headers = headers),
                httr::timeout(timeout_seconds))

    json <-
      res$content %>%
      rawToChar() %>%
      fromJSON(simplifyVector = TRUE)

    closeAllConnections()

    json
  }
