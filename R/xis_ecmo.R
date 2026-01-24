.curl_chinazi <-
  function(url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=P&SeasonType=Regular%20Season&Sorter=DATE") {

    headers <- c(
      `Host` = 'stats.nba.com',
      `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
      `Accept` = 'application/json, text/plain, */*',
      `Accept-Language` = 'en-US,en;q=0.9',
      `Accept-Encoding` = 'gzip, deflate, br',
      `x-nba-stats-origin` = 'stats',
      `x-nba-stats-token` = 'true',
      `Connection` = 'keep-alive',
      `Referer` = 'https://stats.nba.com/',
      `Pragma` = 'no-cache',
      `Cache-Control` = 'no-cache'
    )

    res <-
      GET(url,
                add_headers(.headers = headers))

    json <-
      res$content %>%
      rawToChar() %>%
      fromJSON(simplifyVector = T)

    closeAllConnections()

    json

}
