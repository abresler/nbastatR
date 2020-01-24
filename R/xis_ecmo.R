.curl_chinazi <-
  function(url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=T&SeasonType=Regular%20Season&Sorter=DATE") {


    headers = c(
      `Connection` = 'keep-alive',
      `Accept` = 'application/json, text/plain, */*',
      `x-nba-stats-token` = 'true',
      `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
      `x-nba-stats-origin` = 'stats',
      `Sec-Fetch-Site` = 'same-origin',
      `Sec-Fetch-Mode` = 'cors',
      `Referer` = 'https://downwiththechinazis.com',
      `Accept-Encoding` = 'gzip, deflate, br',
      `Accept-Language` = 'en-US,en;q=0.9'
    )

    # headers = c(
    #   `Connection` = 'close',
    #   `Pragma` = 'no-cache',
    #   `Cache-Control` = 'no-cache',
    #   `DNT` = '1',
    #   `Upgrade-Insecure-Requests` = '1',
    #   `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.29 Safari/537.36',
    #   `Sec-Fetch-User` = '?1',
    #   `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
    #   `Sec-Fetch-Site` = 'cross-site',
    #   `Sec-Fetch-Mode` = 'navigate',
    #   `Referer` = 'https://downwiththechinazis.com',
    #   `Accept-Encoding` = 'gzip, deflate, br',
    #   `Accept-Language` = 'en-US,en;q=0.9'
    # )

    res <-
      httr::GET(url,
                httr::add_headers(.headers = headers))

    json <-
      res$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(simplifyVector = T)

    json

}
