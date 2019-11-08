.curl_chinazi <-
  function(url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=T&SeasonType=Regular%20Season&Sorter=DATE") {
    cookies = c(
      'AMCVS_7FF852E2556756057F000101%40AdobeOrg' = '1',
      's_vi' = '[CS]v1|2EE24D7A050788DB-4000011720008794[CE]',
      'AMCVS_248F210755B762187F000101%40AdobeOrg' = '1',
      's_ecid' = 'MCMID%7C25953548762710494002061391560771239414',
      'AMCV_7FF852E2556756057F000101%40AdobeOrg' = '2121618341%7CMCIDTS%7C18208%7CMCMID%7C25956765233563137992061161239589822345%7CMCAAMLH-1573770612%7C7%7CMCAAMB-1573770613%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1573173012s%7CNONE%7CMCAID%7C2EE24D7A050788DB-4000011720008794',
      'ug' = '5dc49af50145f20a3f95330016b22532',
      'ugs' = '1',
      '_ga' = 'GA1.2.933847444.1573165816',
      '_gid' = 'GA1.2.1733170427.1573165816',
      's_cc' = 'true',
      'AMCV_248F210755B762187F000101%40AdobeOrg' = '1585540135%7CMCIDTS%7C18208%7CMCMID%7C25953548762710494002061391560771239414%7CMCAAMLH-1573770615%7C7%7CMCAAMB-1573770615%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1573173015s%7CNONE%7CMCAID%7C2EE24D7A050788DB-4000011720008794%7CvVersion%7C4.4.0',
      '_gcl_au' = '1.1.1614741273.1573165817',
      '_fbp' = 'fb.1.1573165817535.300482637',
      '__gads' = 'ID=6abeeb7861dfac6c:T=1573165817:S=ALNI_MYFuFgzAgJ5hKGCW2indHcdSrFRzQ',
      'ak_bmsc' = '1ABD5CFE5EDE3100E66F0165BC453AE5B81975A443390000369EC45D58B3E633~plp2oHIsR2fj34/7me0V6ZARoA7pip8MRUg4trtmBEQyyRQFN1Sa9kN/ZaApLyUKikePPkCj2EwQALtFWkoMzaDnHRMD+dIvO/3Kr4hT3JJ25WZ8wvhDqOVrn9EwZBq2XOq45JtUHssOQ3Oc91MBGhBTV6yQ4qbT+5K9vvuyCihm48jLrDcoHdpwZCvtHd1a2uASC5iQfClMgHUqwacDyi9XjviEapNjg4bnFdk3zcv4o=',
      'bm_sv' = 'A27D2BDAE8DE323D591E34420B5876FF~aCnKvUaVDWkUBy2QOvq0NcJ9/qWIyMKc7Z5CzJtAiolEpgKKyUkGAXYvqRDjGlx35ibrE4DnVuitNVhHS8CppIu2AgBzTpmlC3R2q+wkxIu00hGOyLScw4yKhnEuVJOz5tu27H3k/cNc6tgLvjUdDg==',
      'gpv_pn' = 'stats%3Aplayer%3A202681',
      's_sq' = '%5B%5BB%5D%5D',
      's_tp' = '3328',
      's_ppv' = 'stats%253Aplayer%253A202681%2C100%2C20%2C3328',
      's_tps' = '1327',
      's_pvs' = '7593'
    )

    headers = c(
      `Connection` = 'keep-alive',
      `Pragma` = 'no-cache',
      `Cache-Control` = 'no-cache',
      `DNT` = '1',
      `Upgrade-Insecure-Requests` = '1',
      `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.29 Safari/537.36',
      `Sec-Fetch-User` = '?1',
      `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
      `Sec-Fetch-Site` = 'cross-site',
      `Sec-Fetch-Mode` = 'navigate',
      `Referer` = 'https://downwiththechinazis.com',
      `Accept-Encoding` = 'gzip, deflate, br',
      `Accept-Language` = 'en-US,en;q=0.9'
    )

    res <-
      httr::GET(url,
                httr::add_headers(.headers = headers),
                httr::set_cookies(.cookies = cookies))

    json <-
      res$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(simplifyVector = T)

    json

}
