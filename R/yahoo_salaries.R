#' get_yahoo_nba_team_data
#'
#' @return
#' @import rvest xml2 httr stringr dplyr tidyr purrr formattable magrittr readr
#'
#' @examples
get_yahoo_nba_team_data <-
  function() {
    url <-
      'http://sports.yahoo.com/nba/teams/'
    page <-
      url %>%
      read_html

    nameTeam <-
      page %>%
      html_nodes('a[href*="/nba/teams/"]') %>%
      html_text() %>%
      .[!. %in% c('', 'Teams')]

    slugTeam <-
      page %>%
      html_nodes('a[href*="/nba/teams/"]') %>%
      html_attr('href') %>%
      unique %>%
      str_replace_all('/nba/teams|/', '') %>%
      .[!. %in% c('', 'http:sports.yahoo.com')] %>%
      str_to_upper()

    urlImg <-
      page %>%
      html_nodes('img') %>%
      html_attr('src') %>%
      .[7:36]

    team_data <-
      data_frame(nameTeam,
                 slugTeamYahoo = slugTeam,
                 urlImg) %>%
      mutate(
        idRow = 1:n(),
        urlTeamYahoo = 'http://sports.yahoo.com/nba/teams/' %>% paste0(slugTeamYahoo %>% str_to_lower()),
        urlTeamSalaryYahoo = urlTeamYahoo %>% paste0('/salary')
      ) %>%
      left_join(data_frame(
        idRow = seq(1, 30, 5),
        nameDivison = c(
          'Atlantic',
          'Central',
          'Southeast',
          'Pacific',
          'Southwest',
          'Northwest'
        ),
        idConference = c(rep("E", 3),
                         rep("W", 3))
      )) %>%
      dplyr::select(-idRow) %>%
      dplyr::select(idConference, nameDivison, everything()) %>%
      fill(idConference) %>%
      fill(nameDivison) %>%
      suppressMessages()

    return(team_data)
  }

#' get_yahoo_team_url_df
#'
#' @return
#' @import rvest httr stringr dplyr tidyr purrr formattable
#'
#' @examples
get_yahoo_team_url_df <-
  function() {
    team_data <-
      data_frame(
        idConference = c(
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "E",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W",
          "W"
        ),
        nameDivision =
          c(
            "Atlantic",
            "Atlantic",
            "Atlantic",
            "Atlantic",
            "Atlantic",
            "Central",
            "Central",
            "Central",
            "Central",
            "Central",
            "Southeast",
            "Southeast",
            "Southeast",
            "Southeast",
            "Southeast",
            "Pacific",
            "Pacific",
            "Pacific",
            "Pacific",
            "Pacific",
            "Southwest",
            "Southwest",
            "Southwest",
            "Southwest",
            "Southwest",
            "Northwest",
            "Northwest",
            "Northwest",
            "Northwest",
            "Northwest"
          ),
        nameTeam =
          c(
            "Boston Celtics",
            "Brooklyn Nets",
            "New York Knicks",
            "Philadelphia 76ers",
            "Toronto Raptors",
            "Chicago Bulls",
            "Cleveland Cavaliers",
            "Detroit Pistons",
            "Indiana Pacers",
            "Milwaukee Bucks",
            "Atlanta Hawks",
            "Charlotte Hornets",
            "Miami Heat",
            "Orlando Magic",
            "Washington Wizards",
            "Golden State Warriors",
            "Los Angeles Clippers",
            "Los Angeles Lakers",
            "Phoenix Suns",
            "Sacramento Kings",
            "Dallas Mavericks",
            "Houston Rockets",
            "Memphis Grizzlies",
            "New Orleans Pelicans",
            "San Antonio Spurs",
            "Denver Nuggets",
            "Minnesota Timberwolves",
            "Oklahoma City Thunder",
            "Portland Trail Blazers",
            "Utah Jazz"
          ),
        slugTeamYahoo =
          c(
            "BOS",
            "BRO",
            "NYK",
            "PHI",
            "TOR",
            "CHI",
            "CLE",
            "DET",
            "IND",
            "MIL",
            "ATL",
            "CHA",
            "MIA",
            "ORL",
            "WAS",
            "GSW",
            "LAC",
            "LAL",
            "PHO",
            "SAC",
            "DAL",
            "HOU",
            "MEM",
            "NOR",
            "SAS",
            "DEN",
            "MIN",
            "OKC",
            "POR",
            "UTH"
          ),
        urlImg =
          c(
            "http://l.yimg.com/iu/api/res/1.2/GH3PFImYtVKbbx_pm6TNbA--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/bos.png",
            "http://l.yimg.com/iu/api/res/1.2/UK37z8xG4GYL5dVllqSzGA--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/bro.png",
            "http://l.yimg.com/iu/api/res/1.2/4U9kxesj2CbG2nP6LgRCrw--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/nyk.png",
            "http://l.yimg.com/iu/api/res/1.2/BxcTxmPtjr4m78NgmfUh4Q--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/https://sp.yimg.com/j/assets/ipt/philadelphia-76ers_70.png",
            "http://l.yimg.com/dh/ap/default/160427/Toronto-Raptors-70.png",
            "http://l.yimg.com/iu/api/res/1.2/HCoOMiXA_Y.vNMQobWRYZw--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/chi.png",
            "http://l.yimg.com/iu/api/res/1.2/NFa59xCH9bqs504IoC65uw--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/cle.png",
            "http://l.yimg.com/iu/api/res/1.2/q2gZd1.Q5xGc0YNLusATbA--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/det.png",
            "http://l.yimg.com/iu/api/res/1.2/bgJXFXMMwDKe5OYCieeZEw--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/ind.png",
            "http://l.yimg.com/dh/ap/default/160427/Milwakee-Bucks-70.png",
            "http://l.yimg.com/dh/ap/default/160427/Atlanta-Hawks-70.png",
            "http://l.yimg.com/iu/api/res/1.2/I9.oewCbYIYjVUhFccoofg--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/ipt/cha_70.png",
            "http://l.yimg.com/iu/api/res/1.2/oC8Bs9x4ezK2w4mcaDrAsA--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/mia.png",
            "http://l.yimg.com/iu/api/res/1.2/dr9YweCUuKHCfT17LX3hyg--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/orl.png",
            "https://sp.yimg.com/j/assets/ipt/WashingtonWizards+%281%29.png",
            "http://l.yimg.com/iu/api/res/1.2/FKHfaS6M0J6S2mKIoaXdUg--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/gsw.png",
            "http://l.yimg.com/iu/api/res/1.2/O5aLrPSlHnRulkgARuKnMw--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/dh/ap/default/150914/Clippers-logo-70.png",
            "http://l.yimg.com/iu/api/res/1.2/9S2t_Xd.6NbkofXGDH8e1A--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/lal.png",
            "http://l.yimg.com/iu/api/res/1.2/eRmeYQeOGEIpiDJG27Axuw--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/https://sp.yimg.com/j/assets/ipt/PhoenixSuns+%281%29.png",
            "http://l.yimg.com/iu/api/res/1.2/k2dcWAUWK80LPKPy.e83FA--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/https://sp.yimg.com/j/assets/ipt/SacramentoKings+%281%29.png",
            "http://l.yimg.com/iu/api/res/1.2/jSWOdlLXcJ9mvFccOPAtdQ--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/dal.png",
            "http://l.yimg.com/iu/api/res/1.2/KLSUol6JwQZBDClWMY9.hA--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/hou.png",
            "http://l.yimg.com/iu/api/res/1.2/DtxGVxr_K5bJse47Jjf1Gg--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/mem.png",
            "http://l.yimg.com/iu/api/res/1.2/OCJ2DPBdV3VA7axmOpo3dQ--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/ipt/70x70.1.png",
            "http://l.yimg.com/iu/api/res/1.2/IEUczQ5FBN3d.o8d0o3RKw--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/sas.png",
            "http://l.yimg.com/iu/api/res/1.2/bnTvr_4g6wEpSvNfK8H4RA--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/den.png",
            "http://l.yimg.com/iu/api/res/1.2/.GMRLuHdbmgQ19umQCYszg--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/min.png",
            "http://l.yimg.com/iu/api/res/1.2/ErHxVWy1vGiVxT9cxYqV0Q--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/okc.png",
            "http://l.yimg.com/iu/api/res/1.2/jodmzZlxuywCdYB4oOENfg--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/http://l.yimg.com/xe/i/us/sp/v/nba/teams/83/70x70/por.png",
            "http://l.yimg.com/iu/api/res/1.2/ZBwPek4uPY6lMhMrhDzWRQ--/YXBwaWQ9c2hhcmVkO2ZpPWZpbGw7aD03MDtxPTEwMDt3PTcw/https://sp.yimg.com/j/assets/ipt/UtahJazz+%281%29.png"
          )
        ,
        urlTeamYahoo =
          c(
            "http://sports.yahoo.com/nba/teams/bos",
            "http://sports.yahoo.com/nba/teams/bro",
            "http://sports.yahoo.com/nba/teams/nyk",
            "http://sports.yahoo.com/nba/teams/phi",
            "http://sports.yahoo.com/nba/teams/tor",
            "http://sports.yahoo.com/nba/teams/chi",
            "http://sports.yahoo.com/nba/teams/cle",
            "http://sports.yahoo.com/nba/teams/det",
            "http://sports.yahoo.com/nba/teams/ind",
            "http://sports.yahoo.com/nba/teams/mil",
            "http://sports.yahoo.com/nba/teams/atl",
            "http://sports.yahoo.com/nba/teams/cha",
            "http://sports.yahoo.com/nba/teams/mia",
            "http://sports.yahoo.com/nba/teams/orl",
            "http://sports.yahoo.com/nba/teams/was",
            "http://sports.yahoo.com/nba/teams/gsw",
            "http://sports.yahoo.com/nba/teams/lac",
            "http://sports.yahoo.com/nba/teams/lal",
            "http://sports.yahoo.com/nba/teams/pho",
            "http://sports.yahoo.com/nba/teams/sac",
            "http://sports.yahoo.com/nba/teams/dal",
            "http://sports.yahoo.com/nba/teams/hou",
            "http://sports.yahoo.com/nba/teams/mem",
            "http://sports.yahoo.com/nba/teams/nor",
            "http://sports.yahoo.com/nba/teams/sas",
            "http://sports.yahoo.com/nba/teams/den",
            "http://sports.yahoo.com/nba/teams/min",
            "http://sports.yahoo.com/nba/teams/okc",
            "http://sports.yahoo.com/nba/teams/por",
            "http://sports.yahoo.com/nba/teams/uth"
          )
        ,
        urlTeamSalaryYahoo =
          c(
            "http://sports.yahoo.com/nba/teams/bos/salary",
            "http://sports.yahoo.com/nba/teams/bro/salary",
            "http://sports.yahoo.com/nba/teams/nyk/salary",
            "http://sports.yahoo.com/nba/teams/phi/salary",
            "http://sports.yahoo.com/nba/teams/tor/salary",
            "http://sports.yahoo.com/nba/teams/chi/salary",
            "http://sports.yahoo.com/nba/teams/cle/salary",
            "http://sports.yahoo.com/nba/teams/det/salary",
            "http://sports.yahoo.com/nba/teams/ind/salary",
            "http://sports.yahoo.com/nba/teams/mil/salary",
            "http://sports.yahoo.com/nba/teams/atl/salary",
            "http://sports.yahoo.com/nba/teams/cha/salary",
            "http://sports.yahoo.com/nba/teams/mia/salary",
            "http://sports.yahoo.com/nba/teams/orl/salary",
            "http://sports.yahoo.com/nba/teams/was/salary",
            "http://sports.yahoo.com/nba/teams/gsw/salary",
            "http://sports.yahoo.com/nba/teams/lac/salary",
            "http://sports.yahoo.com/nba/teams/lal/salary",
            "http://sports.yahoo.com/nba/teams/pho/salary",
            "http://sports.yahoo.com/nba/teams/sac/salary",
            "http://sports.yahoo.com/nba/teams/dal/salary",
            "http://sports.yahoo.com/nba/teams/hou/salary",
            "http://sports.yahoo.com/nba/teams/mem/salary",
            "http://sports.yahoo.com/nba/teams/nor/salary",
            "http://sports.yahoo.com/nba/teams/sas/salary",
            "http://sports.yahoo.com/nba/teams/den/salary",
            "http://sports.yahoo.com/nba/teams/min/salary",
            "http://sports.yahoo.com/nba/teams/okc/salary",
            "http://sports.yahoo.com/nba/teams/por/salary",
            "http://sports.yahoo.com/nba/teams/uth/salary"
          )
      )
    return(team_data)
  }

#' get_yahoo_item_name_df
#'
#' @return
#' @import dplyr
#'
#' @examples
get_yahoo_item_name_df <-
  function() {
    yahoo_item_df <-
      data_frame(
        item = c(
          "Guaranteed Salaries",
          "Dead Money",
          "Non-Guaranteed",
          "Free Agent Cap Hold",
          'Free Agent Cap Holds',
          "Incomplete Roster Charge",
          "Tax Variance",
          "Salaries: Cap",
          "Salaries: Tax",
          "Salary Cap",
          "Luxury Tax",
          "Cap Space",
          "Tax Room"
        ),
        nameItem = c(
          "amountSalaryGuaranteed",
          "amountSalaryDead",
          "amountSalaryNonGuaranteed",
          "amountCapHold",
          "amountCapHold",
          "amountIncompleteRoster",
          "amountTaxVariance",
          "totalSalaryTeamCap",
          "totalSalaryTeamTax",
          "amountSalaryCap",
          "amountLuxuryTax",
          "amountCapSpace",
          "amountTaxRoom"
        )
      )
    return(yahoo_item_df)
  }

#' parse_yahoo_team_salary_data
#'
#' @param page
#' @param return_wide
#'
#' @return
#' @import rvest xml2 httr stringr dplyr tidyr purrr formattable magrittr
#' @importFrom readr parse_number
#' @examples
parse_yahoo_team_salary_data <-
  function(page, return_wide = F) {
    tables <-
      page %>%
      html_table()

    team_tbl <-
      tables[[3]] %>%
      as_data_frame()

    names(team_tbl)[1] <-
      c('item')

    team_tbl <-
      team_tbl %>%
      gather(idSeason, value, -item) %>%
      mutate(value = value %>% parse_number()) %>%
      suppressWarnings() %>%
      left_join(get_yahoo_item_name_df()) %>%
      dplyr::select(idSeason, nameItem, value) %>%
      dplyr::filter(!value %>% is.na) %>%
      suppressMessages()

    if (return_wide) {
      team_tbl <-
        team_tbl %>%
        spread(idSeason, value)
    }
    return(team_tbl)
  }

#' parse_yahoo_player_salary_data
#'
#' @param page
#' @param format_currency
#' @param return_wide
#'
#' @return
#' @import rvest httr stringr dplyr tidyr purrr formattable
#' @importFrom readr parse_number
#'
#' @examples
parse_yahoo_player_salary_data <-
  function(page,
           format_currency = T,
           return_wide = F) {
    tables <-
      page %>%
      html_table()

    players_tbl <-
      tables[[2]] %>%
      as_data_frame()

    names(players_tbl)[c(1:2)] <-
      c('namePlayer',
        'typeRoster')

    names(players_tbl)[names(players_tbl) %in% c("Cap Hold", "Insider Info", "Salary Protection")] <-
      c('yearCapHold',
        'detailsInsider',
        'detailsSalary')

    players_tbl <-
      players_tbl %>%
      gather(
        idSeason,
        amountSalary,
        -c(
          namePlayer,
          typeRoster,
          yearCapHold,
          detailsInsider,
          detailsSalary
        )
      ) %>%
      mutate_each_(funs(parse_number), c('yearCapHold', 'amountSalary')) %>%
      mutate_each_(funs(gsub('\\-', NA, .)),
                   c(
                     'typeRoster',
                     'yearCapHold',
                     'detailsInsider',
                     'detailsSalary'
                   )) %>%
      dplyr::select(idSeason, namePlayer, everything()) %>%
      suppressWarnings() %>%
      dplyr::filter(!amountSalary %>% is.na)

    players_tbl <-
      players_tbl %>%
      mutate(
        yearCapHold = yearCapHold %>% as.numeric,
        isOnRoster = ifelse(typeRoster %in% c('Draft Pick', 'Roster'), T, F),
        isDeadMoney = ifelse(typeRoster %in% c('Waiver'), T, F),
        isUnguaranteed = ifelse(detailsSalary %>% str_detect('Protection: None'), T, F),
        hasPlayerOption = ifelse(detailsSalary %>% str_detect('Player Option'), T, F),
        hasTeamOption = ifelse(detailsSalary %>% str_detect('Team Option'), T, F),
        hasETO = ifelse(detailsSalary %>% str_detect('Early Termination Option'), T, F)
      ) %>%
      replace_na(list(
        isUnguaranteed = F,
        hasPlayerOption = F,
        hasTeamOption = F
      )) %>%
      dplyr::select(
        idSeason:yearCapHold,
        isOnRoster:hasTeamOption,
        detailsInsider,
        detailsSalary,
        everything()
      )

    players_tbl <-
      players_tbl %>%
      mutate(
        idSeasonFinal = (yearCapHold - 1) %>% as.character() %>% paste0("-",
                                                                        substr(yearCapHold, 3, 4)),
        isContractYear = ifelse(idSeasonFinal == idSeason, T, F)
      ) %>%
      dplyr::select(
        idSeason:yearCapHold,
        isOnRoster:hasTeamOption,
        detailsInsider,
        detailsSalary,
        idSeasonFinal,
        isContractYear,
        everything()
      )

    if (return_wide) {
      players_tbl <-
        players_tbl %>%
        spread(idSeason, amountSalary)
    }
    return(players_tbl)
  }

#' get_roster_salary_data
#'
#' @param url
#' @param return_wide
#'
#' @return
#' @import rvest xml2 httr stringr dplyr tidyr purrr formattable magrittr
#'
#' @examples
get_yahoo_roster_salary_data <-
  function(url = 'http://sports.yahoo.com/nba/teams/bro/salary/',
           nest_data = T,
           return_wide = F,
           format_currency = T,
           return_message = T) {
    slugTeamYahoo <-
      url %>%
      str_replace_all('http://sports.yahoo.com/nba/teams/|/salary/|salary/', '') %>%
      str_replace_all('/salary', '') %>%
      str_to_upper()

    page <-
      url %>%
      read_html

    team <-
      page %>%
      parse_yahoo_team_salary_data(return_wide = F)

    player <-
      page %>%
      parse_yahoo_player_salary_data(return_wide =  F)

    salary_data <-
      player %>%
      left_join(team %>%
                  dplyr::filter(
                    nameItem %in% c('amountSalaryCap', 'amountLuxuryTax', 'totalSalaryTeamCap')
                  ) %>%
                  spread(nameItem, value)) %>%
      mutate(slugTeamYahoo) %>%
      dplyr::select(slugTeamYahoo, everything()) %>%
      suppressMessages()

    if (!'team_data' %>% exists) {
      team_data <-
        get_yahoo_team_url_df()
    }

    salary_data <-
      salary_data %>%
      left_join(team_data) %>%
      dplyr::select(idSeason,
                    idConference,
                    nameDivision,
                    nameTeam,
                    slugTeamYahoo,
                    everything()) %>%
      suppressMessages()

    if (format_currency) {
      salary_data <-
        salary_data %>%
        mutate_each_(funs(currency(., digits = 2)),
                     salary_data %>% dplyr::select(matches("amount|total")) %>% names)
    }

    if (return_message == T) {
      total_salary <-
        salary_data$amountSalary %>% sum(na.rm = T) %>% currency(digits = 0)
      team_name <-
        salary_data$nameTeam %>% unique

      seasons <-
        salary_data$idSeason %>% unique %>%
        .[c(1, length(.))]

      message_data <-
        "The " %>%
        paste0(
          team_name,
          " have ",
          total_salary,
          ' in committed salary from the ',
          seasons[1],
          ' through the ',
          seasons[2],
          ' season'
        )
    }

    if (nest_data) {
      salary_data <-
        salary_data %>%
        nest(
          -c(
            idSeason,
            idConference,
            nameDivision,
            nameTeam,
            slugTeamYahoo,
            amountLuxuryTax,
            amountSalaryCap,
            urlImg,
            urlTeamYahoo,
            urlTeamSalaryYahoo
          )
        )
    }

    if (return_wide) {
      if (nest_data) {
        salary_data <-
          salary_data %>%
          unnest
      }
      salary_data <-
        salary_data %>%
        dplyr::select(-c(amountSalaryCap, amountLuxuryTax, totalSalaryTeamCap)) %>%
        spread(idSeason, amountSalary) %>%
        distinct()
    }

    if (return_message) {
      message_data %>%
        message
    }

    return(salary_data)
  }

#' get_yahoo_team_salary_data
#'
#' @param url
#' @param nest_data
#' @param return_wide
#' @param format_currency
#' @param return_message
#'
#' @return
#'
#' @examples
get_yahoo_team_salary_data <-
  function(url = 'http://sports.yahoo.com/nba/teams/bro/salary/',
           nest_data = T,
           return_wide = F,
           format_currency = T,
           return_message = T) {
    slugTeamYahoo <-
      url %>%
      str_replace_all('http://sports.yahoo.com/nba/teams/|/salary/', '') %>%
      str_replace_all('/salary', '') %>%
      str_to_upper()

    page <-
      url %>%
      read_html

    team <-
      page %>%
      parse_yahoo_team_salary_data(return_wide = F)

    team <-
      team %>%
      mutate(slugTeamYahoo) %>%
      dplyr::select(idSeason, slugTeamYahoo, everything())


    if (!'team_data' %>% exists) {
      team_data <-
        get_yahoo_team_url_df()
    }

    salary_data <-
      team %>%
      left_join(team_data) %>%
      dplyr::select(idSeason,
                    idConference,
                    nameDivision,
                    nameTeam,
                    slugTeamYahoo,
                    everything()) %>%
      suppressMessages()

    if (format_currency) {
      salary_data <-
        salary_data %>%
        mutate_each_(funs(currency(., digits = 2)),
                     salary_data %>% dplyr::select(matches("amount|total|value")) %>% names)
    }

    if (nest_data) {
      salary_data <-
        salary_data %>%
        nest(
          -c(
            idSeason,
            idConference,
            nameDivision,
            nameTeam,
            slugTeamYahoo,
            urlImg,
            urlTeamYahoo,
            urlTeamSalaryYahoo
          )
        )
    }

    if (return_wide) {
      if (nest_data) {
        salary_data <-
          salary_data %>%
          unnest
      }
      salary_data <-
        salary_data %>%
        spread(idSeason, value) %>%
        distinct()
    }

    if (return_message) {
      "You got team summary salary data for " %>%
        paste0(salary_data$nameTeam %>% unique) %>%
        message()
    }

    return(salary_data)

  }

#' get_team_yahoo_player_salary_data
#'
#' @param team_name
#' @param nest_data
#' @param return_wide
#'
#' @return
#'
#' @examples
get_team_yahoo_player_salary_data <-
  function(team_name = c("New York Knicks"),
           nest_data = T,
           return_wide = F,
           format_currency = T,
           return_message = T) {
    if (!'team_data' %>% exists) {
      team_data <-
        get_yahoo_team_url_df()

    }

    if (!team_name %in% team_data$nameTeam) {
      stop("Sorry teams can only be:\n",
           paste0(paste0(team_data$nameTeam, collapse = '\n')))
    }

    url_data <-
      team_data %>%
      dplyr::filter(team_name == nameTeam) %>%
      .$urlTeamSalaryYahoo

    team_data <-
      get_yahoo_roster_salary_data(
        url = url_data,
        nest_data = nest_data,
        return_wide = return_wide,
        format_currency = format_currency,
        return_message = return_message
      )

    return(team_data)

  }


#' get_teams_yahoo_player_salary_data
#'
#' @param team_names
#' @param use_all_teams
#' @param nest_data
#' @param return_wide
#'
#' @return
#' @export
#' @import purrr magrittr
#' @examples
get_teams_yahoo_player_salary_data <-
  function(team_names = c("Boston Celtics", "Brooklyn Nets", "New York Knicks"),
           use_all_teams = F,
           format_currency = T,
           nest_data = T,
           return_wide = F,
           return_message = T) {
    if (use_all_teams == T) {
      team_names <-
        get_yahoo_team_url_df() %>%
        .$nameTeam
    }

    all_team_data <-
      team_names %>%
      map(function(x) {
        get_team_yahoo_player_salary_data(
          team_name = x,
          nest_data = nest_data,
          return_wide = return_wide,
          format_currency = format_currency,
          return_message = return_message
        )
      }) %>%
      compact %>%
      bind_rows()

    ### Fix Salary Cap and Luxury Tax #s

    seasons_salary_cap <-
      all_team_data %>% dplyr::select(idSeason, amountLuxuryTax, amountSalaryCap) %>% distinct() %>%
      gather(item, value, -idSeason) %>%
      group_by(idSeason, item) %>%
      dplyr::filter(value == max(value)) %>%
      ungroup %>%
      distinct() %>%
      spread(item, value)

    all_team_data <-
      all_team_data %>%
      dplyr::select(-c(amountLuxuryTax, amountSalaryCap)) %>%
      left_join(seasons_salary_cap) %>%
      dplyr::select(idSeason:slugTeamYahoo,
                    amountLuxuryTax,
                    amountSalaryCap,
                    everything()) %>%
      suppressMessages()

    if (format_currency) {
      if (nest_data) {
        all_team_data <-
          all_team_data %>%
          unnest()
      }
      all_team_data <-
        all_team_data %>%
        mutate_each_(funs(currency(., digits = 0)),
                     all_team_data %>% dplyr::select(matches("amount|total")) %>% names)

      if (nest_data) {
        all_team_data <-
          all_team_data %>%
          nest(
            -c(
              idSeason,
              idConference,
              nameDivision,
              nameTeam,
              slugTeamYahoo,
              amountLuxuryTax,
              amountSalaryCap,
              urlImg,
              urlTeamYahoo,
              urlTeamSalaryYahoo
            )
          )
      }
    }

    return(all_team_data)
  }

#' get_team_yahoo_player_salary_data
#'
#' @param team_name
#' @param nest_data
#' @param return_wide
#'
#' @return
#'
#' @examples
get_team_yahoo_team_salary_data <-
  function(team_name = c("New York Knicks"),
           format_currency = T,
           nest_data = T,
           return_wide = F,
           return_message = T) {
    if (!'team_data' %>% exists) {
      team_data <-
        get_yahoo_team_url_df()

    }

    if (!team_name %in% team_data$nameTeam) {
      stop("Sorry teams can only be:\n",
           paste0(paste0(team_data$nameTeam, collapse = '\n')))
    }

    url_data <-
      team_data %>%
      dplyr::filter(team_name == nameTeam) %>%
      .$urlTeamSalaryYahoo

    team_data <-
      get_yahoo_team_salary_data(
        url = url_data,
        nest_data = nest_data,
        return_wide = return_wide,
        format_currency = format_currency,
        return_message = return_message
      )

    return(team_data)

  }

#' get_teams_yahoo_team_salary_data
#'
#' @return
#' @export
#'
#' @examples
get_teams_yahoo_team_salary_data <-
  function(team_names = c("Boston Celtics", "Brooklyn Nets", "New York Knicks"),
           use_all_teams = F,
           format_currency = T,
           nest_data = T,
           return_wide = F,
           return_message = T) {
    if (use_all_teams == T) {
      team_names <-
        get_yahoo_team_url_df() %>%
        .$nameTeam
    }

    all_team_data <-
      team_names %>%
      map(function(x) {
        get_team_yahoo_team_salary_data(
          team_name = x,
          nest_data = nest_data,
          return_wide = return_wide,
          format_currency = format_currency,
          return_message = return_message
        )
      }) %>%
      compact %>%
      bind_rows()
    return(all_team_data)

    }
