


.dictionary.sites <-
  memoise::memoise(function() {
    df_formulas <-
      tibble(
        nameSite = c("bref", "yahoo", "realgm", "hoopshype"),
        formulatDict = c(
          ".dictionary.bref.nba.missing()",
          "dictionary.yahoo.nba.missing()",
          "dictionary.realgm.nba.missing()",
          "dictionary.hoopshype.nba.missing()"
        )
      )
    df_formulas
  })

.dictionary.bref.nba.missing <-
  memoise::memoise(function() {
    tibble(
      namePlayer = c(
        "J.J. Anderson",
        "Tiny Archibald",
        "Billy Ray Bates",
        "Chubby Cox",
        "Geoff Crompton",
        "Charles Davis",
        "World B. Free",
        "Dave Greenwood",
        "Joe Hassett",
        "Hutch Jones",
        "Fat Lever",
        "Charles Pittman",
        "James Ray",
        "Cliff Robinson",
        "Danny Schayes",
        "Ed Sherod",
        "Pete Verhoeven",
        "DeWayne Scales",
        "Michael Wilson",
        "Kenton Edelin",
        "Mike Holton",
        "Melvin Turpin",
        "Eddie Lee Wilkins",
        "Michael Phelps",
        "Maurice Martin",
        "McKinley Singleton",
        "Pearl Washington",
        "Ron Grandison",
        "Rob Lock",
        "Rob Rose",
        "Isaac Austin",
        "Steve Bardo",
        "LaBradford Smith",
        "Steve Smith",
        "Jo Jo English",
        "Clarence Weatherspoon",
        "Rich Manning",
        "Logan Vander Velden",
        "Horacio Llamas Grey",
        "LaMark Baker",
        "Makhtar N'Diaye",
        "Jeffrey Sheppard",
        "Stanislav Medvedenko",
        "Mamadou N'Diaye",
        "Wang Zhizhi",
        "Isaac Fontaine",
        "Norm Richardson",
        "Nene Hilario",
        "Roger Mason",
        "Ronald Murray",
        "Efthimi Rentzias",
        "Mike Sweetney",
        "Didier Ilunga-Mbenga",
        "Ibo Kutluay",
        "Ha Seung-Jin",
        "J.R. Smith",
        "C.J. Miles",
        "Boniface N'Dong",
        "J.J. Redick",
        "P.J. Tucker",
        "D.J. Strawberry",
        "J.J. Hickson",
        "D.J. White",
        "Sun Yue",
        "A.J. Price",
        "Eugene Jeter",
        "Hamady N'Diaye",
        "Perry Jones",
        "Luigi Datome",
        "C.J. McCollum",
        "Otto Porter",
        "D.J. Stephens",
        "James Ennis",
        "P.J. Hairston",
        "K.J. McDaniels",
        "Johnny O'Bryant",
        "T.J. Warren",
        "C.J. Wilcox",
        "R.J. Hunter",
        "J.J. O'Brien",
        "Kelly Oubre",
        "Wade Baldwin",
        "A.J. Hammons",
        "Derrick Jones",
        "Taurean Waller-Prince",
        "Wesley Iwundu",
        "T.J. Leaf",
        "Zhou Qi",
        "Dennis Smith",
        "Derrick Walton",
        "Andrew White",
        "James Webb",
        "Juan Hernangomez", "Marcus Williams",
        "Matt Williams", "Mike Dunleavy", "Mike James", "Vince Hunter",
        "Walt Lemon, Jr."
      )
      ,
      namePlayerNBA = c(
        "Mitchell Anderson",
        "Nate Archibald",
        "Billyray Bates",
        "John Cox",
        "Jeffrey Crompton",
        "Charlie Davis",
        "World Free",
        "David Greenwood",
        "Joey Hassett",
        "Willie Jones",
        "Lafayette Lever",
        "Charlie Pittman",
        "Jamesearl Ray",
        "Cliff T. Robinson",
        "Dan Schayes",
        "Edmund Sherod",
        "Peter Verhoeven",
        "Dewayne Scales",
        "Mike Wilson",
        "Kent Edelin",
        "Michael Holton",
        "Mel Turpin",
        "Eddielee Wilkins",
        "Mike Phelps",
        "Mo Martin",
        "Mckinley Singleton",
        "Dwayne Washington",
        "Ronnie Grandison",
        "Robert Lock",
        "Robert Rose",
        "Ike Austin",
        "Stephen Bardo",
        "Labradford Smith",
        "Steven Smith",
        "Jojo English",
        "Clar. Weatherspoon",
        "Richard Manning",
        "Log Vander Velden",
        "Horacio Llamas",
        "Mark Baker",
        "Makhtar N'diaye",
        "Jeff Sheppard",
        "Slava Medvedenko",
        "Mamadou N'diaye",
        "Wang Zhi-zhi",
        "Ike Fontaine",
        "Norman Richardson",
        "Nene",
        "Roger Mason Jr.",
        "Flip Murray",
        "Efthimios Rentzias",
        "Michael Sweetney",
        "DJ Mbenga",
        "Ibrahim Kutluay",
        "Ha Seung-Jin",
        "JR Smith",
        "CJ Miles",
        "Boniface N'Dong",
        "JJ Redick",
        "PJ Tucker",
        "DJ Strawberry",
        "JJ Hickson",
        "DJ White",
        "Sun Yue",
        "AJ Price",
        "Pooh Jeter",
        "Hamady Ndiaye",
        "Perry Jones III",
        "Gigi Datome",
        "CJ McCollum",
        "Otto Porter Jr.",
        "DJ Stephens",
        "James Ennis III",
        "PJ Hairston",
        "KJ McDaniels",
        "Johnny O'Bryant III",
        "TJ Warren",
        "CJ Wilcox",
        "RJ Hunter",
        "JJ O'Brien",
        "Kelly Oubre Jr.",
        "Wade Baldwin IV",
        "AJ Hammons",
        "Derrick Jones Jr.",
        "Taurean Prince",
        "Wes Iwundu",
        "TJ Leaf",
        "Qi Qi",
        "Dennis Smith Jr",
        "Derrick Walton Jr.",
        "Andrew White III",
        "Juancho Hernangomez",
        "James Webb III",
        "Marcus Williams",
        "Matt Williams Jr.", "Mike Dunleavy Jr.", "Mike James", "Vincent Hunter",
        "Walter Lemon Jr."
      )
    )

  })


.resolve.players <-
  memoise::memoise(function(data, site = "bref") {

    data <-
      data %>%
      mutate(
        namePlayer = case_when(
          namePlayer %>% str_detect("Larry Nance") &
            yearSeason > 1993 ~ "Larry Nance Jr.",
          namePlayer %>% str_detect("Tim Hardaway") &
            yearSeason > 2005 ~ "Tim Hardaway Jr.",
          namePlayer %>% str_detect("Gary Payton") &
            yearSeason > 2010 ~ "Gary Payton II",
          namePlayer %>% str_detect("Glenn Robinson") &
            yearSeason > 2012 ~ "Glenn Robinson III",
          namePlayer %>% str_detect("Jaren Jackson Jr.") &
            yearSeason > 2017 ~ "Jaren Jackson",
          namePlayer %>% str_detect("Gary Trent") &
            yearSeason > 2017 ~ "Gary Trent Jr.",
          TRUE                      ~  namePlayer
        )
      )

    data_players <-
      data %>%
      dplyr::select(one_of(c(
        "namePlayer", "slugPlayerBREF", "yearSeason"
      ))) %>%
      distinct()



    #### FIX 2nd gen players -- Nance - Hardaway - Rice -Payton

    if (!"df_dict_nba_players" %>% exists()) {
      assign_nba_players()
    }

    name_site <-
      glue::glue("namePlayer{str_to_upper(site)}")

    df_nba_stats_players <-
      df_dict_nba_players %>%
      dplyr::select(
        namePlayer,
        yearSeasonFirst,
        idPlayerNBA = idPlayer,
        urlPlayerThumbnail,
        urlPlayerHeadshot,
        urlPlayerPhoto,
        urlPlayerStats,
        urlPlayerActionPhoto,
      ) %>%
      mutate(namePlayerNBA = namePlayer) %>%
      dplyr::select(namePlayerNBA, everything()) %>%
      group_by(idPlayerNBA, namePlayerNBA) %>%
      filter(yearSeasonFirst == max(yearSeasonFirst)) %>%
      ungroup()

    data_players <-
      data_players %>%
      left_join(df_nba_stats_players) %>%
      suppressMessages()

    data_players <-
      data_players %>%
      mutate(
        idPlayerNBA = as.integer(idPlayerNBA),
        idPlayerNBA = case_when(
          slugPlayerBREF == "jonesma02" ~ 90000L,
          slugPlayerBREF == "jonesma03" ~ 2891L,
          slugPlayerBREF == "mitchto02" ~ 203502L,
          slugPlayerBREF == "mitchto03" ~ 203183L,
          slugPlayerBREF == "jonesbo01" ~ 77193L,
          slugPlayerBREF == "jonesbo02" ~ 200784L,
          slugPlayerBREF == "hendece01" ~ 76990L,
          slugPlayerBREF == "hendece02" ~ 1538L,
          slugPlayerBREF == "jonesch01" ~ 279L,
          slugPlayerBREF == "jonesch02" ~ 77178L,
          slugPlayerBREF == "jonesch03" ~ 1869L,
          slugPlayerBREF == "smithch01" ~ 293L,
          slugPlayerBREF == "smithch02" ~ 78179L,
          slugPlayerBREF == "smithch04" ~ 1520L,
          slugPlayerBREF == "johnsch03" ~ 202419L,
          slugPlayerBREF == "johnsch04" ~ 203187L,
          slugPlayerBREF == "smithch03" ~ 1814L,
          slugPlayerBREF == "smithch05" ~ 203147L,
          slugPlayerBREF == "brownde01" ~ 244L,
          slugPlayerBREF == "brownde03" ~ 200793L,
          slugPlayerBREF == "johnsed02" ~ 77144L,
          slugPlayerBREF == "johnsed03" ~ 698L,
          slugPlayerBREF == "johnsge02" ~ 77149L,
          slugPlayerBREF == "johnsge03" ~ 77148L,
          slugPlayerBREF == "kingge03" ~ 1628994L,
          slugPlayerBREF == "hendege01" ~ 76993L,
          slugPlayerBREF == "hendege02" ~ 201945L,
          slugPlayerBREF == "smithgr02" ~ 202962L,
          slugPlayerBREF == "paxsoji02" ~ 77819L,
          slugPlayerBREF == "johnske01" ~ 77154L,
          slugPlayerBREF == "johnske03" ~ 2256L,
          slugPlayerBREF == "johnsla02" ~ 913L,
          slugPlayerBREF == "willima03" ~ 200766L,
          slugPlayerBREF == "willima04" ~ 201173L,
          slugPlayerBREF == "davisma01" ~ 76528L,
          slugPlayerBREF == "davisma02" ~ 707L,
          slugPlayerBREF == "smithmi01" ~ 78197L,
          slugPlayerBREF == "smithmi02" ~ 63L,
          slugPlayerBREF == "dunlemi01" ~ 76616L,
          slugPlayerBREF == "dunlemi02" ~ 2399L,
          slugPlayerBREF == "jamesmi01" ~ 2229L,
          slugPlayerBREF == "jamesmi02" ~ 1628455L,
          slugPlayerBREF == "ewingpa01" ~ 121L,
          slugPlayerBREF == "ewingpa02" ~ 201607L,
          slugPlayerBREF == "willire01" ~ 199L,
          slugPlayerBREF == "willire02" ~ 202130L,
          slugPlayerBREF == "smithst03" ~ 200848L,
          slugPlayerBREF == "russewa01" ~ 78048L,
          slugPlayerBREF == "russewa02" ~ 201041L,
          slugPlayerBREF == "wrighch01" ~ 202874L,
          slugPlayerBREF == "wrighch02" ~ 203203L,
          slugPlayerBREF == "wilsobu01" ~ 78587L,
          slugPlayerBREF == "hawkiro01" ~ 76975L,
          slugPlayerBREF == "johnsch01" ~ 77133L,
          slugPlayerBREF == "johnsch02" ~ 77159L,
          slugPlayerBREF == "houseda01" ~ 1627863L,
          slugPlayerBREF == "brittda01" ~ 76261L,
          slugPlayerBREF == "willidu01" ~ 78545L,
          slugPlayerBREF == "lawreed01" ~ 77348L,
          slugPlayerBREF == "heardga01" ~ 76985L,
          slugPlayerBREF == "gilesha01" ~ 1628385L,
          slugPlayerBREF == "whitnha02" ~ 78519L,
          slugPlayerBREF == "vanbrja01" ~ 78400L,
          slugPlayerBREF == "whitejo01" ~ 78510L,
          slugPlayerBREF == "walkelo01" ~ 1629022L,
          slugPlayerBREF == "dampilo01" ~ 76499L,
          slugPlayerBREF == "baglema01" ~ 1628963L,
          slugPlayerBREF == "frazime01" ~ 1628982L,
          slugPlayerBREF == "creekmi01" ~ 1628249L,
          slugPlayerBREF == "bambamo01" ~ 1628964L,
          slugPlayerBREF == "williro04" ~ 1629057L,
          slugPlayerBREF == "valenro01" ~ 78396L,
          slugPlayerBREF == "hamilro01" ~ 76928L,
          slugPlayerBREF == "smithsa02" ~ 78205L,
          slugPlayerBREF == "mykhasv01" ~ 1629004L,
          slugPlayerBREF == "mcclate01" ~ 77510L,
          slugPlayerBREF == "greento01" ~ 76879L,
          slugPlayerBREF == "browntr01" ~ 1628972L,
          slugPlayerBREF == "edwarvi01" ~ 1629053L,
          slugPlayerBREF == "lemonwa01" ~ 1627215L,
          slugPlayerBREF == "cartewe01" ~ 1628976L,
          slugPlayerBREF == "davisch01" ~ 76519L,
          slugPlayerBREF == "duffybo02" ~ 76610L,
          slugPlayerBREF == "guokama02" ~ 76908L,
          slugPlayerBREF == "johnsge01" ~ 77147L,
          slugPlayerBREF == "johnsla01" ~ 77156L,
          slugPlayerBREF == "joneswi01" ~ 77202L,
          slugPlayerBREF == "kingge01" ~ 77268L,
          slugPlayerBREF == "paxsoji01" ~ 77818L,
          slugPlayerBREF == "smithbi01" ~ 78209L,
          slugPlayerBREF == "smithgr01" ~ 78209L,
          slugPlayerBREF == "turneja01" ~ 78380L,
          slugPlayerBREF == "turneja02" ~ 78382L,
          slugPlayerBREF == "ackerdo01" ~ 76008L,
          slugPlayerBREF == "actonbu01" ~ 76010L,
          slugPlayerBREF == "anderda02" ~ 76036L,
          slugPlayerBREF == "armstcu01" ~ 76060L,
          slugPlayerBREF == "athadi01" ~ 76069L,
          slugPlayerBREF == "attleal01" ~ 76070L,
          slugPlayerBREF == "austijo01" ~ 76073L,
          slugPlayerBREF == "averibi01" ~ 76076L,
          slugPlayerBREF == "barrmo01" ~ 76113L,
          slugPlayerBREF == "bellwh01" ~ 76143L,
          slugPlayerBREF == "bockhbu01" ~ 76191L,
          slugPlayerBREF == "bonsage01" ~ 76203L,
          slugPlayerBREF == "borsaik01" ~ 76209L,
          slugPlayerBREF == "boydfr01" ~ 76222L,
          slugPlayerBREF == "brianfr01" ~ 76250L,
          slugPlayerBREF == "bryanem01" ~ 76289L,
          slugPlayerBREF == "budkowa01" ~ 76298L,
          slugPlayerBREF == "burdeti01" ~ 76303L,
          slugPlayerBREF == "clemeba01" ~ 76403L,
          slugPlayerBREF == "crossch01" ~ 76479L,
          slugPlayerBREF == "darcepe01" ~ 76506L,
          slugPlayerBREF == "davisdw01" ~ 76521L,
          slugPlayerBREF == "davismi02"  ~ 76522L,
          slugPlayerBREF == "davisre01" ~ 76524L,
          slugPlayerBREF ==  "devlico01" ~ 76561L,
          slugPlayerBREF ==  "dilloho01" ~ 76571L,
          slugPlayerBREF ==  "doveso01" ~ 76592L,
          slugPlayerBREF ==  "eddledi01" ~ 76636L,
          slugPlayerBREF ==  "eichhdi01" ~ 76651L,
          slugPlayerBREF ==  "ellisbo01" ~ 76658L,
          slugPlayerBREF ==  "gaborbi01" ~ 76768L,
          slugPlayerBREF ==  "gaineel01" ~ 76769L,
          slugPlayerBREF ==  "garredi01" ~ 76788L,
          slugPlayerBREF ==  "gibsoho01" ~ 76811L,
          slugPlayerBREF ==  "glickno01" ~ 76823L,
          slugPlayerBREF ==  "hahnro01" ~ 76914L,
          slugPlayerBREF ==  "halbech01" ~ 76918L,
          slugPlayerBREF ==  "halbrsw01" ~ 76919L,
          slugPlayerBREF ==   "hermskl01" ~ 77007L,
          slugPlayerBREF ==   "hertzso01" ~ 77011L,
          slugPlayerBREF ==   "hoffmpa01" ~ 77036L,
          slugPlayerBREF ==   "holtaw01" ~ 77046L,
          slugPlayerBREF ==   "hoskebi01" ~ 77061L,
          slugPlayerBREF ==   "hundlho01" ~ 77082L,
          slugPlayerBREF ==   "ingramc01" ~ 77095L,
          slugPlayerBREF ==   "johnsra01" ~ 77139L,
          slugPlayerBREF ==  "joneswa01" ~ 77199L,
          slugPlayerBREF ==   "kearnmi01" ~ 77230L,
          slugPlayerBREF ==   "kennego01" ~ 77243L,
          slugPlayerBREF ==   "kennepi01" ~ 77245L,
          slugPlayerBREF ==   "kenvibi01" ~ 77247L,
          slugPlayerBREF ==   "kerrre01" ~ 77248L,
          slugPlayerBREF ==   "killuea01" ~ 77257L,
          slugPlayerBREF ==   "kingma01" ~ 77272L,
          slugPlayerBREF ==   "kronto01" ~ 77313L,
          slugPlayerBREF ==   "kupeccj01" ~ 77325L,
          slugPlayerBREF ==   "laytomo01" ~ 77350L,
          slugPlayerBREF ==  "leaksma01" ~ 77351L,
          slugPlayerBREF ==   "leeru01" ~ 77365L,
          slugPlayerBREF ==   "leonasl01" ~ 77371L,
          slugPlayerBREF ==   "loganjo01" ~ 77403L,
          slugPlayerBREF ==   "lowerch01" ~ 77416L,
          slugPlayerBREF ==   "mahonmo01" ~ 77444L,
          slugPlayerBREF ==  "marshri01" ~ 77465L,
          slugPlayerBREF ==  "martila01" ~ 77475L,
          slugPlayerBREF ==  "martiwh01" ~ 77479L,
          slugPlayerBREF ==  "mcconbu01" ~ 77514L,
          slugPlayerBREF ==  "mcguial01" ~ 77536L,
          slugPlayerBREF ==  "mckinbo01" ~ 77546L,
          slugPlayerBREF ==  "mclemmc01" ~ 77548L,
          slugPlayerBREF == "mcmulma01" ~ 77556L,
          slugPlayerBREF ==   "meinemo01" ~ 77573L,
          slugPlayerBREF ==   "morrire01" ~ 77648L,
          slugPlayerBREF == "nashco01" ~ 77687L,
          slugPlayerBREF == "niemiri01" ~ 77717L,
          slugPlayerBREF == "nordmbe01" ~ 77727L,
          slugPlayerBREF == "obriera01" ~ 77743L,
          slugPlayerBREF == "ogdenbu01" ~ 77747L,
          slugPlayerBREF == "oldhajo01" ~ 77758L,
          slugPlayerBREF == "olsenbu01" ~ 77765L,
          slugPlayerBREF == "owensre01" ~ 77783L,
          slugPlayerBREF == "parhaea01" ~ 77797L,
          slugPlayerBREF == "parkme01" ~ 77799L,
          slugPlayerBREF == "patteto01" ~ 77813L,
          slugPlayerBREF == "phelaja02" ~ 77849L,
          slugPlayerBREF == "rackllu01" ~ 77894L,
          slugPlayerBREF == "reisech01" ~ 77938L,
          slugPlayerBREF == "rittete01" ~ 77968L,
          slugPlayerBREF == "sandeto01" ~ 78060L,
          slugPlayerBREF == "saulpe01" ~ 78063L,
          slugPlayerBREF == "scolafr01" ~ 78094L,
          slugPlayerBREF == "searske01" ~ 78106L,
          slugPlayerBREF == "sharech01" ~ 78125L,
          slugPlayerBREF == "shippch01" ~ 78653L,
          slugPlayerBREF == "skinnta01" ~ 78168L,
          slugPlayerBREF == "smithwi01" ~ 78207L,
          slugPlayerBREF == "sobekch01" ~ 78214L,
          slugPlayerBREF == "stallbu01" ~ 78239L,
          slugPlayerBREF == "taylofa01" ~ 78302L,
          slugPlayerBREF == "todormi01" ~ 78346L,
          slugPlayerBREF == "towerbl01" ~ 78363L,
          slugPlayerBREF == "vanbrbu01" ~ 78401L,
          slugPlayerBREF == "vaughch01" ~ 78409L,
          slugPlayerBREF == "walthis01" ~ 78448L,
          slugPlayerBREF == "warrejo01" ~ 1969L,
          slugPlayerBREF == "whitask01" ~ 78506L,
          slugPlayerBREF == "williar01" ~ 78539L,
          slugPlayerBREF == "zawolze01" ~ 78639L,
          TRUE ~ as.integer(idPlayerNBA)
        )
      ) %>%
      select(yearSeasonFirst, namePlayer:idPlayerNBA) %>%
      distinct()

    remove_names <- df_nba_stats_players %>%
      select(-c(
      namePlayer, idPlayerNBA
    )) %>% names()

    data_players <-
      data_players %>%
      select(-one_of(remove_names)) %>%
      left_join(
        df_nba_stats_players %>% select(-namePlayer)
      ) %>%
      suppressMessages() %>%
      suppressWarnings()


    has_double <-
      data_players %>% select(one_of(c(
        "namePlayer", "namePlayerNBA", "slugPlayerBREF", "idPlayerNBA"
      ))) %>% distinct() %>% count(slugPlayerBREF) %>%
      filter(n > 1) %>% nrow() > 0

    if (has_double) {
      data_players <-
        data_players %>%
        left_join(data_players %>% select(one_of(
          c("namePlayer", "namePlayerNBA", "idPlayer", "idPlayerNBA")
        )) %>% distinct() %>% count(idPlayer)) %>%
        suppressMessages()

      df_unique <-
        data_players %>%
        filter(n == 1) %>%
        dplyr::select(-n)

      df_double <-
        data_players %>%
        dplyr::filter(n > 1) %>%
        dplyr::select(namePlayer, idPlayer, yearSeason) %>%
        distinct() %>%
        group_by(namePlayer, idPlayer) %>%
        transmute(yearSeasonFirst = min(yearSeason, na.rm = T)) %>%
        ungroup()


      df_double_2 <-
        data_players %>%
        filter(n > 1) %>%
        select(namePlayer, idPlayer, yearSeason) %>%
        distinct()


      df_double <-
        df_double %>%
        left_join(df_double_2) %>%
        left_join(df_nba_stats_players) %>%
        suppressMessages()

      data_players <-
        df_unique %>%
        bind_rows(df_double) %>%
        arrange(namePlayer)
    }

    df_na_players <-
      data_players %>%
      filter(is.na(namePlayerNBA))

    if (df_na_players %>% nrow() == 0) {
      data <-
        data %>%
        left_join(data_players) %>%
        suppressMessages()
      return(data)
    }

    df_good <-
      data_players %>%
      filter(!is.na(namePlayerNBA))

    players_good <- df_good %>% pull(namePlayer)

    df_good <-
      df_good %>%
      mutate(!!name_site := players_good) %>%
      dplyr::select(dplyr::matches("idPlayer|namePlayer"), everything())

    df_good <-
      df_good %>%
      left_join(data) %>%
      dplyr::select(-namePlayerNBA) %>%
      suppressMessages()

    df_formulas <-
      .dictionary.sites()

    df_player_dict <-
      df_formulas %>%
      filter(nameSite == site %>% str_to_lower()) %>%
      pull(formulatDict) %>%
      rlang::parse_expr() %>%
      eval()

    players_na <-
      df_na_players %>% pull(namePlayer)

    df_na_players <-
      df_na_players %>%
      select(namePlayer) %>%
      distinct() %>%
      left_join(df_player_dict) %>%
      mutate(!!name_site := unique(players_na)) %>%
      left_join(data) %>%
      dplyr::select(-namePlayer) %>%
      dplyr::rename(namePlayer = namePlayerNBA) %>%
      left_join(
        df_dict_nba_players %>% select(namePlayer, idPlayerNBA = idPlayer, urlPlayerThumbnail)
      ) %>%
      suppressMessages()

    df <-
      df_good %>%
      bind_rows(df_na_players) %>%
      distinct() %>%
      dplyr::select(-one_of("namePlayerNBA")) %>%
      suppressWarnings() %>%
      filter(!is.na(idPlayerNBA)) %>%
      arrange(namePlayer)

    df


  })
