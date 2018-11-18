


.dictionary.sites <-
  memoise::memoise(function() {
    df_formulas <-
      data_frame(
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
    data_frame(
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
      dplyr::select(one_of(c("namePlayer", "idPlayer", "yearSeason"))) %>%
      distinct()



    #### FIX 2nd gen players -- Nance - Hardaway - Rice -Payton

    if (!"df_dict_nba_players" %>% exists()) {
      assign_nba_players()
    }

    name_site <-
      glue::glue("namePlayer{str_to_upper(site)}")

    df_nba_stats_players <-
      df_dict_nba_players %>%
      dplyr::select(namePlayer,
                    idPlayerNBA = idPlayer,
                    urlPlayerThumbnail,
                    urlPlayerHeadshot,
                    yearSeasonFirst) %>%
      mutate(namePlayerNBA = namePlayer) %>%
      dplyr::select(namePlayerNBA, everything()) %>%
      group_by(namePlayerNBA) %>%
      filter(yearSeasonFirst == max(yearSeasonFirst)) %>%
      ungroup()

    data_players <-
      data_players %>%
      left_join(df_nba_stats_players) %>%
      suppressMessages()

    has_double <-
      data_players %>% select(one_of(c(
        "namePlayer", "namePlayerNBA", "idPlayer", "idPlayerNBA"
      ))) %>% distinct() %>% count(idPlayer) %>%
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
