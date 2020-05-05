.get_bref_players_ids  <-
  function(players = c("Aaron McKie", "Aaron Gordon"), player_ids = "bonnean01") {
    if (players %>% purrr::is_null() && player_ids %>% purrr::is_null()) {
      stop("Please Enter IDS")
    }
    ids <- c()
    df_bref_player_dict <-  dictionary_bref_players() %>% suppressMessages()
    if (!players %>% purrr::is_null()) {
      search_ids <-
        df_bref_player_dict %>%
        filter(namePlayerBREF %>% str_detect(players %>% str_c(collapse = "|"))) %>%
        pull(slugPlayerBREF)
      ids <-
        ids %>% append(search_ids)
    }

    if (!player_ids %>% purrr::is_null()) {
      ids <-
        player_ids %>%
        append(ids)
    }

    ids %>%
      unique() %>%
      sort()

  }

#' Ba
.resolve_bref_names <-
  function(json_names) {
    df_nba_names <-
      .dictionary_bref_bio_names()

    json_names %>%
      map_chr(function(name){
        no_name <-
          df_nba_names %>%
          filter(nameBREF == name) %>%
          nrow() == 0

        if (no_name) {
          glue::glue("Missing {name} in dictionary") %>% cat(fill = T)
          return(name)
        }
        df_nba_names %>%
          filter(nameBREF == name) %>%
          pull(nameActual) %>%
          unique() %>%
          .[[1]]
      })
  }

.dictionary_bref_bio_names <-
  memoise::memoise(function() {
    tibble(nameBREF = c("twitter", "position", "shoots", "height", "weight", "team",
               "born", "birthplace", "college", "namehighschool", "locationhighschool",
               "nba debut", "experience", "playernicknames", "yearrankhighschool",
               "pronunciation", "hof", "death", "relatives"),
               nameActual = c("nameTwitter", "namePosition", "handShoots", "heightInches", "weightLBS", "nameTeamCurrent",
                              "dateBirth", "locationBirthplace", "nameCollege", "nameHighSchool", "locationHighSchool",
                              "dateNBADebut", "yearsExperience", "playerNicknames", "yearRankHighSchool",
                              "namePronunciation", "descriptionHOF", "dateDeath", "descriptionRelatives"))
  })

.check_table <-
  function(page, css = "#div_all_salaries table") {
    table <-
      page %>%
      html_nodes(css = css) %>%
      html_table()

    if (table %>% length() == 0) {
      return(invisible())
    }
    table
  }

.parse.salary <-
  function(page) {
    table <-
      .check_table(page = page, css =  "#div_all_salaries table")

    if (table %>% length() == 0) {
      return(invisible())
    }

    table %>%
      flatten_df() %>%
      purrr::set_names(c("slugSeason", "nameTeam", "slugLeague", "amountSalary")) %>%
      filter(!slugSeason %>% str_detect("Career")) %>%
      mutate(amountSalary = readr::parse_number(as.character(amountSalary)))
  }

.parse.contracts  <-
  function(page) {
    ids <- page %>% html_nodes('div') %>% html_attr('id')
    ids <- ids[!ids %>% is.na()]

    contracts <- ids[ids %>% str_detect("contracts")]
    contract_id <- contracts[!contracts %>% str_detect("div")]
    if (contract_id %>% length() == 0) {
      return(invisible())
    }

    conract_css <-
      glue::glue("#{contract_id} table") %>% as.character()

    table <-
      .check_table(page = page, css =  conract_css)

    if (table %>% length() == 0) {
      return(invisible())
    }

    table <-
      table %>%
      flatten_df() %>%
      dplyr::rename(nameTeam = Team) %>%
      tidyr::gather(slugSeason, amountSalary, -nameTeam) %>%
      mutate(amountSalary = amountSalary %>% as.character() %>%  readr::parse_number())
    contract_details <- page %>% html_nodes(".bullets") %>% html_text()
    if (contract_details %>% length() > 0) {
      table <-
        table %>%
        mutate(detailsContract = contract_details) %>%
        select(nameTeam, detailsContract, everything())
    }

    table
  }

.parse.bio <-
  function(page) {
    bio <-
      page %>%
      html_nodes("#meta > div[itemscope] > p")

    if (bio %>% length() == 0) {
      return(invisible())
    }
    bio_length <- seq_along(bio)
    all_data <-
      bio_length %>%
      future_map_dfr(function(x){
          node <-
            bio[x]
          node_text <-
            node %>% html_text() %>%
            stringi::stri_trans_general("Latin-ASCII") %>%
            str_split("\\.") %>%
            flatten_chr()

          node_text_single <-
            node_text %>% str_c(collapse = "")

          is_dead <- node_text_single %>% str_detect("\\Died")

          if (is_dead) {
            parts <-
              node_text %>% str_split("\\:") %>% flatten_chr() %>% str_trim()

            data <- tibble(item = "death", value = parts[length(parts)] )
            return(data)
          }

          isPron <-
            node_text_single %>% str_detect("Pronunciation")

          if (isPron) {
            parts <- node_text_single %>% str_split("\\:") %>% flatten_chr() %>% str_trim()

            data <- tibble(item = "Pronunciation", value = parts %>% last() )
            return(data)
          }

          is_hof <- node_text_single %>% str_detect("Hall of Fame")

          if (is_hof) {
            parts <-
              node_text %>%
              str_split("\\:") %>%
              flatten_chr() %>%
              str_replace_all("(Full List)", "") %>%
              str_replace_all("\\)|\\(", "") %>%
              str_trim()

            data <-
              tibble(item = "hof", value = parts[length(parts)] )
            return(data)
          }


          isTwitter <- node_text %>% str_detect("Twitter") %>% sum(na.rm = T) > 0
          if (isTwitter) {
            values <-
              node_text %>% str_split("\\:") %>% flatten_chr()
            value <- values[length(values)]
            data <-
              tibble(item = c("Twitter"),
                       value)
            return(data)
          }

          is_recruiting <-
            node_text_single %>% str_detect("Recruiting Rank")

          if (is_recruiting) {
            parts <- node_text %>% str_split("\\:") %>% flatten_chr()
            data <-
              tibble(item = "yearRankHighSchool", value = parts[length(parts)])
            return(data)
          }

          if (node_text_single %>% str_detect("High School:")) {
            parts <-
              node_text %>% str_split("in [A-Z]") %>% flatten_chr()
            start <-
              node_text %>% str_locate('in [A-Z]') %>% max()

            parts[[2]] <-
              str_c(node_text %>% substr(start, start), parts[[2]], collapse = "")

            parts <-
              parts %>% str_replace_all("\\High School:", "")

            items <- c("nameHighSchool", "locationHighSchool")

            data <-
              tibble(item = items[seq_along(parts)],
                       value = parts) %>%
              mutate_all(str_trim)
            return(data)
          }

          is_position <- node_text_single %>% str_detect("^Position:")

          if (is_position) {
            matches <- node_text_single %>% str_match("^Position:(.*)\\\u25AA")
            position <- matches %>% last() %>% str_replace_all(c(" and" = ",")) %>% str_trim()

            data <- tibble(item = "Position", value = position)
            return(data)
          }

          is_height_weight <-
            (node_text_single %>% str_detect("[0-9]lb")) %>% sum(na.rm = T) > 0

          if (is_height_weight) {
            hw <- node_text %>% str_split("\\(") %>% flatten_chr() %>% str_trim() %>% .[[1]]
            if (hw %>% str_detect("\\,")) {
              values <- hw %>% str_split(",") %>% flatten_chr() %>% str_trim()
              data <- tibble(item = c("height", "weight"), value = values)
              return(data)
            }
            if (hw %>% str_detect("\\-") ){
              data <- tibble(item = "height", value = hw)
              return(data)
            }
            data <-
              tibble(item = "weight", value = hw)
          }
          if (node_text_single %>% str_detect("Born:")) {
          node_text <-
            node_text %>% str_replace_all("\\in", replacement = "\\;birthplace:") %>%
            str_split("\\;") %>%
            flatten_chr()
          }



          if (node_text_single %>% str_detect("Draft:")) {
            return(invisible())
          }

          is_nicknames <-
            node_text_single %>% str_count('\\,') >= 2 &
            node_text_single %>% str_count("\\(") == 1

          if (is_nicknames) {

            value <- node_text_single %>% str_replace_all("\\(|\\)", "")
            data <-
              tibble(item = "playerNicknames", value)

            return(data)
          }

          node_text <- node_text %>% str_replace_all("\\ and ", "\\, ")

          tibble(node_text) %>%
            tidyr::separate(node_text, into = c("item", "value"), sep = "\\:") %>%
            mutate_all(str_trim)
        }) %>%
      mutate(item = item %>% str_to_lower()) %>%
      filter(!is.na(value)) %>%
      dplyr::rename(nameBREF = item) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      left_join(
        .dictionary_bref_bio_names()
      ) %>%
      suppressMessages()

    actual_names <-
      all_data$nameBREF %>%
      .resolve_bref_names()

    all_data <-
      all_data %>%
      mutate(nameActual = actual_names) %>%
      select(nameActual, value) %>%
      tidyr::pivot_wider(names_from = nameActual, values_from = value) %>%
      dplyr::select(one_of(actual_names))

    if (all_data %>% tibble::has_name("heightInches")) {
      all_data <-
        all_data %>%
        mutate(heightInches = height_in_inches(height = heightInches))
    }

    if (all_data %>% tibble::has_name("weightLBS")) {
      all_data <-
        all_data %>%
        mutate(weightLBS = readr::parse_number(as.character(weightLBS)))
    }


    if (all_data %>% tibble::has_name("yearsExperience")) {
      all_data <-
        all_data %>%
        mutate(yearsExperience = readr::parse_number(as.character(yearsExperience)))
    }


    if (all_data %>% tibble::has_name("dateBirth")) {
      all_data <-
        all_data %>%
        mutate(dateBirth = lubridate::mdy(dateBirth))
    }

    if (all_data %>% tibble::has_name("dateDeath")) {
      all_data <-
        all_data %>%
        mutate(dateDeath = lubridate::mdy(dateDeath))

    }

    if (all_data %>% tibble::has_name("dateNBADebut")) {
      all_data <-
        all_data %>%
        mutate(dateNBADebut = lubridate::mdy(dateNBADebut))

    }

    if (all_data %>% tibble::has_name("locationBirthplace")) {
      all_data <-

        all_data %>%
        tidyr::separate(locationBirthplace, into = c("cityBirthplace", "stateBirthplace"), sep = "\\,") %>%
        mutate_if(is.character,
                  str_trim) %>%
        mutate(stateBirthplace = stateBirthplace %>% str_replace_all("$us", "")) %>%
        tidyr::unite(locationBirthplace, cityBirthplace, stateBirthplace, sep = "\\, ", remove = F)

    }

    if (all_data %>% tibble::has_name("locationHighSchool")) {
      all_data <-
        all_data %>%
        tidyr::separate(
          locationHighSchool,
          into = c("ciyHighSchool", "stateHighSchool"),
          sep = "\\,"
        ) %>%
        mutate_if(is.character,
                  str_trim) %>%
        tidyr::unite(
          locationHighSchool,
          ciyHighSchool,
          stateHighSchool,
          sep = "\\, ",
          remove = F
        )

    }

    if (all_data %>% tibble::has_name("yearRankHighSchool")) {
      all_data <-
        all_data %>%
        tidyr::separate(
          yearRankHighSchool,
          into = c("yearHighSchool", "rankHighSchool"),
          sep = "\\ "
        ) %>%
        mutate_at(c("yearHighSchool", "rankHighSchool"),
                  funs(. %>% as.character() %>% readr::parse_number()))

    }




    all_data
  }


.parse.transactions <-
  function(page) {
    transactions <-
      page %>%
      html_nodes("#div_transactions .transaction") %>%
      html_text()

    if (transactions %>% length() == 0) {
      return(invisible())
    }

    data <-
      tibble(transactions) %>%
      tidyr::separate(transactions, into = c("dateTransaction", "descriptionTransaction"), sep = "\\:") %>%
      mutate_all(str_trim) %>%
      mutate(dateTransaction = dateTransaction %>% lubridate::mdy()) %>%
      mutate(numberTransactionPlayer = 1:n()) %>%
      select(numberTransactionPlayer, everything())

    data %>%
      mutate(desl = str_to_lower(descriptionTransaction),
             isGLeagueMovement = desl %>% str_detect("g-league"),
             isDraft = desl %>%  str_detect("drafted"),
             isSigned = desl %>%  str_detect("signed"),
             isWaived = desl %>%  str_detect("waived"),
             isTraded = desl %>%  str_detect("trade")
      ) %>%
      select(-desl)

  }

.parse_bref_player_data_url <-
  function(url = "https://www.basketball-reference.com/players/d/dinwisp01.html",
           return_message = TRUE) {
  page <-
    url %>%
    .read_page()

  image <-
    page %>%
    html_nodes("#meta img")

  parts <- url %>% str_split("/") %>% flatten_chr()

  id_bref <- parts[length(parts)] %>% str_replace_all("\\.html", "")

  player <- page %>% html_nodes("h1") %>% html_text() %>% .[[1]]

  if (return_message) {
    glue::glue("Parsing basketball reference biography data for {player}") %>% cat(fill = T)
  }


  .parse.transactions.safe <-
    purrr::possibly(.parse.transactions, tibble())

  .parse.bio.safe <-
    purrr::possibly(.parse.bio, tibble())

  .parse.contracts.safe <-
    purrr::possibly(.parse.contracts, tibble())

  .parse.salary.safe <-
    purrr::possibly(.parse.salary, tibble())
  dataPlayerBio =
    .parse.bio.safe(page = page)
  dataPlayerTransactions =
    .parse.transactions.safe(page = page)
  dataPlayerContracts =
    .parse.contracts.safe(page = page)
  dataPlayerSalaries =  .parse.salary.safe(page = page)

  data <-
    tibble(
      nameTable = c("Biography", "Transactions", "Contracts", "Salaries"),
      dataTable = list(
        dataPlayerBio,
        dataPlayerTransactions,
        dataPlayerContracts,
        dataPlayerSalaries
      )
    ) %>%
    mutate(
      lengthTable = dataTable %>% map_dbl(length),
      namePlayerBREF = player ,
      slugPlayerBREF = id_bref,
      urlPlayerBioBREF = url
    ) %>%
    filter(lengthTable != 0) %>%
    select(-lengthTable) %>%
    dplyr::select(slugPlayerBREF,
                  namePlayerBREF,
                  urlPlayerBioBREF,
                  nameTable,
                  dataTable)

  if (image %>% length() > 0) {
    urlPlayerImageBREF <- page %>% html_nodes("#meta img") %>% html_attr("src")
    data <-
      data %>%
      mutate(urlPlayerImageBREF) %>%
      select(dplyr::matches("^id|name|url"), everything())
  }
  data

}

.parse_bref_player_data_urls <-
  function(urls, return_message = T){
    .parse_bref_player_data_url_safe <-
      purrr::possibly(.parse_bref_player_data_url, tibble())
    all_data <-
    urls %>%
    map_dfr(function(url){
      .parse_bref_player_data_url_safe(url = url, return_message = return_message)
    })
  all_data
}




#' Basketball Reference players bios
#'
#' Includes player bios, salaries, contracts and transactions
#'
#' @param players vector of player names
#' @param player_ids vector of basketball reference player ids
#' @param assign_to_environment if \code{TRUE} assigns each table to environment
#' @param return_message if \code{TRUE} returns
#'
#' @return a \code{tibble}
#' @export
#' @family BREF
#' @family player
#' @family salaries
#'
#' @examples
#' \dontrun{
#' bref_bios( players = c("Jarrett Allen", "Mitch Richmond", "Michael Adams"),
#' player_ids = NULL,
#' assign_to_environment = TRUE)
#' }
bref_bios <-
  function(players = NULL,
           player_ids = NULL,
           assign_to_environment = TRUE,
           return_message = T) {
    ids <-
      .get_bref_players_ids(players = players, player_ids = player_ids)

    df_bref_player_dict <-  dictionary_bref_players()

    urls <-
      df_bref_player_dict %>%
      filter(slugPlayerBREF %in% ids) %>%
      pull(urlPlayerBioBREF)

    .parse_bref_player_data_urls_safe <-
      purrr::possibly(.parse_bref_player_data_urls, tibble())

    all_data <-
      .parse_bref_player_data_urls(urls = urls, return_message = T)

    if (assign_to_environment) {
      tables <- all_data$nameTable
      tables %>%
        walk(function(table){
          df_table <-
            all_data %>%
            filter(nameTable == table) %>%
            select(-nameTable) %>%
            unnest()

          table_name <-
            glue::glue("dataBREFPlayers{table}") %>% as.character()
          assign(x = table_name, value = df_table, envir = .GlobalEnv)
        })
    }
    all_data
  }
