function_packages <- c(
  'dplyr',
  'magrittr',
  'jsonlite',
  'tidyr',
  'httr',
  'rvest',
  'purrr',
  'stringr',
  'lubridate',
  'tidyr'
)

install_needed_packages <-
  function(required_packages = function_packages) {
    needed_packages <-
      required_packages[!(required_packages %in% installed.packages()[, "Package"])]
    
    if (length(needed_packages) > 0) {
      if (!require("pacman"))
        install.packages("pacman")
      pacman::p_load(needed_packages)
    }
  }
load_needed_packages <-
  function(required_packages = function_packages) {
    loaded_packages <-
      search() %>%
      gsub('package:', '', .)
    
    package_to_load <-
      required_packages[!required_packages %in% loaded_packages]
    if (package_to_load %>% length > 0) {
      lapply(package_to_load, library, character.only = T)
    }
  }

get_nba_coach_history_df <-
  function(return_message = T) {
    load_needed_packages()
    install_needed_packages()
    
    url <-
      'http://www.basketball-reference.com/coaches/?lid=front_qi_coaches'
    
    page <-
      url %>%
      read_html()
    
    coaches_df <-
      page %>%
      html_table(header = F, fill = T) %>%
      data.frame() %>%
      tbl_df %>%
      slice(3:nrow(.))
    
    names(coaches_df) <-
      c('name.coach',
        'year.start',
        'year.end',
        'date.birth',
        'college')
    
    coaches_df %<>%
      dplyr::filter(!name.coach %>% str_detect('Last Name')) %>%
      mutate(
        is.hof_coach = ifelse(name.coach %>% str_detect("\\*"), T, F),
        name.coach = name.coach %>% str_replace("\\*", ''),
        date.birth = date.birth %>% strptime("%B %d, %Y") %>% as.Date()
      ) %>%
      mutate_each(funs(as.numeric), contains("year."))
    
    coach_link_df <-
      data_frame(
        name.coach =
          page %>%
          html_nodes('td:nth-child(1) a') %>%
          html_text(),
        url.coach.bref =
          page %>%
          html_nodes('td:nth-child(1) a') %>%
          html_attr('href') %>%
          paste0('http://www.basketball-reference.com', .),
        id.coach.bref =
          page %>%
          html_nodes('td:nth-child(1) a') %>%
          html_attr('href') %>%
          str_replace_all("/coaches/", '') %>%
          str_replace_all(".html", '')
      ) %>%
      left_join(data_frame(
        name.coach =
          page %>%
          html_nodes('td:nth-child(1) strong a') %>%
          html_text(),
        is.active_coach = T
      )) %>%
      mutate(is.active_coach = ifelse(is.active_coach %>% is.na, F, T)) %>%
      suppressMessages()
    
    college_url_df <-
      data_frame(
        college =
          page %>%
          html_nodes('td:nth-child(5) a') %>%
          html_text(),
        url.college.nba_players.bref =
          page %>%
          html_nodes('td:nth-child(5) a') %>%
          html_attr('href') %>%
          paste0('http://www.basketball-reference.com', .)
      ) %>%
      distinct()
    
    coaches_df %<>%
      left_join(coach_link_df) %>%
      left_join(college_url_df) %>%
      dplyr::select(id.coach.bref,
                    name.coach,
                    is.active_coach,
                    is.hof_coach,
                    everything()) %>%
      dplyr::filter(!id.coach.bref %>% is.na()) %>%
      suppressMessages()
    if (return_message == T) {
      "You meta data for all " %>%
        paste0(coach_link_df %>% nrow(),
               " active & historic ABA & NBA coaches.") %>%
        message()
    }
    return(coaches_df)
  }

get_nba_coach_awards <-
  function(page) {
    name.coach <-
      page %>%
      html_nodes('h1') %>%
      html_text()
    
    awards_table <-
      data_frame(
        name.coach,
        id.season =
          page %>%
          html_nodes('#awards td:nth-child(1)') %>%
          html_text(),
        id.league =
          page %>%
          html_nodes('#awards td:nth-child(2)') %>%
          html_text(),
        detail.award =
          page %>%
          html_nodes('#awards td:nth-child(3)') %>%
          html_text()
      ) %>%
      mutate(is.coach_of_the_year = detail.award %>% str_to_lower %>% str_detect("coach of the year")) %>%
      dplyr::select(id.season, id.league, everything())
    
    return(awards_table)
    
  }

get_bref_coaching_table_stats <-
  function(page) {
    name.coach <-
      page %>%
      html_nodes('h1') %>%
      html_text()
    coach_table <-
      page %>%
      html_nodes('#stats') %>%
      html_table(header = F) %>%
      data.frame() %>%
      tbl_df %>%
      slice(3:nrow(.))
    
    names(coach_table) <-
      c(
        "id.season",
        "age.coach",
        'id.league',
        'slug.team.bref',
        'gp.regular_season',
        'wins.regular_season',
        'losses.regular_season',
        'pct.wins.regular_season',
        'wins.over_500',
        'finish.division',
        'gp.playoffs',
        'wins.playoffs',
        'losses.playoffs',
        'pct.wins.playoffs',
        'notes'
      )
    coach_table %<>%
      dplyr::filter(!id.season %>% str_detect("Career")) %>%
      dplyr::filter(!id.season %>% str_detect("seasons")) %>%
      dplyr::filter(!id.season == '') %>%
      mutate(id.coach.season = 1:n())
    
    if ('Assistant Coach' %in% coach_table$gp.regular_season) {
      ast_df <-
        coach_table %>%
        dplyr::filter(gp.regular_season == 'Assistant Coach') %>%
        dplyr::select(id.season:slug.team.bref, id.coach.season) %>%
        mutate(is.assistant_coach = T, is.head_coach = F)
      
      coach_table %<>%
        dplyr::filter(!gp.regular_season == 'Assistant Coach') %>%
        mutate(is.head_coach = T, is.assistant_coach = F) %>%
        bind_rows(ast_df) %>%
        arrange(id.coach.season) %>%
        dplyr::select(-id.coach.season)
    }
    seasons <-
      page %>%
      html_nodes('td:nth-child(1)') %>%
      html_text
    
    seasons <-
      seasons[!seasons %>% str_detect("Career|seasons")] %>%
      unique()
    
    coach_table %<>%
      mutate_each_(funs(as.numeric(.)) ,
                   vars =
                     coach_table %>%
                     dplyr::select(
                       -c(id.season, id.league, slug.team.bref, notes),
                       -contains("is.")
                     ) %>%
                     names()) %>%
      mutate(name.coach,
             notes = ifelse(notes == '', NA, notes)) %>%
      dplyr::select(id.season,
                    id.league,
                    name.coach,
                    slug.team.bref,
                    everything()) %>%
      suppressMessages()
    
    return(coach_table)
  }

get_bref_page_person_metadata <-
  function(page) {
    name.coach <-
      page %>%
      html_nodes('h1') %>%
      html_text()
    if (page %>%
        html_nodes('#info_box .bold_text') %>%
        html_text() %>% length >= 1) {
      fields <-
        page %>%
        html_nodes('#info_box .bold_text') %>%
        html_text()
      
      name.full.coach <-
        fields[1]
      
      if (fields %>% length > 1) {
        items_values <-
          page %>%
          html_nodes(".margin_top+ p , #necro-birth") %>%
          html_text() %>%
          .[1] %>%
          str_replace_all("\n\n", '\n') %>%
          str_split('\n') %>%
          unlist() %>%
          .[!. %in% ''] %>%
          gsub("\\(Full Record)", '', .) %>%
          gsub("\\(Full List)", '', .) %>%
          str_trim %>%
          str_replace_all(" in ", "-")
        metadata <-
          data_frame(name.full.coach, items_values) %>%
          separate(items_values,
                   into = c('item', 'value'),
                   sep = '\\: ') %>%
          mutate(item = item %>% str_to_lower %>% str_replace_all('\\ ', '_')) %>%
          spread(item , value) %>%
          mutate(name.coach)
        
        if ('born' %in% names(metadata)) {
          metadata %<>%
            dplyr::rename(date.birth = born)
        }
        if ('date.birth' %in% names(metadata)) {
          metadata %<>%
            separate(date.birth,
                     sep = '\\-',
                     into = c('date.birth', 'place.birth')) %>%
            mutate(date.birth = date.birth %>% strptime("%B %d, %Y") %>% as.Date())
        }
        if ('high_school' %in% names(metadata)) {
          metadata %<>%
            separate(
              high_school,
              sep = '\\-',
              into = c('high_school', 'place.high_school')
            )
        }
        
        if ('hall_of_fame' %in% names(metadata)) {
          metadata %<>%
            separate(hall_of_fame,
                     sep = '\\-',
                     into = c('detail.hof', 'year.hof')) %>%
            mutate(year.hof = year.hof %>% as.numeric())
        }
        
        if ('died' %in% names(metadata)) {
          metadata %<>%
            dplyr::rename(date.death = died) %>%
            mutate(date.death = date.death %>% strptime("%B %d, %Y") %>% as.Date())
        }
        
        metadata %<>%
          dplyr::select(name.coach, everything())
      } else {
        metadata <-
          data_frame(name.coach)
      }
      
      return(metadata)
      
    }
    else {
      "No metadata data for " %>%
        paste0(name.coach) %>%
        message
    }
  }

get_bref_coach_transactions <-
  function(page) {
    name.coach <-
      page %>%
      html_nodes('h1') %>%
      html_text()
    if (page %>%
        html_nodes('#transactions p') %>%
        html_text() %>% length > 0) {
      date_values <-
        page %>%
        html_nodes('#transactions p') %>%
        html_text()
      
      transaction_df <-
        data_frame(date_values) %>%
        separate(date_values,
                 into = c('date.action', 'detail.action'),
                 sep = '\\: ') %>%
        mutate(
          id.coaching.item = 1:n(),
          detail = detail.action %>% str_replace(" as ", "-") %>% str_replace(" by the ", "-") %>% 
            str_replace(" from contract ", '') %>% 
            str_replace(" of the ",'-') %>% str_replace(" of ","-") %>% 
            str_replace(" to the ",'-') %>% 
            str_replace("Appointed ",'Appointed-') %>% str_replace(' from ', '-') %>% str_replace("role",''),
          date.action = date.action %>% strptime("%B %d, %Y") %>% as.Date(),
          is.hiring = detail.action %>% str_detect("Hired"),
          is.firing = detail.action %>% str_detect("Fired")
        ) %>%
        separate(detail,
                 into = c('action', 'role', 'team'),
                 sep = '\\-') %>%
        mutate(
          action = action %>% str_to_lower,
          year = date.action %>% year,
          month = date.action %>% month,
          id.season = ifelse(
            month > 5,
            paste0(year, "-",
                   (year + 1) %>% substr(3, 4)),
            paste0(year - 1, "-",
                   (year) %>% substr(3, 4))
          )
        ) %>%
        dplyr::select(-c(month, year)) %>%
        mutate(name.coach) %>%
        dplyr::select(
          name.coach,
          id.coaching.item,
          id.season,
          date.action,
          is.hiring,
          is.firing,
          team:action,
          everything()
        )
      
      if (transaction_df %>% nrow() > 1) {
        transaction_df %<>%
          mutate(
            coaching.length.days = date.action - dplyr::lag(date.action),
            coaching.length.days = coaching.length.days %>% as.numeric()
          )
      }
      return(transaction_df)
      
    } else {
      name.coach %>%
        paste0(" has no information") %>%
        message()
    }
    
    
  }

get_bref_coaching_data_tables <-
  function(coach, coach_id = NA) {
    if ('Transactions' %in% coach_table_names) {
      page %>%
        get_bref_coach_transactions()
    }
  }

get_bref_coach_award_df <-
  function(coach = "Red Auerbach",
           coach_id = NA,
           merge_coaching_metadata = T,
           return_message = T) {
    if (!'coach' %>% exists() & coach_id %>% is.na()) {
      stop("Please enter a coach or coach id")
    }
    install_needed_packages()
    load_needed_packages()
    if (!'all_coaches' %>% exists())
      all_coaches <-
      get_nba_coach_history_df(return_message = F)
    
    if (coach_id %>% is.na()) {
      if (!coach %in% all_coaches$name.coach) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(all_coaches$name.coach, collapse = '\n')))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(name.coach == coach)
    }  else {
      if (!coach_id %in% all_coaches$id.coach.bref) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(
                 all_coaches$id.coach.bref, collapse = '\n'
               )))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(id.coach.bref == coach_id)
    }
    
    url <-
      coach_df$url.coach.bref
    
    page <-
      url %>%
      read_html()
    
    coach_table_names <-
      page %>%
      html_nodes('h2') %>%
      html_text()
    
    if ("Awards" %in% coach_table_names) {
      data <-
        page %>%
        get_nba_coach_awards()
      if (merge_coaching_metadata == T) {
        data %<>%
          right_join(coach_df)
      }
      
      if (return_message == T) {
        "You got coaching data for " %>%
          paste0(coach_df$name.coach) %>%
          message()
      }
      return(data)
    } else {
      coach_df$name.coach %>%
        paste0(" has no coaching data") %>%
        message()
    }
    
  }

get_bref_coach_bio_df <-
  function(coach = "Red Auerbach",
           coach_id = NA,
           merge_coaching_metadata = T,
           return_message = T) {
    if (!'coach' %>% exists() & coach_id %>% is.na()) {
      stop("Please enter a coach or coach id")
    }
    install_needed_packages()
    load_needed_packages()
    if (!'all_coaches' %>% exists()) {
      all_coaches <-
        get_nba_coach_history_df(return_message = F)
    }
    
    if (coach_id %>% is.na()) {
      if (!coach %in% all_coaches$name.coach) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(all_coaches$name.coach, collapse = '\n')))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(name.coach == coach)
    }  else {
      if (!coach_id %in% all_coaches$id.coach.bref) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(
                 all_coaches$id.coach.bref, collapse = '\n'
               )))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(id.coach.bref == coach_id)
    }
    
    url <-
      coach_df$url.coach.bref
    
    page <-
      url %>%
      read_html()
    
    coach_table_names <-
      page %>%
      html_nodes('h2') %>%
      html_text()
    
    if (page %>%
        get_bref_page_person_metadata() %>%
        nrow() > 0) {
      data <-
        page %>%
        get_bref_page_person_metadata()
      
      if (merge_coaching_metadata == T) {
        data %<>%
          right_join(coach_df) %>%
          dplyr::select(id.coach.bref,
                        name.coach,
                        is.active_coach:year.end,
                        everything()) %>%
          suppressMessages()
      }
      if (return_message == T) {
        "You got coaching biography data for " %>%
          paste0(coach_df$name.coach) %>% message()
      }
      data <- data[,names(data) %in% c("name.coach", "name.full.coach", "as_player", "date.birth", 
                         "place.birth", "college", "high_school", "place.high_school", 
                         "as_executive", "date.death", "detail.hof", "year.hof")]
      if('as_executive' %in% names(data)) {
        data %<>% 
          dplyr::rename(as.executive = as_executive)
      }
      return(data)
    } else {
      coach_df$name.coach %>%
        paste0(" has no biography data.") %>%
        message()
    }
  }

get_bref_coach_stat_df <-
  function(coach = "Red Auerbach",
           coach_id = NA,
           merge_coaching_metadata = T,
           return_message = T) {
    if (!'coach' %>% exists() & coach_id %>% is.na()) {
      stop("Please enter a coach or coach id")
    }
    install_needed_packages()
    load_needed_packages()
    if (!'all_coaches' %>% exists()) {
      all_coaches <-
        get_nba_coach_history_df(return_message = F)
    }
    
    if (coach_id %>% is.na()) {
      if (!coach %in% all_coaches$name.coach) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(all_coaches$name.coach, collapse = '\n')))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(name.coach == coach)
    }  else {
      if (!coach_id %in% all_coaches$id.coach.bref) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(
                 all_coaches$id.coach.bref, collapse = '\n'
               )))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(id.coach.bref == coach_id)
    }
    
    url <-
      coach_df$url.coach.bref
    
    page <-
      url %>%
      read_html()
    
    coach_table_names <-
      page %>%
      html_nodes('h2') %>%
      html_text()
    
    if ('Coaching Record' %in% coach_table_names) {
      data <-
        page %>%
        get_bref_coaching_table_stats()
      
      if (merge_coaching_metadata == T) {
        data %<>%
          right_join(coach_df) %>%
          suppressMessages()
        
      }
      data %<>%
        dplyr::select(id.season,
                      id.league,
                      name.coach,
                      slug.team.bref,
                      everything())
      
      data %<>% 
        mutate(
          is.nba.champion = ifelse(notes == 'NBA Champions', T, F),
          is.western.champion = ifelse(notes == 'WC Champions', T, F),
          is.eastern.champion = ifelse(notes  == 'EC Champions', T, F),
          is.aba.champion = ifelse(notes == 'ABA Champions', T, F),
          is.baa.champion = ifelse(notes == 'BAA Champions', T, F)
               )
      
      if (return_message == T) {
        "You got coaching stats data for " %>%
          paste0(coach_df$name.coach) %>%
          message()
      }
      return(data)
    } else {
      coach_df$name.coach %>%
        paste0(" has no coaching stat data.") %>%
        message()
    }
  }

get_bref_coach_transaction_df <-
  function(coach = "Lionel Hollins",
           coach_id = NA,
           merge_coaching_metadata = T,
           return_message = T) {
    if (!'coach' %>% exists() & coach_id %>% is.na()) {
      stop("Please enter a coach or coach id")
    }
    install_needed_packages()
    load_needed_packages()
    if (!'all_coaches' %>% exists()) {
      all_coaches <-
        get_nba_coach_history_df(return_message = F)
    }
    
    if (coach_id %>% is.na()) {
      if (!coach %in% all_coaches$name.coach) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(all_coaches$name.coach, collapse = '\n')))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(name.coach == coach)
    }  else {
      if (!coach_id %in% all_coaches$id.coach.bref) {
        stop("Sorry not a valid coach these are the valid coaches:\n" %>%
               paste0(paste0(
                 all_coaches$id.coach.bref, collapse = '\n'
               )))
      }
      coach_df <-
        all_coaches %>%
        dplyr::filter(id.coach.bref == coach_id)
    }
    
    url <-
      coach_df$url.coach.bref
    
    page <-
      url %>%
      read_html()
    
    coach_table_names <-
      page %>%
      html_nodes('h2') %>%
      html_text()
    
    if ('Transactions' %in% coach_table_names) {
      data <-
        page %>%
        get_bref_coach_transactions()
      
      if (merge_coaching_metadata == T) {
        data %<>%
          right_join(coach_df) %>%
          suppressMessages()
      }
      data %<>%
        dplyr::select(id.season,
                      name.coach,
                      team,
                      everything())
      
      if (return_message == T) {
        "You got coaching transaction data for " %>%
          paste0(coach_df$name.coach) %>%
          message()
      }
      return(data)
    } else {
      coach_df$name.coach %>%
        paste0(" has no coaching transaction data.") %>%
        message()
    }
  }

get_bref_coaches_bios_df <- function(is_all_coaches = T,
                                     coaches,
                                     message = F,
                                     merge_metadata = F) {
  if (is_all_coaches == T) {
    if (!'all_coaches' %>% exists()) {
      all_coaches <-
        get_nba_coach_history_df(return_message = message)
    }
    coaches <-
      all_coaches$name.coach
  } else {
    if (!'coaches' %>% exists()) {
      stop("Please enter coaches names")
    }
  }
  
  all_data <-
    coaches %>%
    map(
      function(x)
        get_bref_coach_bio_df(
          x,
          merge_coaching_metadata = merge_metadata,
          return_message = message
        )
    ) %>%
    compact %>%
    bind_rows()
  
  return(all_data)
}

get_bref_coaches_stats_df <- function(is_all_coaches = T,
                                      coaches,
                                      message = F,
                                      merge_metadata = F) {
  if (is_all_coaches == T) {
    if (!'all_coaches' %>% exists()) {
      all_coaches <-
        get_nba_coach_history_df(return_message = F)
    }
    coaches <-
      all_coaches$name.coach
  } else {
    if (!'coaches' %>% exists()) {
      stop("Please enter coaches names")
    }
  }
  
  all_data <-
    coaches %>%
    map(
      function(x)
        get_bref_coach_stat_df(
          x,
          merge_coaching_metadata = merge_metadata,
          return_message = message
        )
    ) %>%
    compact %>%
    bind_rows()
  
  return(all_data)
}

get_bref_coaches_transactions_df <-
  function(is_all_coaches = T,
           coaches,
           message = F,
           merge_metadata = F) {
    if (is_all_coaches == T) {
      if (!'all_coaches' %>% exists()) {
        all_coaches <-
          get_nba_coach_history_df(return_message = F)
      }
      coaches <-
        all_coaches$name.coach
    } else {
      if (!'coaches' %>% exists()) {
        stop("Please enter coaches names")
      }
    }
    
    all_data <-
      coaches %>%
      map(
        function(x)
          get_bref_coach_transaction_df(
            x,
            merge_coaching_metadata = merge_metadata,
            return_message = message
          )
      ) %>%
      compact %>%
      bind_rows()
    
    return(all_data)
  }

get_bref_coaches_awards_df <-
  function(is_all_coaches = T,
           coaches,
           message = F,
           merge_metadata = F) {
    if (is_all_coaches == T) {
      if (!'all_coaches' %>% exists()) {
        all_coaches <-
          get_nba_coach_history_df(return_message = F)
      }
      coaches <-
        all_coaches$name.coach
    } else {
      if (!'coaches' %>% exists()) {
        stop("Please enter coaches names")
      }
    }
    
    all_data <-
      coaches %>%
      map(
        function(x)
          get_bref_coach_award_df(
            x,
            merge_coaching_metadata = merge_metadata,
            return_message = message
          )
      ) %>%
      compact %>%
      bind_rows()
    
    return(all_data)
  }
