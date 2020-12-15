#' .assign.synergy <-
#'   function(all_data, nest_data = F, return_wide = F) {
#'     result_types <-
#'       all_data$typeResult %>% unique()
#'
#'     result_types %>%
#'       walk(function(result_type){
#'         table_name <-
#'           glue::glue("dataSynergy{str_to_title(result_type)}") %>%
#'           as.character()
#'         df_result <-
#'           all_data %>%
#'           filter(typeResult == result_type) %>%
#'           select(-typeResult) %>%
#'           select(slugSeason, everything()) %>%
#'           arrange(slugSeason) %>%
#'           unnest() %>%
#'           distinct()
#'
#'         if (result_type == "player") {
#'           df_result <-
#'             df_result %>%
#'             filter(!is.na(idPlayer))
#'         }
#'
#'         if (return_wide) {
#'           df_long <-
#'             df_result %>%
#'             gather_data(
#'               variable_name = 'item',
#'               numeric_ids = c("^id", "gp", "numberJersey"),
#'               use_logical_keys = TRUE,
#'               use_factor_keys = TRUE,
#'               unite_columns = NULL,
#'               separate_columns = NULL,
#'               use_date_keys = FALSE
#'             )
#'           df_result <-
#'             df_long %>%
#'             unite(item, item, categorySynergy, sep = "") %>%
#'             return_wide()
#'         }
#'
#'         if (nest_data) {
#'           df_result <-
#'             df_result %>%
#'             nest(-c(slugSeason))
#'         }
#'
#'         assign(table_name, df_result, envir = .GlobalEnv)
#'
#'       })
#'   }
#'
#' .number_to_pct <-
#'   function(data) {
#'     pct_cols <- data %>% select(dplyr::matches("pct[A-Z]")) %>% names()
#'     data %>%
#'       mutate_at(pct_cols,
#'                 funs(. %>% as.numeric() /  100))
#'   }
#'
#' .dictionary_synergy_categories <-
#'   memoise::memoise(function() {
#'     tibble(
#'       nameSynergy = c(
#'         "Transition",
#'         "Isolation",
#'         "PRBallHandler",
#'         "PRRollman",
#'         "Postup",
#'         "Spotup",
#'         "Handoff",
#'         "Cut",
#'         "OffScreen",
#'         "OffRebound",
#'         "Misc"
#'       ),
#'       nameTable = c(
#'         "Transition",
#'         "Isolation",
#'         "Pick and Roll Ball Handler",
#'         "Pick and Roll Rollman",
#'         "Post Up",
#'         "Spot Up",
#'         "Handoff",
#'         "Cut",
#'         "OffScreen",
#'         "Off Rebound",
#'         "Misc"
#'       )
#'     )
#'   })
#'
#' .get_synergy_category_data <-
#'   function(season = 2020,
#'          result_type = "player",
#'          season_type = "Regular Season",
#'          category = "transition",
#'          set_type = "offensive",
#'          results = 500,
#'          return_message = T
#'          ) {
#'     if (season < 2016) {
#'       stop("Synergy data starts for the 2015-16 season")
#'     }
#'
#'     categories <-
#'       c(
#'         "Transition",
#'         "Isolation",
#'         "PRBallHandler",
#'         "PRRollman",
#'         "Postup",
#'         "Spotup",
#'         "Handoff",
#'         "Cut",
#'         "OffScreen",
#'         "OffRebound",
#'         "Misc"
#'       )
#'     cat_slug <- category %>% str_to_lower()
#'     wrong_cat <-
#'       cat_slug  %>% str_detect(str_to_lower(categories)) %>% sum(na.rm = T) == 0
#'     if (wrong_cat) {
#'       stop(glue::glue(
#'         "Synergy categories can only be:\n{str_c(categories, collapse = '\n')}"
#'       ))
#'     }
#'
#'     slug_season_type <-
#'       case_when(season_type %>% str_to_lower() %>% str_detect("regular") ~ "REG",
#'                 TRUE ~ "Post")
#'     slug_season <- generate_season_slug(season = season)
#'
#'     season_synergy <- season - 1
#'     json_url <-
#'       glue::glue(
#'         "https://stats-prod.nba.com/wp-json/statscms/v1/synergy/{result_type}/?category={category}&season={season_synergy}&seasonType={slug_season_type}&names={set_type}&limit={results}"
#'       ) %>%
#'       as.character()
#'
#'     if (return_message) {
#'       glue::glue(
#'         "Acquiring {result_type} synergy data for {str_to_lower(set_type)} {str_to_lower(category)} in the {str_to_lower(season_type)} during the {slug_season}"
#'       ) %>%
#'         cat(fill = T)
#'     }
#'     json <-
#'       json_url %>%
#'       fromJSON()
#'     json_names <- json$results %>% names()
#'     actual_names <-
#'       json_names %>%
#'       resolve_nba_names()
#'
#'     data <-
#'       json$results %>%
#'       as_tibble() %>%
#'       purrr::set_names(actual_names) %>%
#'       mutate(
#'         categorySynergy = category,
#'         typeResult = result_type,
#'         slugSeason = slug_season) %>%
#'       select(-one_of("yearSeason")) %>%
#'       dplyr::select(
#'         typeResult,
#'         typeSet,
#'         categorySynergy,
#'         slugSeason,
#'         typeSeason,
#'         everything()
#'       ) %>%
#'       mutate_at("idTeam",
#'                 funs(. %>% as.numeric()))
#'
#'     if (result_type %>% str_to_lower() == "player") {
#'       data <-
#'         data %>%
#'         unite(namePlayer, nameFirst, nameLast, sep =  " ") %>%
#'         mutate(idPlayer = idPlayer %>% as.numeric())
#'     }
#'
#'     if (data %>% has_name("tov")){
#'       data <-
#'         data %>%
#'         dplyr::rename(pctTOV = tov)
#'     }
#'
#'   num_cols <- data %>% select(-dplyr::matches(char_words())) %>% names()
#'
#'   data <-
#'     data %>%
#'     mutate_at(num_cols,
#'              funs(. %>% as.character() %>% readr::parse_number()))
#'
#'   ppp_names <- data %>% dplyr::select(dplyr::matches("PPP")) %>% names()
#'
#'   if (ppp_names %>% length() >0) {
#'     data <- data %>%
#'       mutate_at(ppp_names,
#'                 funs(. %>% as.numeric()))
#'   }
#'
#'   data <-
#'       data %>%
#'       .number_to_pct()
#'
#'     data %>%
#'       nest(-c(slugSeason, typeResult, categorySynergy, typeSet),
#'            .key = dataSynergy)
#'   }
#'
#'
#' #' Get Synergy data for specified season
#' #'
#' #' Get Synergy data for specified result type,
#' #' category, season type and set
#' #'
#' #' @param seasons vector of seasons from 2016 onward
#' #' @param result_types result type \itemize{
#' #' \item team
#' #' \item player
#' #' }
#' #' @param categories vector of synergy categories options include: \itemize{
#' #' \item Transition
#' #' \item Isolation
#' #' \item PRBallHandler
#' #' \item PRRollman
#' #' \item Postup
#' #' \item Spotup
#' #' \item Handoff
#' #' \item Cut
#' #' \item OffScreen
#' #' \item OffRebound
#' #' \item Misc
#' #' }
#' #' @param season_types type of season play \itemize{
#' #' \item Playoffs
#' #' \item Regular Season
#' #' }
#' #' @param set_types set type \itemize{
#' #' \item offensive
#' #' \item defensive
#' #' }
#' #' @param results number of results
#' #' @param assign_to_environment if \code{TRUE} assigns table to environment
#' #' @param return_wide if \code{return_wide} returns a spread \code{tibble}
#' #' @param return_message
#' #'
#' #' @return a \code{tibble}
#' #' @export
#' #' @import dplyr stringr magrittr curl jsonlite readr magrittr purrr tidyr rlang
#' #' @importFrom glue glue
#' #' @examples
#' #' synergy(seasons = 2019, result_types = c("player", "team"), season_types = c("Regular Season"), set_types = c("offensive", "defensive"), categories = c("Transition", "Isolation", "PRBallHandler", "PRRollman", "Postup",  "Spotup", "Handoff", "Cut", "OffScreen", "OffRebound", "Misc"), results = 500, assign_to_environment = TRUE, return_wide = F, return_message = TRUE)
#' synergy <-
#'   function(seasons = 2016:2018,
#'            result_types = c("player", "team"),
#'            season_types = c("Regular Season"),
#'            set_types = c("offensive", "defensive"),
#'            categories = c("Transition", "Isolation", "PRBallHandler", "PRRollman", "Postup",
#'                           "Spotup", "Handoff", "Cut", "OffScreen", "OffRebound", "Misc"
#'            ),
#'            results = 500,
#'            assign_to_environment = TRUE,
#'            return_wide = F,
#'            nest_data = F,
#'            return_message = TRUE) {
#'     if (seasons %>% purrr::is_null()) {
#'       stop("please enter season")
#'     }
#'
#'     if (result_types %>% str_to_lower() %in% c("player", "team") %>% sum(na.rm = T) == 0) {
#'       stop("Result type can only be player and/or team")
#'     }
#'     input_df <-
#'       expand.grid(
#'         season = seasons,
#'         result_type = result_types,
#'         season_type = season_types,
#'         set_type = set_types,
#'         category = categories,
#'         stringsAsFactors = F
#'       ) %>%
#'       as_tibble() %>%
#'       distinct()
#'
#'     .get_synergy_category_data_safe <-
#'       purrr::possibly(.get_synergy_category_data, tibble())
#'
#'     all_data <-
#'       1:nrow(input_df) %>%
#'       future_map_dfr(function(x) {
#'         df_row <- input_df %>% slice(x)
#'         df_row %$%
#'           .get_synergy_category_data_safe(
#'             season = season,
#'             result_type = result_type,
#'             season_type = season_type,
#'             category = category,
#'             set_type = set_type,
#'             results = results,
#'             return_message = return_message
#'           )
#'       }) %>%
#'       suppressWarnings()
#'
#'
#'     if (assign_to_environment) {
#'       all_data %>%
#'         .assign.synergy(nest_data = nest_data,
#'                       return_wide = return_wide)
#'     }
#'
#'     all_data
#'   }
