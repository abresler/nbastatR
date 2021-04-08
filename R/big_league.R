.dictionary_blg_names <-
  function() {
    tibble(
      nameBLG = c(
        "_id",
        "teamCity",
        "teamName",
        "colorhex",
        "colorhex2",
        "colorhex3",
        "darkerhex1",
        "darkerhex2",
        "darkerhex3",
        "conference",
        "division",
        "teamlogo"
      ),
      nameActual =
        c(
          "slugTeamBLG",
          "cityTeam",
          "teamName",
          "hexColor1",
          "hexColor2",
          "hexColor3",
          "darkhexColor1",
          "darkhexColor2",
          "darkhexColor3",
          "nameConference",
          "nameDivision",
          "urlWikiLogo"
        )
    )
  }

.validate_dictionary_names <-
  function(data,
           dictionary = .dictionary_blg_names(),
           dictionary_name = "nameBLG") {
    names(data) %>%
      map_chr(function(x) {
        df_row <- dictionary %>% filter(!!sym(dictionary_name) == x)
        if (nrow(df_row) == 0) {
          glue("Missing {x} from {dictionary_name} dictionary") %>%
            cat(fill = T)

          return(x)
        }
        df_row$nameActual
      })
  }

.blg_teams <- function() {
  data <-
    "https://bigleaguegraphs.com/api/nba/teams/list" %>%
    fromJSON() %>%
    dplyr::as_tibble()

  data <-
    data %>%
    set_names(.validate_dictionary_names(data = data, dictionary = .dictionary_blg_names(), dictionary_name = "nameBLG")) %>%
    unite(nameTeam, cityTeam, teamName, sep = " ", remove = F)

  data
  }
