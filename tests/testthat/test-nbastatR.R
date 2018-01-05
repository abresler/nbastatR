context("basic functionality")
test_that("we can do something", {

  library(dplyr)
  df_players <- get_nba_players()
  names_players <- df_players %>% names()
  expect_that(names_players, equals(c("isActive", "isRookie", "namePlayer", "idPlayer", "countSeasons",
                                      "nameTeam", "namePlayerLastFirst", "yearSeasonFirst", "yearSeasonLast",
                                      "slugPlayer", "idTeam", "cityTeam", "teamName", "slugTeam", "codeTeam",
                                      "hasGamesPlayedFlag", "urlPlayerStats", "urlPlayerThumbnail",
                                      "urlPlayerHeadshot", "urlPlayerActionPhoto", "namePlayerLast",
                                      "namePlayerFirst")
  ))
})
