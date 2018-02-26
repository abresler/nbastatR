context("basic functionality")
test_that("Check Names", {
  library(testthat)
  library(dplyr)
  df_players <- get_nba_players()
  names_players <-
    df_players %>% names()
  actual_names <- c("isActive", "isRookie", "namePlayer", "idPlayer", "countSeasons",
                    "nameTeam", "namePlayerLastFirst", "yearSeasonFirst", "yearSeasonLast",
                    "slugPlayer", "idTeam", "cityTeam", "teamName", "slugTeam", "codeTeam",
                    "hasGamesPlayedFlag", "urlPlayerStats", "urlPlayerThumbnail",
                    "urlPlayerHeadshot", "urlPlayerActionPhoto", "namePlayerLast",
                    "namePlayerFirst")
  expect_identical(object = names_players, expected = actual_names)
})
