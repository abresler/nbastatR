context("basic functionality")

test_that("nba_players returns expected structure", {
  library(testthat)
  library(dplyr)

  df_players <- nba_players()

  # Test that we get a tibble with data

  expect_s3_class(df_players, "tbl_df")
  expect_gt(nrow(df_players), 0)

  # Test core columns that should always exist
  core_columns <- c("isActive", "isRookie", "namePlayer", "idPlayer",
                    "countSeasons", "yearSeasonFirst", "yearSeasonLast",
                    "idTeam", "hasGamesPlayedFlag")

  for (col in core_columns) {
    expect_true(col %in% names(df_players),
                info = paste("Missing expected column:", col))
  }

  # Test URL columns exist
  url_columns <- c("urlPlayerStats", "urlPlayerThumbnail",
                   "urlPlayerHeadshot", "urlPlayerActionPhoto")

  for (col in url_columns) {
    expect_true(col %in% names(df_players),
                info = paste("Missing URL column:", col))
  }

  # Test data types (idPlayer may be integer or double depending on API)
  expect_true(is.numeric(df_players$idPlayer))
  expect_type(df_players$namePlayer, "character")
  expect_type(df_players$isActive, "logical")
})
