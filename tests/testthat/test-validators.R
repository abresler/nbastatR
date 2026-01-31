context("validators")

test_that("validate_nba_players works with valid data", {
  valid_data <- tibble::tibble(
    idPlayer = c(1, 2, 3),
    namePlayer = c("A", "B", "C"),
    isActive = c(TRUE, TRUE, FALSE)
  )

  # Should not error
  expect_no_error(validate_nba_players(valid_data))
})

test_that("validate_nba_players fails with missing columns", {
  invalid_data <- tibble::tibble(
    idPlayer = c(1, 2, 3),
    # Missing namePlayer and isActive
    someOtherCol = c("x", "y", "z")
  )

  expect_error(validate_nba_players(invalid_data), "missing expected columns")
})

test_that("validate_nba_players warns instead of errors when warn_only = TRUE", {
  invalid_data <- tibble::tibble(
    idPlayer = c(1, 2, 3)
  )

  expect_warning(validate_nba_players(invalid_data, warn_only = TRUE))
})

test_that("validate_nba_teams works with valid data", {
  valid_data <- tibble::tibble(
    idTeam = c(1, 2),
    slugTeam = c("LAL", "BOS"),
    cityTeam = c("Los Angeles", "Boston"),
    teamName = c("Lakers", "Celtics")
  )

  expect_no_error(validate_nba_teams(valid_data))
})

test_that("validate_game_logs works with valid data", {
  valid_data <- tibble::tibble(
    slugSeason = "2023-24",
    typeSeason = "Regular Season",
    yearSeason = 2024
  )

  expect_no_error(validate_game_logs(valid_data))
})

test_that("validate_box_scores works with valid data", {
  valid_data <- tibble::tibble(
    typeBoxScore = "Traditional",
    typeResult = "player",
    idGame = 12345
  )

  expect_no_error(validate_box_scores(valid_data))
})

test_that(".validate_response handles empty data", {
  empty_data <- tibble::tibble()

  # Should warn about empty data, not error
  expect_warning(
    nbastatR:::.validate_response(empty_data, c("col1"), "test")
  )
})

test_that(".validate_response errors on non-dataframe", {
  expect_error(
    nbastatR:::.validate_response("not a df", c("col1"), "test"),
    "unexpected data type"
  )
})
