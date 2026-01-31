context("visualizations")

test_that("plot_shot_chart validates input", {
  # Missing required columns
  bad_data <- tibble::tibble(x = 1:5, y = 1:5)
  expect_error(plot_shot_chart(bad_data), "missing required columns")
})

test_that("plot_scoring_trend validates input", {
  bad_data <- tibble::tibble(x = 1:5)
  expect_error(plot_scoring_trend(bad_data), "missing required columns")
})

test_that("plot_player_comparison requires multiple players", {
  single_player <- tibble::tibble(
    namePlayer = "LeBron James",
    pts = 25,
    ast = 7
  )
  expect_error(
    plot_player_comparison(single_player, stat_cols = c("pts", "ast")),
    "at least 2 players"
  )
})

test_that("plot_standings validates columns", {
  # Missing nameTeam
  bad_data <- tibble::tibble(wins = 1:5)
  expect_error(plot_standings(bad_data, stat_col = "wins"), "nameTeam")

  # Missing stat column
  bad_data2 <- tibble::tibble(nameTeam = letters[1:5])
  expect_error(plot_standings(bad_data2, stat_col = "wins"), "not found")
})

test_that("plot_efficiency_scatter validates columns", {
  bad_data <- tibble::tibble(x = 1:5)
  expect_error(
    plot_efficiency_scatter(bad_data, x_col = "x", y_col = "y"),
    "not found"
  )
})
