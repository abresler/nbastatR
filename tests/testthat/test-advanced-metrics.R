context("advanced metrics")

test_that("calculate_ts_pct works correctly", {
  # Standard case
  expect_equal(
    round(calculate_ts_pct(pts = 30, fga = 20, fta = 10), 3),
    0.615
  )

  # Zero attempts = NA
expect_true(is.na(calculate_ts_pct(pts = 0, fga = 0, fta = 0)))

  # Vectorized
  result <- calculate_ts_pct(pts = c(20, 30), fga = c(15, 20), fta = c(5, 10))
  expect_length(result, 2)
})

test_that("calculate_efg_pct works correctly", {
  # 10 FGM with 3 threes on 20 FGA = (10 + 0.5*3) / 20 = 0.575
  expect_equal(
    round(calculate_efg_pct(fgm = 10, fg3m = 3, fga = 20), 3),
    0.575
  )

  # Zero attempts = NA
  expect_true(is.na(calculate_efg_pct(fgm = 0, fg3m = 0, fga = 0)))
})

test_that("calculate_game_score works correctly", {
  # Typical stat line
  gs <- calculate_game_score(
    pts = 25, fgm = 9, fga = 18, ftm = 5, fta = 6,
    orb = 1, drb = 5, stl = 2, ast = 7, blk = 1, pf = 2, tov = 3
  )
  expect_gt(gs, 15)  # Should be a decent game score
  expect_lt(gs, 30)

  # Empty stat line = 0
  expect_equal(
    calculate_game_score(pts = 0, fgm = 0, fga = 0, ftm = 0, fta = 0,
                         orb = 0, drb = 0, stl = 0, ast = 0, blk = 0, pf = 0, tov = 0),
    0
  )
})

test_that("calculate_usage_rate works correctly", {
  usg <- calculate_usage_rate(
    fga = 20, fta = 8, tov = 3, mp = 36,
    tm_fga = 85, tm_fta = 25, tm_tov = 12, tm_mp = 240
  )

  # Should be between 0 and 100
  expect_gt(usg, 0)
  expect_lt(usg, 100)

  # Zero minutes = NA
  expect_true(is.na(calculate_usage_rate(
    fga = 20, fta = 8, tov = 3, mp = 0,
    tm_fga = 85, tm_fta = 25, tm_tov = 12
  )))
})

test_that("calculate_ast_tov_ratio works correctly", {
  expect_equal(calculate_ast_tov_ratio(ast = 10, tov = 2), 5)
  expect_true(is.na(calculate_ast_tov_ratio(ast = 10, tov = 0)))
})

test_that("calculate_four_factors returns tibble", {
  ff <- calculate_four_factors(
    fgm = 40, fg3m = 10, fga = 85,
    tov = 12, orb = 10, opp_drb = 35,
    ftm = 18, fta = 22
  )

  expect_s3_class(ff, "tbl_df")
  expect_true("pctEFG" %in% names(ff))
  expect_true("rateTOV" %in% names(ff))
  expect_true("pctORB" %in% names(ff))
  expect_true("rateFT" %in% names(ff))
})

test_that("calculate_net_rating works correctly", {
  # Team outscored opponent 110-100 in 100 possessions
  net <- calculate_net_rating(pts = 110, pts_allowed = 100, poss = 100)
  expect_equal(net, 10)

  # Zero possessions = NA
  expect_true(is.na(calculate_net_rating(pts = 110, pts_allowed = 100, poss = 0)))
})
