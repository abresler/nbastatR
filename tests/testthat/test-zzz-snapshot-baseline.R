# Snapshot baseline test - Auto-generated on 2026-01-31
# This captures the current state before cleanup

context("Baseline Snapshot")

test_that("Package exports match baseline", {
  exports <- getNamespaceExports("nbastatR")
  expect_equal(length(exports), 77)
  expect_true("nba_players" %in% exports)
  expect_true("game_logs" %in% exports)
  expect_true("box_scores" %in% exports)
})

test_that("Core dictionaries load", {
  # Test that dictionary functions work
  expect_silent(dictionary_nba_names())
  expect_silent(nba_teams())
})

