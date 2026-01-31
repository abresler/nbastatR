# Snapshot baseline test - Updated after adding advanced metrics & cache management
# This captures the current state after enhancements

context("Baseline Snapshot")

test_that("Package exports match baseline", {
  exports <- getNamespaceExports("nbastatR")
  expect_gte(length(exports), 100)  # Now 106 with new modules
  expect_true("nba_players" %in% exports)
  expect_true("game_logs" %in% exports)
  expect_true("box_scores" %in% exports)
})

test_that("Core dictionaries load", {
  # Test that dictionary functions work
  expect_silent(dictionary_nba_names())
  expect_silent(nba_teams())
})

