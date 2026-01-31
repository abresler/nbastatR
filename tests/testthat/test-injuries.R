context("injuries")

test_that("INJURY_SEVERITY constant exists with expected values", {
  expect_true(exists("INJURY_SEVERITY"))
  expect_equal(INJURY_SEVERITY[["OUT"]], 100)
  expect_equal(INJURY_SEVERITY[["QUESTIONABLE"]], 50)
  expect_equal(INJURY_SEVERITY[["PROBABLE"]], 20)
})

test_that("score_injury_severity handles common statuses", {
  expect_equal(score_injury_severity("OUT"), 100)
  expect_equal(score_injury_severity("DOUBTFUL"), 80)
  expect_equal(score_injury_severity("QUESTIONABLE"), 50)
  expect_equal(score_injury_severity("PROBABLE"), 20)
  expect_equal(score_injury_severity("GTD"), 40)
})

test_that("score_injury_severity handles case insensitivity", {
  expect_equal(score_injury_severity("out"), 100)
  expect_equal(score_injury_severity("Out"), 100)
  expect_equal(score_injury_severity("questionable"), 50)
})

test_that("score_injury_severity handles NA and empty", {
  expect_equal(score_injury_severity(NA_character_), 0)
  expect_equal(score_injury_severity(""), 0)
})

test_that("categorize_injury_severity returns expected categories", {
  expect_equal(categorize_injury_severity(100), "CRITICAL")
  expect_equal(categorize_injury_severity(80), "CRITICAL")
  expect_equal(categorize_injury_severity(50), "HIGH")
  expect_equal(categorize_injury_severity(25), "MEDIUM")
  expect_equal(categorize_injury_severity(10), "LOW")
  expect_equal(categorize_injury_severity(0), "NONE")
  expect_equal(categorize_injury_severity(NA), "UNKNOWN")
})

test_that("add_fantasy_impact handles empty data", {
  empty_df <- tibble::tibble(
    slugTeam = character(),
    namePlayer = character(),
    isGTD = logical(),
    injuryTimeline = character()
  )
  result <- add_fantasy_impact(empty_df)
  expect_equal(nrow(result), 0)
})

test_that("add_fantasy_impact adds expected columns", {
  sample_data <- tibble::tibble(
    slugTeam = "LAL",
    namePlayer = "Test Player",
    isGTD = TRUE,
    injuryTimeline = "day-to-day"
  )
  result <- add_fantasy_impact(sample_data)
  expect_true("severityScore" %in% names(result))
  expect_true("severityCategory" %in% names(result))
  expect_true("fantasyImpact" %in% names(result))
  expect_true("recommendedAction" %in% names(result))
})
