# test-api-contracts.R
# Contract tests for critical nbastatR endpoints
# These tests verify API response structure matches expectations
# Run daily via GitHub Actions to catch API changes early

context("API Contracts")

# Skip on CRAN (no network access)
skip_on_cran()

# ============================================================================
# NBA PLAYERS CONTRACT
# ============================================================================

test_that("nba_players() returns expected contract", {
  skip_if_offline()

  players <- nba_players()

  # Structure checks
  expect_s3_class(players, "tbl_df")
  expect_gt(nrow(players), 100)  # Should have many players

  # Required columns exist
  required_cols <- c("idPlayer", "namePlayer", "isActive")
  for (col in required_cols) {
    expect_true(col %in% names(players),
                info = paste("Missing required column:", col))
  }

  # Type checks
  expect_true(is.numeric(players$idPlayer))
  expect_type(players$namePlayer, "character")
  expect_type(players$isActive, "logical")

  # Data sanity checks
  expect_true(any(players$isActive))  # Some active players
  expect_true(all(!is.na(players$idPlayer)))  # No missing IDs
  expect_true(all(nchar(players$namePlayer) > 0))  # No empty names
})

# ============================================================================
# NBA TEAMS CONTRACT
# ============================================================================

test_that("nba_teams() returns expected contract", {
  skip_if_offline()

  teams <- nba_teams()

  # Structure checks
  expect_s3_class(teams, "tbl_df")
  expect_gte(nrow(teams), 30)  # At least 30 NBA teams

  # Required columns exist
  required_cols <- c("idTeam", "slugTeam", "cityTeam", "teamName")
  for (col in required_cols) {
    expect_true(col %in% names(teams),
                info = paste("Missing required column:", col))
  }

  # Type checks
  expect_true(is.numeric(teams$idTeam))
  expect_type(teams$slugTeam, "character")

  # Data sanity checks
  non_na_slugs <- teams$slugTeam[!is.na(teams$slugTeam)]
  expect_true(length(non_na_slugs) > 0)  # At least some non-NA slugs
  expect_true("LAL" %in% teams$slugTeam)  # Lakers exist
  expect_true("BOS" %in% teams$slugTeam)  # Celtics exist
})

# ============================================================================
# DICTIONARY FUNCTIONS CONTRACTS
# ============================================================================

test_that("dictionary_nba_names() returns expected contract", {
  skip_if_offline()

  dict <- dictionary_nba_names()

  expect_s3_class(dict, "tbl_df")
  expect_gt(nrow(dict), 0)
})

# ============================================================================
# HELPER: Check if offline
# ============================================================================

skip_if_offline <- function() {
  tryCatch({
    con <- url("https://stats.nba.com", "rb")
    close(con)
  }, error = function(e) {
    skip("No internet connection available")
  })
}
