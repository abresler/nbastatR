context("cache management")

test_that("cache_status returns tibble", {
  status <- cache_status(verbose = FALSE)
  expect_s3_class(status, "tbl_df")
  expect_true("component" %in% names(status))
  expect_true("value" %in% names(status))
})

test_that("cache_get_options returns list", {
  opts <- cache_get_options()
  expect_type(opts, "list")
  expect_true("max_age_hours" %in% names(opts))
  expect_true("warn_on_stale" %in% names(opts))
})

test_that("cache_set_options updates options", {
  # Save original
orig <- cache_get_options()

  # Set new options
  cache_set_options(max_age_hours = 12, warn_on_stale = FALSE)

  # Check updated
  new_opts <- cache_get_options()
  expect_equal(new_opts$max_age_hours, 12)
  expect_equal(new_opts$warn_on_stale, FALSE)

  # Restore original
  cache_set_options(
    max_age_hours = orig$max_age_hours,
    warn_on_stale = orig$warn_on_stale
  )
})

test_that("cache_list_functions returns character vector", {
  fns <- cache_list_functions()
  expect_type(fns, "character")
  expect_gt(length(fns), 0)
})

test_that("cache_is_memoized works", {
  # nba_players should be memoized
  expect_true(cache_is_memoized("nba_players"))

  # Non-existent function should return FALSE
  expect_false(cache_is_memoized("nonexistent_function_xyz"))
})

test_that("cache_clear_function handles errors gracefully", {
  # Should not error on non-existent function
  expect_no_error(
    invisible(capture.output(cache_clear_function("nonexistent_xyz")))
  )
})
