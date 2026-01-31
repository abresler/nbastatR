# cache_management.R
# Cache Management Utilities for nbastatR
# Makes memoization visible and controllable

# ============================================================================
# PACKAGE-LEVEL CACHE REGISTRY
# ============================================================================

# Registry of memoized functions in the package
.nbastatR_memoized_functions <- c(
 ".dictionary_boxscore_slugs",
 ".get_box_score_type",
  "nba_players",
  "nba_teams",
  ".hoops_hype_salaries",
  ".bref_players_dictionary",
  ".bref_teams_dictionary",
  ".bref_coaches_dictionary",
  ".bref_awards_dictionary"
)

# ============================================================================
# CACHE STATUS
# ============================================================================

#' Get Cache Status
#'
#' Returns information about the current state of memoized function caches.
#' Shows which functions have cached data and approximate cache age.
#'
#' @param verbose if \code{TRUE} prints status messages
#'
#' @return a \code{tibble} with cache status information
#' @export
#' @family cache management
#'
#' @examples
#' \dontrun
#' cache_status()
#' }
cache_status <- function(verbose = TRUE) {
  if (verbose) {
    cat("
\n")
  }

  # Get cache directory info

cache_dir <- file.path(Sys.getenv("HOME"), ".nbastatR", "cache")

  status <- dplyr::tibble(
    component = c("memoise_functions", "cache_directory", "max_age_hours"),
    value = c(
      as.character(length(.nbastatR_memoized_functions)),
      ifelse(dir.exists(cache_dir), cache_dir, "Not created"),
      as.character(getOption("nbastatR.cache.max_age", 24))
    ),
    description = c(
      "Number of memoized functions in package",
      "File cache location (if using file cache)",
      "Maximum cache age before staleness warning"
    )
  )

  if (verbose) {
    cat("nbastatR Cache Status\n")
    cat("=====================\n\n")
    cat("Memoized functions:", length(.nbastatR_memoized_functions), "\n")
    cat("Max cache age:", getOption("nbastatR.cache.max_age", 24), "hours\n")
    cat("\nNote: memoise uses in-memory cache by default.\n")
    cat("Cache is cleared when R session ends.\n\n")
    cat("Use cache_clear() to manually clear all caches.\n")
  }

  invisible(status)
}

# ============================================================================
# CACHE CLEARING
# ============================================================================

#' Clear All Caches
#'
#' Clears all memoized function caches in nbastatR.
#' This forces fresh data to be fetched on next function call.
#'
#' @param confirm if \code{TRUE} (default), asks for confirmation before clearing
#'
#' @return invisible NULL
#' @export
#' @family cache management
#'
#' @examples
#' \dontrun{
#' cache_clear(confirm = FALSE)
#' }
cache_clear <- function(confirm = TRUE) {
  if (confirm) {
    cat("This will clear all cached data.\n")
    cat("Next API calls will fetch fresh data (may be slow).\n\n")
    response <- readline("Continue? (y/n): ")
    if (!tolower(response) %in% c("y", "yes")) {
      cat("Cache clear cancelled.\n")
      return(invisible(NULL))
    }
  }

  # Clear memoized functions that exist in namespace
  cleared <- 0

  # List of key memoized functions to clear
  functions_to_clear <- c(
    "nba_players",
    "nba_teams",
    ".dictionary_boxscore_slugs",
    ".get_box_score_type"
  )

  for (fn_name in functions_to_clear) {
    tryCatch({
      fn <- get(fn_name, envir = asNamespace("nbastatR"))
      if (memoise::is.memoised(fn)) {
        memoise::forget(fn)
        cleared <- cleared + 1
      }
    }, error = function(e) {
      # Function doesn't exist or isn't memoized, skip
    })
  }

  cat("\n Cleared", cleared, "memoized function caches.\n")
  cat(" Fresh data will be fetched on next call.\n")

  invisible(NULL)
}

#' Clear Cache for Specific Function
#'
#' Clears the cache for a specific memoized function.
#'
#' @param function_name character name of the function to clear cache for
#'
#' @return invisible NULL
#' @export
#' @family cache management
#'
#' @examples
#' \dontrun{
#' cache_clear_function("nba_players")
#' }
cache_clear_function <- function(function_name) {
  if (!is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string")
  }

  tryCatch({
    fn <- get(function_name, envir = asNamespace("nbastatR"))
    if (memoise::is.memoised(fn)) {
      memoise::forget(fn)
      cat(" Cache cleared for:", function_name, "\n")
    } else {
      cat("", function_name, "is not a memoized function.\n")
    }
  }, error = function(e) {
    cat(" Function not found:", function_name, "\n")
  })

  invisible(NULL)
}

# ============================================================================
# CACHE OPTIONS
# ============================================================================

#' Set Cache Options
#'
#' Configure cache behavior for nbastatR.
#'
#' @param max_age_hours maximum cache age in hours before staleness warning (default: 24)
#' @param warn_on_stale if \code{TRUE}, warn when using cached data older than max_age
#'
#' @return invisible list of current options
#' @export
#' @family cache management
#'
#' @examples
#' \dontrun{
#' # Set cache to warn after 12 hours
#' cache_set_options(max_age_hours = 12)
#' }
cache_set_options <- function(max_age_hours = 24, warn_on_stale = TRUE) {
  options(nbastatR.cache.max_age = max_age_hours)
  options(nbastatR.cache.warn_stale = warn_on_stale)

  cat(" Cache options updated:\n")
  cat("   Max age:", max_age_hours, "hours\n")
  cat("   Warn on stale:", warn_on_stale, "\n")

  invisible(list(
    max_age_hours = max_age_hours,
    warn_on_stale = warn_on_stale
  ))
}

#' Get Current Cache Options
#'
#' Returns the current cache configuration options.
#'
#' @return list of current cache options
#' @export
#' @family cache management
#'
#' @examples
#' cache_get_options()
cache_get_options <- function() {
  list(
    max_age_hours = getOption("nbastatR.cache.max_age", 24),
    warn_on_stale = getOption("nbastatR.cache.warn_stale", TRUE)
  )
}

# ============================================================================
# CACHE INFO HELPERS
# ============================================================================

#' List Memoized Functions
#'
#' Returns a character vector of function names that use memoization.
#'
#' @return character vector of memoized function names
#' @export
#' @family cache management
#'
#' @examples
#' cache_list_functions()
cache_list_functions <- function() {
  .nbastatR_memoized_functions
}

#' Check if Function is Memoized
#'
#' Checks whether a given function uses memoization.
#'
#' @param function_name character name of the function to check
#'
#' @return logical TRUE if function is memoized
#' @export
#' @family cache management
#'
#' @examples
#' cache_is_memoized("nba_players")
cache_is_memoized <- function(function_name) {
  tryCatch({
    fn <- get(function_name, envir = asNamespace("nbastatR"))
    memoise::is.memoised(fn)
  }, error = function(e) {
    FALSE
  })
}
