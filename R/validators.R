# validators.R
# API Response Validators for nbastatR
# Detect schema changes and API breakages before users encounter them

# ============================================================================
# CORE VALIDATION FRAMEWORK
# ============================================================================

#' Validate API Response Structure
#'
#' Core validation function that checks if a response has expected structure.
#'
#' @param data the data frame/tibble to validate
#' @param required_cols character vector of required column names
#' @param source character description of the data source
#' @param warn_missing if TRUE, warn instead of error on missing columns
#'
#' @return the original data (invisibly) if valid, otherwise stops with error
#' @keywords internal
.validate_response <- function(data, required_cols, source = "API",
                                warn_missing = FALSE) {
  if (!is.data.frame(data)) {
    stop(
      glue::glue("
{source} returned unexpected data type.
Expected: data.frame or tibble
Received: {class(data)[1]}

This may indicate an API change. Please report at:
https://github.com/abresler/nbastatR/issues
"),
      call. = FALSE
    )
  }

  if (nrow(data) == 0) {
    warning(
      glue::glue("{source} returned empty data (0 rows)."),
      call. = FALSE
    )
    return(invisible(data))
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    msg <- glue::glue("
{source} response is missing expected columns:
  Missing: {paste(missing_cols, collapse = ', ')}

This may indicate an API schema change.
Current columns: {paste(head(names(data), 10), collapse = ', ')}...

Please report at: https://github.com/abresler/nbastatR/issues
")

    if (warn_missing) {
      warning(msg, call. = FALSE)
    } else {
      stop(msg, call. = FALSE)
    }
  }

  invisible(data)
}

# ============================================================================
# ENDPOINT-SPECIFIC VALIDATORS
# ============================================================================

#' Validate NBA Players Response
#'
#' Validates the response from nba_players() has expected structure.
#'
#' @param data tibble returned from nba_players()
#' @param warn_only if TRUE, warn instead of error
#'
#' @return validated data (invisibly)
#' @export
#' @family validators
#'
#' @examples
#' \dontrun{
#' players <- nba_players()
#' validate_nba_players(players)
#' }
validate_nba_players <- function(data, warn_only = FALSE) {
  required <- c(
    "idPlayer", "namePlayer", "isActive"
  )

  .validate_response(
    data = data,
    required_cols = required,
    source = "nba_players()",
    warn_missing = warn_only
  )
}

#' Validate NBA Teams Response
#'
#' Validates the response from nba_teams() has expected structure.
#'
#' @param data tibble returned from nba_teams()
#' @param warn_only if TRUE, warn instead of error
#'
#' @return validated data (invisibly)
#' @export
#' @family validators
validate_nba_teams <- function(data, warn_only = FALSE) {
  required <- c(
    "idTeam", "slugTeam", "cityTeam", "teamName"
  )

  .validate_response(
    data = data,
    required_cols = required,
    source = "nba_teams()",
    warn_missing = warn_only
  )
}

#' Validate Game Logs Response
#'
#' Validates the response from game_logs() has expected structure.
#'
#' @param data tibble returned from game_logs()
#' @param warn_only if TRUE, warn instead of error
#'
#' @return validated data (invisibly)
#' @export
#' @family validators
validate_game_logs <- function(data, warn_only = FALSE) {
  required <- c(
    "slugSeason", "typeSeason", "yearSeason"
  )

  .validate_response(
    data = data,
    required_cols = required,
    source = "game_logs()",
    warn_missing = warn_only
  )
}

#' Validate Box Scores Response
#'
#' Validates the response from box_scores() has expected structure.
#'
#' @param data tibble returned from box_scores()
#' @param warn_only if TRUE, warn instead of error
#'
#' @return validated data (invisibly)
#' @export
#' @family validators
validate_box_scores <- function(data, warn_only = FALSE) {
  required <- c(
    "typeBoxScore", "typeResult", "idGame"
  )

  .validate_response(
    data = data,
    required_cols = required,
    source = "box_scores()",
    warn_missing = warn_only
  )
}

# ============================================================================
# API HEALTH CHECK
# ============================================================================

#' Run API Health Check
#'
#' Tests connectivity and response validity for key nbastatR endpoints.
#' Useful for diagnosing issues and monitoring API status.
#'
#' @param endpoints character vector of endpoints to check. Default is
#'   c("players", "teams"). Options: "players", "teams", "game_logs"
#' @param verbose if TRUE, print progress messages
#'
#' @return tibble with health check results
#' @export
#' @family validators
#'
#' @examples
#' \dontrun{
#' # Check all endpoints
#' api_health_check()
#'
#' # Check specific endpoints
#' api_health_check(endpoints = c("players", "teams"))
#' }
api_health_check <- function(endpoints = c("players", "teams"),
                              verbose = TRUE) {
  results <- list()

  for (endpoint in endpoints) {
    if (verbose) cat("Checking", endpoint, "...\n")

    result <- tryCatch({
      start_time <- Sys.time()

      data <- switch(endpoint,
        "players" = {
          d <- nba_players()
          validate_nba_players(d, warn_only = TRUE)
          d
        },
        "teams" = {
          d <- nba_teams()
          validate_nba_teams(d, warn_only = TRUE)
          d
        },
        "game_logs" = {
          d <- game_logs(seasons = 2024, return_message = FALSE)
          validate_game_logs(d, warn_only = TRUE)
          d
        },
        {
          warning("Unknown endpoint: ", endpoint)
          NULL
        }
      )

      end_time <- Sys.time()

      list(
        endpoint = endpoint,
        status = "OK",
        rows = if (!is.null(data)) nrow(data) else NA,
        cols = if (!is.null(data)) ncol(data) else NA,
        response_time_sec = as.numeric(difftime(end_time, start_time, units = "secs")),
        error = NA_character_,
        checked_at = Sys.time()
      )

    }, error = function(e) {
      list(
        endpoint = endpoint,
        status = "ERROR",
        rows = NA,
        cols = NA,
        response_time_sec = NA,
        error = conditionMessage(e),
        checked_at = Sys.time()
      )
    })

    results[[endpoint]] <- result
  }

  # Combine results
  result_df <- dplyr::bind_rows(results)

  if (verbose) {
    cat("\n API Health Check Summary\n")
    cat("========================\n")
    for (i in seq_len(nrow(result_df))) {
      row <- result_df[i, ]
      status_icon <- if (row$status == "OK") "\u2705" else "\u274C"
      cat(status_icon, row$endpoint, "-", row$status)
      if (!is.na(row$rows)) {
        cat(" (", row$rows, " rows, ", round(row$response_time_sec, 2), "s)\n", sep = "")
      } else {
        cat("\n")
      }
    }
  }

  invisible(result_df)
}

# ============================================================================
# VALIDATION HELPERS
# ============================================================================

#' Check Data Types in Response
#'
#' Validates that columns have expected data types.
#'
#' @param data the data frame to check
#' @param type_spec named list of expected types (column = type)
#' @param source description of data source for error messages
#'
#' @return TRUE if all types match, otherwise stops with error
#' @keywords internal
.validate_types <- function(data, type_spec, source = "API") {
  mismatches <- character()

  for (col in names(type_spec)) {
    if (col %in% names(data)) {
      expected_type <- type_spec[[col]]
      actual_type <- typeof(data[[col]])

      # Handle numeric types flexibly
      if (expected_type == "numeric") {
        if (!is.numeric(data[[col]])) {
          mismatches <- c(mismatches,
            glue::glue("{col}: expected numeric, got {actual_type}"))
        }
      } else if (actual_type != expected_type) {
        mismatches <- c(mismatches,
          glue::glue("{col}: expected {expected_type}, got {actual_type}"))
      }
    }
  }

  if (length(mismatches) > 0) {
    warning(
      glue::glue("
{source} has unexpected column types:
{paste(mismatches, collapse = '\n')}
"),
      call. = FALSE
    )
  }

  invisible(length(mismatches) == 0)
}
