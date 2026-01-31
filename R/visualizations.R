# visualizations.R
# Quick Visualization Functions for nbastatR
# Provides simple ggplot2-based plots for common NBA statistics

# ============================================================================
# SHOT CHART VISUALIZATION
# ============================================================================

#' Plot Shot Chart
#'
#' Creates a shot chart visualization showing shot locations and outcomes.
#'
#' @param data tibble with shot location data (requires locX, locY, isShotMade)
#' @param court_theme character; either "light" or "dark" court background
#' @param point_alpha numeric; transparency of shot points (0-1)
#' @param title character; optional plot title
#'
#' @return a ggplot2 object
#' @export
#' @family visualizations
#'
#' @examples
#' \dontrun{
#' shots <- shots_player(player_id = 201566, season = "2023-24")
#' plot_shot_chart(shots)
#' plot_shot_chart(shots, court_theme = "dark")
#' }
plot_shot_chart <- function(data,
                            court_theme = c("light", "dark"),
                            point_alpha = 0.6,
                            title = NULL) {
  court_theme <- match.arg(court_theme)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations.", call. = FALSE)
  }

  required_cols <- c("locX", "locY", "isShotMade")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(
      glue::glue("Data missing required columns: {paste(missing, collapse = ', ')}"),
      call. = FALSE
    )
  }

  # Court colors
  if (court_theme == "light") {
    court_bg <- "#FFFAF0"
    court_lines <- "#333333"
    made_color <- "#228B22"
    missed_color <- "#DC143C"
  } else {
    court_bg <- "#1a1a1a"
    court_lines <- "#cccccc"
    made_color <- "#00ff7f"
    missed_color <- "#ff6b6b"
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$locX, y = .data$locY)) +
    # Court outline (simplified)
    ggplot2::annotate("rect", xmin = -250, xmax = 250, ymin = -47, ymax = 422,
                      fill = NA, color = court_lines, linewidth = 0.5) +
    # Three point line (simplified arc)
    ggplot2::annotate("path",
                      x = 220 * cos(seq(0, pi, length.out = 100)),
                      y = 220 * sin(seq(0, pi, length.out = 100)) - 47 + 140,
                      color = court_lines, linewidth = 0.5) +
    # Paint area
    ggplot2::annotate("rect", xmin = -80, xmax = 80, ymin = -47, ymax = 143,
                      fill = NA, color = court_lines, linewidth = 0.5) +
    # Shots
    ggplot2::geom_point(
      ggplot2::aes(color = .data$isShotMade),
      alpha = point_alpha,
      size = 2
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = made_color, "FALSE" = missed_color),
      labels = c("TRUE" = "Made", "FALSE" = "Missed"),
      name = "Shot Result"
    ) +
    ggplot2::coord_fixed(xlim = c(-250, 250), ylim = c(-47, 400)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = court_bg, color = NA),
      plot.background = ggplot2::element_rect(fill = court_bg, color = NA),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(title = title)

  p
}

# ============================================================================
# PLAYER COMPARISON VISUALIZATION
# ============================================================================

#' Plot Player Comparison Radar
#'
#' Creates a radar/spider chart comparing player statistics.
#'
#' @param data tibble with player stats (one row per player, numeric columns)
#' @param player_col character; name of column containing player names
#' @param stat_cols character vector; columns to include in comparison
#' @param normalize logical; if TRUE, normalize stats to 0-1 scale
#' @param title character; optional plot title
#'
#' @return a ggplot2 object
#' @export
#' @family visualizations
#'
#' @examples
#' \dontrun{
#' stats <- player_career_stats(players = c("LeBron James", "Kevin Durant"))
#' plot_player_comparison(
#'   stats,
#'   player_col = "namePlayer",
#'   stat_cols = c("pts", "ast", "reb", "stl", "blk")
#' )
#' }
plot_player_comparison <- function(data,
                                   player_col = "namePlayer",
                                   stat_cols,
                                   normalize = TRUE,
                                   title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations.", call. = FALSE)
  }

  if (nrow(data) < 2) {
    stop("Need at least 2 players for comparison.", call. = FALSE)
  }

  if (nrow(data) > 6) {
    warning("More than 6 players may result in cluttered visualization.",
            call. = FALSE)
  }

  # Validate columns
  missing_stats <- setdiff(stat_cols, names(data))
  if (length(missing_stats) > 0) {
    stop(
      glue::glue("Missing stat columns: {paste(missing_stats, collapse = ', ')}"),
      call. = FALSE
    )
  }

  # Prepare data for radar chart
  plot_data <- data[, c(player_col, stat_cols), drop = FALSE]

  # Normalize if requested
  if (normalize) {
    for (col in stat_cols) {
      vals <- plot_data[[col]]
      min_val <- min(vals, na.rm = TRUE)
      max_val <- max(vals, na.rm = TRUE)
      if (max_val > min_val) {
        plot_data[[col]] <- (vals - min_val) / (max_val - min_val)
      } else {
        plot_data[[col]] <- 0.5
      }
    }
  }

  # Reshape for ggplot
  plot_data_long <- tidyr::pivot_longer(
    plot_data,
    cols = tidyr::all_of(stat_cols),
    names_to = "stat",
    values_to = "value"
  )

  # Calculate angles for radar
  n_stats <- length(stat_cols)
  angles <- seq(0, 2 * pi, length.out = n_stats + 1)[1:n_stats]
  angle_df <- tibble::tibble(stat = stat_cols, angle = angles)
  plot_data_long <- dplyr::left_join(plot_data_long, angle_df, by = "stat")

  # Calculate x, y coordinates
  plot_data_long <- plot_data_long %>%
    dplyr::mutate(
      x = .data$value * cos(.data$angle),
      y = .data$value * sin(.data$angle)
    )

  # Colors for players
  n_players <- nrow(data)
  player_colors <- scales::hue_pal()(n_players)

  p <- ggplot2::ggplot(plot_data_long) +
    # Grid circles
    ggplot2::geom_polygon(
      data = data.frame(
        x = 0.5 * cos(seq(0, 2*pi, length.out = 100)),
        y = 0.5 * sin(seq(0, 2*pi, length.out = 100))
      ),
      ggplot2::aes(x = .data$x, y = .data$y),
      fill = NA, color = "gray80", linetype = "dashed"
    ) +
    ggplot2::geom_polygon(
      data = data.frame(
        x = cos(seq(0, 2*pi, length.out = 100)),
        y = sin(seq(0, 2*pi, length.out = 100))
      ),
      ggplot2::aes(x = .data$x, y = .data$y),
      fill = NA, color = "gray60"
    ) +
    # Spokes
    ggplot2::geom_segment(
      data = tibble::tibble(
        x = 0, y = 0,
        xend = cos(angles),
        yend = sin(angles)
      ),
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      color = "gray80"
    ) +
    # Player polygons
    ggplot2::geom_polygon(
      ggplot2::aes(
        x = .data$x, y = .data$y,
        group = .data[[player_col]],
        fill = .data[[player_col]],
        color = .data[[player_col]]
      ),
      alpha = 0.2, linewidth = 1
    ) +
    # Points
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$x, y = .data$y,
        color = .data[[player_col]]
      ),
      size = 3
    ) +
    # Stat labels
    ggplot2::geom_text(
      data = tibble::tibble(
        stat = stat_cols,
        x = 1.15 * cos(angles),
        y = 1.15 * sin(angles)
      ),
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$stat),
      size = 3.5
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(
      title = title,
      fill = "Player",
      color = "Player"
    )

  p
}

# ============================================================================
# SCORING TREND VISUALIZATION
# ============================================================================

#' Plot Scoring Trend
#'
#' Creates a line chart showing scoring trends over time.
#'
#' @param data tibble with game log data (requires dateGame, pts)
#' @param rolling_window integer; window size for rolling average (NULL for no smoothing)
#' @param show_average logical; if TRUE, show season average line
#' @param title character; optional plot title
#'
#' @return a ggplot2 object
#' @export
#' @family visualizations
#'
#' @examples
#' \dontrun{
#' logs <- game_logs(seasons = 2024, result_types = "player")
#' player_logs <- logs[logs$namePlayer == "Jayson Tatum", ]
#' plot_scoring_trend(player_logs)
#' plot_scoring_trend(player_logs, rolling_window = 5)
#' }
plot_scoring_trend <- function(data,
                               rolling_window = NULL,
                               show_average = TRUE,
                               title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations.", call. = FALSE)
  }

  required_cols <- c("dateGame", "pts")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(
      glue::glue("Data missing required columns: {paste(missing, collapse = ', ')}"),
      call. = FALSE
    )
  }

  # Sort by date
  plot_data <- data %>%
    dplyr::arrange(.data$dateGame)

  # Add rolling average if requested
  if (!is.null(rolling_window) && rolling_window > 1) {
    plot_data <- plot_data %>%
      dplyr::mutate(
        pts_rolling = zoo::rollmean(.data$pts, k = rolling_window,
                                    fill = NA, align = "right")
      )
  }

  # Calculate season average
  season_avg <- mean(plot_data$pts, na.rm = TRUE)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$dateGame)) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$pts),
      alpha = 0.5, color = "#1f77b4", size = 2
    )

  # Add rolling average line if calculated

if (!is.null(rolling_window) && rolling_window > 1) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$pts_rolling),
        color = "#ff7f0e", linewidth = 1.2, na.rm = TRUE
      )
  } else {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$pts),
        color = "#1f77b4", alpha = 0.5
      )
  }

  # Add season average line
  if (show_average) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = season_avg,
        linetype = "dashed", color = "#d62728", linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text",
        x = max(plot_data$dateGame, na.rm = TRUE),
        y = season_avg + 1,
        label = paste0("Avg: ", round(season_avg, 1)),
        hjust = 1, size = 3.5, color = "#d62728"
      )
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = "Game Date",
      y = "Points"
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  p
}

# ============================================================================
# TEAM STANDINGS VISUALIZATION
# ============================================================================

#' Plot Team Standings
#'
#' Creates a horizontal bar chart showing team standings/records.
#'
#' @param data tibble with team data (requires nameTeam and a numeric stat column)
#' @param stat_col character; name of column to plot
#' @param fill_by character; optional column to color bars by (e.g., conference)
#' @param title character; optional plot title
#'
#' @return a ggplot2 object
#' @export
#' @family visualizations
#'
#' @examples
#' \dontrun{
#' standings <- standings(seasons = 2024)
#' plot_standings(standings, stat_col = "pctWins", title = "Win Percentage")
#' plot_standings(standings, stat_col = "wins", fill_by = "conference")
#' }
plot_standings <- function(data,
                           stat_col,
                           fill_by = NULL,
                           title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations.", call. = FALSE)
  }

  if (!"nameTeam" %in% names(data)) {
    stop("Data must contain 'nameTeam' column.", call. = FALSE)
  }

  if (!stat_col %in% names(data)) {
    stop(glue::glue("Column '{stat_col}' not found in data."), call. = FALSE)
  }

  # Sort by stat
  plot_data <- data %>%
    dplyr::arrange(dplyr::desc(.data[[stat_col]]))

  # Create ordered factor for teams
  plot_data$nameTeam <- factor(plot_data$nameTeam,
                               levels = rev(plot_data$nameTeam))

  if (!is.null(fill_by) && fill_by %in% names(data)) {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[[stat_col]],
        y = .data$nameTeam,
        fill = .data[[fill_by]]
      )
    )
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data[[stat_col]], y = .data$nameTeam)
    ) +
      ggplot2::geom_col(fill = "#1f77b4", alpha = 0.8)
  }

  if (!is.null(fill_by) && fill_by %in% names(data)) {
    p <- p + ggplot2::geom_col(alpha = 0.8)
  }

  p <- p +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data[[stat_col]], 2)),
      hjust = -0.1, size = 3
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = stat_col,
      y = NULL
    ) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))

  p
}

# ============================================================================
# EFFICIENCY SCATTER PLOT
# ============================================================================

#' Plot Efficiency Scatter
#'
#' Creates a scatter plot comparing two efficiency metrics with optional labels.
#'
#' @param data tibble with player/team data
#' @param x_col character; column name for x-axis
#' @param y_col character; column name for y-axis
#' @param label_col character; optional column for point labels
#' @param highlight character vector; names to highlight
#' @param title character; optional plot title
#'
#' @return a ggplot2 object
#' @export
#' @family visualizations
#'
#' @examples
#' \dontrun{
#' stats <- player_stats(seasons = 2024)
#' stats <- stats %>%
#'   mutate(ts_pct = calculate_ts_pct(pts, fga, fta))
#' plot_efficiency_scatter(
#'   stats,
#'   x_col = "usgPct",
#'   y_col = "ts_pct",
#'   label_col = "namePlayer",
#'   highlight = c("Nikola Jokic", "Luka Doncic")
#' )
#' }
plot_efficiency_scatter <- function(data,
                                    x_col,
                                    y_col,
                                    label_col = NULL,
                                    highlight = NULL,
                                    title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations.", call. = FALSE)
  }

  for (col in c(x_col, y_col)) {
    if (!col %in% names(data)) {
      stop(glue::glue("Column '{col}' not found in data."), call. = FALSE)
    }
  }

  # Add highlight flag
  if (!is.null(highlight) && !is.null(label_col)) {
    data <- data %>%
      dplyr::mutate(
        is_highlighted = .data[[label_col]] %in% highlight
      )
  } else {
    data$is_highlighted <- FALSE
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])
  )

  # Non-highlighted points
  p <- p +
    ggplot2::geom_point(
      data = data[!data$is_highlighted, ],
      alpha = 0.4, color = "gray50", size = 2
    )

  # Highlighted points
  if (any(data$is_highlighted)) {
    p <- p +
      ggplot2::geom_point(
        data = data[data$is_highlighted, ],
        color = "#d62728", size = 4
      )

    if (!is.null(label_col) && requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p +
        ggrepel::geom_text_repel(
          data = data[data$is_highlighted, ],
          ggplot2::aes(label = .data[[label_col]]),
          size = 3.5, color = "#d62728",
          box.padding = 0.5
        )
    } else if (!is.null(label_col)) {
      p <- p +
        ggplot2::geom_text(
          data = data[data$is_highlighted, ],
          ggplot2::aes(label = .data[[label_col]]),
          size = 3.5, color = "#d62728",
          vjust = -1
        )
    }
  }

  # Add quadrant lines at means
  x_mean <- mean(data[[x_col]], na.rm = TRUE)
  y_mean <- mean(data[[y_col]], na.rm = TRUE)

  p <- p +
    ggplot2::geom_vline(xintercept = x_mean, linetype = "dashed",
                        color = "gray70", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = y_mean, linetype = "dashed",
                        color = "gray70", linewidth = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = x_col,
      y = y_col
    )

  p
}
