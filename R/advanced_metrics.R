# advanced_metrics.R
# NBA Advanced Analytics Metrics Calculator
# Formulas sourced from Basketball-Reference.com
# https://www.basketball-reference.com/about/glossary.html

# ============================================================================
# SHOOTING EFFICIENCY METRICS
# ============================================================================

#' Calculate True Shooting Percentage
#'
#' True Shooting Percentage (TS%) measures scoring efficiency by accounting
#' for field goals, 3-pointers, and free throws.
#'
#' Formula: PTS / (2 * (FGA + 0.44 * FTA))
#' Source: Basketball-Reference
#'
#' @param pts points scored
#' @param fga field goal attempts
#' @param fta free throw attempts
#'
#' @return numeric vector of TS% values (0-1 scale)
#' @export
#' @family advanced metrics
#'
#' @examples
#' # A player with 30 pts on 20 FGA and 10 FTA
#' calculate_ts_pct(pts = 30, fga = 20, fta = 10)
calculate_ts_pct <- function(pts, fga, fta) {
  tsa <- fga + 0.44 * fta
  ifelse(tsa == 0, NA_real_, pts / (2 * tsa))
}

#' Calculate Effective Field Goal Percentage
#'
#' Effective Field Goal Percentage (eFG%) adjusts field goal percentage
#' to account for three-pointers being worth more than two-pointers.
#'
#' Formula: (FGM + 0.5 * FG3M) / FGA
#' Source: Basketball-Reference
#'
#' @param fgm field goals made
#' @param fg3m three-point field goals made
#' @param fga field goal attempts
#'
#' @return numeric vector of eFG% values (0-1 scale)
#' @export
#' @family advanced metrics
#'
#' @examples
#' # A player with 10 FGM (3 threes) on 20 FGA
#' calculate_efg_pct(fgm = 10, fg3m = 3, fga = 20)
calculate_efg_pct <- function(fgm, fg3m, fga) {
  ifelse(fga == 0, NA_real_, (fgm + 0.5 * fg3m) / fga)
}

# ============================================================================
# USAGE AND PACE METRICS
# ============================================================================
#' Calculate Usage Rate
#'
#' Usage Rate estimates the percentage of team plays used by a player
#' while on the floor.
#'
#' Formula: 100 * ((FGA + 0.44 * FTA + TOV) * (Tm MP / 5)) /
#'          (MP * (Tm FGA + 0.44 * Tm FTA + Tm TOV))
#' Source: Basketball-Reference
#'
#' @param fga player field goal attempts
#' @param fta player free throw attempts
#' @param tov player turnovers
#' @param mp player minutes played
#' @param tm_fga team field goal attempts
#' @param tm_fta team free throw attempts
#' @param tm_tov team turnovers
#' @param tm_mp team total minutes (usually 240 for regulation)
#'
#' @return numeric vector of usage rate values (0-100 scale)
#' @export
#' @family advanced metrics
#'
#' @examples
#' calculate_usage_rate(fga = 20, fta = 8, tov = 3, mp = 36,
#'                      tm_fga = 85, tm_fta = 25, tm_tov = 12, tm_mp = 240)
calculate_usage_rate <- function(fga, fta, tov, mp,
                                  tm_fga, tm_fta, tm_tov, tm_mp = 240) {
  player_poss <- fga + 0.44 * fta + tov
  team_poss <- tm_fga + 0.44 * tm_fta + tm_tov

  ifelse(mp == 0 | team_poss == 0, NA_real_,
         100 * (player_poss * (tm_mp / 5)) / (mp * team_poss))
}

#' Calculate Pace (Possessions per 48 minutes)
#'
#' Estimates the number of possessions per 48 minutes for a team.
#'
#' Formula: 48 * ((Tm Poss + Opp Poss) / (2 * (Tm MP / 5)))
#' Possessions = FGA + 0.44 * FTA - ORB + TOV
#'
#' @param tm_fga team field goal attempts
#' @param tm_fta team free throw attempts
#' @param tm_orb team offensive rebounds
#' @param tm_tov team turnovers
#' @param opp_fga opponent field goal attempts
#' @param opp_fta opponent free throw attempts
#' @param opp_orb opponent offensive rebounds
#' @param opp_tov opponent turnovers
#' @param tm_mp team minutes played (default 240)
#'
#' @return numeric pace value
#' @export
#' @family advanced metrics
calculate_pace <- function(tm_fga, tm_fta, tm_orb, tm_tov,
                           opp_fga, opp_fta, opp_orb, opp_tov,
                           tm_mp = 240) {
  tm_poss <- tm_fga + 0.44 * tm_fta - tm_orb + tm_tov
  opp_poss <- opp_fga + 0.44 * opp_fta - opp_orb + opp_tov

  48 * ((tm_poss + opp_poss) / (2 * (tm_mp / 5)))
}

# ============================================================================
# REBOUNDING METRICS
# ============================================================================

#' Calculate Offensive Rebound Percentage
#'
#' Estimates the percentage of available offensive rebounds obtained.
#'
#' Formula: 100 * (ORB * (Tm MP / 5)) / (MP * (Tm ORB + Opp DRB))
#' Source: Basketball-Reference
#'
#' @param orb player offensive rebounds
#' @param mp player minutes played
#' @param tm_orb team offensive rebounds
#' @param opp_drb opponent defensive rebounds
#' @param tm_mp team minutes (default 240)
#'
#' @return numeric ORB% value
#' @export
#' @family advanced metrics
calculate_orb_pct <- function(orb, mp, tm_orb, opp_drb, tm_mp = 240) {
  ifelse(mp == 0 | (tm_orb + opp_drb) == 0, NA_real_,
         100 * (orb * (tm_mp / 5)) / (mp * (tm_orb + opp_drb)))
}

#' Calculate Defensive Rebound Percentage
#'
#' Estimates the percentage of available defensive rebounds obtained.
#'
#' Formula: 100 * (DRB * (Tm MP / 5)) / (MP * (Tm DRB + Opp ORB))
#' Source: Basketball-Reference
#'
#' @param drb player defensive rebounds
#' @param mp player minutes played
#' @param tm_drb team defensive rebounds
#' @param opp_orb opponent offensive rebounds
#' @param tm_mp team minutes (default 240)
#'
#' @return numeric DRB% value
#' @export
#' @family advanced metrics
calculate_drb_pct <- function(drb, mp, tm_drb, opp_orb, tm_mp = 240) {
  ifelse(mp == 0 | (tm_drb + opp_orb) == 0, NA_real_,
         100 * (drb * (tm_mp / 5)) / (mp * (tm_drb + opp_orb)))
}

#' Calculate Total Rebound Percentage
#'
#' Estimates the percentage of available rebounds obtained.
#'
#' Formula: 100 * (TRB * (Tm MP / 5)) / (MP * (Tm TRB + Opp TRB))
#' Source: Basketball-Reference
#'
#' @param trb player total rebounds
#' @param mp player minutes played
#' @param tm_trb team total rebounds
#' @param opp_trb opponent total rebounds
#' @param tm_mp team minutes (default 240)
#'
#' @return numeric TRB% value
#' @export
#' @family advanced metrics
calculate_trb_pct <- function(trb, mp, tm_trb, opp_trb, tm_mp = 240) {
  ifelse(mp == 0 | (tm_trb + opp_trb) == 0, NA_real_,
         100 * (trb * (tm_mp / 5)) / (mp * (tm_trb + opp_trb)))
}

# ============================================================================
# FOUR FACTORS (Dean Oliver)
# ============================================================================

#' Calculate Four Factors
#'
#' Dean Oliver's Four Factors identify the key components of team success:
#' 1. Effective FG% (shooting)
#' 2. Turnover Rate (ball security)
#' 3. Offensive Rebound Rate (second chances)
#' 4. Free Throw Rate (getting to the line)
#'
#' @param fgm field goals made
#' @param fg3m three-point field goals made
#' @param fga field goal attempts
#' @param tov turnovers
#' @param orb offensive rebounds
#' @param opp_drb opponent defensive rebounds
#' @param ftm free throws made
#' @param fta free throw attempts
#'
#' @return tibble with four factors
#' @export
#' @family advanced metrics
#'
#' @examples
#' calculate_four_factors(fgm = 40, fg3m = 10, fga = 85,
#'                        tov = 12, orb = 10, opp_drb = 35,
#'                        ftm = 18, fta = 22)
calculate_four_factors <- function(fgm, fg3m, fga, tov, orb, opp_drb, ftm, fta) {

  # Effective FG%
  efg_pct <- calculate_efg_pct(fgm, fg3m, fga)

  # Turnover Rate: TOV / (FGA + 0.44 * FTA + TOV)
  poss <- fga + 0.44 * fta + tov
  tov_rate <- ifelse(poss == 0, NA_real_, tov / poss)

  # Offensive Rebound Rate: ORB / (ORB + Opp DRB)
  orb_rate <- ifelse((orb + opp_drb) == 0, NA_real_, orb / (orb + opp_drb))

  # Free Throw Rate: FTM / FGA (or FTA / FGA depending on definition)
  ft_rate <- ifelse(fga == 0, NA_real_, ftm / fga)

dplyr::tibble(
    pctEFG = efg_pct,
    rateTOV = tov_rate,
    pctORB = orb_rate,
    rateFT = ft_rate
  )
}

# ============================================================================
# OFFENSIVE/DEFENSIVE RATINGS
# ============================================================================

#' Calculate Offensive Rating
#'
#' Points produced per 100 possessions.
#'
#' @param pts points scored
#' @param poss possessions
#'
#' @return numeric offensive rating
#' @export
#' @family advanced metrics
calculate_ortg <- function(pts, poss) {
  ifelse(poss == 0, NA_real_, 100 * pts / poss)
}

#' Calculate Defensive Rating
#'
#' Points allowed per 100 possessions.
#'
#' @param pts_allowed points allowed
#' @param poss possessions
#'
#' @return numeric defensive rating
#' @export
#' @family advanced metrics
calculate_drtg <- function(pts_allowed, poss) {
  ifelse(poss == 0, NA_real_, 100 * pts_allowed / poss)
}

#' Calculate Net Rating
#'
#' Point differential per 100 possessions (ORTG - DRTG).
#'
#' @param pts points scored
#' @param pts_allowed points allowed
#' @param poss possessions
#'
#' @return numeric net rating
#' @export
#' @family advanced metrics
calculate_net_rating <- function(pts, pts_allowed, poss) {
  ortg <- calculate_ortg(pts, poss)
  drtg <- calculate_drtg(pts_allowed, poss)
  ortg - drtg
}

# ============================================================================
# ASSIST METRICS
# ============================================================================

#' Calculate Assist Percentage
#'
#' Estimates the percentage of teammate field goals assisted while on floor.
#'
#' Formula: 100 * AST / (((MP / (Tm MP / 5)) * Tm FGM) - FGM)
#' Source: Basketball-Reference
#'
#' @param ast player assists
#' @param mp player minutes played
#' @param fgm player field goals made
#' @param tm_fgm team field goals made
#' @param tm_mp team minutes (default 240)
#'
#' @return numeric AST% value
#' @export
#' @family advanced metrics
calculate_ast_pct <- function(ast, mp, fgm, tm_fgm, tm_mp = 240) {
  teammate_fgm <- ((mp / (tm_mp / 5)) * tm_fgm) - fgm
  ifelse(mp == 0 | teammate_fgm <= 0, NA_real_,
         100 * ast / teammate_fgm)
}

#' Calculate Assist to Turnover Ratio
#'
#' Simple ratio of assists to turnovers.
#'
#' @param ast assists
#' @param tov turnovers
#'
#' @return numeric AST/TOV ratio
#' @export
#' @family advanced metrics
calculate_ast_tov_ratio <- function(ast, tov) {
  ifelse(tov == 0, NA_real_, ast / tov)
}

# ============================================================================
# STEAL/BLOCK METRICS
# ============================================================================

#' Calculate Steal Percentage
#'
#' Estimates the percentage of opponent possessions ending in a steal.
#'
#' Formula: 100 * (STL * (Tm MP / 5)) / (MP * Opp Poss)
#' Source: Basketball-Reference
#'
#' @param stl player steals
#' @param mp player minutes played
#' @param opp_poss opponent possessions
#' @param tm_mp team minutes (default 240)
#'
#' @return numeric STL% value
#' @export
#' @family advanced metrics
calculate_stl_pct <- function(stl, mp, opp_poss, tm_mp = 240) {
  ifelse(mp == 0 | opp_poss == 0, NA_real_,
         100 * (stl * (tm_mp / 5)) / (mp * opp_poss))
}

#' Calculate Block Percentage
#'
#' Estimates the percentage of opponent 2PA blocked.
#'
#' Formula: 100 * (BLK * (Tm MP / 5)) / (MP * (Opp FGA - Opp 3PA))
#' Source: Basketball-Reference
#'
#' @param blk player blocks
#' @param mp player minutes played
#' @param opp_fga opponent field goal attempts
#' @param opp_fg3a opponent three-point attempts
#' @param tm_mp team minutes (default 240)
#'
#' @return numeric BLK% value
#' @export
#' @family advanced metrics
calculate_blk_pct <- function(blk, mp, opp_fga, opp_fg3a, tm_mp = 240) {
  opp_2pa <- opp_fga - opp_fg3a
  ifelse(mp == 0 | opp_2pa <= 0, NA_real_,
         100 * (blk * (tm_mp / 5)) / (mp * opp_2pa))
}

# ============================================================================
# GAME SCORE (John Hollinger)
# ============================================================================

#' Calculate Game Score
#'
#' John Hollinger's Game Score metric provides a quick measure of
#' a player's productivity in a single game.
#'
#' Formula: PTS + 0.4*FGM - 0.7*FGA - 0.4*(FTA-FTM) + 0.7*ORB + 0.3*DRB +
#'          STL + 0.7*AST + 0.7*BLK - 0.4*PF - TOV
#' Source: Basketball-Reference
#'
#' @param pts points
#' @param fgm field goals made
#' @param fga field goal attempts
#' @param ftm free throws made
#' @param fta free throw attempts
#' @param orb offensive rebounds
#' @param drb defensive rebounds
#' @param stl steals
#' @param ast assists
#' @param blk blocks
#' @param pf personal fouls
#' @param tov turnovers
#'
#' @return numeric game score value
#' @export
#' @family advanced metrics
#'
#' @examples
#' # Calculate game score for a typical stat line
#' calculate_game_score(pts = 25, fgm = 9, fga = 18, ftm = 5, fta = 6,
#'                      orb = 1, drb = 5, stl = 2, ast = 7, blk = 1,
#'                      pf = 2, tov = 3)
calculate_game_score <- function(pts, fgm, fga, ftm, fta,
                                  orb, drb, stl, ast, blk, pf, tov) {
  pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) +
    0.7 * orb + 0.3 * drb + stl + 0.7 * ast + 0.7 * blk -
    0.4 * pf - tov
}

# ============================================================================
# WRAPPER FUNCTION
# ============================================================================

#' Enrich Box Score Data with Advanced Metrics
#'
#' Takes a tibble of box score data and adds calculated advanced metrics.
#' Automatically detects available columns and calculates applicable metrics.
#'
#' @param data a tibble containing box score data with standard column names
#' @param metrics character vector of metrics to calculate, or "all" for all available.
#'   Options: "ts_pct", "efg_pct", "game_score", "ast_tov_ratio"
#'
#' @return tibble with additional columns for calculated metrics
#' @export
#' @family advanced metrics
#'
#' @examples
#' \dontrun{
#' # Get game logs and enrich with advanced metrics
#' logs <- game_logs(seasons = 2024)
#' enriched <- enrich_with_advanced_metrics(logs)
#' }
enrich_with_advanced_metrics <- function(data, metrics = "all") {

  if (!"data.frame" %in% class(data)) {
    stop("Input must be a data frame or tibble")
  }

  # Column name mapping (nbastatR uses these names)
  has_col <- function(col) col %in% names(data)

  # True Shooting %
  if (metrics == "all" || "ts_pct" %in% metrics) {
    if (has_col("pts") && has_col("fga") && has_col("fta")) {
      data <- data %>%
        dplyr::mutate(pctTS = calculate_ts_pct(pts, fga, fta))
    }
  }

  # Effective FG%
  if (metrics == "all" || "efg_pct" %in% metrics) {
    if (has_col("fgm") && has_col("fg3m") && has_col("fga")) {
      data <- data %>%
        dplyr::mutate(pctEFG = calculate_efg_pct(fgm, fg3m, fga))
    }
  }

  # Game Score
  if (metrics == "all" || "game_score" %in% metrics) {
    required <- c("pts", "fgm", "fga", "ftm", "fta", "oreb", "dreb",
                  "stl", "ast", "blk", "pf", "tov")
    if (all(sapply(required, has_col))) {
      data <- data %>%
        dplyr::mutate(gameScore = calculate_game_score(
          pts, fgm, fga, ftm, fta, oreb, dreb, stl, ast, blk, pf, tov
        ))
    }
  }

  # AST/TOV Ratio
  if (metrics == "all" || "ast_tov_ratio" %in% metrics) {
    if (has_col("ast") && has_col("tov")) {
      data <- data %>%
        dplyr::mutate(ratioASTTOV = calculate_ast_tov_ratio(ast, tov))
    }
  }

  data
}
