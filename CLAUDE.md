# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

nbastatR is an R package that provides a unified interface to NBA data from multiple sources:
- **NBA Stats API** (stats.nba.com) - Primary source for official statistics
- **Basketball-Reference.com** - Historical data, awards, rankings
- **HoopsHype** - Salary information
- **RealGM** - Transactions, rosters
- **nbadraft.net** - Mock drafts
- **Basketball Insiders** - Salary cap data

## Development Commands

```r
# Generate documentation and NAMESPACE from Roxygen2 comments
devtools::document()

# Build the package
devtools::build()

# Run R CMD check
devtools::check()

# Install locally
devtools::install()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-nbastatR.R")
```

## Code Architecture

### File Organization by Data Source

| Data Source | Primary File(s) | Functions |
|-------------|-----------------|-----------|
| **NBA Stats API** | `new_stats.R` (18k lines), `game_logs.R`, `box_scores.R`, `play_by_play.R`, `standings.R` | `game_logs()`, `box_scores()`, `nba_players()`, `nba_teams()` |
| **Basketball-Reference** | `bref.R`, `bref_bio.R` | `bref_players_stats()`, `bref_awards()`, `bref_bios()` |
| **HoopsHype** | `hoops_hype_functions.R` | `hoopshype_salaries()` |
| **RealGM** | `real_gm.R` | `transactions()` |
| **nbadraft.net** | `mockdrafts.R` | `nbadraftnet_mock_drafts()` |
| **Basketball Insiders** | `basketball_insider_functions.R` | `nba_insider_salaries()` |

### Core Infrastructure Files

| File | Purpose |
|------|---------|
| `R/xis_ecmo.R` | `.curl_chinazi()` - Core HTTP wrapper for NBA Stats API with required headers |
| `R/utils.R` | `make_url()` for API URL construction, data transformation utilities |
| `R/resolver.R` | Player name resolution across data sources |
| `R/new_ish.R` | Dictionary functions, team/player ID lookups |

### NBA Stats API Pattern

All NBA Stats API calls go through `.curl_chinazi()` in `xis_ecmo.R`. This function:
1. Sets required HTTP headers (User-Agent, x-nba-stats-token, x-nba-stats-origin)
2. Makes GET request via httr
3. Parses JSON response

Example flow:
```r
# game_logs.R
url <- glue("https://stats.nba.com/stats/leaguegamelog?...")
json <- .curl_chinazi(url = url)
data <- json %>% nba_json_to_df()
```

### Key Patterns

**Memoization**: Most API functions use `memoise()` to cache responses:
```r
function_name <- memoise::memoise(function(...) { ... })
```

**Parallel Processing**: Functions support `future`/`furrr` for batch operations:
```r
library(future)
plan(multisession)
game_logs(seasons = 2010:2019)  # Runs in parallel
```

**Internal Functions**: Prefixed with `.` (e.g., `.curl_chinazi()`, `.get_current_season()`)

**Name Resolution**: Player names differ across sources. `resolver.R` and `.resolve_bref_players()` in `bref.R` handle mapping (150+ hardcoded name corrections).

### Naming Conventions

**Functions:**
- Exported: `snake_case` (e.g., `game_logs()`, `box_scores()`)
- Internal: `.dot_prefix` (e.g., `.get_current_season()`)
- Section headers: `# section_name ---`

**Column Prefixes:**
| Prefix | Meaning | Examples |
|--------|---------|----------|
| `id*` | Numeric identifier | `idPlayer`, `idTeam`, `idGame` |
| `name*` | Display name | `namePlayer`, `nameTeam` |
| `slug*` | Short code | `slugTeam` (e.g., "LAL"), `slugSeason` (e.g., "2019-20") |
| `type*` | Category/type | `typeSeason`, `typeResult` |
| `date*` | Date field | `dateGame` |
| `url*` | URL reference | `urlPlayerThumbnail`, `urlTeamSeasonLogo` |
| `is*` | Boolean flag | `isActive`, `isWin` |
| `count*` | Numeric count | `countSeasons` |

## Testing

Tests are in `tests/testthat/`. Current coverage is minimal. Tests make live API calls, so they require network access.

## API Notes

The NBA Stats API requires specific headers. The package handles this via `.curl_chinazi()` in `xis_ecmo.R`. If API calls fail, check if endpoints have changed - the NBA occasionally updates their API structure.

## Navigating new_stats.R

This 18,685-line file contains mostly static data (player photo dictionaries, etc.) and helper functions. Key exports are near the end:
- `nba_teams()` - line ~18401
- `nba_teams_seasons()` - line ~18435
- `nba_stats_api_items()` - line ~18463
- `nba_players()` - line ~18505

Use grep to find specific functions: `grep -n "^function_name" R/new_stats.R`

## Large File Handling - MANDATORY

**NEVER read large files directly.** Use `tools/sheldon-toolkit/` scripts to inspect, sample, or extract what you need.

| Need | Command |
|------|---------|
| Understand structure | `python ~/.claude/tools/sheldon-toolkit/inspector.py <file>` |
| Sample rows | `python ~/.claude/tools/sheldon-toolkit/sampler.py <file> --count 10` |
| Get slice | `python ~/.claude/tools/sheldon-toolkit/slicer.py <file> --start 0 --end 100` |
| Infer schema | `python ~/.claude/tools/sheldon-toolkit/structurer.py <file>` |
| Filter rows | `python ~/.claude/tools/sheldon-toolkit/filterer.py <file> --field X --value Y` |

**Thresholds:** Code >300 lines, Data >100 rows = use toolkit first.
