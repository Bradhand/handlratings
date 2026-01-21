# Standalone script to generate table.html
# Run this with: Rscript generate_table.R

library(cfbfastR)
library(tidyverse)
library(DT)
library(htmlwidgets)

Sys.setenv(CFBD_API_KEY = "HSHzfpq6dCSMrlkihoPaMm2jZ2zlbUfIbdn1YHBRraJhMCWdYzxzoceQRG1DDRi/")

cat("Loading play-by-play data...\n")
df <- load_cfb_pbp(year = 2025, api_key = Sys.getenv("CFBD_API_KEY"))
logos <- load_cfb_teams(fbs_only = TRUE) %>%
  select(school, logo)

cat("Filtering garbage time plays...\n")
pbp <- df %>%
  filter(
    !(
      (period == 2 & (score_diff) >= 28) |
      (period == 3 & (score_diff) >= 28) |
      (period == 4 & (score_diff) >= 22) |
      (period == 4 & clock_minutes <= 3 & (score_diff) > 16) |
      is.na(EPA)
    )
  ) %>%
  filter(home_team_division == 'fbs' & away_team_division == 'fbs') %>%
  select('pos_team', 'def_pos_team', 'game_id', 'EPA')

cat("Calculating EPA with iterative opponent adjustment...\n")

# Start with raw EPA calculations
off <- pbp %>%
  group_by(pos_team) %>%
  summarize(off_epa = mean(EPA), off_plays = n())

def <- pbp %>%
  group_by(def_pos_team) %>%
  summarize(def_epa = mean(EPA), def_plays = n())

# Iterative opponent adjustment (5 iterations for convergence)
for (i in 1:5) {
  pbp_adj <- pbp %>%
    left_join(off %>% select(pos_team, off_epa), by = "pos_team") %>%
    left_join(def %>% select(def_pos_team, def_epa), by = "def_pos_team")

  league_avg <- mean(pbp$EPA, na.rm = TRUE)

  off <- pbp_adj %>%
    group_by(pos_team) %>%
    summarize(
      off_epa = mean(EPA - (def_epa - league_avg), na.rm = TRUE),
      off_plays = n()
    )

  def <- pbp_adj %>%
    group_by(def_pos_team) %>%
    summarize(
      def_epa = mean(EPA - (off_epa - league_avg), na.rm = TRUE),
      def_plays = n()
    )
}

off <- off %>% arrange(desc(off_epa))
def <- def %>% arrange(def_epa)

# Apply regression to the mean based on sample size
avg_plays <- mean(c(off$off_plays, def$def_plays))
regression_factor <- 0.3

off <- off %>%
  mutate(
    reliability = pmin(off_plays / avg_plays, 1),
    off_epa = off_epa * (reliability + (1 - reliability) * (1 - regression_factor))
  )

def <- def %>%
  mutate(
    reliability = pmin(def_plays / avg_plays, 1),
    def_epa = def_epa * (reliability + (1 - reliability) * (1 - regression_factor))
  )

cat("Creating HANDL ratings table...\n")

handl <- left_join(
  off %>% select(pos_team, off_epa),
  def %>% select(def_pos_team, def_epa),
  by = c("pos_team" = "def_pos_team")
)

handl <- handl %>%
  mutate(
    rating = round((off_epa - def_epa) * 100, 2),
    off_epa = round(off_epa * 100, 2),
    def_epa = round(def_epa * -100, 2)
  ) %>%
  arrange(desc(rating)) %>%
  select(pos_team, rating, off_epa, def_epa) %>%
  rename(
    Team = pos_team,
    'HANDL Rating' = rating,
    'Off Rating' = off_epa,
    'Def Rating' = def_epa
  )

handl <- handl %>%
  mutate('HANDL Rank' = row_number()) %>%
  arrange(desc(`Off Rating`)) %>% mutate('Off Rank' = row_number()) %>%
  arrange(desc(`Def Rating`)) %>% mutate('Def Rank' = row_number()) %>%
  left_join(logos, by = c("Team" = "school")) %>%
  select(logo, Team, `HANDL Rating`, `HANDL Rank`, `Off Rating`, `Off Rank`, `Def Rating`, `Def Rank`)

handl$logo <- paste0('<img src="', handl$logo, '" height="40">')
handl <- handl %>% arrange(desc(`HANDL Rating`)) %>% rename(" " = logo)

cat("Generating HTML table...\n")

w <- datatable(
  handl,
  escape = FALSE,
  rownames = FALSE,
  options = list(
    pageLength = 50,
    autoWidth = TRUE,
    columnDefs = list(list(width = '60px', targets = 0))
  )
)

saveWidget(w, "table.html", selfcontained = FALSE, libdir = "lib")

cat("Done! table.html has been generated.\n")
