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

cat("Calculating team EPA...\n")

# Calculate offensive EPA (higher = better offense)
off <- pbp %>%
  group_by(pos_team) %>%
  summarize(off_epa = mean(EPA))

# Calculate defensive EPA - POSITIVE means defense is creating value (stops, turnovers)
def <- pbp %>%
  group_by(def_pos_team) %>%
  summarize(def_epa = mean(EPA))

# Combine: higher off_epa = better, lower def_epa = better
team_ratings <- left_join(off, def, by = c("pos_team" = "def_pos_team")) %>%
  mutate(raw_rating = off_epa - def_epa)

cat("\nCreating HANDL ratings table...\n")

# Create final table
handl <- team_ratings %>%
  mutate(
    rating = round(raw_rating * 100, 2),
    off_display = round(off_epa * 100, 2),
    def_display = round(def_epa * -100, 2)  # Flip sign so higher = better defense
  ) %>%
  arrange(desc(rating)) %>%
  select(pos_team, rating, off_display, def_display) %>%
  rename(
    Team = pos_team,
    'HANDL Rating' = rating,
    'Off Rating' = off_display,
    'Def Rating' = def_display
  )

handl <- handl %>%
  mutate('HANDL Rank' = row_number()) %>%
  arrange(desc(`Off Rating`)) %>% mutate('Off Rank' = row_number()) %>%
  arrange(desc(`Def Rating`)) %>% mutate('Def Rank' = row_number()) %>%
  left_join(logos, by = c("Team" = "school")) %>%
  select(logo, Team, `HANDL Rating`, `HANDL Rank`, `Off Rating`, `Off Rank`, `Def Rating`, `Def Rank`)

handl$logo <- paste0('<img src="', handl$logo, '" height="40">')
handl <- handl %>% arrange(desc(`HANDL Rating`)) %>% rename(" " = logo)

# Print top 20 for verification
cat("\n========== TOP 20 TEAMS ==========\n")
print(handl %>% select(Team, `HANDL Rating`, `HANDL Rank`, `Off Rank`, `Def Rank`) %>% head(20))

# Print playoff teams
cat("\n========== 2024 PLAYOFF TEAMS ==========\n")
playoff_teams <- c("Notre Dame", "Ohio State", "Texas", "Penn State", "Oregon", "Georgia",
                   "Boise State", "Arizona State", "Clemson", "Tennessee", "SMU", "Indiana")
print(handl %>% filter(Team %in% playoff_teams) %>%
        select(Team, `HANDL Rating`, `HANDL Rank`, `Off Rank`, `Def Rank`) %>%
        arrange(`HANDL Rank`))

cat("\nGenerating HTML table...\n")

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
