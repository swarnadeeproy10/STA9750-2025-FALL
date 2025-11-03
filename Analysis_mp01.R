if  (!dir.exists(file.path("data", "mp01"))) {
  dir.create(file.path("data", "mp01"), showWarnings = FALSE, recursive = TRUE)
}

GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")
if (!file.exists(GLOBAL_TOP_10_FILENAME)) {
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-global.tsv", destfile = GLOBAL_TOP_10_FILENAME)
}

COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")
if (!file.exists(COUNTRY_TOP_10_FILENAME)) {
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-countries.tsv", destfile = COUNTRY_TOP_10_FILENAME)
}


library(readr)
library(dplyr)

# Import global data, then clean 'N/A' to NA in season_title
GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)
GLOBAL_TOP_10 <- GLOBAL_TOP_10 %>%
  mutate(season_title = if_else(season_title == "N/A", NA_character_, season_title))
glimpse(GLOBAL_TOP_10)

# Import country data, cleaning 'N/A' to NA on import
COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME, na = "N/A")
glimpse(COUNTRY_TOP_10)


install.packages("DT")





library(DT)
library(stringr)

format_titles <- function(df) {
  colnames(df) <- colnames(df) %>%
    str_replace_all("_", " ") %>%
    str_to_title()
  df
}

GLOBAL_TOP_10 %>%
  head(20) %>%
  format_titles() %>%
  datatable(options = list(searching = FALSE, info = FALSE)) %>%
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))


GLOBAL_TOP_10 |> 
  select(-season_title) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))


GLOBAL_TOP_10 |> 
  mutate(`runtime_(minutes)` = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))


library(dplyr)
num_countries <- COUNTRY_TOP_10 %>% 
  distinct(country_name) %>% 
  count()
num_countries
# For Quarto inline: `r num_countries$n`


non_english_top <- GLOBAL_TOP_10 %>%
  filter(!grepl("English", category)) %>%
  group_by(show_title) %>%
  summarise(total_weeks = sum(cumulative_weeks_in_top_10, na.rm = TRUE)) %>%
  arrange(desc(total_weeks)) %>%
  slice(1)
non_english_top



longest_film <- GLOBAL_TOP_10 %>%
  filter(!is.na(runtime)) %>%               # ignore missing runtime
  mutate(runtime_minutes = round(runtime * 60)) %>%
  arrange(desc(runtime_minutes)) %>%
  select(show_title, runtime_minutes, category, week) %>%
  slice(1)
longest_film


most_hours_by_category <- GLOBAL_TOP_10 %>%
  group_by(category, show_title) %>%
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE), .groups = "drop") %>%
  group_by(category) %>%
  slice_max(total_hours, n = 1) %>%
  ungroup()
most_hours_by_category



longest_run_country <- COUNTRY_TOP_10 %>%
  group_by(country_name, show_title) %>%
  summarise(longest_run = max(cumulative_weeks_in_top_10, na.rm = TRUE)) %>%
  arrange(desc(longest_run)) %>%
  slice(1)
longest_run_country



country_week_counts <- COUNTRY_TOP_10 %>%
  group_by(country_name) %>%
  summarise(weeks_of_data = n(),
            last_week = max(week))
country_week_counts %>% 
  filter(weeks_of_data < 200)


squid_game_viewership <- GLOBAL_TOP_10 %>%
  filter(grepl("Squid Game", show_title, ignore.case = TRUE)) %>%
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE))
squid_game_viewership


library(lubridate)
red_notice_data <- GLOBAL_TOP_10 %>%
  filter(show_title == "Red Notice", year(week) == 2021)

red_notice_total_hours <- sum(red_notice_data$weekly_hours_viewed, na.rm = TRUE)
red_notice_runtime_hours <- 1 + 58/60
red_notice_estimated_views <- red_notice_total_hours / red_notice_runtime_hours
red_notice_estimated_views





country_week_counts <- COUNTRY_TOP_10 %>%
  group_by(country_name) %>%
  summarise(weeks_of_data = n(),
            last_week = max(week))

country_week_counts %>%
  filter(weeks_of_data < 200)


country_week_counts %>% 
  arrange(weeks_of_data) %>%
  head(10)




us_films_1 <- COUNTRY_TOP_10 %>%
  filter(country_name == "United States", weekly_rank == 1) %>%
  group_by(show_title) %>%
  summarise(first_rank = min(weekly_rank),
            first_week = min(week),
            min_rank = min(weekly_rank),
            .groups = "drop")

us_films_1



debut_top10 <- COUNTRY_TOP_10 %>%
  group_by(show_title, season_title, week) %>%
  summarise(countries_in_top10 = n_distinct(country_name)) %>%
  arrange(desc(countries_in_top10)) %>%
  slice(1)
debut_top10




# Find the debut with the most countries
debut_top10 <- COUNTRY_TOP_10 %>%
  group_by(show_title, season_title, week) %>%
  summarise(countries_in_top10 = n_distinct(country_name), .groups = "drop") %>%
  arrange(desc(countries_in_top10)) %>%
  slice(1)

# Extract show, season, week of the max debut
top_show <- debut_top10$show_title[1]
top_season <- debut_top10$season_title[1]
top_week <- debut_top10$week[1]

# List countries for that show/season/week
countries_list <- COUNTRY_TOP_10 %>%
  filter(show_title == top_show,
         season_title == top_season,
         week == top_week) %>%
  distinct(country_name)

countries_list




library(dplyr)
library(stringr)

# Find the show/season/week with the most debut country appearances AND collapse country names into one cell
debut_top10_chart <- COUNTRY_TOP_10 %>%
  group_by(show_title, season_title, week) %>%
  summarise(
    countries_in_top10 = n_distinct(country_name),
    country_names = str_c(sort(unique(country_name)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(countries_in_top10)) %>%
  slice(1)

debut_top10_chart




library(dplyr)
library(stringr)

# Get top 10 debut weeks by number of countries, including country names
top_debut_10 <- COUNTRY_TOP_10 %>%
  group_by(show_title, season_title, week) %>%
  summarise(
    countries_in_top10 = n_distinct(country_name),
    country_names = str_c(sort(unique(country_name)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(countries_in_top10)) %>%
  slice(1:10)  # show top 10 debuts by country count

top_debut_10





COUNTRY_TOP_10 %>%
  group_by(show_title, season_title, week) %>%
  summarise(
    num_countries = n_distinct(country_name),
    country_names = paste(sort(unique(country_name)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(num_countries >= 1) %>%
  arrange(desc(num_countries))


