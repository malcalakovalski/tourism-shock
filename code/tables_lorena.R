# Setup -------------------------------------------------------------------
librarian::shelf(
  tidyverse,
  countrycode,
  glue,
  forcats,
  readxl,
  gt)

tourism <-
  readr::read_rds('data/tourism.rds')

# Functions ---------------------------------------------------------------

# Default theme using Brookings styling in gt tables
my_theme <- function(data, ...){
  data %>%
    opt_row_striping() %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()))  %>%
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    cols_align(where(is.numeric),
               align = 'right') %>%
    cols_align(where(is.character),
               align = 'left') %>%
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    #Apply different style to the title
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    tab_options(
      column_labels.border.top.width = px(5),
      column_labels.border.top.color = "#FFFFFF",
      table.border.top.color = "#FFFFFF",
      table.border.bottom.color = "#FFFFFF",
      heading.background.color = '#003A79',
      data_row.padding = px(5),
      source_notes.font.size = 12,
      heading.align = "center",
      row_group.background.color = '#D0D3D4',
      ...)
}
# Convenience function to mutate but only on certain rows according to a logical condition
mutate_where <- function(.data, .where, ...) {
  rows_lgl <- as.logical(rlang::eval_tidy(enquo(.where), .data, parent.frame()))
  .data[rows_lgl,] <- dplyr::mutate(.data[rows_lgl,], ...)
  .data
}
# Include country flags in table
add_flags <- function(data){
  mutate(data,
         iso2 = stringr::str_to_lower(countrycode(sourcevar = country, origin = "country.name", destination = "iso2c", warn = FALSE)),
         #Create custom URL for each country
         flag_URL = glue('https://flagpedia.net/data/flags/w1160/{iso2}.png')) %>%
    select(flag_URL, everything())
}

# Format flags so they appear properly in table
format_flags <- function(data){
  data %>%
    text_transform(
      #Apply a function to a column
      locations = cells_body(c(flag_URL)),
      fn = function(x) {
        #Return an image of set dimensions
        web_image(
          url = x,
          height = 20
        )
      }
    ) %>%

    #Hide column header flag_URL and reduce width
    cols_width(c(flag_URL) ~ px(30)) %>%
    cols_label(flag_URL = "") %>%
    cols_hide(c(iso2))
}
format_flags2 <- function(data){
  data %>%
    text_transform(
      #Apply a function to a column
      locations = cells_body(c(flag_URL2)),
      fn = function(x) {
        #Return an image of set dimensions
        web_image(
          url = x,
          height = 20
        )
      }
    ) %>%

    #Hide column header flag_URL and reduce width
    cols_width(c(flag_URL2) ~ px(30)) %>%
    cols_label(flag_URL2 = "") %>%
    cols_hide(c(iso22))
}

# Negate then %in% operator to exclude observations based on a vector of names/positions
`%notin%` <- Negate(`%in%`)


# Data ----------------------------------------------------------------
services_raw <- read_excel("data/BOPS_data_services.xlsx",
                           sheet = "Travel_net_USD", skip = 3)
services <-
  services_raw %>%
  select(country, starts_with('travel_balance'), population, gdp) %>%
  filter(country != 'Euro Area')

# Table 1: Large travel ---------------------------------------------------



islands <- c('Turks and Caicos Islands',
             'Sint Maarten, Kingdom of the Netherlands',
             'Anguilla',
             'Cayman Islands',
             'Curaçao, Kingdom of the Netherlands',
             'French Polynesia')
in_sample <-
  services %>%
  select(country, travel_balance_1519_share, population, gdp) %>%
  filter(country %notin% islands,
         country %notin% c('Eastern Caribbean Currency Union')) %>%
  arrange(desc(travel_balance_1519_share)) %>%
  filter(travel_balance_1519_share > 5) %>%
  mutate(sample = 'Sample countries') %>%
  mutate(country = recode(country,
                          `China, P.R.: Macao`  = 'Macao',
                          `Andorra, Principality of` = "Andorra",
                          `Aruba, Kingdom of the Netherlands` = "Aruba",
                          `Palau, Rep. of` = "Palau",
                          `Bahamas, The` = "Bahamas",
                          `Croatia, Rep. of` = "Croatia",
                          `Fiji, Rep. of` = "Fiji",
                          `Kosovo, Rep. of` = "Kosovo",
                          `Bahrain, Kingdom of` = "Bahrain"))

first_half <- in_sample %>%
  head(19) %>%
  add_flags() %>%
  mutate_where(country == 'Aruba',
               iso2 = 'aw',
               flag_URL = 'https://flagpedia.net/data/flags/w1160/aw.png')

second_half <-
  in_sample %>%
  tail(18) %>%
  add_flags() %>%
  select(flag_URL2 = flag_URL,
         country2 = country,
         travel_balance_1519_share2 =travel_balance_1519_share,
         population2 = population,
         gdp2 = gdp,
         iso22 = iso2) %>%
  mutate_where(country2 == 'Kosovo',
               flag_URL2 = 'https://flagpedia.net/data/flags/w1160/xk.png')%>%
  add_row(country2 = '',
          flag_URL2 = '')
second_half %>%
  mutate_where(country2 == 'Morocco',
               flag_URL2 = NA_character_,
               country2 = NA_character_,
               travel_balance_1519_share2 = NA_real_,
               population2 = NA_real_,
               gdp2 = NA_real_,
               iso22 = NA_character_) %>%
  View()

large_travel_tbl <-
  bind_cols(first_half, second_half) %>%
  gt() %>%
  my_theme() %>%
  format_flags() %>%
  format_flags2() %>%
  cols_hide(sample) %>%
  cols_label(travel_balance_1519_share = md("Travel balance<br>(pct of GDP)"),
             population = md("Population<br>(millions)"),
             gdp = md("GDP<br>(bil.US$)"),
             country2 = "country",
             travel_balance_1519_share2 = md("Travel balance<br>(pct of GDP)"),
             population2 = md("Population<br>(millions)"),
             gdp2 = md("GDP<br>(bil.US$)")) %>%
  tab_header(title = md('Table 1. Economies with large net revenues from international travel as a share of GDP')) %>%
  tab_source_note(md("**Note:** Median travel balance: 18.9%; Median population: ½ million; Median GDP in 2019: $5.6 billion.<br>**Source:** Author’s calculations based on IMF, Balance of Payments Statistics; IMF, World Economic Outlook; and national sources. The economies excluded from the sample lack data on pre-COVID growth forecasts.
")) %>%
  fmt_currency(c(gdp, gdp2),
               decimals = 1) %>%
  fmt_number(columns = c(population, population2),
             decimals = 2) %>%
  fmt_number(c(travel_balance_1519_share, travel_balance_1519_share2),
             decimals = 1) %>%
  fmt_missing(
    columns = 10:12,
    missing_text = "")


gtsave(large_travel_tbl,
       'figures_tables/tbl_01_large_travel.png')


# Table 2: Exporters and importers ------------------------------------------------------

exporters <- services %>%
  select(country, travel_balance_1519, travel_balance_1519_share) %>%
  arrange(desc(travel_balance_1519)) %>%
  head(10) %>%
  add_flags() %>%
  mutate(key = 'A. Largest Exporters: net international travel revenues')


importers <-
  services %>%
  select(country, travel_balance_1519, travel_balance_1519_share) %>%
  arrange(travel_balance_1519) %>%
  head(10) %>%
  mutate(travel_balance_1519 = -travel_balance_1519,
         travel_balance_1519_share = -travel_balance_1519_share) %>%
  add_flags() %>%
  mutate(key = 'B. Largest importers: net international travel expenditures')

travel_balance <-
  bind_rows(exporters, importers)

travel_balance_tbl <-
  travel_balance %>%
  gt(groupname_col = 'key') %>%
  cols_label(country = 'Country',
             travel_balance_1519 = md('Billions of USD<br>average 2015-19'),
             travel_balance_1519_share = md('Percent of GDP<br>average 2015-19')) %>%
  tab_header(title = md('Table 2. Net exporters and importers of travel services')) %>%
  tab_source_note(md('**Source:** Author’s calculation based on IMF, Balance of Payments Statistics and, national sources')) %>%
  fmt_currency(columns = c(travel_balance_1519),
               decimals = 1) %>%
  fmt_number(columns = c(travel_balance_1519_share),
             decimals = 1) %>%
  my_theme() %>%
  fmt_currency(c(travel_balance_1519),
               decimals = 1) %>%
  fmt_number(c(travel_balance_1519_share),
             decimals = 1) %>%
  text_transform(
    #Apply a function to a column
    locations = cells_body(c(flag_URL)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = 20
      )
    }
  ) %>%

  #Hide column header flag_URL and reduce width
  cols_width(c(flag_URL) ~ px(30)) %>%
  cols_label(flag_URL = "") %>%
  cols_hide(c(iso2)) %>%
  my_theme()

gtsave(travel_balance_tbl, 'figures_tables/tbl_02_net_exporters_importers.png')


# Table 3_CA_2019 ---------------------------------------------------------


tourism <- readRDS('data/tourism.rds') %>%
  filter(country != 'Euro Area')

balance <- tourism %>%
  select(country,
         current_account_2015_2019,
         goods_balance_2015_2019,
         oil_balance_2015_2019,
         travel_balance_2015_2019,
         services_net_travel_balance_2015_2019,
         transportation_balance_2015_2019,
         primary_income_balance_2015_2019,
         secondary_income_balance_2015_2019) %>%
  rename_with(.cols = ends_with('2019'),
              .fn = ~stringr::str_replace(.x, '_2015_2019', ''))


balance_summary <-
  balance %>%
  filter(travel_balance > 5) %>%
  select(where(is.numeric)) %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="category")  %>%
  print() %>%
  select(category,
         Observations = n,
         Min = min, Q25 = Q0.25,  Median = median, Q75 = Q0.75,
         Max = max, Mean = mean, `St. Dev.` = sd) %>%
  mutate(category = snakecase::to_sentence_case(category))

balance_tbl <-
  balance_summary %>%
  gt() %>%
  cols_label(category = '') %>%
  tab_header(title = md('Table 3. Tourism-dependent economies:<br>current account balance and composition'),
             subtitle = md('(Percent of GDP, 2015-19 averages)')) %>%
  tab_source_note(md('**Source:** Author’s calculation based on IMF, Balance of Payments Statistics and national sources')) %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(Observations),
             decimals = 0) %>%
  tab_style(
    style = cell_text(indent = px(25),
                      style = 'italic'),
    locations = cells_body(
      columns = c(category),

      rows = category %in% c("Oil balance", 'Transportation balance')
    )
  ) %>%
  my_theme()

gtsave(balance_tbl,
       'figures_tables/tbl_03_ca_2019.png')


# Table 4. Tourism dependent economies relative to other economies: ----------------------------------------------------------------


islands <- c('Turks and Caicos Islands',
             'Sint Maarten, Kingdom of the Netherlands',
             'Anguilla',
             'Cayman Islands',
             'Curaçao, Kingdom of the Netherlands',
             'French Polynesia')
creditor_position <-
  tourism %>%
  select(country, tourism_total, iip_2019, gdp_per_capita_2019,
         travel_balance = travel_balance_2015_2019, population_2019,

         tourism_travel) %>%
  mutate(tourism_type = case_when(travel_balance >= 5 ~ 'High tourism revenues',
                                  travel_balance > 0  ~ 'Positive tourism revenues',
                                  travel_balance <= 0 ~ 'Negative tourism revenues'),
         .after = 'country') %>%
  mutate(iip_2019 = 100 * iip_2019)






creditor_stats <-
  creditor_position %>%
  group_by(tourism_type) %>%

  summarise(across(
    # Columns to summarise
    .cols = c(
      iip_2019,
      gdp_per_capita_2019,
      population_2019
    ),
    # Functions to use.
    # Note: To use multiple functions inside across you can put them in a list!
    .fns = list(
      obs = ~ n(),
      min = min,
      q25 = ~ quantile(.x, 0.25, na.rm = TRUE),
      median = median,
      q75 = ~ quantile(.x, 0.75, na.rm = TRUE),
      max = max,
      mean = mean,
      sd = sd
    ),
    # Additional arguments to pass on to the list of functions
    na.rm = TRUE,
    .names = '{.col}_{.fn}'
  ))



creditor_stats_wide <-
  creditor_stats %>%
  pivot_longer(
    # Pivot on all numeric values
    where(is.numeric),
    # What we want our new column names to be
    names_to = c('variable', 'statistic'),
    # Alternatively, we can use (.*)_(.*) which is more general.
    # I'm just being explicit here to show what the .* placeholder is doing!
    names_pattern = '(.*)_(obs|min|q25|mean|median|q75|max|sd)',
    values_to = 'values'
  ) %>%
  drop_na() %>%
  # Take the names of the values in the statistic column and turn them into columns.
  # I.e, we want columns for min, mean, median, max, and range
  pivot_wider(names_from = statistic, values_from = values) %>%
  arrange(desc(variable),factor(tourism_type, levels = c('High tourism revenues', 'Positive tourism revenues', 'Negative tourism revenues')))
creditor_tbl <-
  creditor_stats_wide %>%
  mutate(obs = c(35, 113, 63, 37, 115, 64, 37, 116, 64)) %>%
  gt(
    rowname_col = 'tourism_type') %>%
  tab_header(title = md('Table 4. Tourism dependent economies relative to other economies:'),
             subtitle = md('Net creditor position, GDP per capita, and population, 2019')) %>%
  tab_source_note(source_note = md('**Note:** High tourism revenues: average net revenues from international travel in 2015-19 above 5 percent of GDP; Positive tourism revenues: average net revenues from international travel in 2015-19 between 0 and 5 percent of GDP. IIP is the ratio of the net international investment position (excluding gold) to GDP.<br>**Sources:** IIP: Lane and Milesi-Ferretti, EWN database. GDP per capita in current US$ and population: World Bank, World Development Indicators. ')) %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(obs),
             decimals = 0) %>%
  tab_row_group(label = html("Population<br>(Millions, 2019)"),
                rows = variable == 'population_2019') %>%
  tab_row_group(label = html('GDP per capita<br>(2019)'),
                rows = variable == 'gdp_per_capita_2019') %>%
  tab_row_group(label = html('International investment position<br>(percent of GDP, 2019)'),
                rows = variable == 'iip_2019') %>%

  cols_hide(variable) %>%
  fmt_number(columns = where(is.numeric),
             decimals = 0,
             drop_trailing_zeros = TRUE) %>%
  fmt_number(columns = 4:10,
             rows = 1:3,
             decimals = 2) %>%
  my_theme()

gtsave(creditor_tbl, 'figures_tables/tbl_04_tourism_dependence.png')

