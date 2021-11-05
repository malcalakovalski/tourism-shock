# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, readxl, countrycode, glue, forcats, gt, scales, ggrepel,
                 countrycode, ggbrookings, ggtext, showtext,
                 broom, modelsummary)
tourism <-
  readr::read_rds('data/tourism.rds')
# Functions -------------------------------------------------------------------
main_model <- function(.data){
lm(current_account_deviation_2020 ~ current_account_2015_2019 + oil_balance_2015_2019 + travel_balance_2015_2019, data = .data)
}
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
symmetric_limits <- function (x) {
  max <- max(abs(x))
  c(-max, max)
}
mutate_where <- function(.data, .where, ...) {
  rows_lgl <- as.logical(rlang::eval_tidy(enquo(.where), .data, parent.frame()))
  .data[rows_lgl,] <- dplyr::mutate(.data[rows_lgl,], ...)
  .data
}

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

add_flags <- function(data){
  mutate(data,
         iso2 = stringr::str_to_lower(countrycode(sourcevar = country, origin = "country.name", destination = "iso2c", warn = FALSE)),
         #Create custom URL for each country
         flag_URL = glue('https://flagpedia.net/data/flags/w1160/{iso2}.png')) %>%
    select(flag_URL, everything())
}
# Data ----------------------------------------------------------------
services_raw <- read_excel("data/BOPS_data_services.xlsx",
                           sheet = "Travel_net_USD", skip = 3)
services <-
  services_raw %>%
  select(country, starts_with('travel_balance'), population, gdp) %>%
  filter(country != 'Euro Area')



# Figure 1: Export of services --------------------------------------------
credit <-
  readr::read_rds('data/credit') %>%

  group_by(year) %>%
  mutate(gdp = sum(gdp, na.rm = TRUE),
         year = as.numeric(year)) %>%
  summarise(across(c(ends_with('credit')),
                   ~sum(.x, na.rm = TRUE) / 1e4,
                   na.rm = TRUE),
            gdp) %>%
  mutate(service_credit = service_credit - travel_credit - transportation_credit) %>%
  mutate(across(ends_with('credit'),
                ~  1000 * .x / gdp)) %>%
  ungroup() %>%
  distinct()

 credit %>%
  pivot_longer(-c(year, gdp)) %>%
  ggplot(aes(x = year, y = value, fill = name, label = signif(value, 2))) +
  geom_col(
    position = 'stack') +
  ggbrookings::scale_fill_brookings('misc',
                                    name = '',
                                    labels = c('Other services',
                                               'Transport',
                                               'Travel')) +
  guides(fill = guide_legend(reverse=T))+
  labs(title = 'Export of Services',
       subtitle = 'Percent of world GDP',
       tag = 'Figure 1.',
       caption = "**Source:** Author's calculations based on IMF, Balance of Payments Statistics and national sources.",
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(2005, 2020, 2)) +
  theme_brookings()


ggsave(
  here::here('figures_tables', 'fig_01_export_services.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)


# Figure 2: Current account composition -----------------------------------
adjustment <-
  readxl::read_excel("data/tidy_tables.xlsx",
                                 sheet = "Data_adjustment", skip = 1) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  rename(category = high_tourism_revenues_5_percent_of_gdp)
adjustment

adjustment %>%
  select(category, median) %>%
  mutate(sign = if_else(median > 0,
                        'positive',
                        'negative')) %>%
  arrange(desc(median)) %>%
  mutate(category = snakecase::to_sentence_case(category)) %>%
  ggplot(aes(x = median, y = reorder(category, -median), fill = sign)) +
  geom_col(width = 0.5,
           show.legend = FALSE) +
  geom_label(
    aes(label = round(median, 1)),
    hjust = 0,
    nudge_x = 0.1,
    nudge_y = 0,
    size = 3,
    fontface = "bold",
    family = "Roboto",
    ## Box without outline
    fill = "#FFFFFF",
    label.size = 0
  ) +
  scale_fill_manual(values = c('#ED3A35', '#1479BB'),
  ) +
  scale_x_continuous(limits = symmetric_limits,
  ) +
  labs(title = 'Tourism-dependent economies: Composition of current account adjustment',
       subtitle = 'Percent of GDP, 2020 vs 2015-19 average (median values)',
       tag = 'Figure 2.',
       x = NULL,
       y = NULL,
       caption = '**Note**: The bars depicts the median difference across countries between the 2020 current account in percent of GDP (and each of its components) and their average values during the 5 preceding years (2015-19). Given the nature of the calculation the median adjustment values of the components will not add up to the median current account adjustment.<br>**Source:** Author’s calculations based on IMF, Balance of Payments Statistics and national sources') +
  theme_brookings()

ggsave(
  here::here('figures_tables', 'fig_02_adjustment.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)




# Figure 3: Travel type ---------------------------------------------------

transport_services <-
  readxl::read_xlsx('data/transport_services.xlsx', sheet = 'transport_tidy') %>%
  pivot_longer(-type) %>%
  pivot_wider(names_from = 'type',
              values_from = 'value') %>%
  rename(year = name) %>%
  mutate(year = as.numeric(year))

transport_services %>%
  pivot_longer(-year) %>%
  mutate(name_lab = if_else(year == 2020,
                            snakecase::to_title_case(name),
                            NA_character_)) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  scale_color_brookings(palette = 'semantic2') +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.45, 0.05)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2005, 2023.5),
    breaks = seq(2005, 2020, by = 1)) +
  labs(title = 'Exports of Transport Services, Selected Countries',
       subtitle = '(Percent of world GDP)',
       tag = 'Figure 3.',
       caption = '**Note:** Sum across countries of exports of transport services for countries providing a decomposition between passenger, freight, and other. Decomposition available for about 2/3 of global exports of transportation services.<br>**Source:** Author’s calculations based on IMF, Balance of Payments Statistics and national sources. ',
       x = NULL,
       y = NULL) +
  geom_text_repel(
    aes(color = name, label = name_lab),
    show.legend = FALSE,
    family = "Roboto",
    fontface = "bold",
    size = 8,
    direction = "y",

    nudge_y = -0.01,
    nudge_x = 0.2,
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  coord_cartesian(clip = 'off')

ggsave(
  here::here('figures_tables', 'fig_03_transport_services_exports.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)
# Figure 4: Passenger revenues --------------------------------------------


tourism %>%
  select(country, bpas_2015_2019, bpas_2020) %>%
  arrange(desc(bpas_2015_2019)) %>%
  head(10) %>%
  drop_na() %>%
  pivot_longer(where(is.numeric)) %>%
  mutate(country = as_factor(country)) %>%
  mutate(country = fct_relevel(country, "Fiji", "Iceland", "Antigua and Barbuda", "Qatar", "Panama","Ethiopia", "Vanuatu", "Seychelles")) %>%
  ggplot(aes(x = country, y = value, fill = name, label = round(value, 1),
             group = name)) +
  geom_col(position = position_dodge2()) +
  scale_y_continuous(limits = c(-1, 6.5), breaks = seq(-1, 6.5, 1)) +
  geom_hline(yintercept = 0) +
  scale_fill_brookings(palette = 'analogous1',
                       labels = c('2015-19', '2020')) +
  labs(title = 'Net revenues from international passenger transport (pct of GDP)',
       tag = 'Figure 4.',
       x = NULL,
       y = NULL,
       caption = '**Source:** Author’s calculation based on IMF, Balance of Payments Statistics and national sources.')
ggsave(
  here::here('figures_tables', 'fig_04_passenger_transport.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)

# Figure 5: Scatterplot ---------------------------------------------------

tourism <-
  readr::read_rds('data/tourism.rds')
`%notin%` <- Negate(`%in%`)
theme_set(theme_brookings())
caption <- '
Note: horizontal axis measures the average direct share of tourism in GDP over the period 2015-19 (WTTC, 2020). The vertical axis measures the difference between the GDP growth rate in 2020 and its projected value as of January 2020.
'
remove_countries <- tourism %>% filter(sample1 !=0) %>% pull(country)
tourism_scatter <- tourism %>%
  filter(country %notin% remove_countries) %>%
  mutate(tourism_dependence = case_when(tourism_total > 20 ~ 'High tourism',
                                        tourism_total <= 20 ~ 'Other',
                                        TRUE ~ NA_character_))
tourism_scatter %>%
  select(country_code, tourism_total, growth_dif_2020, tourism_dependence) %>% drop_na() %>%
  ggplot(aes(x = tourism_total, y = growth_dif_2020, color = tourism_dependence, label = country_code)) +
  geom_point(size = 2) +
  ggrepel::geom_label_repel(color = 'black') +
  scale_color_brookings() +
  labs(title = 'Tourism dependence and growth performance in 2020',
       tag = 'Figure 2.',
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)',
       caption = "**Note:** Horizontal axis measures the average direct share of tourism in GDP over the period 2015-19 (WTTC, 2020). The vertical axis measures the difference between the GDP growth rate in 2020 and its projected value as of January 2020.")

ggsave(
  here::here('figures_tables', 'fig_05_scatter_blog.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)
# Table 1: Large travel ---------------------------------------------------


`%notin%` <- Negate(`%in%`)
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

gtsave(balance_tbl, 'figures_tables/tbl_03_ca_2019.png')


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


# Table 5 ---------------ø--------------------------------------------------

passengers <- tourism %>%
  select(country, country_code, bpas_2015_2019) %>%
  arrange(desc(bpas_2015_2019)) %>%
  head(10) %>%
  add_flags()

transport <-
  tourism %>%
  select(country, country_code, transportation_balance_2015_2019) %>%
  arrange(desc(transportation_balance_2015_2019)) %>%
  head(10) %>%
  mutate(country = case_when(country == 'Hong Kong SAR' ~ "Hong Kong SAR*",
                             country == "United Arab Emirates" ~ "United Arab Emirates*",
                             TRUE ~ country)) %>%
  add_flags() %>%
  rename(country_transport = country) %>%
  select(flag_URL_transport = flag_URL, country_transport, transportation_balance_2015_2019)


passenger_tbl <-
  transport %>%
  bind_cols(passengers) %>%
  gt() %>%
  text_transform(
    #Apply a function to a column
    locations = cells_body(c(flag_URL, flag_URL_transport)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = 20
      )
    }
  ) %>%
  #Hide column header flag_URL and reduce width
  cols_width(c(flag_URL, flag_URL_transport) ~ px(30)) %>%
  cols_label(flag_URL = "",
             flag_URL_transport = "",
             country_transport = "Country",
             bpas_2015_2019 = md("Passenger transport<br>(pct of GDP)"),
             transportation_balance_2015_2019 = md('Transport<br>(pct of GDP)')) %>%
  cols_hide(c(iso2, country_code)) %>%
  cols_align(
    align = "left",
    columns = -c(flag_URL, flag_URL_transport)
  ) %>%

  opt_row_striping() %>%
  tab_header(title = md('Table 5. Economies with largest net international transportation revenues in percent of GDP'),
             subtitle = md('Average, 2015-19')) %>%
  tab_source_note(md("**Note**: economies denoted with an asterisk do not provide a breakdown of transport revenues into passenger revenues, freight, and others.<br>**Source**: Author's calculations based on IMF Balance of Payments statistics and naitonal sources.")) %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  cols_label(bpas_2015_2019 = md('Passenger Transport<br>(pct of GDP)')) %>%
 my_theme()
gtsave(passenger_tbl, 'figures_tables/tbl_05_transportation_and_passenger_revenues.png')

# Figure 4: International passenger revenue -------------------------------




# Table 6: Current account regression -------------------------------------


tourism <- readr::read_rds('data/tourism.rds')
tourism <- filter(tourism, sample == 0)


fit1 <- main_model(tourism)
fit2 <- main_model(filter(tourism, country != 'Kuwait'))
fit3 <- main_model(filter(tourism, country != 'Kuwait', economy == 'advanced'))
fit4 <- main_model(filter(tourism, country != 'Kuwait', economy == 'emerging'))
fit5 <- main_model(filter(tourism, country != 'Kuwait', travel_balance_2015_2019 > 0))
fit6 <- main_model(filter(tourism, country != 'Kuwait', travel_balance_2015_2019 < 0))



models <- list(fit1, fit2, fit3, fit4, fit5, fit6)

coef_names <-
  c('current_account_2015_2019' = 'Current account balance (pct of GDP, 2015-19 average)',
    'oil_balance_2015_2019' = 'Oil balance (pct of GDP, 2015-19 average)',
    'travel_balance_2015_2019' = 'Net revenues from international travel (pct of GDP, 2015-19 average)',
    '(Intercept)' = 'Constant')

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      html("Observations"),             0,
  "r.squared", html("R@2~"), 2)



main_model <- modelsummary(models, vcov = 'stata',
                           gof_map = gm,
                           escape = FALSE,
                           gof_omit = 'Log.Lik.|F|Std. Errors|AIC|BIC|Sigma|Deviance|Statistics|p', coef_map = coef_names,
                           estimate = "{estimate}{stars}",
                           output = "gt",
                           statistic = 'statistic',
                           fmt = 2,
                           stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))
tbl6 <-
  main_model %>%
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>%
        str_replace_all("~",
                        "</sup>") }
  ) %>%
  cols_label("Model 1" = html('All countries<br>(1)'),
             "Model 2" = html('All countries excluding Kuwai<br>(2)'),
             "Model 3" = html('Advanced economies<br>(3)'),
             "Model 4" = html('Emerging markets excluding Kuwait<br>(4)'),
             "Model 5" = html('Positive travel balance<br>(5)'),
             "Model 6" = html('Negative travel balance<br>(6)'))%>%

  tab_header(title = md('Table 6: Current account balance in percent of GDP, 2020 (deviation from pre-crisis forecast)')) %>%
  tab_source_note(md('**Note**: dependent variable is the difference between the current account balance in percent of GDP in 2020 and its October 2019 WEO forecast for the same year (2020). Estimation by Ordinary Least Squares, t statistics in parentheses, robust standard errors. * p<0.10, ** p<0.05,  *** p<0.01.')) %>%
  my_theme()

gtsave(tbl6, 'figures_tables/tbl_06_ca_regression.png')


# Table 7a: ---------------------------------------------------------------
tourism_share <- readxl::read_xlsx('data/tidy_tables.xlsx',
                                   sheet = 'tourism_tidy') %>%
  mutate(key = 'A. Share of tourism in GDP (average, 2015-19)')


growth <- readxl::read_xlsx('data/tidy_tables.xlsx',
                            sheet = 'surprise_tidy') %>%
  mutate(key = 'B. Growth Surprise in 2020')

growth_surprise <-
  bind_rows(tourism_share, growth) %>%
  group_by(key) %>%
  gt(groupname_col = c('key'),
     rowname_col = 'tourism') %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(obs),
             decimals = 0) %>%
  cols_label(country_group = '') %>%
  tab_header(title = md('Table 7. Share of tourism, 2015-19 and growth surprise in 2020:'),
             subtitle = md('Stylized facts')) %>%
  tab_source_note(md('**Note:** High tourism countries are those with an average direct share of tourism in GDP during 2015-19 above 6 percent (roughly the 75th percentile). For a few countries data on the tourism share in GDP is not available: hence the observations in “high tourism” and “other countries” are fewer than those for all countries. The sample excludes a few stressed economies (Iran, Lebanon, Libya, Sudan, Yemen), Guyana, as well as Macao. The latter (which is an advanced economy according to the IMF classification) has a direct tourism share of 28 percent, a total one of 59 percent, and a growth “surprise” of -55 percent.<br>**Source:** Author’s calculation based on IMF (2020), (2021) and national sources.									')) %>%
  tab_style(
    locations = cells_column_labels(columns = 3:10),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold")
    )
  ) %>%
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
    locations = cells_column_labels(columns = 3:10),
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
    row_group.background.color = '#D0D3D4')

share_tbl <-
  tourism_share %>%
  gt(groupname_col   = 'tourism',
     rowname_col = 'country_group') %>%
  tab_header(title = md('Table 7. Tourism and growth surprises: stylized facts<br><br>A. Share of tourism in GDP (average, 2015-19)')) %>%
  tab_source_note(md('**Source:** Author’s calculation based on IMF, Balance of Payments Statistics, and national sources')) %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(obs),
             decimals = 0) %>%
  my_theme()

gtsave(growth_surprise, 'figures_tables/tbl_07_share_and_growth_surprise.png')
# Table 7b: Growth surprise ------------------------------------------------




surprise_tbl <-
  growth %>%
  gt(
    rowname_col = 'country_group') %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  cols_hide(c(tourism)) %>%
  tab_row_group(label = html('Other'),
                rows = tourism == 'other') %>%
  tab_row_group(label = html('High tourism'),
                rows = tourism == 'high') %>%
  tab_row_group(label = html(''),
                rows = tourism == 'all') %>%

  opt_row_striping() %>%
  tab_header(title = md('B.	Growth Surprise in 2020')) %>%
  tab_source_note(md('**Note**: Note: High tourism countries are those with an average direct share of tourism in GDP during 2015-19 above 6 percent (roughly the 75th percentile). For a few countries data on the tourism share in GDP is not available: hence the observations in “high tourism” and “other countries” are fewer than those for all countries. The sample excludes a few stressed economies (Iran, Lebanon, Libya, Sudan, Yemen), Guyana, as well as Macao. The latter (which is an advanced economy according to the IMF classification) has a direct tourism share of 28 percent, a total one of 59 percent, and a growth “surprise” of -55 percent.<br>
**Source**: Author’s calculation based on IMF (2020), (2021) and national sources.
')) %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(obs),
             decimals = 0) %>%


gtsave(surprise_tbl, 'figures_tables/tbl_07_b_growth_surprise.png')




# Table 8: Growth regression (I) -----------------------------------------------------------------


g1 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency, data = tourism)
g2 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total, data = tourism)
g3 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation, data = tourism)
g4 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = tourism)
g5 <-lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = filter(tourism, economy == 'advanced'))
g6 <-lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = filter(tourism, economy == 'emerging'))


growth_models1 <- list(g1, g2, g3, g4, g5, g6)

coef_names <- c('cases1000' ="COVID cases in 2020 (per 1000 population)",
                'deaths1000' ="COVID deaths in 2020 (per 1000 population)",
                'stringency' ="Oxford stringency index (average 2020)",
                'tourism_total'= 'Tourism activity (total share of GDP)',
                'lgdppc' = 'log GDP per capita, 2019',
                'lpopulation' ="log population, 2019",
                'agr_2014_2019' = 'VA share of agriculture (average 2014-19)',
                'manufacturing_2014_2019' = 'VA share of manufacturing  (average 2014-19)',
                '(Intercept)' = 'Constant')
growth_reg1 <-
  modelsummary(growth_models1,
               vcov = 'stata',
               gof_omit = 'R2 Adj.|Log.Lik.|F|Std. Errors|AIC|BIC|Sigma|Deviance|Statistics|p',
               gof_map = gm,
               coef_map = coef_names,
               estimate = "{estimate}{stars}",
               output = "gt",
               statistic = 'statistic',
               stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
               fmt = 2) %>%
  cols_label("Model 1" = html('All countries<br>(1)'),
             "Model 2" = html('All countries<br>(2)'),
             "Model 3" = html('All countries<br>(3)'),
             "Model 4" = html('All countries<br>(4)'),
             "Model 5" = html('Advanced economies<br>(5)'),
             "Model 6" = html('Emerging markets<br>(6)')) %>%
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>%
        str_replace_all("~",
                        "</sup>") }
  ) %>%

  tab_header(title = md('Table 8: Growth Regressions (I)')) %>%
  tab_source_note(md('**Note**: Dependent variable is GDP growth in 2020 minus the pre-COVID growth forecast (from WEO, 2020). Estimation by Ordinary Least Squares t-statistics in parentheses, robust standard errors. * p<0.10, ** p<0.05, *** p<0.01.
')) %>%
   my_theme()
gtsave(growth_reg1, 'figures_tables/tbl_08_growth_regression_a.png')


# Table 9: Growth regression (II) -----------------------------------------




growth_models2 <- function(.data){
  lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_direct_share + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = .data)
}


m1 <- growth_models2(tourism)
m2 <- growth_models2(filter(tourism, gdpd_2019 > 100))
m3 <- growth_models2(filter(tourism, gdpd_2019 < 100))
m4 <- growth_models2(filter(tourism, tourism_direct_share > 5))
m5 <- growth_models2(filter(tourism, tourism_direct_share < 5))
m6 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019 + bpas_2015_2019 + tourism_direct_share, data = tourism)
m7 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019 + bpas_2015_2019 + tourism_direct_share, data = filter(tourism, economy == 'emerging'))
m8 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + travel_balance_2015_2019 + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = tourism)




growth_models2 <- list(m1, m2, m3, m4, m5, m6, m7, m8)


coef_names <- c('cases1000' ="COVID cases in 2020 (per 1000 population)",
                'deaths1000' ="COVID deaths in 2020 (per 1000 population)",
                'stringency' ="Oxford stringency index (average 2020)",
                'tourism_total'= 'Tourism activity (total share of GDP)',
                'tourism_direct_share' = 'Tourism activity (direct share of GDP)',
                "bpas_2015_2019" = "Net revenues from intl. passenger transport (pct of GDP, 2015-19)",
                'travel_balance_2015_2019' = 'Net revenues from intl. travel (pct of GDP, 2015-19)',
                'lgdppc' = 'log GDP per capita, 2019',
                'lpopulation' ="log population, 2019",
                'agr_2014_2019' = 'VA share of agriculture (average 2014-19)',
                'manufacturing_2014_2019' = 'VA share of manufacturing  (average 2014-19)',
                '(Intercept)' = 'Constant')


growth_reg2 <-
  modelsummary(growth_models2,
               vcov = 'stata',
               gof_map = gm,
               gof_omit = 'R2 Adj.|Log.Lik.|F|Std. Errors|AIC|BIC|Sigma|Deviance|Statistics|p',
               coef_map = coef_names,
               estimate = "{estimate}{stars}",
               output = "gt",
               statistic = 'statistic',
               fmt = 2,
               stars = c('*' = .1, '**' = 0.05, '***' = .01)) %>%
  cols_label("Model 1" = html('All countries<br>(1)'),
             "Model 2" = html('GDP above $100 billion in 2019<br>(2)'),
             "Model 3" = html('GDP below $100 billion in 2019<br>(3)'),
             "Model 4" = html('Tourism direct share above 5 pct of GDP<br>(4)'),
             "Model 5" = html('Tourism direct share below 5 pct of GDP<br>(5)'),
             "Model 6" = html('All countries<br>(6)'),
             "Model 7" = html('Emerging markets<br>(7)'),
             "Model 8" = html('All countries<br>(8)'))%>%
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>%
        str_replace_all("~",
                        "</sup>") }
  ) %>%
  tab_header(title = md('Table 9. Growth Regressions (II)')) %>%
  tab_source_note(md('**Note**: Dependent variable is GDP growth in 2020 minus the pre-COVID growth forecast (from WEO, 2020). Estimation by Ordinary Least Squares t-statistics in parentheses, robust standard errors. * p<0.10, ** p<0.05,  *** p<0.01.
')) %>%
  my_theme()

gtsave(growth_reg2, 'figures_tables/tbl_09_growth_regression_b.png')



# Table 10: Growth regression (III) ---------------------------------------


# * Regressions for Table 10 **
#   eststo clear
# eststo: reg grow_dif cases1000 deaths1000 stringency tour_dir lgdppc lpop agr_1419 manuf_1419 if sample==0 & tour_dir>5, robust
# eststo: reg grow_dif cases1000 deaths1000 stringency btv_1519 lgdppc lpop agr_1419 manuf_1419 if sample==0 & tour_dir>5, robust
# eststo: reg grow_dif cases1000 deaths1000 stringency tour_dir btv_1519 lgdppc lpop agr_1419 manuf_1419 if sample==0 & tour_dir>5, robust
# eststo: reg grow_dif cases1000 deaths1000 tour_dir btv_1519 lgdppc lpop agr_1419 manuf_1419 if sample==0 & tour_dir>5, robust

m1 <-  lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_direct_share + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = filter(tourism, tourism_direct_share > 5))

m2 <-  lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency +  lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019 + travel_balance_2015_2019, data = filter(tourism, tourism_direct_share > 5))

m3 <-  lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency +  lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019 + travel_balance_2015_2019 + tourism_direct_share, data = filter(tourism, tourism_direct_share > 5))

m4 <-  lm(growth_dif_2020 ~ cases1000 + deaths1000  +  lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019 + travel_balance_2015_2019 + tourism_direct_share, data = filter(tourism, tourism_direct_share > 5))



growth_models3 <- list(m1, m2, m3, m4)


coef_names <- c('cases1000' ="COVID cases in 2020 (per 1000 population)",
                'deaths1000' ="COVID deaths in 2020 (per 1000 population)",
                'stringency' ="Oxford stringency index (average 2020)",
                'tourism_direct_share' = 'Tourism activity (direct share of GDP)',
                "bpas_2015_2019" = "Net revenues from intl. passenger transport (pct of GDP, 2015-19)",
                'travel_balance_2015_2019' = 'Net revenues from intl. travel (pct of GDP, 2015-19)',
                'lgdppc' = 'log GDP per capita, 2019',
                'lpopulation' ="log population, 2019",
                'agr_2014_2019' = 'VA share of agriculture (average 2014-19)',
                'manufacturing_2014_2019' = 'VA share of manufacturing  (average 2014-19)',
                '(Intercept)' = 'Constant')


growth_reg3 <-
  modelsummary(growth_models3,
               vcov = 'stata',
               gof_map = gm,
               gof_omit = 'R2 Adj.|Log.Lik.|F|Std. Errors|AIC|BIC|Sigma|Deviance|Statistics|p',
               coef_map = coef_names,
               estimate = "{estimate}{stars}",
               output = "gt",
               statistic = 'statistic',
               fmt = 2,
               stars = c('*' = .1, '**' = 0.05, '***' = .01)) %>%
    cols_label("Model 1" = html('Dir. Tour > 5<br>(1)'),
               "Model 2" = html('Dir. Tour > 5<br>(2)'),
               "Model 3" = html('Dir. Tour > 5<br>(3)'),
               "Model 4" = html('Dir. Tour > 5<br>(4)')) %>%
    text_transform(
      locations = cells_body(),
      fn = function(x) {
        str_replace_all(x,
                        pattern = "@",
                        replacement = "<sup>") %>%
          str_replace_all("~",
                          "</sup>") }
    ) %>%
    tab_header(title = md('Table 10. Growth regressions (III): Dependence on Domestic and International Tourism')) %>%
    tab_source_note(md('**Note**: Dependent variable is GDP growth in 2020 minus the pre-COVID growth forecast (from WEO, 2020). The sample includes countries with a direct share of tourism exceeding 5% of GDP. Estimation by Ordinary Least Squares, t-statistics in parentheses, robust standard errors. * p<0.10, ** p<0.05,  *** p<0.01.
')) %>%
    my_theme()

gtsave(growth_reg3, 'figures_tables/tbl_10_growth_regression_c.png')


# Extra -------------------------------------------------------------------
emu_countries <- c('IRL', 'LTU', 'LUX', 'NLD', 'BEL', 'SVK', 'FIN', 'LVA', 'DEU', 'SVN', 'ITA', 'ESP', 'AUT', 'EST', 'PRT', 'GRC', 'CYP', 'MLT')
tourism %>%
  filter(country_code %in% emu_countries) %>%
  ggplot(aes(x = tourism_total, y = growth_dif_2020, label = country_code)) +
  geom_point(color = '#FF9E1B') +
  geom_smooth(method = 'lm',
              color = '#003A79',
              se = FALSE) +
  geom_label_repel() +

  scale_x_continuous(breaks = seq(0, 30, 5),
                     limits = c(0, 30)) +
  labs(title = NULL,
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)') +
  ggpubr::stat_regline_equation(aes(label = ..rr.label..))

ggsave(here::here('emu_growth_surprise.png'),
         width = 85 * (14 / 5),
         height = 53 * (14 / 5),
         units = 'mm',
         device = ragg::agg_png(),
         dpi = 300,
         type = 'cairo',
         bg = 'transparent')

library('ggpubr')
ggplot(mtcars ,aes(x = wt, y = hp)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))
