
# Setup -------------------------------------------------------------------
library('tidyverse')
library(readxl)
librarian::shelf(tidyverse, readxl, countrycode, glue)
services_raw <- read_excel("data/BOPS data_services.xlsx",
                                 sheet = "Travel_net_USD", skip = 4)

# Cleaning ----------------------------------------------------------------

services <-
  services_raw %>%
  select(country, starts_with('travel_balance'), population_2019, gdp_2019) %>%
  filter(country != 'Euro Area')

## Largest in absolute terms
largest_absolute <-
  services %>%
  arrange(desc(travel_balance_1519)) %>%
  head(5)

## Largest share
services %>%
  arrange(desc(travel_balance_1519_share)) %>%
  head(5)

## Largest share with population greater than 2 million
services %>%
  filter(population > 2) %>%
  arrange(desc(travel_balance_1519_share)) %>%
  head(5)


## Smallest in absolute terms
  services %>%
  arrange(travel_balance_1519) %>%
  head(5)

## Smallest share
services %>%
  arrange(travel_balance_1519_share) %>%
  head(5)

## Smallest share with population greater than 2 million
services %>%
  filter(population > 2) %>%
  arrange(travel_balance_1519_share) %>%
  head(5)


services_raw %>%
  select(country, starts_with('travel_balance_1519'), gdp, population) %>%
  mutate(log_gdp = log2(gdp),
         log_population = log2(population)) %>%
  ggplot(aes(y = log_population,
             x = travel_balance_1519_share,

             label = country)) +
  geom_point(aes(size = population)) +
  ggrepel::geom_text_repel()


# Net travel balance ------------------------------------------------------
services_raw %>%
  select(country, starts_with('net'), population) %>%
  ggplot(aes(x = net_ca_balance,
             y = net_travel_balance_share,
             label = country,
             size = population)) +
  geom_point() +
  ggrepel::geom_text_repel()

net <-
  services_raw %>%
  select(country, net_travel_balance_share, population, gdp) %>%
  filter(country != 'Euro Area') %>%
  mutate(net_travel_balance = net_travel_balance_share * gdp)


# Prepare table data ------------------------------------------------------

# ABSOLUTE
## Largest in absolute terms
(largest_absolute <-
  net %>%
  arrange(desc(net_travel_balance)) %>%
  head(5) %>%
  select(country_positive = country, positive = net_travel_balance)) %>%
  mutate(type = '(billions of US dollars)')

## Largest in absolute terms
(smallest_absolute <-
    net %>%
    arrange(net_travel_balance) %>%
    head(5) %>%
    select(country_negative = country, negative = net_travel_balance))

absolute <- bind_cols(largest_absolute, smallest_absolute) %>%
  mutate(type = '(billions of US dollars)',
         .before = everything())

## SHARES

(largest_share <-
    net %>%
    arrange(desc(net_travel_balance_share)) %>%
    head(5) %>%
    select(country_positive = country, positive = net_travel_balance_share))

## Largest in absolute terms
(smallest_share <-
    net %>%
    arrange(net_travel_balance_share) %>%
    head(5) %>%
    select(country_negative = country, negative = net_travel_balance_share))

share <- bind_cols(largest_share, smallest_share) %>%
  mutate(type = '(Share of GDP)',
         .before = everything())
## TABLE
librarian::shelf(gt, scales, glue)

net_travel <-
  bind_rows(share,
            absolute) %>%
  mutate(positive = if_else(type == '(Share of GDP)',
                        glue('{round(positive, 0)}%'),
                        str_remove(glue('${comma(round(positive))}'), '(.[0]{1,})$')),
         negative = if_else(type == '(Share of GDP)',
                          glue('{round(negative, 0)}%'),
                          str_remove(glue('${comma(round(negative))}'),'(.[0]{1,})$' )))

net_travel %>%
  group_by(type) %>%
  gt() %>%
  cols_label(country_positive = '',
             positive = '',
             country_negative = '',
             negative = '') %>%
  tab_spanner(label = 'Largest balance',
              columns = c('country_positive', 'positive')) %>%
  tab_spanner(label = 'Smallest balance',
              columns = c('country_negative', 'negative')) %>%
  tab_header(title = md('Countries with largest and smallest
                         net travel balance '),
             subtitle = md('Average from 2015 to 2019')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics and national sources') %>%

  opt_row_striping() %>%
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%


  tab_options(
    column_labels.border.top.width = px(5),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(5),
    source_notes.font.size = 12,
    heading.align = "center",
    #Adjust grouped rows to make them stand out
    row_group.background.color = "#003A79")


# Table 1: Exporters ------------------------------------------------------
add_flags <- function(data){
  mutate(data,
         iso2 = stringr::str_to_lower(countrycode(sourcevar = country, origin = "country.name", destination = "iso2c", warn = FALSE)),
         #Create custom URL for each country
         flag_URL = glue('https://flagpedia.net/data/flags/w1160/{iso2}.png')) %>%
    select(flag_URL, everything())
}
exporters <- services %>%
  select(country, travel_balance_1519, travel_balance_1519_share) %>%
  arrange(desc(travel_balance_1519)) %>%
  head(10) %>%
  add_flags()


exporters_tbl <-
  exporters %>%
  gt() %>%

  cols_label(country = 'Country',
             travel_balance_1519 = 'Billions of USD',
             travel_balance_1519_share = 'Percent of GDP') %>%
  tab_header(title = md('Economies with largest net revenues from international travel'),
             subtitle = md('Average from 2015 to 2019')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics and, national sources') %>%
  fmt_currency(columns = c(travel_balance_1519),
               decimals = 1) %>%
  fmt_number(columns = c(travel_balance_1519_share),
              decimals = 1) %>%


  opt_all_caps() %>%
  opt_row_striping() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%



  tab_options(
    column_labels.border.top.width = px(5),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.background.color = '#003A79',
    data_row.padding = px(5),
    source_notes.font.size = 12,
    heading.align = "left") %>%
  #Apply new style to all column headers
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
  cols_align(
    align = "left",
    columns = everything()
  )
exporters_tbl
gt::gtsave(exporters_tbl, 'tables/01_exporters.png')






# Table 2: Importers ------------------------------------------------------

importers <-
  services %>%
  select(country, travel_balance_1519, travel_balance_1519_share) %>%
  arrange(travel_balance_1519) %>%
  head(10) %>%
  add_flags()

importers_tbl <-
  importers %>%
  gt() %>%
  cols_label(country = 'Country',
             travel_balance_1519 = 'Billions of USD',
             travel_balance_1519_share = 'Percent of GDP') %>%
  tab_header(title = md('Economies with largest net expenditures on international travel'),
             subtitle = md('Average from 2015 to 2019')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics, and national sources') %>%
  fmt_currency(c(travel_balance_1519),
               decimals = 1) %>%
  fmt_number(c(travel_balance_1519_share),
              decimals = 1) %>%


  opt_all_caps() %>%
  opt_row_striping() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%
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
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.background.color = '#003A79',
    data_row.padding = px(5),
    source_notes.font.size = 12,
    heading.align = "left") %>%
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
  cols_align(
    align = "left",
    columns = everything()
  )

gt::gtsave(importers_tbl, 'tables/02_importers.png')
# Table 3: Large travel ---------------------------------------------------
`%notin%` <- Negate(`%in%`)
islands <- c('Turks and Caicos Islands',
             'Sint Maarten, Kingdom of the Netherlands',
             'Anguilla',
             'Cayman Islands',
             'Curaçao, Kingdom of the Netherlands',
             'French Polynesia')
in_sample <- services %>%
  select(country, travel_balance_1519_share, population_2019, gdp_2019) %>%
  filter(country %notin% islands) %>%
  arrange(desc(travel_balance_1519_share)) %>%
  head(38) %>%
  mutate(sample = 'Sample countries')

not_sample <-
  services %>%
  select(country, travel_balance_1519_share, population_2019, gdp_2019) %>%
  filter(country %in% islands) %>%
  arrange(desc(travel_balance_1519_share)) %>%
  mutate(sample = 'Not in sample')


full <-  bind_rows(in_sample, not_sample) %>%
    relocate(sample, .before = everything())


large_travel_table <-
  full %>%
  group_by(sample) %>%
    gt(groupname_col = 'sample') %>%
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
  summary_rows(
    groups = 'Sample countries',
    columns = where(is.numeric),
    fns = list(
      Mean = ~mean(., na.rm = TRUE),
      Median = ~median(.,na.rm = TRUE)),
    decimals = 1
  ) %>%

  opt_row_striping() %>%

  cols_label(travel_balance_1519_share = html('Travel balance (percent of GDP)<br>\tAverage 2015-2019'),
             population_2019 = html('Population<br>2019'),
             gdp_2019 = html('GDP<br>2019'),
             country = html('Country<br>'))%>%
  cols_align(
    align = "left",
    columns = c(country,
                   travel_balance_1519_share,
                   population_2019,
                   gdp_2019)
  ) %>%
  tab_header(title = md('Economies with large net revenues from international travel (in percent of GDP)')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics and national sources') %>%
  fmt_currency(c(gdp_2019)) %>%
  fmt_number(columns = c(population_2019),
             pattern = '{x} M',
             decimals = 1) %>%
  fmt_number(c(travel_balance_1519_share),
             decimals = 1) %>%
  fmt_currency(c(gdp_2019),
               decimals = 1) %>%
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%
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
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.background.color = '#003A79',
    data_row.padding = px(5),
    source_notes.font.size = 12,
    heading.align = "center",
    row_group.background.color = "#D0D3D4")


gtsave(large_travel_table,
       'tables/03_large_travel.png')
# Revenues ----------------------------------------------------------------
# Countries with net revenues from international travel exceeding 5 percent
#(list of 37 countries, with average net travel revenues)


revenues <-
  services_raw %>%
  filter(travel_revenues_1519_share > 5) %>%
  select(country, travel_revenues_1519) %>%
  arrange(desc(travel_revenues_1519)) %>%
  head(37)

revenues %>%
  gt()


# Current account composition ---------------------------------------------

# For the countries with travel surpluses exceeding 5 percent of GDP, the table would show average, median, 25th and 75th percentile, and range for the following variables:
# Current account balance 2015-19
# Travel balance 2015-19
# Balance of goods 2015-19
# Balance of services excl. travel 2015-19
# Primary income balance 2015-19
# Secondary income balance 2015-19
# International investment position in 2019


# Table 4 -----------------------------------------------------------------

tourism <- readRDS('data/tourism.rds')

our_summary(balance, c('current_account'))
names(tourism)
balance <- tourism %>%
  select(country,
         current_account_2015_2019,
         goods_balance_2015_2019,
         oil_balance_2015_2019,
         services_net_travel_balance_2015_2019,
         transportation_balance_2015_2019,
         travel_balance_2015_2019,
         primary_income_balance_2015_2019,
         secondary_income_balance_2015_2019) %>%
  rename_with(.cols = ends_with('2019'),
              .fn = ~stringr::str_replace(.x, '_2015_2019', ''))

balance %>%
  select(where(is.numeric)) %>%
  map_df(.f = ~ broom::tidy(skimr::skim(.x)), .id = "variable")

balance_summary <-
  balance %>%
  filter(travel_balance > 5) %>%
  select(where(is.numeric)) %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="category")  %>%
  print() %>%
  select(category, Observations = n, Min = min, Q25 = Q0.25, Mean = mean, Median = median, Q75 = Q0.75,
         Max = max, `Standard Deviation` = sd) %>%
  mutate(category = snakecase::to_sentence_case(category))

balance_tbl <-
  balance_summary %>%
  gt() %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  opt_row_striping() %>%
  tab_header(title = md('Tourism-dependent economies: stylized facts'),
             subtitle = md('Percent of GDP, 2015-19 averages')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics and national sources') %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%
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
  # data_color(
  #   columns = -c('category', 'Observations'),
  #   colors = scales::col_numeric(
  #     c("#f87274", "#ffeb84",  "#63be7b"),
  #     domain = range(balance_summary %>%
  #                      select(where(is.numeric), -Observations)))
  # ) %>%


  tab_options(
    column_labels.border.top.width = px(5),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.background.color = '#003A79',
    data_row.padding = px(5),
    source_notes.font.size = 12,
    heading.align = "center",
    row_group.background.color = "#D0D3D4")

gtsave(balance_tbl, 'tables/04_stylized_facts.png')

# Figures -----------------------------------------------------------------
library('ggtext')
library('showtext')
symmetric_limits <- function (x) {
  max <- max(abs(x))
  c(-max, max)
}
adjustment <- read_excel("data/tables.xlsx",
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
  labs(title = 'Tourism-dependent economies: <br>Median external adjustment in 2020',
       x = NULL,
       y = NULL)+
  ggthemes::theme_tufte() +
  theme(
    text =
      element_text(
        family = "Roboto",
        face = "plain",
        colour = "black",
        size = 16,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
    plot.title =  ggtext::element_textbox_simple(
      # font size "large"
      size = rel(1.2),
      color = "#003A79",
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = margin(b = 8)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      # font size "regular"
      hjust = 0,

      vjust = 1,
      margin = margin(b = 8)
    ),
    plot.caption = ggtext::element_textbox_simple(
      # font size "small"
      size = rel(0.8),
      vjust = 1,
      family = "Roboto Light",
      color = "#666666",
      hjust = 0,
      margin = margin(t = 8)
    ),
    plot.caption.position = "plot",
    plot.background =    element_rect(colour = "#FAFAFA"),
    axis.text.y = element_text(size = 14, hjust = 1),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_rect(color = "#FAFAFA")
  )

  ggsave(
    here::here('figures', 'adjustment.png'),
    width = 85 * (14 / 5),
    height = 53 * (14 / 5),
    units = 'mm',
    dpi = 300,
    type = 'cairo',
    bg = '#FAFAFA'
  )

