
# Setup -------------------------------------------------------------------
library('tidyverse')
library(readxl)
librarian::shelf(tidyverse, readxl, countrycode, glue, forcats, gt, scales)
services_raw <- read_excel("data/BOPS_data_services.xlsx",
                                 sheet = "Travel_net_USD", skip = 3)


# Theme -------------------------------------------------------------------

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
      column_labels.border.top.color = "transparent",
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      heading.background.color = '#003A79',
      data_row.padding = px(5),
      source_notes.font.size = 12,
      heading.align = "center",
      row_group.background.color = '#D0D3D4',
      ...)

}

# Cleaning ----------------------------------------------------------------

services <-
  services_raw %>%
  select(country, starts_with('travel_balance'), population, gdp) %>%
  filter(country != 'Euro Area')

# Table 3: Large travel ---------------------------------------------------
`%notin%` <- Negate(`%in%`)
islands <- c('Turks and Caicos Islands',
             'Sint Maarten, Kingdom of the Netherlands',
             'Anguilla',
             'Cayman Islands',
             'Curaçao, Kingdom of the Netherlands',
             'French Polynesia')
in_sample <- services %>%
  select(country, travel_balance_1519_share, population, gdp) %>%
  filter(country %notin% islands) %>%
  arrange(desc(travel_balance_1519_share)) %>%
  head(38) %>%
  mutate(sample = 'Sample countries')

not_sample <-
  services %>%
  select(country, travel_balance_1519_share, population, gdp) %>%
  filter(country %in% islands) %>%
  arrange(desc(travel_balance_1519_share)) %>%
  mutate(sample = 'Not in sample')


full <-  bind_rows(in_sample, not_sample) %>%
  relocate(sample, .before = everything())


large_travel_table <-
  full %>%
  group_by(sample) %>%
  gt(groupname_col = 'sample',
     rowname_col = 'country') %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  summary_rows(
    groups = TRUE,
    columns = where(is.numeric),
    fns = list(
      Mean = ~mean(., na.rm = TRUE),
      Median = ~median(.,na.rm = TRUE)),
    decimals = 1,
  ) %>%

  opt_row_striping() %>%

  cols_label(travel_balance_1519_share = html('Travel balance<br>(percent of GDP)<br>Average 2015-2019'),
             population = html('Population<br>millions<br>2019'),
             gdp = html('GDP<br>billions<br>2019'),
             country = html('Country<br>'))%>%
  cols_align(
    align = "left",
    columns = c(country,
                travel_balance_1519_share,
                population,
                gdp)
  ) %>%
  tab_header(title = md('Table 1: Largest Exporters of International Travel Services in percent of GDP')) %>%
  tab_source_note("**Source:** Author's calculations based on IMF, Balance of Payments Statistics and national sources.") %>%
  fmt_currency(c(gdp)) %>%
  fmt_number(columns = c(population),
             decimals = 1) %>%
  fmt_number(c(travel_balance_1519_share),
             decimals = 1) %>%
  fmt_currency(c(gdp),
               decimals = 1) %>%
  my_theme()

gtsave(large_travel_table,
       'tables/01_large_travel.png')
# Net travel balance ------------------------------------------------------
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
my_theme()


# Table 2: Exporters and importers ------------------------------------------------------
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
  my_theme()

exporters_tbl
gt::gtsave(exporters_tbl, 'tables/01_exporters.png')






# Table 2: Importers ------------------------------------------------------

importers <-
  services %>%
  select(country, travel_balance_1519, travel_balance_1519_share) %>%
  arrange(travel_balance_1519) %>%
  head(10) %>%
  mutate(travel_balance_1519 = -travel_balance_1519,
         travel_balance_1519_share = -travel_balance_1519_share) %>%
  add_flags()

importers_tbl <-
  importers %>%
  gt() %>%
  cols_label(country = 'Country',
             travel_balance_1519 = 'Billions of USD',
             travel_balance_1519_share = 'Percent of GDP') %>%
  tab_header(title = md('Table 2: Economies with largest net expenditures on international travel'),
             subtitle = md('Average from 2015 to 2019')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics, and national sources') %>%
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
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  my_theme()

gt::gtsave(importers_tbl, 'tables/02_importers.png')

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

gtsave(balance_tbl, 'tables/03_ca_2019.png')


# Table 5 -----------------------------------------------------------------
islands <- c('Turks and Caicos Islands',
             'Sint Maarten, Kingdom of the Netherlands',
             'Anguilla',
             'Cayman Islands',
             'Curaçao, Kingdom of the Netherlands',
             'French Polynesia')
creditor_position <-
  tourism %>%
  select(country, tourism_total, iip_2019, gdp_per_capita_2019,
         travel_balance = travel_balance_2015_2019,

         tourism_travel) %>%
  mutate(tourism_type = case_when(travel_balance >= 5 ~ 'High tourism revenues',
                                  travel_balance > 0  ~ 'Positive tourism revenues',
                                  travel_balance <= 0 ~ 'Negative tourism revenues'),
         .after = 'country') %>%
  mutate(iip_2019 = 100 * iip_2019)



creditor_position %>%
  select(tourism_type, iip_2019, gdp_per_capita_2019) %>%
  group_by(tourism_type) %>%
  skimr::skim()
  count(tourism_type)



creditor_stats <-
  creditor_position %>%
  group_by(tourism_type) %>%

  summarise(across(
    # Columns to summarise
    .cols = c(
      gdp_per_capita_2019,
      iip_2019
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



creditor_stats <-
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
  creditor_stats %>%
  mutate(obs = c(35, 81, 66, 37, 81, 67)) %>%
  gt(
     rowname_col = 'tourism_type') %>%
  tab_header(title = md('Size and creditor position: tourism-dependent economies and other economies'),
             subtitle = md('Percent of GDP, 2015-19 averages')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics and national sources') %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(obs),
             decimals = 0) %>%
  tab_row_group(label = html('GDP per capita<br>(2019)'),
                rows = variable == 'gdp_per_capita_2019') %>%
  tab_row_group(label = html('International investment position<br>(percent of GDP, 2019)'),
                rows = variable == 'iip_2019') %>%

  cols_hide(variable) %>%
  fmt_number(columns = where(is.numeric),
             decimals = 0,
             drop_trailing_zeros = TRUE) %>%
  my_theme()

gtsave(creditor_tbl, 'tables/05_creditor_position.png')



# Table 5B: Economies with largest net international transportation revenues in percent of GDP ------------------------------------------------
transportation_tbl <- tourism %>%
  select(country, transportation_balance_2015_2019) %>%
  arrange(desc(transportation_balance_2015_2019)) %>%
  head(10) %>%
  add_flags() %>%
  gt() %>%
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
  ) %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
fmt_number(c(transportation_balance_2015_2019),
           decimals = 1) %>%

  opt_row_striping() %>%

  cols_label(transportation_balance_2015_2019 = html('Transportation balance<br>(percent of GDP)<br>\tAverage 2015-2019'),
             country = html('Country<br>'))%>%
  cols_align(
    align = "left",
    columns = c(country, transportation_balance_2015_2019)
  )

temp %>%
  tab_header(title = md('Economies with large net revenues from international travel<br>(in percent of GDP)')) %>%
  tab_source_note(md('**Source**: Author’s calculation based on IMF, Balance of Payments Statistics, and national sources')) %>%
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
    heading.align = "left",
    row_group.background.color = "#D0D3D4")

gtsave(transportation_tbl, 'tables/05_transportation_revenues.png')
# Table 6: Tourism Share--------------------------------------------------------

tourism_share <- readxl::read_xlsx('data/tables.xlsx',
                                   sheet = 'tourism_tidy')

share_tbl <-
  tourism_share %>%
  gt(groupname_col   = 'tourism',
     rowname_col = 'country_group') %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%

  opt_row_striping() %>%
  tab_header(title = md('Share of tourism in GDP: stylized facts'),
             subtitle = md('Percent of GDP, 2015-19 averages')) %>%
  tab_source_note('Source: Author’s calculation based on IMF, Balance of Payments Statistics, and national sources') %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(obs),
             decimals = 0) %>%
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

gtsave(share_tbl, 'tables/06_tourism_share.png')


# Table 7: Growth surprise ------------------------------------------------


growth <- readxl::read_xlsx('data/tables.xlsx',
                                   sheet = 'surprise_tidy')

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
  tab_header(title = md('Growth Surprise in 2020')) %>%
  tab_source_note(md('**Note**: High tourism countries are those with an average surplus in international travel above 5 percent of GDP during 2015-19 (see Table 1). For a few countries balance of payments data on international travel is not available: hence the observations in “high tourism” and “other countries” are fewer than those for all countries.<br><br>
**Source**: Author’s calculation based on IMF (2020), (2021) and national sources.
')) %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  fmt_number(c(obs),
             decimals = 0) %>%
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
      cell_text(weight = "bold"))) %>%
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



gtsave(surprise_tbl, 'tables/07_growth_surprise.png')
# Figures -----------------------------------------------------------------
library('ggtext')
library('showtext')
symmetric_limits <- function (x) {
  max <- max(abs(x))
  c(-max, max)
}
adjustment <- readxl::read_excel("data/tables.xlsx",
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
  labs(title = 'Tourism-dependent economies: <br>Median external adjustment in 2020',
       x = NULL,
       y = NULL,
       caption = '**Source:** Author’s calculations based on IMF, Balance of Payments Statistics and national sources')+
  theme_brookings()

  ggsave(
    here::here('figures', 'adjustment.png'),
    width = 85 * (14 / 5),
    height = 53 * (14 / 5),
    units = 'mm',
    dpi = 300,
    type = 'cairo',
    bg = '#FAFAFA'
  )


# Fig2: Services share ----------------------------------------------------


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



  p1 <- credit %>%
    pivot_longer(-c(year, gdp)) %>%
    ggplot(aes(x = year, y = value, fill = name, label = signif(value, 2))) +
    geom_col(
             position = 'stack')



p1 +
    ggbrookings::scale_fill_brookings('alternative',
                                      name = '',
                                      labels = c('Other services',
                                                 'Transport',
                                                 'Travel')) +
  guides(fill = guide_legend(reverse=T))+
    labs(title = 'Export of Services',
         subtitle = 'Percent of world GDP',
         x = NULL,
         y = NULL) +
  scale_x_continuous(breaks = seq(2005, 2020, 2)) +

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
      panel.background = element_rect(color = "#FAFAFA"),
      legend.position = 'top')


ggsave(
  here::here('figures', 'export_services.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = '#FAFAFA'
)


# Passenger ---------------------------------------------------------------

library('countrycode')
library('glue')
library('gt')
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
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
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
  tab_header(title = md('Economies with largest net international transportation revenues in percent of GDP')) %>%
  tab_source_note(md("**Note**: economies denoted with an asterisk do not provide a breakdown of transport revenues into passenger revenues, freight, and others.<br>**Source**: author's calculations based on IMF Balance of Payments statistics and naitonal sources.")) %>%
  fmt_number(where(is.numeric),
             decimals = 1) %>%
  cols_label(bpas_2015_2019 = md('Passenger Transport<br>(pct of GDP)')) %>%
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(5)),
      #Make text bold
      cell_text(weight = "bold")
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
gtsave(passenger_tbl, 'tables/05_transportation_and_passenger_revenues.png')
