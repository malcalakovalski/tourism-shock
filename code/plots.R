librarian::shelf(tidyverse, ggrepel, ggbrookings, patchwork)
tourism <-
  readr::read_rds('data/tourism.rds')
`%notin%` <- Negate(`%in%`)
theme_set(theme_brookings())

remove_countries <- tourism %>% filter(sample1 !=0) %>% pull(country)
manu <- tourism %>%
  filter(country %notin% remove_countries) %>%
  mutate(tourism_dependence = case_when(tourism_total > 20 ~ 'High tourism',
                                        tourism_total <= 20 ~ 'Other',
                                        TRUE ~ NA_character_))
manu %>%
  select(country_code, tourism_total, growth_dif_2020, tourism_dependence) %>% drop_na() %>%
  ggplot(aes(x = tourism_total, y = growth_dif_2020, color = tourism_dependence, label = country_code)) +
  geom_point(size = 2) +
  geom_label_repel(color = 'black') +
  scale_color_brookings() +
  labs(title = 'Tourism dependence and growth performance in 2020',
       tag = 'Figure 4',
       caption = '**Note**: horizontal axis measures the average direct share of tourism in GDP over the period 2015-19 (WTTC, 2020). The vertical axis measures the difference between the GDP growth rate in 2020 and its projected value as of January 2020.',
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)')


ggsave(
  here::here('figures', '04_scatter_grouped.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)


p1 <- tourism %>%
  ggplot(aes(x = tourism_total, y = growth_dif_2020,  label = country_code)) +
  geom_point(size = 2) +
  geom_label_repel(max.overlaps = 20) +
  labs(title = 'Tourism dependence and growth performance in 2020',
       tag = 'Figure 4',
       caption = '**Note**: horizontal axis measures the average direct share of tourism in GDP over the period 2015-19 (WTTC, 2020). The vertical axis measures the difference between the GDP growth rate in 2020 and its projected value as of January 2020.',
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)')

p2 <- tourism %>%
  filter(tourism_dependence == 'High tourism') %>%
  ggplot(aes(x = tourism_total, y = growth_dif_2020,  label = country_code)) +
  geom_point(size = 2, color = '#003A79') +
  geom_label_repel(max.overlaps = 20) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_color_brookings() +
  labs(title = 'High tourism',
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)')

p <- tourism %>%
  ggplot(aes(x = tourism_total, y = growth_dif_2020,  label = country_code, color = tourism_dependence)) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_label_repel(max.overlaps = 115, color = 'black') +
  scale_color_brookings(na.translate = FALSE) +
  labs(title = 'High tourism',
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)') +
  facet_grid(.~ tourism_dependence, scales = 'free', drop = TRUE)

gt<-ggplot_gtable(ggplot_build(p))
gt <- gt[, -3]
grid::grid.draw(gt)
other <- filter(tourism, tourism_dependence == 'Other')
  ggplot(other, aes(x = tourism_total, y = growth_dif_2020,  label = country_code)) +
  geom_point() +
  geom_label_repel() +theme_light()
  scale_x_continuous(limits = c(0, 25)) +
  labs(title = 'Other',
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)')
ggsave(
  here::here('figures', '04_scatter_grouped.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
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
    plot.background =    element_rect(colour = "transparent"),
    axis.text.y = element_text(size = 14, hjust = 1),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_rect(color = "transparent"),
    legend.position = 'top')


ggsave(
  here::here('figures', 'export_services.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)

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
  labs(title = 'Tourism-dependent economies: Composition of current account adjustment',
       subtitle = 'Percent of GDP, 2020 vs 2015-19 average (median values)',
       x = NULL,
       y = NULL,
       caption = '**Note**: The bars depicts the median difference across countries between the 2020 current account in percent of GDP (and each of its components) and their average values during the 5 preceding years (2015-19). Given the nature of the calculation the median adjustment values of the components will not add up to the median current account adjustment.<br>**Source:** Author’s calculations based on IMF, Balance of Payments Statistics and national sources') +
  theme_brookings()

ggsave(
  here::here('figures', 'adjustment.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
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
    plot.background =    element_rect(colour = "transparent"),
    axis.text.y = element_text(size = 14, hjust = 1),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_rect(color = "transparent"),
    legend.position = 'top')


# Passenger ---------------------------------------------------------------


tourism %>%
  select(country, bpas_2015_2019, bpas_2020) %>%
  arrange(desc(bpas_2015_2019)) %>%
  head(10) %>%
  drop_na() %>%
  pivot_longer(where(is.numeric)) %>%
  ggplot(aes(x = country, y = value, fill = name, label = round(value, 1),
             group = name)) +
  geom_col(position = position_dodge2()) +
  scale_y_continuous(limits = c(-1, 6.5), breaks = seq(-1, 6.5, 1)) +
  geom_hline(yintercept = 0) +
  scale_fill_brookings(palette = 'analogous1',
                       labels = c('2015-19', '2020')) +
  labs(title = 'Net revenues from international passenger transport (pct of GDP)',
       x = NULL,
       y = NULL)
ggsave(
  here::here('figures', 'passenger_transport.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = 'transparent'
)
