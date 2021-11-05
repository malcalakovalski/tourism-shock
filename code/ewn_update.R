
# Setup -------------------------------------------------------------------


librarian::shelf(tidyverse, ggtext, ggbrookings, ggrepel)
ewn <- readxl::read_xlsx('data/ewn_blog.xlsx', sheet = 'bar_chart')

# Wrangling ---------------------------------------------------------------


creditors <-
  ewn %>%
  pivot_longer(Oil:NIE) %>%
  select(date, name, value) %>%
  mutate(type = 'creditors', .after = 'name')

debtors <-
  ewn %>%
  pivot_longer(`EUR debtors`:`Em. Eur`) %>%
  select(date, name, value) %>%
  mutate(type = 'debtors', .after = 'name')


ewn_long <-
  bind_rows(creditors, debtors) %>%
  rename(country_group = name)


# Bar chart ---------------------------------------------------------------
theme_set(theme_brookings())
# In terms of order, could we please have the US first among the debtors (so top) followed by EUR deficit and then the others by 2020 size (declining order)? On the creditor side, please put EUR creditors first (from the bottom), then oil, and then NIEs, then Japan, and then China (so all the Asia bars are next to each other). Thx again!

librarian::shelf(RColorBrewer)
ewn_long %>%
  filter(date >= 2010) %>%
  ggplot(aes(x = date, y = value, fill = factor(country_group,
                                                levels = c('US', 'EUR debtors', 'LAC', 'Em. Eur', 'Asia',  'CHN','JPN','NIE',  'Oil', 'EUR creditors')))) +

  geom_col() +
  geom_hline(yintercept = 0,
             size = 1,
             color = 'black') +
  scale_x_continuous(breaks = seq(2010, 2020, 1)) +
  scale_y_continuous(breaks = seq(-30, 30, 10),
                     limits = c(-30, 30)) +
  scale_fill_brewer(palette = 'Set3', direction = 1) +
  labs(title = 'Main external creditor and debtor regions (percent of world GDP)',
       x = NULL,
       y = NULL,
       tag = "FIGURE 2",
       caption = "<br>")  +
  theme(axis.line.x=element_blank())


ggsave(here::here('figures/ewn/bar.png'),
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       device = ragg::agg_png(),
       dpi = 300,
       type = 'cairo',
       bg = '#FAFAFA')

bar_logo <- ggbrookings::add_logo('figures/ewn/bar.png', logo_position = 'bottom right', logo_path = 'hc', logo_scale = 5, height_padding = 0.02)

magick::image_write(bar_logo, 'figures/ewn/bar_with_logo.png')

new_colors <- c("#0F78BA", "#5192C7", "#87ABD3", "#B1C5DE", "#DAE1E9", "#E02928", "#E02928", "#EA5D52", "#F28C7D", "#F1DBD7", "#F28C7D", "#F6B5A9")
ewn_long2 <-
  ewn_long %>%
  mutate(col_lab = case_when(country_group == 'Oil' ~ '#DAE1E9' ,
                             country_group == 'EUR creditors' ~ '#5192C7',
                             country_group == 'CHN' ~ '#87ABD3',
                             country_group == 'JPN' ~ '#B1C5DE',
                             country_group == 'NIE' ~ '#0F78BA',
                             country_group == 'Em. Eur' ~ '#E02928',
                             country_group == 'LAC' ~ '#EA5D52',
                             country_group == 'Asia' ~ '#F28C7D',
                             country_group == 'US' ~ '#F1DBD7',
                             country_group == 'EUR debtors' ~ 'red'))
final<-
  ewn_long2%>%
  filter(date=="2020")%>%              # Keep only 2017 value
  arrange(desc(country_group))%>%                # Inverse factor order (first is at the bottom of plot)
  mutate(                              # Create new column ypos and
    ypos=value      # fill with cumulative sum of invest for 2017
  )
ewn_long2 %>%
  mutate( name_lab = if_else(date == 2020, country_group, NA_character_),
          country_group = as.factor(country_group)) %>%
  filter(date >= 2010) %>%
  ggplot(aes(x = date, y = value, fill = country_group,
             label = country_group)) +
  geom_area() +
  geom_text_repel(data = final, aes(y = ypos, label = country_group), x = 2020)
  scale_x_continuous(breaks = seq(2010, 2020, 1),
                     expand = expansion(0.5, 0.5)) +
  scale_fill_brookings('categorical') +
  scale_color_brookings('categorical') +

  labs(title = 'Main external creditor and debtor regions (percent of world GDP)',
       x = NULL,
       y = NULL) +
  theme(legend.position = 'none')
# Scatterplot -------------------------------------------------------------

ewn_scatter <- readxl::read_xlsx('data/ewn_blog.xlsx', sheet = 'Scatter_data')
update_geom_defaults('point',
                     list(size = 2))
ewn_scatter %>%
  ggplot(aes(x = neqy_l, y = dnfay, label = ccode)) +
  geom_vline(xintercept = 0, color = '#CCCCCC') +
  geom_hline(yintercept = 0, color = '#CCCCCC') +
  geom_point(color = '#FF9E1B') +
  geom_smooth(method = 'lm', se = FALSE, color = '#003A79', linetype = 'dashed') +
  geom_label_repel() +

  scale_x_continuous(breaks = seq(-100, 250, 50),
                     limits = c(-100, 250)) +
  scale_y_continuous(breaks = seq(-30, 40, 10),
                     limits = c(-30, 40)) +

  labs(title = 'Net equity positions and change in net IIP<br>',
       x = 'Net FDI + Portfolio equity position, 2019',
       y = 'Change in net IIP 2019-2020 (percent of 2020 GDP)',
       tag = "FIGURE 1",
       caption = "<br><br>") +
  theme(axis.line.x=element_blank())

ggsave(here::here('figures/ewn/scatter.png'),
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       device = ragg::agg_png(),
       dpi = 300,
       type = 'cairo',
       bg = '#FAFAFA')



# Tables ------------------------------------------------------------------

ewn <- readxl::read_xlsx('data/stylized facts_EWN_blog.xlsx')
