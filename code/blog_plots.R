
# Setup -------------------------------------------------------------------


librarian::shelf(tidyverse, ggrepel, ggbrookings, ggpubr)
europe <- readxl::read_xlsx('data/europe.xlsx') %>%
  rename(tourism_total = tour_tot,
         country_code = ccode,
         tourism_direct = tour_dir,
         growth_dif_2020 = grow_dif_20)



# Figure 1 ----------------------------------------------------------------
theme_set(theme_brookings())
europe %>%
  ggplot(aes(x = tourism_total, y = growth_dif_2020, label = country_code)) +
  geom_point(color = '#FF9E1B') +
  geom_smooth(method = 'lm',
              color = '#003A79',
              se = FALSE) +
  geom_label_repel() +
  scale_y_continuous(
                     limits = c(-13, -3),
                     breaks = seq(-3, -13, -2)) +
  scale_x_continuous(breaks = seq(0, 30, 5),
                     limits = c(0, 30)) +
  labs(title = 'Tourism share and COVID growth shock in the euro area',
       caption = '**Source:** Author’s calculations based on IMF, World Economic Outlook and national sources (growth shock)<br>World Travel and Tourism Council via the World Bank (tourism).',
       x = 'Tourism activities, total share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)') +
  ggpubr::stat_regline_equation(aes(label = ..rr.label..),
                                label.x = 22.75,
                                label.y = -10.1)

ggsave(here::here('emu_growth_surprise_total.png'),
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       device = ragg::agg_png(),
       dpi = 300,
       type = 'cairo',
       bg = 'transparent')


total <- add_logo('emu_growth_surprise_total.png', logo_position = 'bottom right', logo_path = 'hc', logo_scale = 6, height_padding = 0.03)
magick::image_write(total, 'emu_growth_surprise_total_logo.png')


europe %>%
  ggplot(aes(x = tourism_direct, y = growth_dif_2020, label = country_code)) +
  geom_point(color = '#FF9E1B') +
  geom_smooth(method = 'lm',
              color = '#003A79',
              se = FALSE) +
  geom_label_repel() +
  labs(title = 'Tourism share and COVID growth shock in the euro area<br>',
       caption = '**Source:** Author’s calculations based on IMF, World Economic Outlook and national sources (growth shock)<br> and World Travel and Tourism Council via the World Bank (tourism).',
       x = 'Tourism activities, direct share of GDP (average 2015-19)',
       y = 'Growth surprise in 2020 (relative to Jan. 2020 WEO)') +
  ggpubr::stat_regline_equation(aes(label = ..rr.label..),
                                label.x = 10,
                                label.y = -10.4) +
  scale_y_continuous(breaks = seq(-3, -13, -2),
                     limits = c(-13, -2)) +
  scale_x_continuous(breaks = seq(0, 15, 2)) +
  theme(plot.background = element_rect(fill = '#FAFAFA'))


ggsave(here::here('emu_growth_surprise_direct.png'),
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       device = ragg::agg_png(),
       dpi = 300,
       type = 'cairo',
       bg = '#FAFAFA')

direct <- add_logo('emu_growth_surprise_direct.png', logo_position = 'bottom right', logo_path = 'hc', logo_scale = 6, height_padding = 0.03)
magick::image_write(direct, 'emu_growth_surprise_direct_logo.png')
