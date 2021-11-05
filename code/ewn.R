librarian::shelf(tidyverse, ggrepel, ggbrookings)
theme_set(theme_brookings())

# scatter dnfay_l tour_tot_1519 if tour_tot_1519>16 & tour_tot!=. & date==2019, mlabel(ccode);
# * save graph tour_tot, replace;
# scatter dnfay_l tour_dir_1519  if tour_dir_1519>6 & tour_dir!=. & date==2019, mlabel(ccode);
# * save graph tour_dir, replace;
# scatter dnfay_l btv_1519 if btv_1519>5 & btv_1519!=. & date==2019, mlabel(ccode);
# * save graph btv, replace;
#
# label var neqy "Net equity position (FDI + portfolio) in pct of GDP";
# scatter dnfay_l neqy if neqy>20 & fc==0 & date==2019, mlabel(ccode);
# * save graph neqy_cr, replace;
# scatter dnfay_l neqy if neqy<-20 & fc==0 & date==2019, mlabel(ccode);
# * save graph neqy_deb, replace;

nfa <- haven::read_dta('data/nfa_clean.dta')
nfa %>%
  filter(em_eur == 1) %>%
  ggplot(aes(x = tour_tot, y = grow_dif))
nfa %>%
  filter(tour_tot_1519 > 16,
         date == 2019) %>%
  ggplot(aes(y = dnfay_l, x = tour_tot_1519,  label = ccode)) +
  geom_point(size = 1.5, color = "#003A79") +
  geom_label_repel() +
  labs(y = 'Change in IIP net of gold to GDP, 2019-20',
       x = 'Total share of tourism in GDP, 2015-19 avg') +
  scale_x_continuous(breaks = seq(0, 100, 25))
ggsave('ewn/plot1.png',
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       dpi = 300,
       type = 'cairo',
       bg = 'transparent')


nfa %>%
  filter(tour_dir_1519 > 16,
         date == 2019) %>%
  ggplot(aes(y = dnfay_l, x = tour_dir_1519,  label = ccode)) +
  geom_point(size = 1.5, color = "#003A79") +
  geom_label_repel() +
  labs(y = 'Change in IIP net of gold to GDP, 2019-20',
       x = 'Direct share of tourism in GDP, 2015-19 avg')
ggsave('ewn/plot2.png',
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       dpi = 300,
       type = 'cairo',
       bg = 'transparent')


nfa %>%
  filter(btv_1519 > 5,
         date == 2019) %>%
  ggplot(aes(y = dnfay_l, x = btv_1519,  label = ccode)) +
  geom_point(size = 1.5, color = "#003A79") +
  geom_label_repel() +
  labs(y = 'Change in IIP net of gold to GDP, 2019-20',
       x = 'Net intl. travel balance, 2015-19 avg (pct of GDP)')
ggsave('ewn/plot3.png',
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       dpi = 300,
       type = 'cairo',
       bg = 'transparent')


nfa %>%
  filter(neqy > 20,
         fc == 0,
         date == 2019) %>%
  ggplot(aes(y = dnfay_l, x = neqy,  label = ccode)) +
  geom_point(size = 1.5, color = "#003A79") +
  geom_label_repel() +
  labs(y = 'Change in IIP net of gold to GDP, 2019-20',
       x = 'Net equity position (FDI + portfolio) in pct of GDP')

ggsave('ewn/plot4.png',
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       dpi = 300,
       type = 'cairo',
       bg = 'transparent')

nfa %>%
  filter(neqy < -20,
         fc == 0,
         date == 2019) %>%
  ggplot(aes(y = dnfay_l, x = neqy,  label = ccode)) +
  geom_point(size = 1.5, color = "#003A79") +
  geom_label_repel() +
  labs(y = 'Change in IIP net of gold to GDP, 2019-20',
       x = 'Net equity position (FDI + portfolio) in pct of GDP')

ggsave('ewn/plot5.png',
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       dpi = 300,
       type = 'cairo',
       bg = 'transparent')

