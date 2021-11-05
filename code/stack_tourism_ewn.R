librarian::shelf(tidyverse, tsibble)

tourism_direct <-
  readxl::read_xlsx('data/tourism_EWN.xlsx', sheet = 'Tourism_Direct_GDP') %>%
  select(Country, ccode, ifs_code, where(is.numeric), -`Indicator Id`) %>%
  pivot_longer(`2015`:`2019`,
               names_to = 'year',
               values_to = 'tourism_direct_share')

tourism_total <-
  readxl::read_xlsx('data/tourism_EWN.xlsx', sheet = 'Tourism_Total_GDP') %>%
  select(Country, ccode, ifs_code, where(is.numeric), -`Indicator Id`) %>%
  pivot_longer(`2015`:`2019`,
               names_to = 'year',
               values_to = 'tourism_total_share')


travel_balance <-
  readxl::read_xlsx('data/tourism_EWN.xlsx', sheet = 'Travel_balance') %>%
  pivot_longer(`2005`:`2020`,
               names_to = 'year',
               values_to = 'travel_balance')

keys <- c('ifs_code', 'year', 'Country', 'ccode')

tourism <-
  left_join(travel_balance, tourism_total, by = keys) %>%
  left_join(tourism_direct, by = keys)  %>%
  mutate(year = as.double(year)) %>%
  as_tsibble(index = year, key = c(Country, ccode, ifs_code)) %>%
  append_row(n = -35L) %>%
  arrange(ifs_code, year) %>%
  select(-variable)

openxlsx::write.xlsx(tourism , 'data/tourism_EWN_stacked.xlsx')
