
# Setup -------------------------------------------------------------------

librarian::shelf('tidyverse', 'stringr')

tourism_raw <- haven::read_dta("data/tourism_CA_dataset.dta")


# Cleaning names ----------------------------------------------------------

## Example
var_name <- "btv_1519"
stringr::str_replace(var_name, pattern = '1519', replacement = '2015_2019')

tourism_raw %>%
  # Want to change variables ending in 1519 to be 2015_2019
  rename_with(.cols = ends_with('1519'),
              .fn = ~ stringr::str_replace(string = .x,
                                           pattern = '1519',
                                           replacement = '2015_2019')) %>%
  rename_with(.cols = ends_with('1419'),
              .fn = ~ stringr::str_replace(string = .x,
                                           pattern = '1419',
                                           replacement = '2014_2019')) %>%
  rename_with(.cols = ends_with('20') & !contains('2020'),
              .fn = ~ stringr::str_replace(string = .x,
                                           pattern = '20',
                                           replacement = '2020')) %>%
  rename_with(.cols = ends_with('19') & !contains('2019'),
              .fn = ~ stringr::str_replace(string = .x,
                                           pattern = '19',
                                           replacement = '2019')) %>%
  # Tourism, total, etc shouldn't be abbreviated
  rename_with(.cols = contains('tour'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'tour',
                                  replacement = "tourism")) %>%
  rename_with(.cols = contains('tot'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'tot',
                                  replacement = "total")) %>%
  rename_with(.cols = contains('pop') & !contains('population'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'pop',
                                  replacement = "population")) %>%
  rename_with(.cols = contains('dens'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'dens',
                                  replacement = "density")) %>%
  rename_with(.cols = contains('grow'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'grow',
                                  replacement = "growth")) %>%
  rename_with(.cols = contains('ca_'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'ca',
                                  replacement = "current_account")) %>%
  rename_with(.cols = contains('boil'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'boil',
                                  replacement = "oil_balance")) %>%
  # Travel related variables
  rename_with(.cols = contains('bsecin'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'bsecin',
                                  replacement = "secondary_income_balance"))  %>%
  rename_with(.cols = contains('bprin'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'bprin',
                                  replacement = "primary_income_balance"))  %>%
  rename_with(.cols = contains('btt'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'btt',
                                  replacement = "transportation_travel_balance"))  %>%
  rename_with(.cols = contains('btv'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'btv',
                                  replacement = "travel_balance"))  %>%
  rename_with(.cols = contains('btr'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'btr',
                                  replacement = "transportation_balance"))  %>%
  rename_with(.cols = contains('bg'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'bg',
                                  replacement = "goods_balance"))  %>%
  rename_with(.cols = contains('bsoth'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'bsoth',
                                  replacement = "services_net_travel_balance"))  %>%
  rename_with(.cols = contains('bs'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'bs',
                                  replacement = "services_balance"))  %>%
  rename_with(.cols = contains('tv_cr'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'tv_cr',
                                  replacement = "travel_credit"))  %>%



  rename_with(.cols = contains('fcast'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'fcast',
                                  replacement = "forecast"))  %>%
  rename_with(.cols = contains('manuf'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'manuf',
                                  replacement = "manufacturing"))  %>%
  rename_with(.cols = ends_with('_dif'),
              .fn = ~ str_replace(string = .x,
                                  pattern = '_dif',
                                  replacement = "_diff"))  %>%
  rename(country_code = ccode,
         tourism_direct_share = tourism_dir,
         life_expectancy = life_exp,
         advaned_economies = ae,
         africa = afr,
         caribbean = carib,
         latin_america = latam,
         western_hemisphere = weshem,
         transportation = trans,
         transportation_3 = trans3,
         service_2014_2019 = serv_2014_2019

  )


