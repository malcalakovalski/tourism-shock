# Description -------------------------------------------------------------
# This script will prepare background tables for the paper. I have in mind tables showing the countries with the largest receipts and expenditures for international travel, in absolute terms and as a share of GDP (pre-COVID).
#
# Maybe we can use an average over 2015-19, since this is what we use in the empirical analysis.
# And then tables showing the largest decline in net revenues in relation to GDP in 2020.
#
# The underlying data is in the attached Excel file. Look at the last sheet on the right, where I put a number of the relevant series. We could use something like the top 5 countries, without including the euro area (since we have the member countries separately).


# Setup -------------------------------------------------------------------
library('tidyverse')
library(readxl)
services_raw <- read_excel("data/BOPS data_services.xlsx",
                                 sheet = "Travel_net_USD", skip = 4)

# Cleaning ----------------------------------------------------------------

services <- services_raw %>%
  select(country, starts_with('travel_balance'), population) %>%
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

## Largest in absolute terms
(largest_absolute <-
  net %>%
  arrange(desc(net_travel_balance)) %>%
  head(5))

## Largest share
net %>%
  arrange(desc(net_travel_balance_share)) %>%
  head(5)

## Largest share with population greater than 2 million
net %>%
  filter(population > 2) %>%
  arrange(desc(net_travel_balance_share)) %>%
  head(5)


## Smallest in absolute terms
net %>%
  arrange(net_travel_balance) %>%
  head(5)

## Smallest share
net %>%
  arrange(net_travel_balance_share) %>%
  head(5)

## Smallest share with population greater than 2 million
net %>%
  filter(population > 2) %>%
  arrange(net_travel_balance_share) %>%
  head(5)


