# •	Identify:
#   o	Salient features of countries dependent on tourism
# 	Eg: they are small, median GDP for countries with travel surplus of over 5% of travel surplus pre 2020
# 	Population
# 	Gdp per capita
# •	Are they poor? Don’t think so
# 	How much did they lose in 2020 in terms of travel? In relation to GDP
# •	How big of a current account did
# •	Ratio of travel to GDP in 2019 vs in 2020
# o	_diff variable in stata
# o	Balance of travel 2020 – Balance of travel 2019 (btv)
# o
# o	We could also look at dollar loss and scale by GDP 2019
# 	GDP collapsed in 2020
# 	10% GDP in 2020 is much less in dollar terms than 10% in 2019
# 	So taking the difference in the ratios to GDP reduces the amount of the loss.
# o	How did travel loss impact external account of countries?
#   	Look at Current Account
# 	Median adjustment in current account vs median travel loss
# •	Why is this?
#   •	What offsets this loss?
#   o	Goods
# o
# o	Primary income
# 	Hotels closed so reduced imports
# 	Expect improvement in primary income balance (where investment income is).
# 	Variable is called priminc
# o	Secondary income
# 	Remmitances
# 	Quite resilient during the crisis
# 	Summ, detail
# •	For Jimena:
#   •	Lets write a function that does sum
# o	Countries relying on travel
# 	If pre covid travel balance is larger than 5
# 	Check robustness
# 

tourism <- readRDS('data/tourism.rds')


tourism %>%
  filter(travel_balance_2015_2019 > 5) %>%
  select(country, population_2019,gdpd_2019) %>%
  summarise(country,  gdpd_2019) %>%
  ggplot(aes(x = fct_reorder(country, (gdpd_2019)),
             y  = gdpd_2019,
             fill = country)) +
  geom_col()  +
  coord_flip()
