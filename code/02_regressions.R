librarian::shelf(tidyverse, broom, estimatr, modelr)

tourism <- filter(tourism, sample == 0)

fit <- lm_robust(current_account_diff ~ current_account_2015_2019 + oil_balance_2015_2019 + travel_balance_2015_2019, data = tourism)

tidy(fit)
grid <- tourism %>%
  data_grid(current_account_diff, .model = fit) %>%
  add_predictions(fit)
grid
#> # A tibble: 5 x 5
#>   cut       lcarat color clarity  pred
#>   <ord>      <dbl> <chr> <chr>   <dbl>
#> 1 Fair      -0.515 G     VS2      11.2
#> 2 Good      -0.515 G     VS2      11.3
#> 3 Very Good -0.515 G     VS2      11.4
#> 4 Premium   -0.515 G     VS2      11.4
#> 5 Ideal     -0.515 G     VS2      11.4

ggplot(grid, aes(current_account_diff, pred)) +
  geom_point()
