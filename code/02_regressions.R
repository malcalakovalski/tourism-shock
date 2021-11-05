librarian::shelf(tidyverse, broom, estimatr, modelr, gt, modelsummary, ggbrookings)
tourism <- readr::read_rds('data/tourism.rds')
tourism <- filter(tourism, sample == 0)
main_model <- function(.data){
  lm(current_account_deviation_2020 ~ current_account_2015_2019 + oil_balance_2015_2019 + travel_balance_2015_2019, data = .data)
}

fit1 <- main_model(tourism)
fit2 <- main_model(filter(tourism, country != 'Kuwait'))
fit3 <- main_model(filter(tourism, country != 'Kuwait', economy == 'advanced'))
fit4 <- main_model(filter(tourism, country != 'Kuwait', economy == 'emerging'))
fit5 <- main_model(filter(tourism, country != 'Kuwait', travel_balance_2015_2019 > 0))
fit6 <- main_model(filter(tourism, country != 'Kuwait', travel_balance_2015_2019 < 0))



models <- list(fit1, fit2, fit3, fit4, fit5, fit6)

coef_names <-
  c('current_account_2015_2019' = 'Current account balance (pct of GDP, 2015-19 average)',
    'oil_balance_2015_2019' = 'Oil balance (pct of GDP, 2015-19 average)',
    'travel_balance_2015_2019' = 'Net revenues from international travel (pct of GDP, 2015-19 average)',
    '(Intercept)' = 'Constant')

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      html("Observations"),             0,
  "r.squared", html("R@2~"), 2)



main_model <- modelsummary(models, vcov = 'stata',
                           gof_map = gm,
                           escape = FALSE,
                           gof_omit = 'Log.Lik.|F|Std. Errors|AIC|BIC|Sigma|Deviance|Statistics|p', coef_map = coef_names,
 estimate = "{estimate}{stars}",
 output = "gt",
 statistic = 'statistic',
 fmt = 2,
 stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))
tbl6 <-
  main_model %>%
    text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>%
        str_replace_all("~",
                        "</sup>") }
  ) %>%
  cols_label("Model 1" = html('All countries<br>(1)'),
             "Model 2" = html('All countries excluding Kuwai<br>(2)'),
             "Model 3" = html('Advanced economies<br>(3)'),
             "Model 4" = html('Emerging markets excluding Kuwait<br>(4)'),
             "Model 5" = html('Positive travel balance<br>(5)'),
             "Model 6" = html('Negative travel balance<br>(6)'))%>%

  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%

  opt_row_striping() %>%
  tab_header(title = md('Table 6: Current account balance in percent of GDP, 2020 (deviation from pre-crisis forecast)')) %>%
  tab_source_note(md('**Note**: dependent variable is the difference between the current account balance in percent of GDP in 2020 and its October 2019 WEO forecast for the same year (2020).  t statistics in parentheses, robust standard errors. * p<0.10, ** p<0.05,  *** p<0.01.')) %>%
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

gtsave(tbl6, 'tables/06_ca_regression.png')


# Growth regression 1 -----------------------------------------------------


g1 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency, data = tourism)
g2 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total, data = tourism)
g3 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation, data = tourism)
g4 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = tourism)
g5 <-lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = filter(tourism, economy == 'advanced'))
g6 <-lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_total + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = filter(tourism, economy == 'emerging'))


growth_models1 <- list(g1, g2, g3, g4, g5, g6)

coef_names <- c('cases1000' ="COVID cases in 2020 (per 1000 population)",
                'deaths1000' ="COVID deaths in 2020 (per 1000 population)",
                'stringency' ="Oxford stringency index (average 2020)",
                'tourism_total'= 'Tourism activity (total share of GDP)',
                'lgdppc' = 'log GDP per capita, 2019',
                'lpopulation' ="log population, 2019",
                'agr_2014_2019' = 'VA share of agriculture (average 2014-19)',
                'manufacturing_2014_2019' = 'VA share of manufacturing  (average 2014-19)',
                '(Intercept)' = 'Constant')
growth_reg1 <-
  modelsummary(growth_models1,
             vcov = 'stata',
             gof_omit = 'R2 Adj.|Log.Lik.|F|Std. Errors|AIC|BIC|Sigma|Deviance|Statistics|p',
             gof_map = gm,
             coef_map = coef_names,
             estimate = "{estimate}{stars}",
             output = "gt",
             statistic = 'statistic',
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             fmt = 2) %>%
  cols_label("Model 1" = html('All countries<br>(1)'),
             "Model 2" = html('All countries<br>(2)'),
             "Model 3" = html('All countries<br>(3)'),
             "Model 4" = html('All countries<br>(4)'),
             "Model 5" = html('Advanced economies<br>(5)'),
             "Model 6" = html('Emerging markets<br>(6)')) %>%
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>%
        str_replace_all("~",
                        "</sup>") }
  ) %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  opt_row_striping() %>%
  tab_header(title = md('Table 8: Growth Regressions (I)')) %>%
  tab_source_note(md('**Note**: Dependent variable is GDP growth in 2020 minus the pre-COVID growth forecast (from WEO, 2020). t-statistics in parentheses, robust standard errors. * p<0.10, ** p<0.05,  *** p<0.01.
')) %>%
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

gtsave(growth_reg1, 'tables/08_growth_regression_a.png')

# Growth regressions table 2 ----------------------------------------------



growth_models2 <- function(.data){
 lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + tourism_direct_share + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = .data)
}


m1 <- growth_models2(tourism)
m2 <- growth_models2(filter(tourism, gdpd_2019 > 100))
m3 <- growth_models2(filter(tourism, gdpd_2019 < 100))
m4 <- growth_models2(filter(tourism, tourism_direct_share > 5))
m5 <- growth_models2(filter(tourism, tourism_direct_share < 5))
m6 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019 + bpas_2015_2019 + tourism_direct_share, data = tourism)
m7 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019 + bpas_2015_2019 + tourism_direct_share, data = filter(tourism, economy == 'emerging'))
m8 <- lm(growth_dif_2020 ~ cases1000 + deaths1000 + stringency + travel_balance_2015_2019 + lgdppc + lpopulation + agr_2014_2019 + manufacturing_2014_2019, data = tourism)




growth_models2 <- list(m1, m2, m3, m4, m5, m6, m7, m8)


coef_names <- c('cases1000' ="COVID cases in 2020 (per 1000 population)",
                'deaths1000' ="COVID deaths in 2020 (per 1000 population)",
                'stringency' ="Oxford stringency index (average 2020)",
                'tourism_total'= 'Tourism activity (total share of GDP)',
                'tourism_direct_share' = 'Tourism activity (direct share of GDP)',
                "bpas_2015_2019" = "Net revenues from international passenger transport (pct of GDP, 2015-19)",
                'travel_balance_2015_2019' = 'Net revenues from international travel (pct of GDP, 2015-19)',
                'lgdppc' = 'log GDP per capita, 2019',
                'lpopulation' ="log population, 2019",
                'agr_2014_2019' = 'VA share of agriculture (average 2014-19)',
                'manufacturing_2014_2019' = 'VA share of manufacturing  (average 2014-19)',
                '(Intercept)' = 'Constant')


growth_reg2 <-
  modelsummary(growth_models2,
             vcov = 'stata',
             gof_map = gm,
             gof_omit = 'R2 Adj.|Log.Lik.|F|Std. Errors|AIC|BIC|Sigma|Deviance|Statistics|p',
             coef_map = coef_names,
             estimate = "{estimate}{stars}",
             output = "gt",
             statistic = 'statistic',
             fmt = 2,
             stars = c('*' = .1, '**' = 0.05, '***' = .01)) %>%
  cols_label("Model 1" = html('All countries<br>(1)'),
             "Model 2" = html('GDP above $100 billion in 2019<br>(2)'),
             "Model 3" = html('GDP below $100 billion in 2019<br>(3)'),
             "Model 4" = html('Tourism direct share above 5 pct of GDP<br>(4)'),
             "Model 5" = html('Tourism direct share below 5 pct of GDP<br>(5)'),
             "Model 6" = html('All countries<br>(6)'),
             "Model 7" = html('All countries<br>(7)'),
             "Model 8" = html('All countries<br>(8)'))%>%
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      str_replace_all(x,
                      pattern = "@",
                      replacement = "<sup>") %>%
        str_replace_all("~",
                        "</sup>") }
  ) %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  opt_row_striping() %>%
  tab_header(title = md('Table 9. Growth Regressions (II)')) %>%
  tab_source_note(md('**Note**: Dependent variable is GDP growth in 2020 minus the pre-COVID growth forecast (from WEO, 2020). t-statistics in parentheses, robust standard errors. * p<0.10, ** p<0.05,  *** p<0.01.
')) %>%
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

gtsave(growth_reg2, 'tables/09_growth_regression_b.png')
