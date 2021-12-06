## ----require-----------------------------------------------------------------------------------------------------------------------------
require(hirem) 
require(tidyr)
require(magrittr)
require(dplyr)
require(tidyverse)

## ----data--------------------------------------------------------------------------------------------------------------------------------
data('reserving_data')
head(reserving_data)

## ----------------------------------------------------------------------------------------------------------------------------------------
upper_triangle <- reserving_data %>% filter(calendar_year <= 6)
lower_triangle <- reserving_data %>% filter(calendar_year > 6)

## ---- eval = FALSE-----------------------------------------------------------------------------------------------------------------------
#  model <- hirem(upper_triangle)

## ----------------------------------------------------------------------------------------------------------------------------------------
model <- hirem(upper_triangle) %>%
  layer_glm('close', binomial(link = cloglog)) %>%
  layer_glm('payment', binomial(link = logit)) %>%
  layer_glm('size', Gamma(link = log),
            filter = function(data){data$payment == 1})

## ----------------------------------------------------------------------------------------------------------------------------------------
model <- fit(model,
             close = 'close ~ factor(development_year) + factor(X1) + factor(X2)',
             payment = 'payment ~ close + factor(development_year) + factor(X1) + factor(X2)',
              size = 'size ~ close + factor(development_year) + factor(X1) + factor(X2)')

## ----------------------------------------------------------------------------------------------------------------------------------------
update <- function(data) {
  data %>%
    dplyr::mutate(development_year = development_year + 1,
                  calendar_year = calendar_year + 1)
}

model <- register_updater(model, update)

## ----------------------------------------------------------------------------------------------------------------------------------------
simul <- simulate(model,
                  nsim = 5,
                  filter = function(data){dplyr::filter(data,
                                                       development_year <= 6,
                                                       close == 0)},
                  data = reserving_data %>% dplyr::filter(calendar_year == 6))

## ----------------------------------------------------------------------------------------------------------------------------------------
head(simul)

## ----------------------------------------------------------------------------------------------------------------------------------------
rbns_estimate <- simul %>%
  dplyr::group_by(simulation) %>%
  dplyr::summarise(rbns = sum(size))

rbns_actual <- reserving_data %>%
  dplyr::filter(calendar_year > 6) %>%
  dplyr::summarise(rbns = sum(size))

rbns_estimate
rbns_actual

## ----------------------------------------------------------------------------------------------------------------------------------------

lower_triangle_predicted <- simul %>%
  dplyr::group_by(reporting_year, development_year) %>%
  dplyr::summarise(total_size = sum(size) / max(simulation)) %>%
  dplyr::arrange(development_year) %>%
  tidyr::pivot_wider(values_from = total_size, names_from = development_year) %>%
  dplyr::arrange(reporting_year)

lower_triangle_actual <- reserving_data %>%
  dplyr::filter(calendar_year > 6) %>%
  dplyr::group_by(reporting_year, development_year) %>%
  dplyr::summarise(total_size = sum(size)) %>%
  dplyr::arrange(development_year) %>%
  tidyr::pivot_wider(values_from = total_size, names_from = development_year) %>%
  dplyr::arrange(reporting_year)

lower_triangle_actual
lower_triangle_predicted


