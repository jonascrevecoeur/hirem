# Construct a hierarchical reserving model with three layers: close, payment and size.
# The layer size is only evaluated when payment == 1
model <- hirem(reserving_data) %>%
  split_data(observed = reserving_data %>% dplyr::filter(calendar_year <= 6)) %>%
  layer_glm('close', binomial(link = logit)) %>%
  layer_glm('payment', binomial(link = logit)) %>%
  layer_glm('size', Gamma(link = log),
            filter = function(data){data$payment == 1})

# Specify and fit a regression model for each of the layers
model <- fit(model,
             close = 'close ~ factor(development_year) + factor(X1) + factor(X2)',
             payment = 'payment ~ close + factor(development_year) + factor(X1) + factor(X2)',
             size = 'size ~ close + factor(development_year) + factor(X1) + factor(X2)')

update <- function(data) {
  data %>%
    dplyr::mutate(development_year = development_year + 1,
                  calendar_year = calendar_year + 1)
}

model <- register_updater(model, update)

simul <- simulate(model,
                  nsim = 5,
                  filter = function(data){dplyr::filter(data,
                                                       development_year <= 6,
                                                       close == 0)},
                  data = reserving_data %>% filter(calendar_year == 6))

rbns_estimate <- simul %>%
  dplyr::group_by(simulation) %>%
  dplyr::summarise(rbns = sum(size))

rbns_actual <- reserving_data %>%
  dplyr::filter(calendar_year > 6) %>%
  dplyr::summarise(rbns = sum(size))

rbns_estimate
rbns_actual

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




fitted <- fit(model$layers[[3]], model, 'size ~ 1')

filter <- dplyr::filter
