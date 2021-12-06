# The `hirem` package 

This is the source code for the  `hirem` package, which is currently under development.

## Installation
To install `hirem` from GitHub you will need `devtools`:

``` r
install.packages('devtools')
devtools::install_github('jonascrevecoeur/hirem')
```

## Overview
The `hirem` package offers tools for implementing hierarchical reserving models, introduced in the paper [Crevecoeur, J., Robben J., Antonio, K., A hierarchical reserving model for reported non-life insurance claims, (2021)](https://arxiv.org/abs/1910.12692).

The `hirem` package constructs hierarchical reserving models by sequentially adding new layers. An example of a three component hierarchical model:

``` r
require(hirem)
data("reserving_data")

model <- hirem(reserving_data %>% dplyr::filter(calendar_year <= 6)) %>%
  layer_glm('close', binomial(link = logit)) %>%
  layer_glm('payment', binomial(link = logit)) %>%
  layer_glm('size', Gamma(link = log),
            filter = function(data){data$payment == 1})
            
model <- fit(model,
             close = 'close ~ factor(development_year)',
             payment = 'payment ~ close + factor(development_year)',
             size = 'size ~ close + factor(development_year)')
            
```

The package is still under development. Currently supported layers are:

* `layer_glm`: Estimates a layer using a generalized linear model
* `layer_gbm`: Estimates a layer using a gradient boosting model

Supported distributions are:

* `binomial`
* `gaussian`
* `gamma`

To use the `gamma` distribution in `layer_gbm` you require an experimental version of the package `gbm` that implements the `gamma` distribution. See
https://github.com/harrysouthworth/gbm.

Use the links below to view examples and vignettes in your current browser:
Vignette: https://htmlpreview.github.io/?https://github.com/jonascrevecoeur/hirem/blob/master/inst/doc/hrm.html
Connection between aggregate and individual reserving: https://htmlpreview.github.io/?https://github.com/jonascrevecoeur/hirem/blob/master/Examples/Connection_individual_aggregate.html
Principles of the simulation machine: https://htmlpreview.github.io/?https://github.com/jonascrevecoeur/hirem/blob/master/Examples/Working-principles-of-the-simulation-machine.html
Application on a simulated portfolio: https://htmlpreview.github.io/?https://github.com/jonascrevecoeur/hirem/blob/master/Examples/Scenario-testing-on-simulated-portfolio.html