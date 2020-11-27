#' Simulated reserving data
#'
#' A dataset describing the evolution of 100000 simulated claims
#'
#' @format A data frame with 123985 rows and 8 variables:
#' \describe{
#'   \item{development year}{number of years elapsed since the reporting of the claim}
#'   \item{close}{one when the claim closes in the current development year, zero otherwise}
#'   \item{payment}{one when there is a payment in the current development year, zero otherwise}
#'   \item{size}{total amount paid for the claim in the current development year}
#'   \item{X1}{claim covariate with 7 levels}
#'   \item{X2}{claim covariate with 6 levels}
#'   \item{reporting_year}{year in which the claim is reported}
#'   \item{calendar_year}{reporting_year + development_year - 1}
#' }
"reserving_data"
