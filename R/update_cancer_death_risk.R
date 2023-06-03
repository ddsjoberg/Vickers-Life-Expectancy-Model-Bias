#' Combine Risk Estimates
#'
#' Provided a risk estimate of death from cancer and an estimate for death
#' from other causes, this function will combine the two risks assuming
#' an exponential distribution for both.
#' @noRd

update_cancer_death_risk <- function(risk_cancer_death, risk_other_cause, time) {
  lambda_cancer_death <- prob_to_exp_lambda(risk_cancer_death, time = time)
  lambda_other_cause <- prob_to_exp_lambda(risk_other_cause, time = time)

  updated_cancer_death_risk <-
    lambda_cancer_death / (lambda_cancer_death + lambda_other_cause) *
    (1 - exp(-(lambda_cancer_death + lambda_other_cause) * time))

  updated_cancer_death_risk
}

#' Convert Prob. to an Exponential lambda Parameter
#'
#' Function takes a vector of probability estimates and a time frame,
#' e.g. probability of death within 10 years, and converts the probability
#' into an exponential distribution lambda parameter. The lambda parameter
#' is the *rate* parameter, e.g. `f(x) = lambda * exp(-lambda * x)`
#'
#' @param p  numeric vector of probability estimates
#' @param time a single numeric time that is the time frame for the probability
#' in `p=`
#' @noRd
prob_to_exp_lambda <- function(p, time = 1) {
  -log(1 - p) / time
}
