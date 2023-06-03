#' Simulate heterogeneous time to event data
#'
#' @param mean population mean
#' @param n sample size to simulate

simulate_time_to_event <- function(n,
                                   mean_death_other_cause = 1,
                                   mean_death_cancer = 1) {
  tibble(
    # setting population means
    population_mean_death_other_cause = rep_len(1 / mean_death_other_cause, n),
    population_mean_death_cancer = rep_len(1 / mean_death_cancer, n),
  ) %>%
    rowwise() %>%
    mutate(
      # setting individual patient means
      patient_mean_death_other_cause = rexp(1, rate = 1 / population_mean_death_other_cause),
      patient_mean_death_cancer = rexp(1, rate = 1 / population_mean_death_cancer),
      # simulating observed times
      patient_obs_death_other_cause = rexp(1, rate = 1 / patient_mean_death_other_cause),
      patient_obs_death_cancer = rexp(1, rate = 1 / patient_mean_death_cancer),
      # constructing competing risks endpoint
      status_fct =
        case_when(
          patient_obs_death_cancer < patient_obs_death_other_cause ~ "death_cancer",
          patient_obs_death_other_cause <=patient_obs_death_cancer ~ "death_other_cause"
        ) %>%
        factor(levels = c("censored", "death_cancer", "death_other_cause")),
      status_num = as.integer(status_fct) - 1L,
      ttevent = pmin(patient_obs_death_other_cause, patient_obs_death_cancer)
    ) %>%
    ungroup()
}
