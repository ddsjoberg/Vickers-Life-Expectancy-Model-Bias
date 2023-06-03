#' Obtain survival estimates from a `coxph()` model

predict_coxph_surv <- function(mod, newdata, time) {
  # aler
  if (any(is.na(get_all_vars(mod, newdata)))) {
    paste("There are missing data points in {.code newdata}.",
          "{.code NA} values were removed before prediction was made") %>%
      cli::cli_alert_warning()
  }

  survival::survfit(mod, newdata = newdata, se.fit = FALSE) %>%
    summary(times = time) %>%
    purrr::pluck("surv") %>%
    c()
}
