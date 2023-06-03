## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------
withr::with_dir(new = here::here(), code = source(".Rprofile", local = TRUE))
set.seed(20220516)
devtools::load_all(path = here::here())


## ----import------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidycmprsk); library(survival)
library(hotverse); library(tidyverse)

# setting MSK ggplot theme, gtsummary theme, and color palette
theme_set(mskRvis::theme_biostat()); theme_gtsummary_msk("hot"); mskRvis::set_msk_palette()


## ----simulate----------------------------------------------------------------------------------------------------------------------------------------------------
df_simulated <-
  list(
    time = 10,
    true_risk_other_cause_death = c(0.05, 0.2, 0.5, 0.8, 0.95),
    true_risk_cancer_death = c(0.05, 0.2, 0.5, 0.8, 0.95)
  ) %>%
  cross_df() %>%
  mutate(
    lambda_other_cause_death = prob_to_exp_lambda(true_risk_other_cause_death, time),
    lambda_cancer_death = prob_to_exp_lambda(true_risk_cancer_death, time),
  ) %>%
  slice(rep(1:n(), 1000)) %>%
  rowwise() %>%
  mutate(
    tt_other_cause_death = rexp(1, rate = lambda_other_cause_death),
    tt_cancer_death = rexp(1, rate = lambda_cancer_death),
    outcome_observed_at_time =
      case_when(
        tt_cancer_death <= tt_other_cause_death &
          tt_cancer_death <= time ~
          "Dead from Cancer",
        tt_other_cause_death <= tt_cancer_death &
          tt_other_cause_death <= time ~
          "Dead from Other Cause",
        TRUE ~ "Alive"
      ),
    cr_outcome =
      case_when(
        tt_cancer_death < tt_other_cause_death ~ "Dead from Cancer",
        TRUE ~ "Dead from Other Cause"
      ) %>%
      factor(levels = c("Censored", "Dead from Cancer", "Dead from Other Cause")),
    cr_time = pmin(tt_cancer_death, tt_other_cause_death),
    across(starts_with("true_"), factor)
  ) %>%
  ungroup()

df_simulated %>%
  select(starts_with("tt"), outcome_observed_at_time, cr_outcome) %>%
  tbl_summary()


## ----models------------------------------------------------------------------------------------------------------------------------------------------------------
mod_other_cause_coxph <-
  coxph(Surv(tt_other_cause_death) ~ true_risk_other_cause_death, data = df_simulated)
mod_other_cause_survfit <-
  survfit(Surv(tt_other_cause_death) ~ true_risk_other_cause_death, data = df_simulated)

mod_cancer_coxph <-
  coxph(Surv(tt_cancer_death) ~ true_risk_cancer_death, data = df_simulated)
mod_cancer_survfit <-
  survfit(Surv(tt_cancer_death) ~ true_risk_cancer_death, data = df_simulated)

mod_cancer_crr <-
  crr(Surv(cr_time, cr_outcome) ~ true_risk_cancer_death, data = df_simulated)
mod_cancer_cuminc <-
  cuminc(Surv(cr_time, cr_outcome) ~ true_risk_cancer_death, data = df_simulated)


## ----predictions-------------------------------------------------------------------------------------------------------------------------------------------------
# df_simulated <-
#   df_simulated %>%
#   mutate(
#     pr_other_cause_death =
#       predict_coxph_surv(mod_other_cause, df_simulated, time = 10),
#     pr_cancer_death_coxph =
#       predict_coxph_surv(mod_cancer_coxph, df_simulated, time = 10),
#     pr_cancer_death_crr =
#       predict(mod_cancer_crr, newdata = df_simulated, times = 10) %>% unlist() %>% unname()
#   )


## ----save--------------------------------------------------------------------------------------------------------------------------------------------------------
# list result objects here. save only those needed for the final report
save(df_simulated, file = here("results", "results_sim_data.Rdata"))

save(
  mod_other_cause_coxph, mod_other_cause_survfit,
  mod_cancer_coxph, mod_cancer_crr, mod_cancer_survfit, mod_cancer_cuminc,
  file = here("results", "results_sim_models.Rdata")
)

