## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------
withr::with_dir(new = here::here(), code = source(".Rprofile", local = TRUE))
set.seed(20220516)
devtools::load_all(path = here::here())

## ----import------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidycmprsk); library(survival)
library(hotverse); library(tidyverse)

# setting MSK ggplot theme, gtsummary theme, and color palette
theme_set(mskRvis::theme_biostat()); theme_gtsummary_msk("hot"); mskRvis::set_msk_palette()

load(here("results", "results_sim_data.Rdata"))
load(here("results", "results_sim_models.Rdata"))

df_cancer_cuminc <-
  mod_cancer_cuminc |>
  tidycmprsk::tidy(time = 10) |>
  filter(outcome == "Dead from Cancer") %>%
  select(true_risk_cancer_death = strata, pr_cancer_cuminc = estimate)

df_cancer_survfit <-
  mod_cancer_survfit |>
  summary(time = 10) %>%
  {tibble(
    true_risk_cancer_death = pluck(., "strata"),
    pr_cancer_survfit = 1- pluck(., "surv")
  )} %>%
  mutate(
    true_risk_cancer_death =
      word(true_risk_cancer_death, 2, sep = fixed("=")) |>
      as.numeric() %>%
      factor(levels = levels(df_simulated$true_risk_cancer_death))
  )

df_other_cause_survfit <-
  mod_other_cause_survfit |>
  summary(time = 10) %>%
  {tibble(
    true_risk_other_cause_death = pluck(., "strata"),
    pr_other_cause_survfit = 1- pluck(., "surv")
  )} %>%
  mutate(
    true_risk_other_cause_death =
      word(true_risk_other_cause_death, 2, sep = fixed("=")) |>
      as.numeric() %>%
      factor(levels = levels(df_simulated$true_risk_other_cause_death))
  )

# add probabilities to df_simulated data frame
df_simulated <-
  df_simulated %>%
  left_join(df_other_cause_survfit, by = "true_risk_other_cause_death") %>%
  left_join(df_cancer_cuminc, by = "true_risk_cancer_death") %>%
  left_join(df_cancer_survfit, by = "true_risk_cancer_death") %>%
  mutate(
    updated_pr_cancer_survfit = update_cancer_death_risk(pr_cancer_survfit, pr_other_cause_survfit, time = 10),
    updated_pr_cancer_cuminc = update_cancer_death_risk(pr_cancer_cuminc, pr_other_cause_survfit, time = 10)
  )


## ----save--------------------------------------------------------------------------------------------------------------------------------------------------------
# list result objects here. save only those needed for the final report
save(df_simulated, file = here("results", "df_simulated.Rdata"))
