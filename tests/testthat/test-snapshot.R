library(testthat)
library(MASS)
library(rms)
library(survival)

## ------------------------------
## LM Tests using Survey Data
## ------------------------------
test_that("Snapshot: Feed non-rms modelfit", {
  df <- MASS::survey

  # Fit a non-rms model (using lm) on the survey data.
  fit_lm <- lm(Wr.Hnd ~ Age + Exer, data = df)
  # Capture the output of modelsummary_rms for a non-rms model.
  expect_snapshot_output(modelsummary_rms(fit_lm, exp_coef = FALSE))
})

test_that("Snapshot: Warning output for non-rms model without setting exp_coef", {
  df <- MASS::survey

  # Fit a non-rms model (using lm) on the survey data.
  fit_lm <- lm(Wr.Hnd ~ Age + Exer, data = df)
  # Capture and snapshot the error produced by modelsummary_rms.
  expect_snapshot_error({
    modelsummary_rms(fit_lm)
  })
})

## ------------------------------
## OLS Tests using Survey Data
## ------------------------------
test_that("Snapshot: OLS tests - simple model", {
  df <- MASS::survey

  # Fit a simple OLS model using rms::ols.
  fit_ols <- ols(Wr.Hnd ~ Age + Exer + Smoke + Height + Sex, data = df)
  summary_df <- modelsummary_rms(fit_ols)
  expect_snapshot_output(summary_df)
})

test_that("Snapshot: OLS tests - model with interactions", {
  df <- MASS::survey

  # Fit an OLS model including an interaction term.
  fit_interact <- ols(Wr.Hnd ~ Age * Exer, data = df)
  summary_interact <- modelsummary_rms(fit_interact)
  expect_snapshot_output(summary_interact)
})

test_that("Snapshot: OLS tests - model with splines", {
  df <- MASS::survey

  # Fit an OLS model with a restricted cubic spline on Age.
  fit_spline <- ols(Wr.Hnd ~ rcs(Age, 4) + Exer, data = df)
  summary_spline <- modelsummary_rms(fit_spline)
  full_spline <- modelsummary_rms(fit_spline, hide_rcs_coef = FALSE, rcs_overallp = FALSE)
  expect_snapshot_output(list(simple = summary_spline, full = full_spline))
})

test_that("Snapshot: OLS tests - model with splines and interactions", {
  df <- MASS::survey

  # Fit an OLS model with splines and an interaction term.
  fit_spline_interact <- ols(Wr.Hnd ~ rcs(Age, 4) * Exer + Sex, data = df)
  summary_spline_interact <- modelsummary_rms(fit_spline_interact)
  full_out <- modelsummary_rms(fit_spline_interact, hide_rcs_coef = FALSE, rcs_overallp = FALSE)
  expect_snapshot_output(list(summary = summary_spline_interact, full = full_out))
})

## ------------------------------
## LRM Tests using Survey Data
## ------------------------------

test_that("Snapshot: LRM tests - simple model", {
  df <- MASS::survey
  # Create a binary outcome variable for logistic regression.
  df$high_wr <- as.factor(df$Wr.Hnd > median(df$Wr.Hnd, na.rm = TRUE))

  fit_lrm <- lrm(high_wr ~ Age + Exer + Smoke + Height + Sex, data = df)
  expect_snapshot_output(list(modelsummary_rms(fit_lrm), modelsummary_rms(fit_lrm)))
})

test_that("Snapshot: LRM tests - model with interactions", {
  df <- MASS::survey
  # Create a binary outcome variable for logistic regression.
  df$high_wr <- as.factor(df$Wr.Hnd > median(df$Wr.Hnd, na.rm = TRUE))

  fit_lrm_int <- lrm(high_wr ~ Age * Exer, data = df)
  expect_snapshot_output(list(modelsummary_rms(fit_lrm_int), modelsummary_rms(fit_lrm_int)))
})

test_that("Snapshot: LRM tests - model with splines", {
  df <- MASS::survey
  # Create a binary outcome variable for logistic regression.
  df$high_wr <- as.factor(df$Wr.Hnd > median(df$Wr.Hnd, na.rm = TRUE))

  fit_lrm_spline <- lrm(high_wr ~ rcs(Age, 4) + Exer, data = df)
  expect_snapshot_output(list(modelsummary_rms(fit_lrm_spline), modelsummary_rms(fit_lrm_spline)))
})

## ------------------------------
## CPH Tests using Lung Data
## ------------------------------

test_that("Snapshot: CPH tests - simple model", {
  data<-survival::lung

  fit_cph <- cph(Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE)
  expect_snapshot_output(list(modelsummary_rms(fit_cph), modelsummary_rms(fit_cph)))
})

test_that("Snapshot: CPH tests - model with interactions", {
  data<-survival::lung

  fit_cph_int <- cph(Surv(time, status) ~ age * sex, data = lung, x = TRUE, y = TRUE)
  expect_snapshot_output(list(modelsummary_rms(fit_cph_int), modelsummary_rms(fit_cph_int)))
})

test_that("Snapshot: CPH tests - model with splines", {
  data<-survival::lung

  fit_cph_spline <- cph(Surv(time, status) ~ rcs(age, 4) + sex, data = lung, x = TRUE, y = TRUE)
  expect_snapshot_output(list(modelsummary_rms(fit_cph_spline), modelsummary_rms(fit_cph_spline)))
})

test_that("Snapshot: CPH tests - model with splines and interactions", {
  data<-survival::lung

  fit_cph_spline_int <- cph(Surv(time, status) ~ rcs(age, 4) * sex, data = lung, x = TRUE, y = TRUE)
  expect_snapshot_output(list(modelsummary_rms(fit_cph_spline_int), modelsummary_rms(fit_cph_spline_int)))
})

## ------------------------------
## Variables Checks: Labels and Special Variable Names
## ------------------------------
test_that("Snapshot: Variables with labels and special names", {
  df <- MASS::survey

  # Add labels to some existing variables
  attr(df$Sex, "label")    <- "Gender of respondent"
  attr(df$Wr.Hnd, "label") <- "Writing hand measurement (cm)"
  attr(df$NW.Hnd, "label") <- "Non-writing hand measurement (cm)"
  attr(df$Age, "label")    <- "Age of respondent (years)"

  set.seed(123)

  # Create new variables with special names.
  df$`random1` <- rnorm(nrow(df))
  df$`"random2"` <- rnorm(nrow(df))

  attr(df$`random1`, "label")   <- "Random normal variable 1"
  attr(df$`"random2"`, "label")  <- "Random normal variable 2 with double quote in its name"

  # Fit a model that includes these variables.
  fit_vars <- ols(Wr.Hnd ~ rcs(Age, 4) * Exer + Sex + random1 + `"random2"`, data = df)
  summary_vars <- modelsummary_rms(fit_vars)
  hidden_vars <- modelsummary_rms(fit_vars, hide_rcs_coef = TRUE, rcs_overallp = TRUE)

  expect_snapshot_output(list(summary = summary_vars, hidden = hidden_vars))
})

test_that("Snapshot: Variables with reserved/special names", {
  df <- MASS::survey

  # Create additional variables with reserved/special names.
  df$`if` <- rnorm(nrow(df))
  attr(df$`if`, "label") <- "Random variable with name 'if'"

  df$`for` <- rnorm(nrow(df))
  attr(df$`for`, "label") <- "Random variable with name 'for'"

  df$`while` <- rnorm(nrow(df))
  attr(df$`while`, "label") <- "Random variable with name 'while'"

  df$`TRUE` <- rnorm(nrow(df))
  attr(df$`TRUE`, "label") <- "Random variable with name 'TRUE'"

  df$`NULL` <- rnorm(nrow(df))
  attr(df$`NULL`, "label") <- "Random variable with name 'NULL'"

  # Check the structure of the updated data frame.
  str_output <- capture.output(str(df))

  # Fit a model including these variables.
  fit_special <- ols(Wr.Hnd ~ rcs(Age, 4) * Exer + Sex + `if` + `for` + `while` + `TRUE` + `NULL`, data = df)
  summary_special <- modelsummary_rms(fit_special)
  hidden_special <- modelsummary_rms(fit_special, hide_rcs_coef = TRUE, rcs_overallp = TRUE)

  expect_snapshot_output(list(structure = str_output,
                              summary = summary_special,
                              hidden = hidden_special))
})
