## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## -----------------------------------------------------------------------------
# Install the package if you haven't already
# install.packages("rmsMD")

library(rms)
library(rmsMD)
library(MASS)

## -----------------------------------------------------------------------------
# Loading the built-in dataset from the MASS package:
data("survey", package = "MASS")

# Fit a linear regression model using the rms package:
fit_ols <- ols(Wr.Hnd ~ Age + Exer + Sex, data = survey)

# Generate a model summary, and assign it to rmsMD_summary
rmsMD_summary <- modelsummary_rms(fit_ols)

# displaying rmsMD dataframe output
rmsMD_summary

# rmsMD dataframe as a table
knitr::kable(rmsMD_summary)

## -----------------------------------------------------------------------------
# Generate a model summary with custom styling options
summary_custom <- modelsummary_rms(fit_ols, 
                                   combine_ci = FALSE, 
                                   round_dp_coef = 2, 
                                   round_dp_p = 5)

# to display the dataframe as a table
knitr::kable(summary_custom)

## -----------------------------------------------------------------------------
# Generate a model summary with custom styling options
summary_fullmodel <- modelsummary_rms(fit_ols, 
                                   combine_ci = FALSE, 
                                   round_dp_coef = 2, 
                                   round_dp_p = 5,
                                   fullmodel = TRUE)
knitr::kable(summary_fullmodel)

## -----------------------------------------------------------------------------

# Note: For demonstration, we create a binary outcome using the survey dataset.
survey$BinaryOutcome <- ifelse(survey$Wr.Hnd > median(survey$Wr.Hnd, na.rm = TRUE), 1, 0)

# fitting the model
fit_lrm <- lrm(BinaryOutcome ~ Age + Exer + Sex, data = survey)

# rmsMD summary
summary_lrm <- modelsummary_rms(fit_lrm)

# displaying as a table
knitr::kable(summary_lrm)

## -----------------------------------------------------------------------------
# Fit a simple linear model using lm() from base R (an example model fit without using rms package)
fit_lm <- lm(Wr.Hnd ~ Age + Exer + Sex, data = survey)

# Generate a model summary for the non-RMS model by explicitly setting exp_coef = FALSE
summary_lm <- modelsummary_rms(fit_lm, 
                               exp_coef = FALSE)

# display rmsMD results as a table
knitr::kable(summary_lm)

## -----------------------------------------------------------------------------
# Using the built-in dataset from the MASS package
data("survey", package = "MASS")

# Fit an OLS model including a restricted cubic spline for Age (with 4 knots)
fit_spline <- ols(Wr.Hnd ~ rcs(Age, 4) + Exer + Sex, data = survey)

# Generate an rmsMD model summary using default settings
summary_spline <- modelsummary_rms(fit_spline)

# Outputting this as a table
knitr::kable(summary_spline)

## -----------------------------------------------------------------------------
# Fit an OLS model including a restricted cubic spline for Age (with 4 knots)
fit_spline_hide <- ols(Wr.Hnd ~ rcs(Age, 4) + Exer + Sex, data = survey)

# Generate a model summary with rcs_overallp set to TRUE and hide_rcs_coef set to TRUE
summary_spline_hide <- modelsummary_rms(fit_spline_hide, 
                                        hide_rcs_coef = FALSE)

# Outputting this as a table
knitr::kable(summary_spline_hide)

## -----------------------------------------------------------------------------
# Fit an OLS model including a restricted cubic spline for Age (with 4 knots)
fit_spline_hide <- ols(Wr.Hnd ~ rcs(Age, 4) + Exer + Sex, data = survey)

# Generate a model summary with rcs_overallp set to TRUE and hide_rcs_coef set to TRUE
summary_spline_hide <- modelsummary_rms(fit_spline_hide, 
                                        rcs_overallp = FALSE, 
                                        hide_rcs_coef = FALSE)
knitr::kable(summary_spline_hide)

## -----------------------------------------------------------------------------
# Using the built-in dataset from the MASS package
data("survey", package = "MASS")

# Fit an OLS model using the rms package with an interaction between Age and Exer.
fit_interact <- ols(Wr.Hnd ~ Age * Exer + Sex, data = survey)

# Generate a model summary that includes the interaction term
summary_interact <- modelsummary_rms(fit_interact)
knitr::kable(summary_interact)

## -----------------------------------------------------------------------------
# Using the built-in dataset from the MASS package
data("survey", package = "MASS")

# Fit an OLS model with a restricted cubic spline for Age and an interaction between Age and Exer.
fit_spline_interact <- ols(Wr.Hnd ~ rcs(Age, 4) * Exer + Sex, data = survey)

# Generate a model summary with default RCS output
summary_spline_interact <- modelsummary_rms(fit_spline_interact)

# Format the output as a nice table
knitr::kable(summary_spline_interact)

## -----------------------------------------------------------------------------
library(officer)
library(flextable)
library(dplyr)

# converting modelsummary_rms dataframe generated above into a flextable
rmsMD_as_table <- flextable(rmsMD_summary)

# use officer to create a 
doc <- read_docx() %>% 
  body_add_flextable(rmsMD_as_table) %>%
  body_add_par("Model summary from rmsMD", style = "heading 2")

# generating a temporary output path for demonstration. This would be replaced by the file path where the word document will be generated
output_path <- file.path(tempdir(), "example_output.docx")

# generating the word document
print(doc, target = output_path)

print(doc, target = "temp.docx")

