## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(rms)
library(rmsMD)

## -----------------------------------------------------------------------------
# Load in the simulated data
data <- simulated_rmsMD_data()

# Set the datadist which is required for rms modelling 
# (these two lines are standard)
dd <- datadist(data)    
options(datadist='dd') 

## -----------------------------------------------------------------------------
# Fit a linear regression model using the rms package:
fit_ols <- ols(lengthstay ~ age + 
                 bmi + 
                 sex + 
                 smoking, 
               data = data)

# Generate a model summary and display it as a dataframe
modelsummary_rms(fit_ols)

# rmsMD dataframe as a table
knitr::kable(modelsummary_rms(fit_ols))

## -----------------------------------------------------------------------------
# Generate a model summary with custom styling options
# & store as summary_custom
summary_custom <- modelsummary_rms(fit_ols, 
                                   combine_ci = FALSE, 
                                   round_dp_coef = 2, 
                                   round_dp_p = 4)

# to display the dataframe as a table
knitr::kable(summary_custom)

## -----------------------------------------------------------------------------
# fitting the model
fit_lrm <- lrm(majorcomplication ~ age + 
                 bmi + 
                 sex + 
                 smoking, 
               data = data)

# rmsMD summary
modelsummary_rms(fit_lrm)

# displaying as a table
knitr::kable(modelsummary_rms(fit_lrm))

## -----------------------------------------------------------------------------
# Fit a simple linear model using lm() from base R 
# (an example model fit without using rms package)
fit_lm <- lm(majorcomplication ~ age + 
               bmi + 
               sex + 
               smoking, 
             data = data)

# Generate a model summary for the non-RMS model 
# by explicitly setting exp_coef = FALSE
modelsummary_rms(fit_lm, exp_coef = FALSE)

# display rmsMD results as a table
knitr::kable(modelsummary_rms(fit_lm, exp_coef = FALSE))

## -----------------------------------------------------------------------------
# Fit a logistic regression model including a RCS for Age & BMI (with 4 knots)
fit_lrm <- lrm(
        majorcomplication ~ 
                rcs(age, 4) +   # Age modelled using RCS with 4 knots
                rcs(bmi, 4) +   # BMI also modelled with RCS with 4 knots
                sex +           # Binary variable for sex
                smoking,        # Categorical variable for smoking status
                data = data,
                # set x = TRUE, y = TRUE to allow 
                # subsequent LR tests for lrm() and cph() models
                x = TRUE, y = TRUE             
)

# Generate an rmsMD model summary using default settings
modelsummary_rms(fit_lrm)

# Outputting this as a table
knitr::kable(modelsummary_rms(fit_lrm))

## -----------------------------------------------------------------------------

# Generate a model summary with rcs_overallp set to TRUE 
# and hide_rcs_coef set to TRUE
modelsummary_rms(fit_lrm, hide_rcs_coef = FALSE)

# Outputting this as a table
knitr::kable(modelsummary_rms(fit_lrm, hide_rcs_coef = FALSE))

## -----------------------------------------------------------------------------
# Fit an OLS model including a restricted cubic spline for Age (with 4 knots)
summary_spline_hide <- modelsummary_rms(fit_lrm, 
                                        hide_rcs_coef = FALSE,
                                        rcs_overallp = FALSE)

# Outputting this as a table
knitr::kable(summary_spline_hide)

## -----------------------------------------------------------------------------
# Fit an OLS model with a restricted cubic spline for Age 
# and an interaction between Age and Exer.
fit_lrm_interaction <- lrm(majorcomplication ~ 
                             rcs(age,4)*smoking + 
                             rcs(bmi,4) + 
                             sex + 
                             smoking, 
                           data = data,
                           x = TRUE, y = TRUE)

# Generate a model summary with default RCS output
modelsummary_rms(fit_lrm_interaction)

# Format the output as a nice table
knitr::kable(modelsummary_rms(fit_lrm_interaction))

## -----------------------------------------------------------------------------
anova(fit_lrm_interaction, test = "LR")

