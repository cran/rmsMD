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
# Fit an OLS model including a restricted cubic spline 
# for Age and BMI (with 4 knots)
fit_lrm <- lrm(
        majorcomplication ~ 
                rcs(age, 4) +     # Age modelled using RCS with 4 knots
                rcs(bmi, 4) +     # BMI also modelled with RCS with 4 knots
                sex +             # Binary variable for sex
                smoking,          # Categorical variable for smoking status
        data = data                  
)

## -----------------------------------------------------------------------------
# most basic output
ggrmsMD(fit_lrm, data)

## -----------------------------------------------------------------------------
ggrmsMD(fit_lrm, data,
        # custom y axis label 
        ylab = "Complications (adjusted OR)", 
        # set y axis limits so variables can be compared more easily
        ylim = c(0,3) 
        )

## -----------------------------------------------------------------------------
ggrmsMD(fit_lrm, data,
        ylab = "Complications (adjusted OR)", 
        ylim = c(0,3), 
        # specifies that OR > 1 reflects poorer outcomes 
        # (e.g. higher OR complications) these will be highlighted in red
        shade_inferior = "higher" 
        )

## -----------------------------------------------------------------------------
# to have the y axis on a log-scale:
ggrmsMD(fit_lrm, data,
        ylab = "Complications (adjusted OR)", 
        ylim = c(0.25,4), 
        shade_inferior = "higher",
        log_y = TRUE, # have the y-axis on a log scale
        log_y_breaks = c(0.25, 0.5 , 1, 2, 4) # optionally set the y-axis breaks
        )

## -----------------------------------------------------------------------------
# to plot predicted probability rather than adjusted OR. 
# This is adjusted to mode/mean of the other variables
ggrmsMD(fit_lrm, data,
        ylim = c(0,0.4), 
        lrm_prob = TRUE # set this to plot predicted probability
        )

## -----------------------------------------------------------------------------
xlabels <- list ("age" = "Age (years)",
                 "bmi" = "Body Mass Index")

xlimits <- list("age" = c(35,65))

ggrmsMD(fit_lrm, data,
        ylab = "Complications (adjusted OR)", 
        ylim = c(0,3), 
        shade_inferior = "higher",
        xlabs = xlabels,
        xlims = xlimits
        )

## -----------------------------------------------------------------------------
xlabels <- list ("age" = "Age (years)",
                 "bmi" = "Body Mass Index")

titles <- list ("age" = "Impact of Age on Complications",
                 "bmi" = "Impact of BMI on Complications")

ggrmsMD(fit_lrm, data,
        ylab = "Complications (adjusted OR)", 
        ylim = c(0,3), 
        shade_inferior = "higher",
        xlabs = xlabels,
        titles = titles
        )

## -----------------------------------------------------------------------------
ggrmsMD(fit_lrm, data,
        ylab = "Complications (adjusted OR)", 
        ylim = c(0,3), 
        shade_inferior = "higher",
        xlabs = xlabels,
        titles = titles,
        log_x_vars = c("age", "bmi")
        )

## -----------------------------------------------------------------------------
ggrmsMD(fit_lrm, data,
        ylab = "Complications (adjusted OR)", 
        ylim = c(0,3), 
        shade_inferior = "higher",
        var = "age"
        )

## -----------------------------------------------------------------------------
fit_ols <- ols(
        lengthstay ~ 
                rcs(age, 4) +
                rcs(bmi, 4) +
                sex +
                smoking,
        data = data
)

ggrmsMD(fit_ols, data,
        ylim = c(20,50),
        ylab = "Predicted length of stay"
        )

