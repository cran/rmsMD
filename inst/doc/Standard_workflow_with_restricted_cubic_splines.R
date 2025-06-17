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
fit_lrm <- lrm(majorcomplication ~ 
                 rcs(age,4) + 
                 rcs(bmi,4) + 
                 sex + 
                 smoking, 
               data = data,
               x = TRUE, y = TRUE)
# setting x = TRUE, y = TRUE allows subsequent likelihood ratio tests to be 
# performed which is recommended for lrm() and cph() models

## -----------------------------------------------------------------------------
# Generate an rmsMD model summary using default settings
modelsummary_rms(fit_lrm)

## -----------------------------------------------------------------------------
# Outputting this as a table
knitr::kable(modelsummary_rms(fit_lrm))

## -----------------------------------------------------------------------------
# Most basic output
ggrmsMD(fit_lrm, data)

## -----------------------------------------------------------------------------
# x axis labels can be stored in a list
xlabels <- list ("age" = "Age (years)",
                 "bmi" = "Body Mass Index")

# titles for each variable can be stored in a list
titles <- list ("age" = "Impact of Age on Complications",
                 "bmi" = "Impact of BMI on Complications")

ggrmsMD(fit_lrm, data,
        # set y axis label for all plots
        ylab = "Complications (adjusted OR)", 
        # set y axis limits
        ylim = c(0,3),
        # set higher OR as inferior outcome to assign red shading
        shade_inferior = "higher", 
        # set x axis labels for each variable
        xlabs = xlabels, 
        # set titles for each variable
        titles = titles 
        )

## -----------------------------------------------------------------------------
library(officer)
library(flextable)
library(dplyr)

# Convert modelsummary_rms dataframe to a flextable
rmsMD_as_table <- flextable(modelsummary_rms(fit_lrm))

# Create a new Word document, add table and a heading
doc <- read_docx() %>%
  body_add_flextable(rmsMD_as_table) %>%
  body_add_par("Model summary from rmsMD", style = "heading 2")

# Temporary file path for output (replace with your actual path as needed)
output_path <- file.path(tempdir(), "example_output.docx")

# Generate the Word document
print(doc, target = output_path)

# Alternatively, save as 'temp.docx' in the working directory
print(doc, target = "temp.docx")

