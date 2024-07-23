# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinyBS)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)
library(bslib)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Package for generating HTML
library(htmltools)

# Run Modules Files ---------------------------------------------------------------------------

module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Page Information --------------------------------------------------------

left_panel_info <- read_csv("data/left_panel_information.csv", show_col_types = FALSE)
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)

# Inputs ---------------------------------------------------------------

equity_links <- c("Equity Planning at PSRC" = "https://www.psrc.org/our-work/equity",
                  "Equity Tracker" = "https://www.psrc.org/our-work/equity/equity-tracker",
                  "Legacy of Structrual Racism" = "https://www.psrc.org/our-work/legacy-structural-racism")

transit_links <- c("Transit Planning at PSRC" = "https://www.psrc.org/our-work/transit",
                   "Transit Access" = "https://www.psrc.org/our-work/transit-access",
                   "Transit Oriented Development" = "https://www.psrc.org/our-work/transit-oriented-development")