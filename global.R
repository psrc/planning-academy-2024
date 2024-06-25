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

# Packages for Chart Creation
library(psrcplot)
library(echarts4r)

# Packages for Map Creation
library(sf)
library(leaflet)

# Packages for Table Creation
library(DT)

# Package for Excel Data Creation
library(openxlsx)

# Package for generating HTML
library(htmltools)

# Run Modules Files ---------------------------------------------------------------------------
# This section runs the modules and unless the folder name changes, it doesn't need to be changed
# It also loads in useful functions for dashboard creation
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Page Information --------------------------------------------------------
# This section reads in the csv files that provide the text used on the relevant pages
# Unless the file names change, it doesn't need to be changed
left_panel_info <- read_csv("data/left_panel_information.csv", show_col_types = FALSE)
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)
place_information <- read_csv("data/place_information.csv", show_col_types = FALSE)

# Inputs ---------------------------------------------------------------
# Section for any standard inputs like the crs for wgs84
wgs84 <- 4326
load_clr <- "#91268F"
latest_yr <- "2023"

rgc_title <- "Regional Growth Center (4/23/2024)"
school_title <- "Planning Academy School 2024"

year_ord <- c("2022", "2017", "2012")

# Data via RDS files ------------------------------------------------------
# Section reading in inputs from the data folder. RDS files are R objects and read quickly
age_data <- readRDS("data/population_by_age.rds") |> mutate(year = factor(year, levels=year_ord))
race_data <- readRDS("data/population_by_race.rds") |> mutate(year = factor(year, levels=year_ord))
income_data <- readRDS("data/households_by_income.rds") |> mutate(year = factor(year, levels=year_ord))
education_data <- readRDS("data/educational_attainment.rds") |> mutate(year = factor(year, levels=year_ord))
tenure_data <- readRDS("data/households_by_tenure.rds") |> mutate(year = factor(year, levels=year_ord))
type_data <- readRDS("data/housing_units_by_type.rds") |> mutate(year = factor(year, levels=year_ord))
burden_data <- readRDS("data/cost_burden.rds") |> mutate(year = factor(year, levels=year_ord))
renter_burden_data <- burden_data |> filter(concept == "Renter Cost Burden")
owner_burden_data <- burden_data |> filter(concept == "Owner Cost Burden")
rgc_shape <- readRDS("data/rgc_shape.rds") |> st_transform(wgs84)
school_shape <- readRDS("data/school_shape.rds") |> st_transform(wgs84)

# Values for Drop Downs ---------------------------------------------------
# Section for creating the values needed in any dropdowns, lists, etc.
rgc_list <- as.character(unique(rgc_shape$name)) |> sort()
random_rgc <- rgc_list[[sample(1:length(rgc_list), 1)]]

school_list <- as.character(unique(school_shape$name)) |> sort()
random_school <- school_list[[sample(1:length(school_list), 1)]]

# Other ----
transit_links <- c("Community Transit" = "https://www.communitytransit.org/",
                   "Everett Transit" = "https://everetttransit.org/",
                   "King County Metro" = "https://kingcounty.gov/en/dept/metro",
                   "Kitsap Transit" = "https://www.kitsaptransit.com/",
                   "Pierce Transit" = "https://www.piercetransit.org/",
                   "Pierce County Ferry" = "https://www.piercecountywa.gov/1793/Ferry",
                   "Sound Transit" = "https://www.soundtransit.org/",
                   "Washington State Ferries" = "https://wsdot.wa.gov/travel/washington-state-ferries",
                   "Transit Planning at PSRC" = "https://www.psrc.org/our-work/transit"
)
