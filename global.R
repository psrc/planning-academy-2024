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

module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Page Information --------------------------------------------------------

left_panel_info <- read_csv("data/left_panel_information.csv", show_col_types = FALSE)
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)
place_information <- read_csv("data/place_information.csv", show_col_types = FALSE)

# Inputs ---------------------------------------------------------------

wgs84 <- 4326
load_clr <- "#91268F"
latest_yr <- "2023"

rgc_title <- "Regional Growth Center (4/23/2024)"
school_title <- "Planning Academy School 2024"

year_ord <- c("2022", "2017", "2012")

# Data via RDS files ------------------------------------------------------

age_data <- readRDS("data/population_by_age.rds") |> mutate(year = factor(year, levels=year_ord))
race_data <- readRDS("data/population_by_race.rds") |> mutate(year = factor(year, levels=year_ord))
income_data <- readRDS("data/households_by_income.rds") |> mutate(year = factor(year, levels=year_ord))
education_data <- readRDS("data/educational_attainment.rds") |> mutate(year = factor(year, levels=year_ord))
tenure_data <- readRDS("data/households_by_tenure.rds") |> mutate(year = factor(year, levels=year_ord))
type_data <- readRDS("data/housing_units_by_type.rds") |> mutate(year = factor(year, levels=year_ord))
burden_data <- readRDS("data/cost_burden.rds") |> mutate(year = factor(year, levels=year_ord))
mode_data <- readRDS("data/mode_to_work.rds") |> mutate(year = factor(year, levels=year_ord))
rgc_shape <- readRDS("data/rgc_shape.rds") |> st_transform(wgs84) |> rename(geometry="Shape") |> mutate(geography_type = rgc_title)
school_shape <- readRDS("data/school_shape.rds") |> st_transform(wgs84) |> mutate(geography_type = school_title)
place_shape <- bind_rows(rgc_shape, school_shape)

# Other ---------------------------------------------------

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
