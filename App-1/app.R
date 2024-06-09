library(sf)
library(ggmap)
library(rvest)
# https://crashinfo.penndot.gov/PCIT/welcome.html
library(data.table)
library(here)
library(dplyr)
library(ggplot2)
library(gganimate)
library(shiny)

# app.R has three components:
# - a user interface object
# - a server function
# - a call to the shinyApp function

load("data/processed/preprocessed.Rdata")

