## upload button
library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(sf)
library(lubridate)
library(magrittr)
library(leaflet)
library(rtide)
library(poisspatial)
library(scales)
library(DT)
library(shinyWidgets)
library(dygraphs)

initial_lat <- 43.6157777
initial_long <- -86.311505
initial_zoom <- 3
click_zoom <- 11

sites <- rtide::harmonics$Station %>%
  ps_longlat_to_sfc() %>%
  ps_activate_sfc() %>%
  select(Station, TZ) %>%
  ps_sfc_to_coords()

source(paste0(helpers, 'functions.R'), local = T)
source(paste0(helpers, 'auth.R'), local = T)

token <- readRDS("helpers/droptoken.rds")

fromadd <- "shinypoisson@gmail.com"
toadd <- "seb@poissonconsulting.ca"

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }"







