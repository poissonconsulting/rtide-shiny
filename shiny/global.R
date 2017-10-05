## upload button
library(shiny)
library(shinyjs)
library(rdrop2)
library(dplyr)
library(readr)
library(gmailr)
library(sf)
library(lubridate)
library(magrittr)
library(leaflet)
library(rtide)
library(poisspatial)
library(leaflet.extras)

helpers <- 'helpers/'
parent <- "Shiny/horse-exploit-upload"

initial_lat <- 40.6157777
initial_long <- -127.311505
initial_zoom <- 3
click_zoom <- 11

sites <- rtide::harmonics$Station %>%
  ps_longlat_to_sfc() %>%
  ps_activate_sfc() %>%
  select(Station, TZ) %>%
  ps_sfc_to_coords()

source(paste0(helpers, 'functions.R'), local = T)
source(paste0(helpers, 'auth.R'), local = T)









