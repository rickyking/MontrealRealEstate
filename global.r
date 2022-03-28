library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(metricsgraphics)
library(DT)
# load data
dt <- readRDS('data/dt_clean.rds')
shp_montreal <- readRDS('data/shp_montreal.rds')

valueDisplayList <- list(
						 Classic=c('Mean'='mean', 'Median'='median'),
						 Quantile=c('25th percentile'='quantile25', '75th percentile'='quantile75')
						)

format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
}