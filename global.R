library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tibble)
library(shinythemes)
print('global')

kmeans_defaults <- list()
kmeans_defaults$num_points <- 30
kmeans_defaults$num_clusters <- 4



dbscan_defaults <- list()