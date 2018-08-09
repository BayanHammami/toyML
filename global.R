library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tibble)
library(shinythemes)
library(glue)
print('global')
options(warn=-1)


kmeans_defaults <- list()
kmeans_defaults$num_points <- 30
kmeans_defaults$num_clusters <- 4



dbscan_defaults <- list()
dbscan_defaults$num_points <- 40
dbscan_defaults$num_clusters <- 4
dbscan_defaults$epsilon <- 0.1
dbscan_defaults$min_points <- 3
dbscan_defaults$num_points_to_expand <- 50
dbscan_defaults$sd <- 0.02

decisiontree_defaults <- list()
decisiontree_defaults$num_points <- 300
decisiontree_defaults$variables <- c('x', 'y')