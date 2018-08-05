dbscan_functions <- list()

dbscan_functions$plot_points <- function(all_points){
  p <- ggplot(data = all_points)
  if(!(sum(all_points$assigned_cluster) == 0)){
    p <- p +   
      geom_point(aes(x = x, y = y, color = as.factor(assigned_cluster)), alpha = 0.5)
  }else{
    p <- p +
      geom_point(aes(x = x, y = y, color = as.factor(assigned_cluster)), alpha = 0.5)
  }
  
  p <- p +
    theme(legend.position="none") +
    xlim(-0.2, 1.2) +
    ylim(-0.2, 1.2)
  
  p
}

dbscan_functions$generate_data <-
  function(num_points,
           num_clusters,
           standard_deviation = 0.02) {
    random_centroids <- initialise_centroids(num_clusters)
    
    all_points_list <- lapply(1:num_clusters, function(cluster_index) {
      data <-
        data.frame(
          x = rnorm(num_points, random_centroids[[cluster_index]][1], standard_deviation),
          y = rnorm(num_points, random_centroids[[cluster_index]][2], standard_deviation),
          cluster_index = cluster_index
        )
    })
    
    all_points <- bind_rows(all_points_list)
    all_points$visited <- FALSE
    #0 means none
    all_points$assigned_cluster <- 0
    all_points <- all_points %>%
      rownames_to_column %>%
      rename(point_id = rowname) %>%
      as.tibble
    
    return(all_points)
    
  }


dbscan_functions$initialise_centroids <- function(num_clusters) {
  random_centroids <- lapply(1:num_clusters, function(i)
    runif(2))
}

dbscan_functions$calculate_distance <- function(point1, point2) {
  distance <- sqrt(sum((point1 - point2) ^ 2))
  return(distance)
}

dbscan_functions$region_query <- function(all_points, point, epsilon) {
  all_points$distance_to_point <-
    sapply(1:nrow(all_points), function(point_index) {
      calculate_distance(c(point$x[1], point$y[1]), c(all_points$x[point_index], all_points$y[point_index]))
    })
  
  sphere_points <- all_points %>%
    filter(distance_to_point < epsilon) %>% 
    as.tibble
  
  return(sphere_points)
}

#Version using a while loop - this is more understandable
dbscan_functions$expand_cluster <-
  function(all_points,
           point,
           cluster,
           epsilon,
           min_points,
           num_iterations,
           sphere_points = NULL) {
    
    if(is.null(sphere_points)){
      sphere_point_list <- list()  
    }else{
      sphere_point_list <- list() 
      sphere_point_list[[1]] <- sphere_points
    }
    
    more_points <- TRUE
    # all_plots <- list()
    iter <- 1
    while(more_points){
      iter <- iter + 1
      # print(iter)
      sphere_points <-
        dbscan_functions$region_query(all_points, point, epsilon)  
      
      sphere_points$center_point_id <- point$point_id 
      if(nrow(sphere_points) > min_points){
        if(all_points[all_points$point_id == point$point_id,]$assigned_cluster == 0){
          all_points[all_points$point_id == point$point_id,]$assigned_cluster <-
            cluster          
        }
      }
      
      all_points[all_points$point_id == point$point_id, ]$visited <-
        TRUE
      sphere_points[sphere_points$point_id == point$point_id, ]$visited <-
        TRUE
      # p <- plot_points(all_points)
      # if((iter %% 10) == 0){
      #   print(p, newpage = F)  
      # }
      
      # all_plots <- c(all_plots, list(p))
      sphere_point_list[[length(sphere_point_list) + 1]] <- sphere_points
      sphere_points <- bind_rows(sphere_point_list)
      # print(all_points)
      point <- sphere_points %>%
        group_by(point_id) %>% 
        summarise(visited = as.logical(max(visited))) %>% 
        filter(visited == FALSE) %>%
        head(1)
      
      point <- all_points %>% 
        filter(point_id %in% point$point_id)

      
      if(nrow(point) == 0){
        more_points_in_cluster <- FALSE
      }else{
        more_points_in_cluster <- TRUE
      }
      
      if(nrow(point) == 0 | iter == num_iterations){
        # print('Stopping')
        # print(p, newpage = F) 
        # stop('Stopped')
        more_points = FALSE
      }
    
    }
    
    return(
      list(
        all_points = all_points,
        more_points_in_cluster = more_points_in_cluster,
        sphere_points = sphere_points
      )
    )
  }


run_dbscan <- function(epsilon = 0.06, min_points = 3, num_points = 20, num_clusters = 5, sd = 0.02){
  
  all_points <- dbscan_functions$generate_data(num_points, num_clusters, sd)

  dbscan_functions$plot_points(all_points)
  
  more_points <- TRUE
  num_iterations <- 10
  while(more_points){
    if(!more_points_in_cluster){
      cluster <- max(all_points$assigned_cluster) + 1  
    }
    
    point <- all_points %>%
      filter(visited == FALSE) %>%
      head(1) %>%
      as.tibble
    
    if(nrow(point) == 0){
      more_points <- FALSE
      print('No more points')
    }else{
      output <-
        dbscan_functions$expand_cluster(all_points, point, cluster, epsilon, min_points, num_iterations)
      all_points <- output$all_points
      more_points_in_cluster <- output$more_points_in_cluster
      p <- dbscan_functions$plot_points(all_points)
    }
    print(p, newpage = F)
    Sys.sleep(0.05)
  }
  
  return(all_points)
}

# all_points <- run_dbscan()
