plot_points <- function(all_points, current_centroids){
  p <- ggplot(data = all_points)
  if(!is.null(all_points$closest_centroid)){
    p <- p +   
      geom_point(aes(x = x, y = y, color = as.factor(closest_centroid)))
  }else{
    p <- p +
      geom_point(aes(x = x, y = y))
  }
  
  centroids_df <- data.frame((t(sapply(current_centroids, function(centroid) centroid))))
  colnames(centroids_df) <- c('x', 'y')
  centroids_df <- centroids_df %>% 
    rownames_to_column %>% 
    rename(closest_centroid = rowname)
  p <- p +
    geom_point(aes(x = x, y = y, size = 4, color = closest_centroid), data = centroids_df) +
    theme(legend.position="none") +
    xlim(-0.2, 1.2) +
    ylim(-0.2, 1.2)
  
  p
}

generate_data <- function(num_points, num_clusters, standard_deviation = 0.05){
  
  random_centroids <- initialise_centroids(num_clusters)
  
  all_points_list <- lapply(1:num_clusters, function(cluster_index){
    data <-
      data.frame(
        x = rnorm(num_points, random_centroids[[cluster_index]][1], standard_deviation),
        y = rnorm(num_points, random_centroids[[cluster_index]][2], standard_deviation),
        cluster_index = cluster_index
      )
  })
  
  all_points <- bind_rows(all_points_list)
  
  return(all_points)
  
}


initialise_centroids <- function(num_clusters){
  random_centroids <- lapply(1:num_clusters, function(i) runif(2))
}


calculate_distance <- function(point, centroid){
  distance <- sqrt(sum((centroid - point)^2))
  return(distance)
}


assign_points_to_centroid <- function(all_points, current_centroids){
  
  #for each point calculate the distance to each centroid and return the closest one
  closest_centroid <- sapply(1:nrow(all_points), function(point_index){
    point_coords <- c(all_points$x[point_index], all_points$y[point_index])
    distances <- sapply(current_centroids, function(centroid) calculate_distance(point_coords, centroid))
    which(min(distances) == distances)
  })
  
  all_points$closest_centroid <- closest_centroid
  
  return(all_points)
}

calculate_new_centroid <- function(all_points, current_centroids){
  centroids <- all_points %>% 
    group_by(closest_centroid) %>% 
    summarise(x = mean(x), y = mean(y))
  
  new_centroids <- lapply(1:length(current_centroids), function(centroid_index){
    new_centroid <- centroids %>% 
      filter(closest_centroid == centroid_index)
    
    if(nrow(new_centroid) == 0) {
      new_centroid <- current_centroids[[centroid_index]]
    }else{
      new_centroid <- c(new_centroid$x, new_centroid$y)
    }
    
    return(new_centroid)
  })
  
  return(new_centroids)
  
}
