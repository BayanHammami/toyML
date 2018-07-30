plot_points <- function(all_points){
  p <- ggplot(data = all_points)
  if(!sum(all_points$assigned_cluster) == 0){
    p <- p +   
      geom_point(aes(x = x, y = y, color = as.factor(assigned_cluster)))
  }else{
    p <- p +
      geom_point(aes(x = x, y = y, color = as.factor(assigned_cluster)))
  }
  
  p <- p +
    theme(legend.position="none") +
    xlim(-0.2, 1.2) +
    ylim(-0.2, 1.2)
  
  p
}

generate_data <-
  function(num_points,
           num_clusters,
           standard_deviation = 0.05) {
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
    
    return(all_points)
    
  }


initialise_centroids <- function(num_clusters) {
  random_centroids <- lapply(1:num_clusters, function(i)
    runif(2))
}

calculate_distance <- function(point1, point2) {
  distance <- sqrt(sum((point1 - point2) ^ 2))
  return(distance)
}

region_query <- function(all_points, point, epsilon) {
  all_points$distance_to_point <-
    sapply(1:nrow(all_points), function(point_index) {
      calculate_distance(c(point$x[1], point$y[1]), c(all_points$x[point_index], all_points$y[point_index]))
    })
  
  sphere_points <- all_points %>%
    filter(distance_to_point < epsilon) %>% 
    as.tibble
  
  return(sphere_points)
}


expand_cluster <-
  function(all_points,
           point,
           sphere_points,
           cluster,
           epsilon,
           min_points) {
    sphere_points_all <- sphere_points
    all_points[all_points$point_id == point$point_id,]$assigned_cluster <-
      cluster
    for (sphere_point_index in 1:nrow(sphere_points)) {
      if (sphere_points$visited[sphere_point_index] == 'not visited') {
        sphere_point <- sphere_points[sphere_point_index,]
        all_points[all_points$point_id == sphere_point$point_id,]$visited <-
          'visited'
        sphere_point <- sphere_points[sphere_point_index,]
        sphere_points_new <-
          region_query(all_points, sphere_point, epsilon)
        
        if (nrow(sphere_points_new) > min_points) {
          if (sphere_point$assigned_cluster == 0) {
            all_points[all_points$point_id == sphere_point$point_id,]$assigned_cluster <-
              cluster
            
            all_points <- expand_cluster(all_points,
                                         point,
                                         sphere_points_new,
                                         cluster,
                                         epsilon,
                                         min_points)
            
          }
        }
        
        
        
      }
    }
    
    return(all_points)
  }


all_points <- generate_data(100, 3, 0.02)
all_points$visited <- 'not visited'
#0 means none
all_points$assigned_cluster <- 0
plot_points(all_points)

all_points <- all_points %>% 
  rownames_to_column %>% 
  rename(point_id = rowname) %>% 
  as.tibble



epsilon <- 0.02
min_points <- 2
#REPEAT BELOW
cluster <- max(all_points$assigned_cluster) + 1
point <- all_points %>%
  filter(visited == 'not visited') %>%
  sample_n(1) %>% 
  as.tibble

sphere_points <- region_query(all_points, point, epsilon)
print(nrow(sphere_points))
if(nrow(sphere_points) > min_points){
  all_points <-
    expand_cluster(all_points, point, sphere_points, cluster, epsilon, min_points)  
}

all_points[all_points$point_id == point$point_id, ]$visited <- 'visited'

plot_points(all_points)

