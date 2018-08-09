output <-
  lapply(paste0(
    "./algorithms/",
    list.files("./algorithms", recursive = T)
  ), source)

server <- function(input, output) {

# KMEANS ------------------------------------------------------------------
  
  kmeans_values <- reactiveValues(
    all_points = generate_data(kmeans_defaults$num_points, kmeans_defaults$num_clusters),
    current_centroids = initialise_centroids(kmeans_defaults$num_clusters)
  )

  observeEvent(input$kmeans_assign, {
    print('kmeans_assign')
    kmeans_values$all_points <- assign_points_to_centroid(kmeans_values$all_points, kmeans_values$current_centroids)
  })
  
  
  observeEvent(input$kmeans_new_centroid, {
    print('kmeans_new_centroid')
    kmeans_values$current_centroids <- calculate_new_centroid(kmeans_values$all_points, kmeans_values$current_centroids)
  })
  
  observeEvent(input$kmeans_generate_data, {
    print('kmeans_generate')
    kmeans_values$all_points <- generate_data(input$kmeans_num_points, input$kmeans_num_clusters)
  })
  
  observeEvent(input$kmeans_generate_centroids, {
    print('kmeans_generate')
    kmeans_values$current_centroids <- initialise_centroids(input$kmeans_num_clusters)
  })

  output$kmeans_plot <- renderPlot({
    print('kmeans_plot')
    plot_points(kmeans_values$all_points, kmeans_values$current_centroids)
    
  })

# DBSCAN ------------------------------------------------------------------
  
  dbscan_values <- reactiveValues(
    all_points = dbscan_functions$generate_data(dbscan_defaults$num_points, dbscan_defaults$num_clusters, dbscan_defaults$sd),
    more_points_in_cluster = FALSE,
    cluster = 0,
    sphere_points = NULL,
    converged = FALSE
  )
  
  observeEvent(input$dbscan_generate_data, {
    print('dbscan_generate')
    dbscan_values$all_points <- dbscan_functions$generate_data(input$dbscan_num_points, input$dbscan_num_clusters, input$dbscan_sd)
    dbscan_values$more_points_in_cluster = FALSE
    dbscan_values$cluster = 0
    dbscan_values$sphere_points = NULL
    dbscan_values$converged = FALSE
  })
  
  observeEvent(input$dbscan_expand_cluster, {
    print('dbscan_expand_cluster')
    all_points <- dbscan_values$all_points
    if(!(sum(!all_points$visited) == 0)){
      if(!dbscan_values$more_points_in_cluster){
        cluster <- max(all_points$assigned_cluster) + 1  
        dbscan_values$cluster <- cluster
        point <- all_points %>%
          filter(visited == FALSE) %>%
          head(1) %>%
          as.tibble
      }else{
       
        
        point <- dbscan_values$sphere_points %>%
          group_by(point_id) %>% 
          summarise(visited = as.logical(max(visited))) %>% 
          filter(visited == FALSE) %>%
          head(1) %>% 
          as.tibble
        
        point <- all_points %>% 
          filter(point_id %in% point$point_id)

      }
      
      if(nrow(point) == 0){
        more_points <- FALSE
        print('No more points')
      }
      
      result <-
        dbscan_functions$expand_cluster(
          all_points,
          point,
          dbscan_values$cluster,
          input$dbscan_epsilon,
          input$dbscan_min_points,
          input$dbscan_num_points_expand,
          dbscan_values$sphere_points
        )
      
      dbscan_values$all_points <- result$all_points
      dbscan_values$more_points_in_cluster <- result$more_points_in_cluster
      dbscan_values$sphere_points <- result$sphere_points      
    }else{
      dbscan_values$converged <- TRUE
    }
  })

  output$dbscan_convergence_text <- renderText({ 
    if(dbscan_values$converged){
      return("The algorithm has converged and there are no more points to explore.")
    }else{
      return("")
    }
  })
    
  output$dbscan_current_cluster <- renderText({ 
    dbscan_values$cluster
  })
  
  output$dbscan_plot <- renderPlot({
    dbscan_functions$plot_points(dbscan_values$all_points)
    
  })  


# Decision Tree -----------------------------------------------------------

  decisiontree_values <- reactiveValues(
    all_points = decisiontree_functions$generate_data(decisiontree_defaults$num_points)$all_points
  )
  
  observeEvent(input$decisiontree_generate_data, {
    print('decisiontree_generate')
    decisiontree_values$all_points <- decisiontree_functions$generate_data(input$decisiontree_num_points)$all_points
  })
  
  observeEvent(input$decisiontree_split_tree, {
    print('decisiontree_splittree')
    decisiontree_values$all_points <- decisiontree_functions$split_tree(decisiontree_values$all_points)
  })
  
  output$decisiontree_plot <- renderPlot({
    print('decisiontree_plot')
    decisiontree_functions$plot_points(decisiontree_values$all_points)
    
  })
  
  
  
  
}
