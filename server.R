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


  output$dbscan_plot <- renderPlot({
    print('dbscan_plot')
    plot(1:10, 1:10)
    
  })  
  
}
