output <-
  lapply(paste0(
    "./algorithms/",
    list.files("./algorithms", recursive = T)
  ), source)

server <- function(input, output) {

  values <- reactiveValues(
    all_points = generate_data(kmeans_defaults$num_points, kmeans_defaults$num_clusters),
    current_centroids = initialise_centroids(kmeans_defaults$num_clusters)
  )
  
  
  observeEvent(input$kmeans_assign, {
    print('kmeans_assign')
    values$all_points <- assign_points_to_centroid(values$all_points, values$current_centroids)
  })
  
  
  observeEvent(input$kmeans_new_centroid, {
    print('kmeans_new_centroid')
    values$current_centroids <- calculate_new_centroid(values$all_points, values$current_centroids)
  })
  
  observeEvent(input$kmeans_generate_data, {
    print('kmeans_generate')
    values$all_points <- generate_data(input$kmeans_num_points, input$kmeans_num_clusters)
  })
  
  observeEvent(input$kmeans_generate_centroids, {
    print('kmeans_generate')
    values$current_centroids <- initialise_centroids(input$kmeans_num_clusters)
  })

  output$kmeans_plot <- renderPlot({
    print('kmeans_plot')
    plot_points(values$all_points, values$current_centroids)
    
  })
}
