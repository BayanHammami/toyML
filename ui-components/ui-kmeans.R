kmeans <- tabPanel("K-Means",
                   sidebarLayout(
                     sidebarPanel(
                       p(
                         "This app demonstrates the K-Means clustering alogirthm. The steps are outlined below:
                         "
                       ),
                       tags$ol(
                         tags$li('Randomly initialise cluster centers')
                       ),
                       p('Repeat below steps until convergence'),
                       tags$ol(
                         tags$li('Assign each point to the closest centroid'),
                         tags$li('Re-calculate cluster center based on new points allocated')
                       ),
                       p("The code for this app can be found", tags$b(
                         tags$a(href = "https://github.com/BayanHammami/toyML", "here")
                       )),
                       fluidRow(
                        column(width = 6,
                               sliderInput(
                                 "kmeans_num_points",
                                 label = h6("Number of Points Per Cluster"),
                                 min = 20,
                                 max = 100,
                                 value = kmeans_defaults$num_points
                               )                         
                        ),
                        column(width = 6,
                               sliderInput(
                                 "kmeans_num_clusters",
                                 label = h6("Number of Clusters "),
                                 min = 2,
                                 max = 10,
                                 value = kmeans_defaults$num_clusters
                               )                        
                        )
                       ),
                       actionButton("kmeans_generate_data", label = "Re-generate Data"),
                       hr(),
                       actionButton("kmeans_generate_centroids", label = "Randomly re-initialise clusters"),
                       hr(),
                       actionButton("kmeans_assign", label = "Assign Points to Closest Centroid"),
                       hr(),
                       actionButton("kmeans_new_centroid", label = "Calculate New Centroids")
                     ),
                     mainPanel(plotOutput("kmeans_plot", height = "600px"), style = "text-align: center;")
                   ))
