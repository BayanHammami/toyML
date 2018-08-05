dbscan <- tabPanel("DBSCAN",
                 sidebarLayout(sidebarPanel(
                   p(
                     "This is a demo of the DBSCAN density based clustering algorithm."
                   ),
                   p(
                     "The code for this app can be found", tags$b(tags$a(href ="https://github.com/BayanHammami/toyml", "here"))
                   ),
                   numericInput("dbscan_sd", label = h6("Standard Deviation (each dimension)"), value = dbscan_defaults$sd),
                   fluidRow(
                     column(width = 6,
                            sliderInput(
                              "dbscan_num_points",
                              label = h6("Number of Points Per Cluster"),
                              min = 20,
                              max = 100,
                              value = dbscan_defaults$num_points
                            )                         
                     ),
                     column(width = 6,
                            sliderInput(
                              "dbscan_num_clusters",
                              label = h6("Number of Clusters "),
                              min = 2,
                              max = 10,
                              value = dbscan_defaults$num_clusters
                            )                    
                     )
                   ),
                   actionButton("dbscan_generate_data", label = "Re-generate Data"),
                   hr(),
                   fluidRow(
                     column(width = 6,
                            sliderInput(
                              "dbscan_epsilon",
                              label = h6("Epsilon"),
                              min = 0.01,
                              max = 1,
                              value = dbscan_defaults$epsilon,
                              step = 0.01
                            )                         
                     ),
                     column(width = 6,
                            sliderInput(
                              "dbscan_min_points",
                              label = h6("Min Points"),
                              min = 2,
                              max = 50,
                              value = dbscan_defaults$min_points
                            )                    
                     )
                   ),
                   hr(),
                   sliderInput(
                     "dbscan_num_points_expand",
                     label = h6("Number of Points to Expand"),
                     min = 2,
                     max = 100,
                     value = dbscan_defaults$num_points_to_expand
                   ),           
                   actionButton("dbscan_expand_cluster", label = "Expand Cluster"),
                   hr(),
                   p('Current Cluster Index'),
                   p(textOutput('dbscan_current_cluster'))
                   
                 ),
                 mainPanel(
                   plotOutput("dbscan_plot", height = "600px"),
                   style="text-align: center;"
                   
                 ))
)


