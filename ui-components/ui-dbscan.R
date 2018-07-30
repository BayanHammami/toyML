dbscan <- tabPanel("DBSCAN",
                 sidebarLayout(sidebarPanel(
                   p(
                     "This is a demo of the DBSCAN density based clustering algorithm."
                   ),
                   p(
                     "The code for this app can be found", tags$b(tags$a(href ="https://github.com/BayanHammami/resume-shiny", "here"))
                   )
                 ),
                 mainPanel(
                   plotOutput("dbscan_plot", height = "600px"),
                   style="text-align: center;"
                   
                 ))
)


