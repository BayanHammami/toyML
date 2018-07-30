home <- tabPanel("Home",
                 sidebarLayout(sidebarPanel(
                   p(
                     "Hi there, this shiny app was built to help visualise and understand commonly used algorithms in statistics and machine learning."
                   ),
                   p(
                     "The code for this app can be found", tags$b(tags$a(href ="https://github.com/BayanHammami/toyML", "here"))
                   ),
                   p(
                     "Please contact me on:", tags$b("bayan.hammami@gmail.com")
                   )
                 ),
                 mainPanel(
                   plotOutput("home_plot", height = "600px"),
                   style="text-align: center;"
                   
                 ))
)


