decisiontree <- tabPanel("Decision Tree",
                   sidebarLayout(
                     sidebarPanel(
                       p(
                         "This app demonstrates the Decision Tree alogirthm with 2 continuous features and 2 classes for classification. The steps are outlined below:
                         "
                       ),
                       tags$ol(
                         tags$li('For each variable, calculate the cutoff value and minimises miss-classified error'),
                         tags$li('Apply a split using the variable that produces to lowest miss-classification error')
                       ),
                       p("The code for this app can be found", tags$b(
                         tags$a(href = "https://github.com/BayanHammami/toyML", "here")
                       )),
                       fluidRow(
                         column(width = 6,
                                sliderInput(
                                  "decisiontree_num_points",
                                  label = h6("Number of Points"),
                                  min = 20,
                                  max = 1000,
                                  value = decisiontree_defaults$num_points
                                )                         
                         )
                       ),
                       actionButton("decisiontree_generate_data", label = "Re-generate Data"),
                       hr(),
                       actionButton("decisiontree_reset_tree", label = "Reset Tree"),
                       hr(),
                       actionButton("decisiontree_split_tree", label = "Split Tree")
                     ),
                     mainPanel(plotOutput("decisiontree_plot", height = "600px"), style = "text-align: center;")
                   ))
