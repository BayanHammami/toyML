output <-
  lapply(paste0(
    "./ui-components/",
    list.files("./ui-components", recursive = T)
  ), source)

ui <- navbarPage(
  "ToyML",
  theme = shinytheme("paper"),
  home,
  navbarMenu("Clustering",
             kmeans,
             dbscan
             ),
  navbarMenu("Classification",
             decisiontree
  )  
  
)
