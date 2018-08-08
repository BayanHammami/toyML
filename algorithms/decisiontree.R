decisiontree_functions <- list()

decisiontree_functions$generate_data <- function(n = 100){
  x_values <- runif(n, -1, 1)
  y_values <- runif(n, -1, 1)
  
  gradient <- runif(1, -4, 4)
  
  all_points <- tibble(x = x_values, y = y_values) %>% 
    rownames_to_column %>% 
    mutate(class = ifelse(y > gradient*x, 'blue', 'red')) %>% 
    mutate(bucket = '')
  
  all_points$predicted_class <- predicted_class(all_points, '')
  
  p <- ggplot(data = all_points) +
    geom_point(aes(x = x, y = y, color = class), alpha = 0.5)
   
  return(list(all_points = all_points, p = p))
  
}


find_optimal_cutoff <- function(variable, points_in_bucket){
  
  values <- unique(points_in_bucket[[variable]])
  
  values <- values[order(values)]
  new_error <- 0
  
  for(value_index in 1:(length(values) - 1)){
    print(value_index)
    value <- values[value_index]
    next_value <- values[value_index + 1]
    average_value <- mean(value, next_value)
    
    
    if(variable == 'x'){
      points_in_bucket_eval <- points_in_bucket %>%
        mutate(bucket = ifelse(x > average_value, paste0(bucket, 'l'), paste0(bucket, 'r')))        
    }else{
      points_in_bucket_eval <- points_in_bucket %>%
        mutate(bucket = ifelse(y > average_value, paste0(bucket, 'l'), paste0(bucket, 'r')))
    }
    if(!exists('new_error')){
      new_error <- 1
    }

    new_error <- 0
    for(bucket_tmp in unique(points_in_bucket_eval$bucket)){
      
      
      points_in_bucket_eval_bucket_tmp <- points_in_bucket_eval %>% 
        filter(bucket == bucket_tmp)
      
      new_error <- calculate_error(points_in_bucket_eval_bucket_tmp) + new_error      
    }

    if(new_error < current_error | !exists('current_value')){
      current_error <- new_error
      current_value <- average_value
      current_points_in_bucket_eval <- points_in_bucket_eval
    }
    
    print(current_error)
    print(current_value)
    
  }
  
  return(
    list(
      cutoff = current_value,
      error = current_error,
      points_in_bucket_eval = current_points_in_bucket_eval
    )
  )
  
}


calculate_error <- function(points_in_bucket){
  
  class_counts <- points_in_bucket %>% 
    group_by(class) %>% 
    summarise(num_points = n()) %>% 
    arrange((num_points)) %>% 
    head(1)
  
  error <- sum(class_counts$num_points)/nrow(points_in_bucket)
  
  if(length(unique(points_in_bucket$class)) == 1){
    error <- 0
  }
  
  return(error)
  
}

predicted_class <- function(points_in_bucket, bucket_name){
  class_counts <- points_in_bucket %>%
    filter(bucket == bucket_name) %>% 
    group_by(class) %>% 
    summarise(num_points = n()) %>% 
    arrange(desc(num_points)) %>% 
    head(1)
  
  predicted_class <- class_counts$class
  
  return(predicted_class)
  
}
output <- decisiontree_functions$generate_data(100)
all_points <- output$all_points
p <- output$p

variables <- c('x', 'y')


new_error <- 1


for(bucket_name in unique(all_points$bucket)){
  points_in_bucket <- all_points %>% 
    filter(bucket == bucket_name)
  
  variable_cutoffs <- list()
  current_error <- 1
  for(variable in variables){
    
    variable_cutoffs[[variable]] <- find_optimal_cutoff(variable, points_in_bucket)
    
    if(variable_cutoffs[[variable]]$error < current_error){
      best_variable <- variable
      current_error <- variable_cutoffs[[variable]]$error
    }
  }
  
  error_of_branch <- calculate_error(points_in_bucket)
  
  if(variable_cutoffs[[best_variable]]$error < error_of_branch){
    points_in_bucket <- variable_cutoffs[[best_variable]]$points_in_bucket_eval
  }
  
  points_in_bucket$predicted_class <- sapply(1:unique(points_in_bucket$bucket), function(bucket_name_new){
    predicted_class(points_in_bucket, bucket_name_new)
  })
  
  all_points <- all_points %>% 
    filter(!(bucket == bucket_name)) %>% 
    bind_rows(points_in_bucket)
  
  
  
}


