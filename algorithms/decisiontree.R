
plot_points <- function(all_points){
  p <-  ggplot(data = all_points) +
    geom_point(aes(x = x, y = y, shape = class, color = predicted_class), alpha = 0.5, size = 3)
  
  return(p)
  
}
decisiontree_functions <- list()

decisiontree_functions$generate_data <- function(n = 100){
  x_values <- runif(n, -1, 1)
  y_values <- runif(n, -1, 1)
  
  gradient <- runif(1, -2, 2)
  
  all_points <- tibble(x = x_values, y = y_values) %>% 
    rownames_to_column %>% 
    mutate(class = ifelse(y > gradient*x, 'blue', 'red')) %>% 
    mutate(bucket = '') %>% 
    mutate(stop = FALSE)
  
  all_points$predicted_class <- predicted_class(all_points, '')
  
  p <- plot_points(all_points)
   
  return(list(all_points = all_points, p = p))
  
}


find_optimal_cutoff <- function(variable, points_in_bucket){
  
  values <- unique(points_in_bucket[[variable]])
  
  values <- values[order(values)]
  new_error <- 0
  
  for(value_index in 1:(length(values) - 1)){
    # print(value_index)
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

    new_error <- 0
    for(bucket_tmp in unique(points_in_bucket_eval$bucket)){
      
      
      points_in_bucket_eval_bucket_tmp <- points_in_bucket_eval %>% 
        filter(bucket == bucket_tmp)
      
      new_error <- calculate_error(points_in_bucket_eval_bucket_tmp) + new_error      
    }
    if(exists('current_error', inherits = FALSE)){
      if(new_error < current_error){
        current_error <- new_error
        current_value <- average_value
        current_points_in_bucket_eval <- points_in_bucket_eval
      }
      
    }else{
      current_error <- new_error
      current_value <- average_value
      current_points_in_bucket_eval <- points_in_bucket_eval
      
    }

    # print(current_error)
    # print(current_value)
    
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
  
  error <- sum(class_counts$num_points)
  
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

output <- decisiontree_functions$generate_data(1000)
all_points <- output$all_points
p <- output$p
p
variables <- c('x', 'y')

#START

bucket_to_test <- all_points %>% 
  filter(stop == FALSE)

bucket_to_test <- unique(bucket_to_test$bucket)
for(bucket_name in bucket_to_test){
  
  print(bucket_name)
  
  points_in_bucket <- all_points %>% 
    filter(bucket == bucket_name)
  
  print(glue('Current error: {calculate_error(points_in_bucket)}'))
  
  if(length(unique(points_in_bucket$class)) == 2){
    print('Not Converged')
    variable_cutoffs <- list()
    variables_errors <- c()
    cutoffs <- c()
    for(variable in variables){
      print(variable)
      variable_cutoffs[[variable]] <- find_optimal_cutoff(variable, points_in_bucket)
      variable_errors <- c(variable_errors, variable_cutoffs[[variable]]$error)
      cutoffs <- c(cutoffs, variable_cutoffs[[variable]]$cutoff)

    }
    
    best_variable <- names(variable_cutoffs)[min(variables_errors) == variables_errors]
    
    print('Best Variable')
    print(best_variable)
    
    points_in_bucket <- variable_cutoffs[[best_variable]]$points_in_bucket_eval
    
    points_in_bucket$predicted_class <- sapply(points_in_bucket$bucket, function(bucket_name_new){
      predicted_class(points_in_bucket, bucket_name_new)
    })
    
    
    
    all_points <- all_points %>% 
      filter(!(bucket == bucket_name)) %>% 
      bind_rows(points_in_bucket)    
  }

}
p <- plot_points(all_points)
print(p, newpage = F)

