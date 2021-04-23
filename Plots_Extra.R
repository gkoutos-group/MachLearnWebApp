

Plot2A <- function(FinalData){
  
  #FinalData <- data.frame(x = c("a", "b", "a"), Label = c("Yes", "No", "No"), one = c(1:3))
  #getDataInsight(FinalData)
  
  Label <- data.frame(Label=FinalData$Label) 
  
  FinalData <- FinalData %>% 
    dplyr::select(-Label)
  
  FinalDataNumeric <- FinalData %>% 
    select_if(is.numeric)  %>%
    add_column(Label) 
  
  FinalDataChar <- FinalData  %>%
    select_if(~!is.numeric(.)) %>% 
    mutate_if(~!is.character(.), as.character) %>% 
    add_column(Label) 
  
  Model1aPivot <- FinalDataChar %>%  
    pivot_longer(-Label, names_to = "Features", values_to = "Value")  %>% 
    group_by(Features) %>%
    dplyr::count(Value, Label,  sort=TRUE) %>%
    ungroup %>% 
    mutate(Value = fct_reorder(Value, n, sum)) 
  
  Model2aPivot <- FinalDataNumeric %>%  
    pivot_longer(-Label, names_to = "Features", values_to = "Value") 
  
  Res <- ggplot() + facet_wrap(~ Features, scales = "free") +
    geom_col(data = Model1aPivot, aes( n, Value, fill = Label)) + 
    geom_density(data = Model2aPivot, aes(Value, fill = Label), alpha=.5) +
    labs(title= "Relationship of Variables and Outcome", x = "Number of Patients", y= "Levels of Feature")
  
  Res + theme_bw()
}

#####################################


hp.detach <- function(package_name){
    
    #Obtained from: https://github.com/genular/simon-frontend

  
  search_item <- paste("package", package_name, sep = ":")
  
  while (search_item %in% search()) {
    unload_package <- tryCatch({
      detach(search_item,
             character.only = TRUE,
             unload = TRUE)
    },
    error = function(msg) {
      cat(
        paste0(
          "===> ERROR: (unload_package) ",
          match.call()[[1]],
          " (error): ",
          msg,
          "\r\n"
        )
      )
      return(NULL)
    },
    warning = function(msg) {
      cat(
        paste0(
          "===> ERROR: (unload_package) ",
          match.call()[[1]],
          " (warning): ",
          msg,
          "\r\n"
        )
      )
      return(NULL)
    },
    finally = {
      
    })
  }
  
  
}

label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}



Conditional <-  function( data ) {
  
  y_is_discrete <- is_discrete(data$Label)
  y_is_binary <- is_binary(data$Label)
  
  mode <-
    case_when(
      y_is_discrete | y_is_binary ~ 'classification',
      TRUE ~ 'regression'
    )
  
  #if something with g1 that automatically detects regression or classification
  
  if (mode == 'classification'){
    

  NumStrat <- data.frame(table(data$Label))
  NumPats <- dim(data)[1]
  
  #Disbalance
  if (( min(NumStrat$Freq) / max(NumStrat$Freq))  < 0.45) {
    
    
    if ( NumPats   <  70){
      print("Disbalanced - too little samples")
      Disbalance <- 3
      
    } else {
      print("Disbalanced - enough samples")
      Disbalance <- 2
    }
  } else {
    
    print("No disbalance")
    Disbalance <- 1
    
  }
  
  } else{
    
    print("No disbalance")
    Disbalance <- 1
    
  }
  
  
  #Type
  
  #delete ordered factors
  #z <- sapply(data, class)
  data <- data %>%
    select_if(~!is.ordered(.))
  #select(-names(which(sapply(z, length) ==2)))
  
  Type <- data.frame(table(sapply(data, class)))
  
  if (any(Type$Var1 == "factor")){
    print("Factor vars")
    Dummy <- 2
    
  } else{
    print("No Factor vars")
    Dummy <- 1
  }
  
  if (any(Type$Var1 == "numeric")){
    print("Numeric vars")
    Normalize <- 2
    
  } else{
    print("No numeric vars")
    Normalize <- 1
  }
  
  #Interact
  
  if (dim(data)[1] > 10) {
    print("No interactions included")
    Interact <- 1
  } else{
    print("Interactions included")
    Interact <- 2
  }
  

  
  data.frame(Disbalance, Dummy, Normalize, Interact)
  
}


