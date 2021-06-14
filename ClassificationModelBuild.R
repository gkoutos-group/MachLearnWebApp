

MergeOutFuns <- function(g1, SelectedModels){
  

  ################### 
  
  TrainingMods <- lapply(seq_along(1:length(g1)), function(i) {
    #print(paste0("i",i))
    lapply(seq_along(1:length(g1[[1]])),
           function(j) {
             #print(paste0("j",j))
             data.frame(g1[[i]][[j]][[2]], Boost = j, Run = i)
           })
  })
  
  m <- do.call("rbind", TrainingMods)
  a <- rbindlist(m)
  
  # Model coding:- those selected in the modelling had been stored in SelectedModels
  # 1 -logit
  # 2 -rf
  # 3 -dt
  # 4 -bt
  # 5 - dtR
  
  namesModels <- c("logit", "rf", "dt", "bt", "dtR")
  
  Basis <- list()
  
  Basis <- lapply(seq_along(1:length(SelectedModels)), function(i) {
    a %>%
      select(starts_with(c(namesModels[SelectedModels][i], "Boost", "Run"))) %>%
      set_colnames(c(
        "num",
        "truth",
        "pred_Alive",
        "pred_Dead",
        "pred_Class",
        "Boost",
        "Run"
      )) %>%
      mutate(Model = namesModels[SelectedModels][i])
    
  })
  
  
  Basis <- rbindlist(Basis)
  
  
  hp.detach("readr")
  #hp.detach("Metrics")
  
  multi_metric <- metric_set(roc_auc)
  multi_metric2  <- metric_set(accuracy, sens, spec)
  
  #make sure both factors are the same 
  
  Basis$pred_Class2 <- factor(Basis$pred_Class, levels = c(levels(Basis$truth)[1], levels(Basis$truth)[2]))
  
  #Obtain statistics from results
  #1 mins
  Output <- bind_rows (
    Basis %>%
      group_by(Run, Model, Boost) %>%
      roc_auc(truth, pred_Alive) %>%
      select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      arrange(desc(Mean)) %>%
      mutate_if(is.numeric, ~ round(., 3)) ,
    
    Basis %>%
      group_by(Run, Model, Boost) %>%
      multi_metric2(truth = truth, estimate = pred_Class2) %>%
      select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      mutate_if(is.numeric, ~ round(., 3))
  )
  
  
  Train <- Output %>%
    ggplot(aes(x = Run, y = Mean, col = Model)) +
    geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd)) +
    facet_grid(. ~ .metric) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Prediction Evaluation Statistics ")
  
  
  FinalFinal <- lapply(seq_along(1:length(g1)), function(i) {
    lapply(seq_along(1:length(g1[[1]])),
           function(j) {
             data.frame(g1[[i]][[j]][[3]], Boost = j, Run = i)
           })
  })
  
  
  m1 <- do.call("rbind", FinalFinal)
  a1 <- rbindlist(m1)
  
  multi_metric2  <- metric_set(accuracy, sens, spec)
  Name <- names(a1)[2]
  
  Output2 <- bind_rows (
    a1 %>%
      group_by(Run, Model, Boost) %>%
      roc_auc(Label, paste0(Name)) %>%
      select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      arrange(desc(Mean)) %>%
      mutate_if(is.numeric, ~ round(., 3)) ,
    
    a1 %>%
      group_by(Run, Model, Boost) %>%
      multi_metric2(Label, estimate = .pred_class) %>%
      select(-c(Boost, .estimator)) %>%
      group_by(Model, Run, .metric) %>%
      dplyr::summarize(Mean = mean(.estimate), sd = sd(.estimate)) %>%
      mutate_if(is.numeric, ~ round(., 3))
  )
  
  
  Test <- Output2 %>% ggplot(aes(x = Run, y = Mean, col = Model)) +
    geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd)) +
    facet_grid(. ~ .metric) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Prediction Evaluation Statistics ")
  
  
  MergeOut <- rbind(Output %>%
                      mutate(Type = "Train"),
                    Output2 %>%
                      mutate(Type = "Test"))
  
  MergeOut$.metric <- as.factor(MergeOut$.metric)
  
  #print("Finished")
  
  MergeOut <- MergeOut %>%
    mutate(ModelFinal = paste0(Model, "+", Type)) %>%
    mutate(
      Metrics = recode(
        .metric,
        'roc_auc' = "AUROC",
        'sens' = "Sensitivity",
        'spec' = "Specificity",
        'accuracy' = "Accuracy"
      )
    )
  
  
}


hp.detach <- function(package_name){
  
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



iter_parallelFinal <- function(i, data, hh,method,Bootstraps,SelectedModels)  {
  
  
  print(i)
  
  Preop <- data
  
  dataFS <- Preop %>%
    dplyr::select(hh[[i]], Label)
  
  
  SelectedModels2 <- SelectedModels
  
  set.seed(132)
  
  Pipeline <- function(j, dataFS, method,SelectedModels2) {
    
    
    print(j)
    
    data <- dataFS
    
    TrackSplit <- initial_split(data, strata = Label)
    uni_split <- TrackSplit
    uni_train <- training(uni_split)
    uni_test <- testing(uni_split)
    
    # why no downsample or dummy in previous?? - This is why 0.7 ??
    
    Conds <- Conditional(uni_train)
    
    PreProc <- recipe(Label ~ . , data = uni_train) %>%
      { if (Conds$Disbalance == 3) step_upsample(., Label, seed = 132) else 
        (if (Conds$Disbalance == 2) step_downsample(., Label, seed = 132, under_ratio = 2) else .) } %>%
      { if (Conds$Normalize == 2) step_normalize(.,all_numeric()) else .} %>%
      { if (Conds$Dummy == 2) step_dummy(.,all_nominal(),-all_outcomes(), one_hot = TRUE) else .} %>%
      #{ if (Conds$Interact == 2) step_interact(terms = ~ all_predictors() * all_predictors()) else .} %>%
      step_nzv(all_predictors()) #%>%
    #step_corr(all_predictors()) 
    
    print('here')
    
    PreparedPreProc <-  PreProc  %>% 
      prep()
    
    f <- juice(PreparedPreProc)
    
    
    logit_tune_pra <- logistic_reg() %>%
      set_engine("glm")
    
    # Hyperparameter grid
    logit_grid <- logit_tune_pra %>%
      parameters()
    
    # Workflow bundling every step
    logit_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(logit_tune_pra)
    
    #problem with mtry
    rf_tune_pra <- rand_forest(trees = tune(), min_n  = tune()) %>%
      set_engine("randomForest") %>%
      set_mode("classification")
    
    rf_grid <- rf_tune_pra %>%
      parameters() %>%
      grid_max_entropy(size = 5)
    
    rf_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(rf_tune_pra)
    
    dt <-
      decision_tree(
        tree_depth = tune(),
        min_n = tune(),
        cost_complexity = tune()
      ) %>%
      set_engine("rpart") %>%
      set_mode("classification")
    
    # Hyperparameter grid
    dt_grid <- dt %>%
      parameters() %>%
      grid_max_entropy(size = 20)
    
    # Workflow bundling every step
    dt_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(dt)
    
    bt <-  boost_tree(trees = tune(), learn_rate = tune()) %>%
      set_engine("xgboost") %>%
      set_mode("classification")
    
    bt_grid <- bt %>%
      parameters() %>%
      grid_max_entropy(size = 3)
    
    # Workflow bundling every step
    bt_wflow <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(bt)
    
    
    dtr  <-
      parsnip::rand_forest(mtry = tune(),
                           trees = tune()) %>%
      parsnip::set_engine(
        "ranger",
        regularization.factor = 0.3,
        regularization.usedepth = FALSE
      ) %>%
      set_mode("classification")
    
    
    dt_gridr <- grid_regular(
      mtry() %>% range_set(c(1, 15)),
      trees() %>% range_set(c(200, 300)),
      min_n() %>% range_set(c(2, 10)),
      levels = 3
    )
    
    dt_wflowr <- workflow() %>%
      add_recipe(PreProc) %>%
      add_model(dtr)
    
    
    metrics <- metric_set(roc_auc, j_index) #spec does not work
    
    mdls <- list(
      'logit' = list(logit_wflow, logit_grid, "logit"),
      'rf' = list(rf_wflow, rf_grid, "rf"),
      'dt' = list(dt_wflow, dt_grid, "dt"),
      'bt' = list(bt_wflow, bt_grid, "bt"),
      'dtR' = list(dt_wflowr, dt_gridr, "dtR")
    )
    
    
    mdls <- mdls[c(SelectedModels2)]
    NumModels <- length(mdls)
    wflow_list <- mdls %>%  map(c(1))
    grid_list <- mdls %>%  map(c(2))
    namesModels  <- mdls %>% map(c(3))
    
    set.seed(132) #new
    
    folds <- vfold_cv(uni_train, v = 10, strata = Label)
    
    trained_models_list <- future_map2(
      .x = wflow_list,
      .y = grid_list,
      ~ tune_grid(
        .x ,
        resamples = folds,
        grid = .y,
        metrics = metrics
      )
    )
    
    gg <-  trained_models_list %>% map_dfr(function(dat) {
      dat %>%
        collect_metrics()
    })
    
    best_params <- trained_models_list %>%
      map(select_best, metric = "roc_auc")
    
    trained_models_list2 <- Map(function(x, y) {
      x$Model <- y
      x
    }, trained_models_list, namesModels)
    
    
    FinWork <-
      map2(.x = wflow_list, .y = best_params, ~ finalize_workflow(.x, .y))
    
    Julia <- map(.x = FinWork, ~ fit(.x, uni_train))
    
    final_metrics3 <- map(.x = Julia,
                          
                          function (Wf = .x)
                          {
                            # Split the data into analysis and assessment tables
                            
                            # Summarise Predictions
                            table <-
                              tibble(
                                num = j,
                                truth = as.factor(uni_train$Label),
                                .pred_Alive =
                                  predict(Wf,
                                          new_data = uni_train,
                                          type = "prob")[[paste0(".pred_",levels(as.factor(data$Label))[1])]],
                                .pred_Dead =
                                  predict(Wf,
                                          new_data = uni_train,
                                          type = "prob")[[paste0(".pred_",levels(as.factor(data$Label))[2])]],
                                .pred_Class =
                                  predict(Wf,
                                          new_data = uni_train) %>%
                                  unlist() %>%
                                  as.character()
                              ) %>%
                              mutate(.pred_Class = factor(.pred_Class))
                          })
    
    
    newNames <- c(paste0(".pred_",levels(as.factor(data$Label))[1]), paste0(".pred_",levels(as.factor(data$Label))[2]))
    oldNames <- names(final_metrics3[[1]][3:4]) 
    secondArgument <- setNames(newNames,oldNames)
    df2 <- lapply(names(final_metrics3), function(x) {plyr::rename(final_metrics3[[x]], secondArgument)})
    names(df2) <- names(final_metrics3)
    final_metrics3 <- df2
    
    
    final_metrics <-
      map_df(.x = FinWork,
             .y = uni_split,
             ~ last_fit(.x, .y, metrics = metrics))
    
    
    FinalPred <-  final_metrics %>% 
      pull(.predictions)
    
    names(FinalPred) <- namesModels
    
    FinalPred3 <- bind_rows(FinalPred, .id = "Model")
    
    h <- names(dataFS)[-which(names(dataFS) %in% "Label")]
    h2 <- names(f)[-which(names(f) %in% "Label")]
    
    
    Back <-    invisible(
      list(
        best_params,
        final_metrics3,
        data.frame(
          FinalPred3,
          num = j,
          Variables = paste(h, collapse = "+"),
          Variables2 = paste(h2, collapse = "+")
        ),
        Julia
      )
    )
    
  }
  
  
  
  FurrDat <- 
    furrr::future_map(.x= c(1:Bootstraps), ~invisible(Pipeline(.x, dataFS, method,SelectedModels2)), future.seed = TRUE)
  
  #print("yes")
  #return(FurrDat)
  
  
  
  
}






