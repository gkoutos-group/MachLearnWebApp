

#source("GeneralFSMachLearnClass.R")
#source("GeneralFSMachLearnReg.R")


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
  
  #
  # detach("package:readr", unload=TRUE) #needed if metric_set does not work well
  ##detach("package:Metrics", unload=TRUE)
  #library(yardstick)
  #detach("package:readr", unload=TRUE)
  #library(tidyverse)
  #library(dplyr)
  #library(rlang)
  #env_name(fn_env(roc_auc))
  #> [1] "namespace:readr"
  #rcompanion
  
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
  
  ## dso_filenames <- dir(tempdir(), pattern = .Platform$dynlib.ext)
  ## filenames  <- dir(tempdir())
  ## for (i in seq(dso_filenames)) {
  ##     if (package_name == dso_filenames[i]) {
  ##         dyn.unload(file.path(tempdir(), dso_filenames[i]))
  ##     }
  ## }
  ## for (i in seq(filenames)) {
  ##     if (file.exists(file.path(tempdir(), filenames[i])) &
  ##         nchar(filenames[i]) < 42) {
  ##         # some files w/ long filenames that didn't like to be removeed
  ##         if (package_name == filenames[i]) {
  ##             file.remove(file.path(tempdir(), filenames[i]))
  ##         }
  ##     }
  ## }
  
}

label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

Plot2 <- function(data){
  
  source("COVIDJune2Functions.R")
  MatrixCorr <- data %>% 
    select_if(~!is.ordered(.)) %>%
    mixed_assoc() 
  
  MatrixCorr  %>%
    select(x, y, assoc) %>%
    spread(y, assoc) %>%
    column_to_rownames("x") %>%
    as.matrix %>%
    as_cordf %>%
    network_plot( colors= c("red","green"))
  
}


Plot1 <- function(FinalData){
  
  Label <- data.frame(Label=FinalData$Label) 
  print("here")
  FinalData <- FinalData %>% 
    dplyr::select(-Label)
  
  FinalDataNumeric <- FinalData %>% 
    select_if(is.numeric)  %>%
    add_column(Label) 
  
  FinalDataChar <- FinalData  %>%
    select_if(~!is.numeric(.)) %>% 
    mutate_if(~!is.character(.), as.character) %>% 
    add_column(Label)
  print("here2")
  
  Model2aPivot <- FinalDataNumeric %>% 
    pivot_longer(  -Label, names_to = "Features", values_to = "Value")
  print("here5")
  
  NumPlots <-  Model2aPivot %>% 
    ggplot(aes( Value, fill = Label)) +
    geom_density(alpha=.5)+ facet_wrap(~ Features, scales = "free") +
    labs(title= "Relationship of Variables and Outcome", x = "Density", y= "Value")
  
  if (dim(FinalDataChar)[2] != 1){
    
    Model1aPivot <- FinalDataChar %>%  
      pivot_longer(-Label, names_to = "Features", values_to = "Value")
    
    CharPlots <-  Model1aPivot %>% 
      group_by(Features) %>%
      count(Value, Label,  sort=TRUE) %>%
      ungroup %>% 
      mutate(Value = fct_reorder(Value, n, sum)) %>%
      ggplot(aes( n, Value, fill = Label)) +
      geom_col()+ facet_wrap(~ Features, scales = "free") +
      labs(title= "Relationship of Variables and Outcome", x = "Number of Patients", y= "Levels of Feature")
    
    return(CharPlots/NumPlots)} else{
      
    }
  return(NumPlots)
  
}


FeatureSelection3 <- function(data){
  
  NumCols <- dim(data)[2]
 
   if (NumCols > 20) {
    
    NumCols <- 20

  } else {
    
    NumCols <- NumCols
    
  }
  
 # PreProcL <- recipe(Label ~ . , data = data) %>%
 #   step_downsample(Label, seed = 132, under_ratio = 2) %>%
 #   step_normalize(all_numeric()) %>%
 #   step_dummy(all_nominal(),-all_outcomes(), one_hot = TRUE) %>%
 #   step_interact(terms = ~ all_predictors() * all_predictors()) %>%
 #   step_nzv(all_predictors()) %>%
 #   step_corr(all_predictors()) %>%
 #   step_lincomb(all_predictors())
  
  
  PreProcL2 <- recipe(Label ~ . , data = data) %>%
    step_downsample(Label, seed = 132, under_ratio = 2) %>%
    step_normalize(all_numeric()) %>%
    step_dummy(all_nominal(),-all_outcomes(), one_hot = TRUE) %>%
    #step_interact(terms = ~ all_predictors() * all_predictors()) %>%
    step_nzv(all_predictors()) #%>%
  #step_corr(all_predictors()) %>%
  #step_lincomb(all_predictors())
  
  
  
  PreProcRF <- recipe(Label ~ . , data = data) %>%
    step_downsample(Label, seed = 132, under_ratio = 2) %>%
    step_normalize(all_numeric()) %>%
    #step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    #step_interact(terms = ~ all_predictors() * all_predictors()) %>%
    step_nzv(all_predictors()) #%>%
  #step_corr(all_predictors()) %>%
  #step_lincomb(all_predictors())
  

  ################# Modeling and parameter setting 
  
  set.seed(132)
  office_bootLASSO <- bootstraps(data, strata = Label, times = 10)
  set.seed(132)
  office_bootRF <- bootstraps(data, strata = Label, times = 5)
  
 # tune_spec <-
 #   logistic_reg(mode = "classification",
 #                penalty = tune(),
 #                mixture = 1) %>%
 #   set_engine("glmnet")
  
  tune_spec2 <-
    logistic_reg(mode = "classification",
                 penalty = tune(),
                 mixture = 0.5) %>%
    set_engine("glmnet")
  
  lambda_grid <- grid_regular(penalty(), levels = 30)
  
 #lasso_wflow <- workflow() %>%
 #  add_recipe(PreProcL) %>%
 #  add_model(tune_spec)
  
  lasso_wflow2 <- workflow() %>%
    add_recipe(PreProcL2) %>%
    add_model(tune_spec2)
  
  RF_tune_spec <-
    rand_forest(mode = "classification",
                trees = tune(),
                min_n = tune()) %>%
    set_engine("ranger", importance = "impurity")
  #set_engine("randomForest", importance = "")
  
  
  rf_workflow <- workflow() %>%
    add_recipe(PreProcRF) %>%
    add_model(RF_tune_spec)
  
  set.seed(132)
  rf_grid <-  grid_regular(parameters(RF_tune_spec), levels = 3)
  
  
  #wf <- list(lasso_wflow, rf_workflow, lasso_wflow2)
  #grid <- list(lambda_grid, rf_grid, lambda_grid)
  #boost <- list(office_bootLASSO, office_bootRF, office_bootLASSO)
  
  wf <- list(rf_workflow, lasso_wflow2)
  grid <- list(rf_grid, lambda_grid)
  boost <- list(office_bootRF, office_bootLASSO)
  
  metrics <- metric_set(roc_auc)
  
  iter_parallel2 <- function(i)  {
    
    print(i)
    set.seed(132)
    Dome <- tune_grid(
      wf[[i]],
      resamples = boost[[i]],
      grid = grid[[i]],
      metrics = metrics
    )
    
    print("tune done")
    
    return (Dome)
  }
  
 jobid <- future_lapply(
   seq_along(1:2), 
   function(i) {
     iter_parallel2(i)
   }
   , future.seed = TRUE)

 RFLASSO  <- jobid
 
 best_params <- RFLASSO %>%
   map(select_best, metric = "roc_auc") #  maximize = FALSE
 
 FinWork <- map2(.x = wf, .y = best_params, ~ finalize_workflow(.x, .y))
 
 FinWorkFeatures <- map(.x = FinWork, ~ pull_workflow_fit(fit(.x, data)))
 
 rankFilt <- 7

    # Get feature importance LASSO
 
 #  s <- FinWorkFeatures[[1]] %>% 
 #    vi(lambda = best_params[[1]]$penalty) %>%
 #    mutate(Importance = abs(Importance),
 #           Variable = fct_reorder(Variable, Importance))
 #  
 #  s <- s %>% 
 #    mutate(rank = dense_rank(desc(Importance))) %>% 
 #    mutate(Model = "LASSO") %>% 
 #    select(Sign, Variable, Importance, rank, Model) 
 #  
 #  p <- s
 # 
 #  PlotLASSO <-  p %>%
 #    mutate(rank = dense_rank(desc(Importance))) %>% 
 #    filter(rank < NumCols) %>%
 #    ggplot(aes(x = Importance, y = reorder(Variable, Importance), fill = Sign)) +
 #    geom_col() +
 #    scale_x_continuous(expand = c(0, 0)) + 
 #    labs(subtitle =  "Glmnet: LASSO and Interactions") + 
 #    xlab("Importance") + 
 #    ylab("Variable") +
 #    theme_light() + 
 #    scale_fill_manual(
 #      name = "Sign",
 #      values = c('springgreen3', 'red3'),
 #      labels = c("Low Risk", "High Risk")
 #    ) + 
 #    theme(text = element_text(size=8))
 #  
   #Lasso 2
   
   s2 <- FinWorkFeatures[[2]] %>% 
     vi(lambda = best_params[[2]]$penalty) %>%
     mutate(Importance = abs(Importance),
            Variable = fct_reorder(Variable, Importance))
   
   s2 <- s2 %>% 
     mutate(rank = dense_rank(desc(Importance))) %>% 
     mutate(Model = "LASSOEN") %>% 
     select(Sign, Variable, Importance, rank, Model)
   
   p2 <- s2
  
   
   PlotLASSO2 <-  p2 %>%
     mutate(rank = dense_rank(desc((Importance)))) %>% 
     filter(rank < NumCols) %>%
     ggplot(aes(x = Importance, y = reorder(Variable, Importance), fill = Sign)) +
     geom_col() +
     scale_x_continuous(expand = c(0, 0)) + 
     labs(subtitle =  "Glmnet: EN") + 
     xlab("Importance") + 
     ylab("Variable") +
     theme_light() + 
     scale_fill_manual(
       name = "Sign",
       values = c('springgreen3', 'red3'),
       labels = c("Low Risk", "High Risk")
     ) + 
     theme(text = element_text(size=8))
   
   
   # Get feature importance RF
   
   qF <- FinWorkFeatures[[1]]$fit$variable.importance %>%
     enframe("Variable", "Importance")
   
   pqF <- qF
   
   
   PlotRF <- pqF %>%
     mutate(rank = dense_rank(desc(Importance))) %>% 
     filter(rank < NumCols) %>%
     ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
     geom_col() +
     coord_flip() +
     scale_fill_viridis_d(end = .7) +
     labs(subtitle =  "Random Forest: Ranger") + 
     xlab("Variables") + 
     ylab("Importance") + theme_light() 
   
   qF <- qF %>% 
     mutate(rank = dense_rank(desc(Importance))) %>% 
     mutate(Model = "RF")
   
   #Merge both LASSO and RF
   
   #FinalFS1 <- rbind(s %>% select(-Sign), qF)
   FinalFS1 <- rbind(s2 %>% select(-Sign), qF)
   #FinalFS1 <- rbind(s2 %>% select(-Sign), FinalFS1)
   FinalFS <- FinalFS1 %>% 
     filter(rank <= rankFilt) %>%
     mutate(rankMod = paste0(rank, "-", Model))
   
   
   FinalFS2 <- FinalFS  %>% 
     separate(Variable, c("first", "second", "third","fourth"), sep="_" ) %>%
     select(c(1, 4, 6,7,8))
   
   #FinalFS2 <- FinalFS2 %>%
   #  filter(Model != "LASSO")
   
   FinalFS2 <- rbind(FinalFS2[,c(1,3,4, 5)], data.frame(first = FinalFS2$fourth, rank = FinalFS2$rank, Model = FinalFS2$Model, rankMod = FinalFS2$rankMod)) %>%
     na.omit() %>%
     add_count(first) %>%
     #filter(Model != "LASSO") %>%
     arrange(desc(n), rank) %>%
     rename('Variable Name' = first) %>%
     rename('Freq of appearance' = n) %>%
     rename('Top rank in model' = rankMod) %>%
     select(-c(rank, Model))
   
   FinalFS2 <- FinalFS2[!duplicated(FinalFS2$'Variable Name'), ] 
   
   PlotLASSO <- list()
   return(list(FinalFS2, PlotRF, PlotLASSO, PlotLASSO2))
   #return(list(FinalFS2, PlotRF, PlotLASSO2))
  
  
  #return(list(PlotLASSO,PlotRF, FinalFS1))
}

# The idea is to place a spinner image in the same container as the plot
# and center it, but give it a below-default z-index
# Whenever the plot is recalculting, make the plot's z-index even lower
# so that the spinner will show

Conditional <-  function( data ) {
  
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
  
  #Type
  
  #delete ordered factors
  #z <- sapply(data, class)
  data <- data %>%
    select_if(~!is.ordered(.))
  #select(-names(which(sapply(z, length) ==2)))
  
  Type <- data.frame(table(sapply(data, class)))
  
  if (Type$Freq[1] < 2){
    print("No factor vars")
    Dummy <- 1
    
  } else{
    print("Factor vars")
    Dummy <- 2
  }
  
  if (!is.na(Type$Freq[2])){
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
                                          type = "prob")[[paste0(".pred_",levels(data$Label)[1])]],
                                .pred_Dead =
                                  predict(Wf,
                                          new_data = uni_train,
                                          type = "prob")[[paste0(".pred_",levels(data$Label)[2])]],
                                .pred_Class =
                                  predict(Wf,
                                          new_data = uni_train) %>%
                                  unlist() %>%
                                  as.character()
                              ) %>%
                              mutate(.pred_Class = factor(.pred_Class))
                          })
    
    
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


################################


Plot1aC <- function(FinalData) {
  
  Label <- data.frame(Label = FinalData$Label)
  
  FinalData <- FinalData %>%
    dplyr::select(-Label)
  
  FinalDataNumeric <- FinalData %>%
    select_if(is.numeric)  %>%
    add_column(Label)
  
  FinalDataChar <- FinalData  %>%
    select_if( ~ !is.numeric(.)) %>%
    mutate_if( ~ !is.character(.), as.character) %>%
    add_column(Label)
  
  Model1aPivot <- FinalDataChar %>%
    
    pivot_longer(-Label, names_to = "Features", values_to = "Value")
  
  cols <-
    c(
      "DD" = "red3",
      "DA" = "darkseagreen1",
      "AD" = "coral1",
      "AA" = "forestgreen"
    )
  
  CharPlots <-  Model1aPivot %>%
    group_by(Features) %>%
    count(Value, Label,  sort = TRUE) %>%
    ungroup %>%
    mutate(Value = fct_reorder(Value, n, sum)) %>%
    ggplot(aes(n, Value, fill = Label)) +
    geom_col(aes(colour = "black")) + facet_wrap( ~ Features, scales = "free") +
    labs(title = "Relationship of Variables and Outcome", x = "Number of Patients", y = "Levels of Feature") + scale_colour_manual(values = cols,
                                                                                                                                   aesthetics = c("fill")) + guides(colour =
                                                                                                                                                                      FALSE)
  
  return(CharPlots)
}



