
#Obtained from: https://www.r-bloggers.com/2020/07/comparing-variable-importance-functions-for-modeling/


.seed <- 42L
set.seed(.seed)

.engines_valid <- c('glm', 'glmnet', 'xgboost', 'ranger')

engines_named <- .engines_valid %>% setNames(., .)

.modes_valid <- c('regression', 'classification')


explain.glmnet <-
  function (object,
            feature_names = NULL,
            X,
            nsim = 1,
            pred_wrapper,
            newdata = NULL,
            exact = FALSE,
            ...) {
    
    if (isTRUE(exact)) {
      if (is.null(X) && is.null(newdata)) {
        stop('Must supply `X` or `newdata` argument (but not both).', call. = FALSE)
      }
      X <- if (is.null(X)) 
        newdata
      else X
      res_init <- stats::predict(object, newx = X, s = 0, type = 'coefficients', ...)
      
      # https://stackoverflow.com/questions/37963904/what-does-predict-glm-type-terms-actually-do
      beta <- object %>% coef(s = 0) %>% as.matrix() %>% t()
      avx <- colMeans(X)
      X1 <- sweep(X, 2L, avx)
      res <- t(beta[-1] * t(X1)) %>% as_tibble() %>% mutate_all(~coalesce(., 0))
      attr(res, which = 'baseline') <- beta[[1]]
      class(res) <- c(class(res), 'explain')
      res
    } else {
      fastshap:::explain.default(
        object,
        feature_names = feature_names,
        X = X,
        nsim = nsim,
        pred_wrapper = pred_wrapper,
        newdata = newdata,
        ...
      )
    }
  }

vip_wrapper <- function(method, ...) {
  res <-
    vip::vip(
      method = method,
      ...
    ) %>% 
    pluck('data') %>% 
    # Will get a "Sign" column when using the default `method = 'model'`.
    rename(var = Variable, imp = Importance)
  
  if(any(names(res) == 'Sign')) {
    res <-
      res %>% 
      mutate(dir = ifelse(Sign == 'POS', +1L, -1L)) %>% 
      mutate(imp = dir * imp)
  }
  res
}

choose_f_fit <- function(engine = .engines_valid, mode = .modes_valid) {
  engine <- match.arg(engine)
  mode <- match.arg(mode)
  f_glm <- list(parsnip::linear_reg, parsnip::logistic_reg) %>% set_names(.modes_valid)
  fs <-
    list(
      'xgboost' = rep(list(parsnip::boost_tree), 2) %>% set_names(.modes_valid),
      'ranger' = rep(list(parsnip::rand_forest), 2) %>% set_names(.modes_valid),
      'glm' = f_glm,
      'glmnet' = f_glm
    )
  res <- fs[[engine]][[mode]]
  res
}

choose_f_predict <- function(engine = .engines_valid) {
  engine <- match.arg(engine)
  f_generic <- function(object, newdata) predict(object, newdata = newdata)
  fs <-
    list(
      'xgboost' = f_generic,
      'ranger' = function(object, newdata) predict(object, data = newdata)$predictions,
      'glm' = f_generic,
      # Choosing no penalty.
      'glmnet' = function(object, newdata) predict(object, newx = newdata, s = 0)
    )
  fs[[engine]]
}

is_binary <- function(x) {
  n <- unique(x)
  length(n) - sum(is.na(n)) == 2L
}

is_discrete <- function(x) {
  is.factor(x) | is.character(x)
}


# I would certainly not recommend a big function like this in a normal type of project or analysis. But, in this case, it makes things more straightforward.

compare_and_rank_vip <-
  
  function(data,
           col_y,
           engine = .engines_valid,
           mode = NULL,
           ...) {
    
    message(engine)
    engine <- match.arg(engine)
    
    if(!is.null(mode)) {
      mode <- match.arg(mode, .modes_valid)
    } else {
      y <- data[[col_y]]
      y_is_discrete <- is_discrete(y)
      y_is_binary <- is_binary(y)
      
      mode <-
        case_when(
          y_is_discrete | y_is_binary ~ 'classification',
          TRUE ~ 'regression'
        )
    }
    
    mode_is_class <- mode == 'classification'
    parsnip_engine <- 
      case_when(
        engine == 'glm' & !mode_is_class ~ 'lm', 
        TRUE ~ engine
      )
    
    f_fit <- choose_f_fit(engine = engine, mode = mode)
    fmla <- formula(sprintf('%s ~ .', col_y))
    set.seed(.seed)
    splits <- data %>% rsample::initial_split(strata = col_y)
    
    data_trn <- splits %>% rsample::training()
    data_tst <- splits %>% rsample::testing()
    
    rec <- 
      recipes::recipe(fmla, data = data_trn) %>% 
      # Note that one-hot encoding caused rank deficiencies with `glm()` and `{DALEX}`.
      recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes(), one_hot = FALSE)
    
    is_ranger <- engine == 'ranger'
    f_engine <- if(is_ranger) {
      partial(parsnip::set_engine, engine = parsnip_engine, importance = 'permutation')
    } else {
      partial(parsnip::set_engine, engine = parsnip_engine)
    }
    
    spec <- 
      f_fit() %>%
      f_engine() %>% 
      parsnip::set_mode(mode)
    
    wf <-
      workflows::workflow() %>%
      workflows::add_recipe(rec) %>%
      workflows::add_model(spec)
    
    fit <- wf %>% parsnip::fit(data_trn)
    fit_wf <- fit %>% workflows::pull_workflow_fit()
    
    data_trn_jui <-
      rec %>% 
      recipes::prep(training = data_trn) %>% 
      recipes::juice()
    
    x_trn_jui <-  data_trn_jui[, setdiff(names(data_trn_jui), col_y)] %>% as.matrix()
    y_trn_jui <- data_trn_jui[[col_y]]
    
    y_trn_jui <- 
      if(mode_is_class) {
        as.integer(y_trn_jui) - 1L
      } else {
        y_trn_jui
      }
    
    vip_wrapper_partial <-
      partial(
        vip_wrapper, 
        object = fit_wf$fit, 
        num_features = x_trn_jui %>% ncol(), 
        ... = 
      )
    
    # Returns POS/NEG for glm/glmnet disc
    vi_vip_model <- vip_wrapper_partial(method = 'model')
    
    # I believe these are the defaults chosen by `{vip}` (although its actual default is `metric = 'auto'`).
    metric <- ifelse(mode_is_class, 'sse', 'rmse')
    f_predict <- choose_f_predict(engine = engine)
    
    vip_wrapper_partial_permute <-
      partial(
        vip_wrapper_partial,
        method = 'permute',
        metric = metric,
        pred_wrapper = f_predict,
        ... = 
      )
    
    # # lm method for regression won't work with the general case.
    # vi_vip_permute <-
    #   if(engine == 'glm') {
    #     vip_wrapper_partial_permute(
    #       train = data_trn_jui,
    #       target = col_y
    #     )
    #   } else {
    #     vip_wrapper_partial_permute(
    #       train = x_trn_jui %>% as.data.frame(),
    #       target = y_trn_jui
    #     )
    #   }
    
    f_coerce_permute <- ifelse(engine != 'glm', function(x) { x }, as.data.frame)
    set.seed(.seed)
    vi_vip_permute <-
      vip_wrapper_partial_permute(
        train = x_trn_jui %>% f_coerce_permute(),
        target = y_trn_jui
      )
    
    # Note that `vip:::vi_shap.default()` uses `{fastshap}` package.
    set.seed(.seed)
    vip_wrapper_partial_shap <-
      partial(
        vip_wrapper_partial, 
        method = 'shap',
        train = x_trn_jui,
        ... = 
      )
    
    vi_vip_shap <-
      if(is_ranger) {
        vip_wrapper_partial_shap(pred_wrapper = f_predict)
      } else {
        vip_wrapper_partial_shap(exact = TRUE)
      }
    
    # # Removed this part since it's basically redundant with the `{vip}` SHAP method (which I checked).
    # fastshap_partial <-
    #   partial(
    #     fastshap::explain,
    #     object = fit_wf$fit,
    #     X = x_trn_jui,
    #     ... = 
    #   )
    # 
    # expl_fastshap <-
    #   if(is_ranger) {
    #     fastshap_partial(pred_wrapper = f_predict)
    #   } else {
    #     fastshap_partial(exact = TRUE)
    #   }
    # 
    # # Need to remove the non-diamonds_modified class in order to use {dplyr} functions.
    # class(expl_fastshap) <- c('tbl_df', 'tbl', 'data.frame')
    # 
    # vi_fastshap <-
    #   expl_fastshap %>%
    #   summarize_all(~mean(abs(.))) %>% 
    #   # This is actually already `imp_abs`, but it won't matter in the end.
    #   pivot_longer(matches('.'), names_to = 'var', values_to = 'imp')
    
    # idk why, but I can use `ifelse()` here and return a function that won't have unexpected output (i.e. a list instead of a dataframe).
    # This is not true for the other `if...else` statements
    f_coerce_dalex <- ifelse(engine == 'xgboost', function(x) { x }, as.data.frame)
    expl_dalex <- 
      DALEX::explain(
        fit_wf$fit, 
        data = x_trn_jui %>% f_coerce_dalex(),
        y = y_trn_jui, 
        verbose = FALSE
      )
    
    # DALEX::loss_root_mean_square == vip::metric_rmse
    # DALEX::DALEX::loss_sum_of_squares == vip::metric_sse
    
    f_loss <- if(mode_is_class) {
      DALEX::loss_sum_of_squares
    } else {
      DALEX::loss_root_mean_square
    }
    set.seed(.seed)
    vi_dalex_init <- 
      expl_dalex %>% 
      DALEX::variable_importance(
        type = 'difference',
        loss_function = f_loss, 
        n_sample = NULL
      )
    vi_dalex_init
    
    # Regarding why `permutation == 0`, see `ingredients:::feature_importance.default()`, which is called by `ingredients:::feature_importance.explainer()`, which is called by `DALEX::variable_importance`
    # Specifically, this line: `res <- data.frame(variable = c("_full_model_", names(res),  "_baseline_"), permutation = 0, dropout_loss = c(res_full, res, res_baseline), label = label, row.names = NULL)`
    vi_dalex <-
      vi_dalex_init %>% 
      as_tibble() %>% 
      filter(permutation == 0) %>% 
      mutate(
        imp = abs(dropout_loss) / max(abs(dropout_loss))
      ) %>% 
      select(var = variable, imp) %>%
      filter(!(var %in% c('_baseline_', '_full_model_'))) %>% 
      arrange(desc(imp))
    
    vi_rnks <-
      list(
        vip_model = vi_vip_model,
        vip_permute = vi_vip_permute,
        vip_shap = vi_vip_shap,
        # fastshap = vi_fastshap,
        dalex = vi_dalex
      ) %>% 
      map_dfr(bind_rows, .id = 'src') %>% 
      group_by(src) %>% 
      mutate(imp_abs = abs(imp)) %>% 
      mutate(imp_abs_norm = imp_abs / sum(imp_abs)) %>% 
      select(var, imp, imp_abs, imp_abs_norm) %>% 
      mutate(rnk = row_number(desc(imp_abs))) %>% 
      ungroup() %>%
      add_column(Type = mode)
    
    vi_rnks
    
    
    
  }


compare_and_rank_vip_q <- quietly(compare_and_rank_vip)

prettify_engine_col <- function(data) {
  res <- data %>% mutate_at(vars(engine), ~sprintf('{%s}', engine))
}

factor_src <- function(x) {
  ordered(x, levels = c('vip_model', 'vip_shap', 'vip_permute', 'dalex'))
}

plot_rnks <- function(df_rnks, option = 'D', Threshold) {
  viz <-
    df_rnks %>% 
    group_by(var) %>% 
    mutate(rnk_mean = rnk %>% mean(na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate_at(vars(var), ~forcats::fct_reorder(., -rnk_mean)) %>% 
    ungroup() %>% 
    prettify_engine_col() %>% 
    mutate_at(vars(src), ~ordered(., levels = c('vip_model', 'vip_shap', 'vip_permute', 'dalex'))) %>% 
    mutate(lab = sprintf('%2d (%s)', rnk, scales::percent(imp_abs_norm, accuracy = 1, width = 2, justify = 'right'))) %>% 
    filter(rnk < Threshold) %>%
    ggplot() +
    aes(x = src, y = var) +
    geom_tile(aes(fill = rnk), alpha = 0.5, show.legend = F) +
    geom_text(aes(label = lab)) +
    scale_fill_viridis_c(direction = -1, option = option, na.value = 'white') +
    theme_minimal(base_family = '') +
    facet_wrap(~engine) +
    theme(
      plot.title.position = 'plot',
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = 'bold'),
      plot.subtitle = ggtext::element_markdown(),
    ) +
    labs(x = NULL, y = NULL)
  viz
}


################

FeatureSelection3a <- function(data, col_y,plotType, Threshold) {
  
  if (Threshold > dim(data)[2]){
    
    Threshold <- dim(data)[2]
  }else{
    
    Threshold <- Threshold
    
  }
  
  # 'glm' gets converted to 'lm' for regression in my code
  .engines_valid <- c('glm', 'glmnet', 'xgboost', 'ranger')
  
  engines_named <- .engines_valid %>% setNames(., .)
  
  .modes_valid <- c('regression', 'classification')
  
  diamonds_c_rnks <-
    engines_named %>%
    map_dfr( 
      ~compare_and_rank_vip_q(
        data, 
        col_y = col_y, 
        engine = .x
      ) %>% 
        pluck('result'),
      .id = 'engine'
    )
  
  
  lab_title <- paste0('Top', Threshold, '- Variable Importance Ranking: ', diamonds_c_rnks$Type[1]) 
  #lab_subtitle_diamonds_c <- '<span style="color:#4E79A7"><b> diamonds_c_rnks$Type[1]</b></span> Target Variable for Model Prediction of <span style="color:#E15759"><b>diamonds</b></span> Data'
  
  # require(ggtext)
  viz_diamonds_c_rnks <- 
    diamonds_c_rnks %>% 
    plot_rnks(option = plotType, Threshold) +
    labs(
      title = lab_title
      #    subtitle = lab_subtitle_diamonds_c
    )
  
  viz_diamonds_c_rnks
  
  FinalTable <- data.frame(Var = as.character(viz_diamonds_c_rnks[["data"]]$var), RankMean = viz_diamonds_c_rnks[["data"]]$rnk_mean) %>%
    unique() %>%
    mutate(NormMean = RankMean/dim(.)[1]) %>%
    rename('Variable Name' = Var) %>% 
    arrange(NormMean)
  
  return(list( FinalTable, viz_diamonds_c_rnks))
  
}

############


#data <- read.csv("GastroMachLearn.csv")[,-1] %>%
#  rename("Label" = "Sarcopenia")
#
#col_y <- "SMI"
#
#plotType <- "D"
#
#Threshold <- 30 #can have all and write dim 
#
#Result <- FeatureSelection3a(data, col_y,plotType, Threshold)
#
#



