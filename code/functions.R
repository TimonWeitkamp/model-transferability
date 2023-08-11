#Function used

split_data <- function(seed_vals, td_data, prop = 0.7){
  set.seed(seed_vals)
  polys_split <- rsample::initial_split(data = td_data, group = code_level2, prop =prop, strata = code_level2)
  TD_df_training <- rsample::training(polys_split)  
  valiDat <- rsample::testing(polys_split) 
  
  return(list(TD_df_training = TD_df_training, valiDat = valiDat))
}

model_function_ffs <- function(scenario_name, algorithm, seed_vals, TD_df_training, ctrl, location_trained, predictors, response){
  set.seed(seed_vals)
  
  if(algorithm == "svmRadial"){
    print("svm model")
    model_ffs_varselect <- ffs( TD_df_training[,predictors],
                                TD_df_training[,response],
                                method=algorithm, 
                                metric="Accuracy",
                                trControl=ctrl,
                                importance=TRUE,
                                withinSE = TRUE,
                                tuneLength = 5,
                                na.rm = TRUE ,
                                preProcess = c("center", "scale"))
    
    
    SVM_radial_vars <- c(model_ffs_varselect$selectedvars ,'code_level2')
    trainDat2 <- TD_df_training %>% select(all_of(SVM_radial_vars)) 
    
    model_ffs <- train(
      # trainDat2[,SVM_radial_vars],
      # trainDat2[,response],
      code_level2 ~ .,
      data = trainDat2,
      method=algorithm, #rf , svmLinear
      metric="Accuracy",
      trControl=ctrl,
      importance=TRUE,
      withinSE = TRUE,
      tuneLength = 5,
      na.rm = TRUE ,
      preProcess = c("center", "scale"))
  }    else if(algorithm == "rf"){
    print( "rf model")
    model_ffs <- ffs( TD_df_training[,predictors],
                      TD_df_training[,response],
                      method=algorithm, 
                      metric="Accuracy",
                      trControl=ctrl,
                      importance=TRUE,
                      withinSE = TRUE,
                      tuneLength = 5,
                      na.rm = TRUE 
    )}
  
  saveRDS(model_ffs, here("output", paste0(algorithm,"_ffs_", location_trained, "_6m_scenario_",scenario_name, "_seed_", seed_vals,".rds")))
  
  return(model_ffs)
}

model_function_train <- function(scenario_name, algorithm, seed_vals, TD_df_training, ctrl, location_trained) {
  set.seed(seed_vals)
  
  # formula <- formula(code_level2 ~ .)
  
  if (algorithm == "svmRadial") {
    print("svm model")
    model_ffs <- train(
      form = code_level2 ~ .,
      data = TD_df_training,
      method = algorithm,
      metric = "Accuracy",
      trControl = ctrl,
      importance = TRUE,
      withinSE = TRUE,
      tuneLength = 5,
      na.rm = TRUE,
      preProcess = c("center", "scale")
    )
  } else if (algorithm == "rf") {
    print("rf model")
    model_ffs <- train(
      form = code_level2 ~ .,
      data = TD_df_training,
      method = algorithm,
      metric = "Accuracy",
      trControl = ctrl,
      importance = TRUE,
      withinSE = TRUE,
      tuneLength = 5,
      na.rm = TRUE
    )
  }
  
  saveRDS(
    model_ffs,
    here(
      "output",
      paste0(algorithm, "_train_", location_trained, "_6m_scenario_", scenario_name, "_seed_", seed_vals, ".rds")
    )
  )
  
  return(model_ffs)
}

process_scenario <- function(algorithm, seed_vals, scenario_name, feature) {
  time_start <- Sys.time()
  
  set.seed(seed_vals)
  message(scenario_name," ", seed_vals," ", algorithm )
  
  TD_chokwe_split <- split_data(seed_vals, TD_chokwe)
  TD_manica_split <- split_data(seed_vals, TD_manica) 
  TD_xai_split <- split_data(seed_vals, TD_xai, 0.99) 
  TD_catandica_split <- split_data(seed_vals, TD_catandica, 0.99) 
  
  class_ratios_chokwe <- table(TD_chokwe_split$TD_df_training$code_level2) %>% as.data.frame() %>% rename(code_level2 = Var1, ratio = Freq) %>%
    bind_rows(., data.frame(code_level2 = "5", ratio = round(mean(.$ratio))))
  class_ratios_manica <- table(TD_manica_split$TD_df_training$code_level2) %>% as.data.frame() %>% rename(code_level2 = Var1, ratio = Freq)%>%
    bind_rows(., data.frame(code_level2 = c("5", "8"), ratio = round(mean(.$ratio))))
  
  
  scenarios <- list(
    "Baseline_Chokwe" = list(
      training = TD_chokwe_split$TD_df_training  ,
      validation = TD_chokwe_split$valiDat,
      location_trained = "Chokwe",
      location_validated = "Chokwe"),
    "Baseline_Manica" = list(
      training = TD_manica_split$TD_df_training ,
      validation = TD_manica_split$valiDat,
      location_trained = "Manica",
      location_validated = "Manica"),
    "Multiple_Manica" = list(
      training = rbind(
        TD_chokwe_split$TD_df_training,
        TD_xai_split$TD_df_training,
        TD_catandica_split$TD_df_training),
      validation = TD_manica_split$valiDat,
      location_trained = "ChokweXaiCat",
      location_validated = "Manica"),
    "Multiple_Chokwe" = list(
      training = rbind(
        TD_manica_split$TD_df_training,
        TD_xai_split$TD_df_training,
        TD_catandica_split$TD_df_training),
      validation = TD_chokwe_split$valiDat,
      location_trained = "ManXaiCat",
      location_validated = "Chokwe"),
    "Chokwe_transfer" = list(
      training = TD_chokwe_split$TD_df_training  ,
      validation = TD_manica_split$valiDat,
      location_trained = "Chokwe",
      location_validated = "Manica"),
    "Manica_transfer" = list(
      training = TD_manica_split$TD_df_training ,
      validation = TD_chokwe_split$valiDat,
      location_trained = "Manica",
      location_validated = "Chokwe")
  )
  
  if(scenario_name %in% c("Baseline_Chokwe", "Baseline_Manica",   "Chokwe_transfer","Manica_transfer")){
    TD_df_training <- scenarios[[scenario_name]][["training"]] %>% 
      mutate(code_level2 = as.factor(code_level2)) %>%
      as.data.frame()
    valiDat <- scenarios[[scenario_name]][["validation"]] %>% 
      mutate(code_level2 = as.factor(code_level2)) %>%
      as.data.frame()
  }else if(scenario_name %in% c("Multiple_Chokwe")) {
    TD_df_training <- scenarios[[scenario_name]][["training"]] %>% 
      mutate(code_level2 = as.factor(code_level2)) %>%
      as.data.frame()
    valiDat <- scenarios[[scenario_name]][["validation"]] %>% 
      mutate(code_level2 = as.factor(code_level2)) %>%
      as.data.frame()  
    
    TD_df_training <- TD_df_training %>%
      inner_join(class_ratios_chokwe) %>%
      group_by(code_level2) %>%
      sample_n(first(ratio), replace = TRUE)%>%
      select(-ratio) %>% as.data.frame()
  }else if(scenario_name %in% c("Multiple_Manica")) {
    TD_df_training <- scenarios[[scenario_name]][["training"]] %>% 
      mutate(code_level2 = as.factor(code_level2)) %>%
      as.data.frame()
    valiDat <- scenarios[[scenario_name]][["validation"]] %>% 
      mutate(code_level2 = as.factor(code_level2)) %>%
      as.data.frame()   
    
    TD_df_training <- TD_df_training %>%
      inner_join(class_ratios_manica) %>%
      group_by(code_level2) %>%
      sample_n(first(ratio), replace = TRUE) %>%
      select(-ratio) %>% as.data.frame()
  }
  
  location_trained <- scenarios[[scenario_name]][["location_trained"]]
  location_validated <- scenarios[[scenario_name]][["location_validated"]]
  
  predictors <- names(TD_df_training) %>% setdiff(c("code_level2", "PolygonID" ,  "location"  ))
  response <- "code_level2"
  
  indices <- CreateSpacetimeFolds(TD_df_training,
                                  spacevar = "PolygonID",
                                  k=5,
                                  class="code_level2")
  TD_df_training <- TD_df_training %>% dplyr::select(-PolygonID, -location)
  TD_df_training <- mutate(TD_df_training, code_level2 = as.factor(code_level2))
  
  ctrl <- trainControl(method="cv",
                       index = indices$index,
                       savePredictions = TRUE,
                       allowParallel= TRUE,
                       number = 5,
                       verboseIter = TRUE)#}
  
  if(feature == TRUE){
    result <- model_function_ffs(scenario_name, algorithm, seed_vals, TD_df_training , ctrl, location_trained, predictors, response   )
    
  }else{
    result <- model_function_train(scenario_name, algorithm, seed_vals, TD_df_training, ctrl, location_trained )
  }
  
  
  prediction_ffs <- predict( object = result, newdata = valiDat) 
  
  valiDat <- mutate(valiDat, code_level2 = as.factor(code_level2))
  
  missing_levels <- setdiff(levels(prediction_ffs), levels(valiDat$code_level2))
  
  if (length(missing_levels) > 0) {
    for (level in missing_levels) {
      valiDat$code_level2 <- factor(valiDat$code_level2, levels = c(levels(valiDat$code_level2), level))
      valiDat$code_level2[prediction_ffs == level] <- NA
    }
  }
  
  valiDat$code_level2 <- forcats::fct_relevel(valiDat$code_level2, levels(prediction_ffs))
  
  
  cm <- confusionMatrix(data = prediction_ffs,
                        reference  = valiDat$code_level2,
                        mode='everything')  
  results_df <- data.frame(
    seed_vals = seed_vals, 
    algorithm = algorithm, 
    oa_validation = cm$overall["Accuracy"],
    cm_validation = paste(list(cm)), 
    algorithm_params = paste(result$bestTune, collapse=', ' ), 
    variables_used = paste(result$optVariables, collapse=', ' ),
    number_of_vars = length(result$optVariables),
    location_trained = location_trained,
    location_validated = location_validated,
    scenario = scenario_name,
    # irrigation stats
    producer_irri_validation = cm$byClass["Class: 2","Sensitivity"],
    user_irri_validation = cm$byClass["Class: 2","Pos Pred Value"],
    f1_irri_validation = cm[["byClass"]]["Class: 2", "F1"]
  )
  
  saveRDS(results_df, here("output", "results", paste0(algorithm,"_", location_trained, "_", location_validated,"_scenario_",scenario_name, "_accuracy_results_seed_", seed_vals,".rds")))
  
  time_end <- Sys.time()
  message("Elapsed time: ", time_end - time_start)
}

prediction_raster_load <- function(location) {
  mo_6_Q23 <- brick(here("data","DEA", paste0(location,"_6mo_2020Q2_2020Q3.tif"))) %>% raster::dropLayer(12) #layer 12 is clear pixel count
  names(mo_6_Q23) <- paste0(names_S2, "_6m_Q23")
  message("mo_6_Q23 ready")
  return(mo_6_Q23)
  
}

predict_raster <- function(model_list, raster,  no_cores = cl, vali_loc){
  # model_list <- all_models_list[1]
  # raster <- chokwe_raster
  raster_name <- str_remove(model_list, ".*/") %>%  str_remove(., "\\.rds$") 
  prediction <- predict(object = raster, model = readRDS(model_list), progress = "text")
  
  writeRaster(prediction, here("output", "Maps",  paste0(raster_name, vali_loc, ".tif")),overwrite=TRUE)
  message(raster_name, " finished")
}

DI_model_calc <- function(model_list){
  model_name <- str_remove(model_list, ".*/") %>%  str_remove(., "\\.rds$")
  output_path <- here("output", "DI_models", paste0(model_name, ".rds"))
  
  # Check if the model already exists
  if (file.exists(output_path)) {
    message("Model '", model_name, "' already exists. Skipping calculation.")
    return(NULL)
  }
  
  model_used <- readRDS(model_list)
  #variables_used <- modelc_used$selectedvars
  
  model_trainDI <- trainDI(model_used)
  saveRDS(model_trainDI, output_path)
  message(model_name, " calculated ", Sys.time())
}

predict_raster_DI <- function(model_list, raster,  no_cores = cl, vali_loc){
  # model_list <- model_list_DI[1]
  # raster <- chokwe_raster
  raster_name <- str_remove(model_list, ".*/") %>%  str_remove(., "\\.rds$") 
  prediction <- aoa(newdata = raster, trainDI = readRDS(model_list))
  
  writeRaster(prediction$AOA, here("output", "Maps", "AOA", paste0(raster_name, vali_loc, "_AOA.tif")),overwrite=TRUE)
  message(raster_name, " finished")
}
remove_seed_numbers <- function(raster_names) {
  seed_rem <- sub("_seed_\\d+", "", raster_names)
  return(sub(".tif", "", seed_rem))
}

remove_seed_numbers_aoa <- function(aoa_names) {
  # return(sub("_seed_\\d+_AOA.tif", "", aoa_names))
  seed_rem <- sub("_seed_\\d+", "", aoa_names)
  seed_rem <- sub("_AOA", "", seed_rem)
  return(sub(".tif", "", seed_rem))
}

select_irrigation_pixels <- function(rasters){
  rasters <- clamp(rast(rasters),  lower =2, upper=2, value = FALSE) %>%
    classify(. , cbind(2, 1)) %>%
    classify(. , cbind(NA, 0)) 
  
}

select_aoa_pixels <- function(aoa_test){
  aoa_test <- clamp(rast(aoa_test),  lower =0, upper=0, value = FALSE) %>%
    classify(. , cbind(0, 1))  %>%
    classify(. , cbind(NA, 0)) 
  
}

accu_calc <- function(algorithm, seed_vals, scenario_name, train = TRUE) {
  set.seed(seed_vals)
  
  TD_chokwe_split <- split_data(seed_vals, TD_chokwe)
  TD_manica_split <- split_data(seed_vals, TD_manica) 
  
  scenarios <- list(
    "Baseline_Chokwe" = list(
      validation = TD_chokwe_split$valiDat,
      location_trained = "Chokwe",
      location_validated = "Chokwe"),
    "Baseline_Manica" = list(
      validation = TD_manica_split$valiDat,
      location_trained = "Manica",
      location_validated = "Manica"),
    "Multiple_Manica" = list(
      validation = TD_manica_split$valiDat,
      location_trained = "ChokweXaiCat",
      location_validated = "Manica"),
    "Multiple_Chokwe" = list(
      validation = TD_chokwe_split$valiDat,
      location_trained = "ManXaiCat",
      location_validated = "Chokwe"),
    "Manica_transfer" = list(
      validation = TD_manica_split$valiDat,
      location_trained = "Chokwe",
      location_validated = "Manica"),
    "Chokwe_transfer" = list(
      validation = TD_chokwe_split$valiDat,
      location_trained = "Manica",
      location_validated = "Chokwe")
  )
  
  valiDat <- scenarios[[scenario_name]][["validation"]] %>% 
    mutate(code_level2 = as.factor(code_level2)) %>%
    as.data.frame()   
  
  
  location_trained <- scenarios[[scenario_name]][["location_trained"]]
  location_validated <- scenarios[[scenario_name]][["location_validated"]]
  
  if(train == TRUE){
    result <- readRDS(    here("output",paste0(algorithm, "_train_", location_trained, "_6m_scenario_", scenario_name, "_seed_", seed_vals, ".rds")
    ))
    features = "train"
  }else{
    result <- readRDS(    here("output",paste0(algorithm, "_ffs_", location_trained, "_6m_scenario_", scenario_name, "_seed_", seed_vals, ".rds")
    ))
    features = "ffs"
  }
  
  prediction_ffs <- predict( object = result, newdata = valiDat) 
  
  valiDat <- mutate(valiDat, code_level2 = as.factor(code_level2))
  
  missing_levels <- setdiff(levels(prediction_ffs), levels(valiDat$code_level2))
  
  if (length(missing_levels) > 0) {
    for (level in missing_levels) {
      valiDat$code_level2 <- factor(valiDat$code_level2, levels = c(levels(valiDat$code_level2), level))
      valiDat$code_level2[prediction_ffs == level] <- NA
    }
  }
  
  valiDat$code_level2 <- forcats::fct_relevel(valiDat$code_level2, levels(prediction_ffs))
  
  
  cm <- confusionMatrix(data = prediction_ffs,
                        reference  = valiDat$code_level2,
                        mode='everything')  
  
  if(train == TRUE){
    variables_used <- paste(result$coefnames, collapse=', ' )
    number_of_vars <- length(result$coefnames)
  }else{
    variables_used <- paste(result$selectedvars, collapse=', ' )
    number_of_vars <- result$minVar
  }
  
  cm_df <- data.frame(
    cm_data = prediction_ffs,
    cm_reference = valiDat$code_level2,
    seed_vals = seed_vals, 
    algorithm = algorithm, 
    scenario = scenario_name
  )
  
  # results_df <- data.frame(
  #   seed_vals = seed_vals, 
  #   algorithm = algorithm, 
  #   oa_validation = cm$overall["Accuracy"],
  #   cm_validation = paste(list(cm)), 
  # 
  #   algorithm_params = paste(result$bestTune, collapse=', ' ), 
  #   variables_used = variables_used,
  #   number_of_vars = number_of_vars,
  #   location_trained = location_trained,
  #   location_validated = location_validated,
  #   scenario = scenario_name,
  #   # irrigation stats
  #   producer_irri_validation = cm$byClass["Class: 2","Sensitivity"],
  #   user_irri_validation = cm$byClass["Class: 2","Pos Pred Value"],
  #   f1_irri_validation = cm[["byClass"]]["Class: 2", "F1"],
  #   features = features
  # )
  
  message(scenario_name," ", seed_vals," ", algorithm, " ", features, " --- DONE" )
  
  return(cm_df)
}

cm_plot_func <- function(data){
  
  cm <- yardstick::conf_mat(estimate = cm_data, truth = cm_reference, data = data)
  
  seed_val_name <- unique(data$seed_vals)
  algo_name <- unique(data$algorithm)
  scenario_name <- unique(data$scenario)
  
  file_name = paste0(
    seed_val_name, "_",
    algo_name,"_",
    scenario_name, "_ffs.png"
  )
  
  plot_name <- paste(
    seed_val_name, 
    algo_name,
    scenario_name, "- ffs"
  )
  
  cm_plot <- autoplot(cm, type = "heatmap") +
    scale_fill_gradient(low = "white", high = "grey") +
    labs(title = plot_name)  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ggsave(plot = cm_plot,
         filename = here("output", "figs", "conmat", paste(file_name)),
         units = "cm",
         width = 13,
         height = 13)
  
  message(file_name, " plotted")
  
}