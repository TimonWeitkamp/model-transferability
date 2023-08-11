#### libs ####
library(tidyverse)
library(doParallel)
library(here)
library(CAST)
library(caret)
library(tidyterra )
library(ggplot2)
library(raster)
library(stringr)
library(terra)

#### functions ####
source("functions.R")

#### load data ####
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)

TD_chokwe <- readRDS(here("data", "TD_chokwe.rds")) 
TD_manica <- readRDS(here("data", "TD_manica.rds")) 
TD_xai <- readRDS(here("data", "TD_xai.rds")) 
TD_catandica <- readRDS(here("data", "TD_catandica.rds")) 

#### run training model script ####

# Use purrr to iterate over the algorithm, seed values, and scenarios
seed_vals <- c(2020, 2021, 2022, 2023)
algoos <- c("rf")
scenario_names <- c("Baseline_Chokwe", "Baseline_Manica","Multiple_Chokwe", "Multiple_Manica",  "Chokwe_transfer","Manica_transfer")
feature <- c(TRUE, FALSE)

results_df <- expand_grid(algorithm = algoos, seed_val = seed_vals, scenario_name = scenario_names, feature = feature) %>%
  pmap_df(process_scenario)

stopCluster(cl)


#### run classification ####
names_S2 <- c('red', 'green', 'blue', 'nir', 'swir_1', 'swir_2', 'red_edge_1', 'red_edge_2', "smad", "emad", "bcmad", "NDVI", "BSI", "MNDWI")

chokwe_raster <- prediction_raster_load("Chokwe")
manica_raster <- prediction_raster_load("Manica")

model_list <- list.files(here("output"), full.names = TRUE, pattern = ".rds")
chokwe_model_list <- model_list[grep("_Chokwe_", model_list)]
chokwe_model_list <- model_list[grep("_Baseline_Chokwe_", model_list)]

# manica_model_list <-model_list[grep("_Manica_", model_list)]
manica_model_list <-model_list[grep("_Baseline_Manica_", model_list)]

# pred <- predict_raster( all_models_list[1])
chokwe_maps <- map(chokwe_model_list[1], ~ predict_raster(.x, chokwe_raster, vali_loc = ""))
chokwe_maps_transfer <- map(chokwe_model_list, ~ predict_raster(.x, manica_raster, vali_loc = "_manica"))

maica_maps <- map(manica_model_list, ~ predict_raster(.x, manica_raster))
maica_maps_transfer <- map(manica_model_list, ~ predict_raster(.x, chokwe_raster, vali_loc = "_chokwe"))

#### aoa model calculations
DI_models <- map(model_list, DI_model_calc)

chokwe_raster <- prediction_raster_load("Chokwe")
manica_raster <- prediction_raster_load("Manica")

model_list_DI <- list.files(here("output", "DI_models"), full.names = TRUE, pattern = ".rds")
chokwe_model_list_DI <- model_list_DI[grep("_Baseline_Chokwe_", model_list)]
manica_model_list_DI <-model_list_DI[grep("_Baseline_Manica_", model_list)]

chokwe_maps <- map(chokwe_model_list_DI, ~ predict_raster_DI(.x, chokwe_raster))
manica_maps <- map(manica_model_list_DI, ~ predict_raster_DI(.x, manica_raster))

manica_maps <- map(manica_model_list_DI, ~ predict_raster_DI(.x, chokwe_raster))
chokwe_maps <- map(chokwe_model_list_DI, ~ predict_raster_DI(.x, manica_raster ))


 #### hotspot maps ####
rasters <- list.files(here("output", "Maps"), full.names = TRUE, pattern = ".tif")
rasters_names <- list.files(here("output", "Maps"), full.names = F, pattern = ".tif")


raster_names_without_seed <- remove_seed_numbers(rasters_names) %>% unique()

# I was not able to get this in purrr format... so then the long way
Chokwe_Chokwe_ffs <- grep(raster_names_without_seed[1], rasters, value = TRUE)
Chokwe_Manica_ffs <- Chokwe_Chokwe_ffs[c(2,4,6,8)]
Chokwe_Chokwe_ffs<- Chokwe_Chokwe_ffs[c(1,3,5,7)]
Multiple_Manica_ffs <- grep(raster_names_without_seed[3], rasters, value = TRUE)
Manica_Manica_ffs <- grep(raster_names_without_seed[4], rasters, value = TRUE)
Manica_Chokwe_ffs <- Manica_Manica_ffs[c(2,4,6,8)]
Manica_Manica_ffs<- Manica_Manica_ffs[c(1,3,5,7)]
Multiple_Chokwe_ffs <- grep(raster_names_without_seed[6], rasters, value = TRUE)
Chokwe_Chokwe_train <- grep(raster_names_without_seed[7], rasters, value = TRUE)
Chokwe_Manica_train <- Chokwe_Chokwe_train[c(2,4,6,8)]
Chokwe_Chokwe_train<- Chokwe_Chokwe_train[c(1,3,5,7)]
Multiple_Manica_train <- grep(raster_names_without_seed[9], rasters, value = TRUE)
Manica_Manica_train <- grep(raster_names_without_seed[10], rasters, value = TRUE)
Manica_Chokwe_train <- Manica_Manica_train[c(2,4,6,8)]
Manica_Manica_train <- Manica_Manica_train[c(1,3,5,7)]
Multiple_Chokwe_train <- grep(raster_names_without_seed[12], rasters, value = TRUE)

selected_raster_names <- grep(paste(raster_names_without_seed, collapse = "|"), rasters_names, value = TRUE)


Chokwe_Chokwe_ffs_irri <- map(Chokwe_Chokwe_ffs, select_irrigation_pixels)
Multiple_Manica_ffs_irri <- map(Multiple_Manica_ffs, select_irrigation_pixels)
Manica_Manica_ffs_irri <- map(Manica_Manica_ffs, select_irrigation_pixels)
Multiple_Chokwe_ffs_irri <- map(Multiple_Chokwe_ffs, select_irrigation_pixels)
Chokwe_Chokwe_train_irri <- map(Chokwe_Chokwe_train, select_irrigation_pixels)
Multiple_Manica_train_irri <- map(Multiple_Manica_train, select_irrigation_pixels)
Manica_Manica_train_irri <- map(Manica_Manica_train, select_irrigation_pixels)
Multiple_Chokwe_train_irri <- map(Multiple_Chokwe_train, select_irrigation_pixels)
Chokwe_Manica_ffs_irri <- map(Chokwe_Manica_ffs, select_irrigation_pixels)
Manica_Chokwe_ffs_irri <- map(Manica_Chokwe_ffs, select_irrigation_pixels)
Chokwe_Manica_train_irri <- map(Chokwe_Manica_train, select_irrigation_pixels)
Manica_Chokwe_train_irri <- map(Manica_Chokwe_train, select_irrigation_pixels)

sum_Chokwe_Chokwe_ffs <- sum(rast(Chokwe_Chokwe_ffs_irri)) %>%  classify(. , cbind(0, NA))
sum_Multiple_Manica_ffs <- sum(rast(Multiple_Manica_ffs_irri))%>%  classify(. , cbind(0, NA))
sum_Manica_Manica_ffs <- sum(rast(Manica_Manica_ffs_irri))%>%  classify(. , cbind(0, NA))
sum_Multiple_Chokwe_ffs <- sum(rast(Multiple_Chokwe_ffs_irri))%>%  classify(. , cbind(0, NA))
sum_Chokwe_Chokwe_train <- sum(rast(Chokwe_Chokwe_train_irri))%>%  classify(. , cbind(0, NA))
sum_Multiple_Manica_train <- sum(rast(Multiple_Manica_train_irri))%>%  classify(. , cbind(0, NA))
sum_Manica_Manica_train <- sum(rast(Manica_Manica_train_irri))%>%  classify(. , cbind(0, NA))
sum_Multiple_Chokwe_train <- sum(rast(Multiple_Chokwe_train_irri))%>%  classify(. , cbind(0, NA))
sum_Chokwe_Manica_ffs <- sum(rast(Chokwe_Manica_ffs_irri))%>%  classify(. , cbind(0, NA))
sum_Manica_Chokwe_ffs <- sum(rast(Manica_Chokwe_ffs_irri))%>%  classify(. , cbind(0, NA))
sum_Chokwe_Manica_train <- sum(rast(Chokwe_Manica_train_irri))%>%  classify(. , cbind(0, NA))
sum_Manica_Chokwe_train <- sum(rast(Manica_Chokwe_train_irri))%>%  classify(. , cbind(0, NA))


#### hotspot with AOA ####
aoa_list <- list.files(here("output", "Maps", "AOA"), full.names = TRUE, pattern = ".tif")
aoa_names <- list.files(here("output", "Maps", "AOA"), full.names = F, pattern = ".tif")

aoa_names_without_seed <- remove_seed_numbers_aoa(aoa_names) %>% unique()

Chokwe_Chokwe_ffs_aoa  <- grep(aoa_names_without_seed[1], aoa_list, value = TRUE)
Chokwe_Manica_ffs_aoa  <- Chokwe_Chokwe_ffs_aoa [c(2,4,6,8)]
Chokwe_Chokwe_ffs_aoa <- Chokwe_Chokwe_ffs_aoa [c(1,3,5,7)]
Multiple_Manica_ffs_aoa  <- grep(aoa_names_without_seed[3], aoa_list, value = TRUE)
Manica_Manica_ffs_aoa  <- grep(aoa_names_without_seed[4], aoa_list, value = TRUE)
Manica_Chokwe_ffs_aoa  <- Manica_Manica_ffs_aoa [c(2,4,6,8)]
Manica_Manica_ffs_aoa <- Manica_Manica_ffs_aoa [c(1,3,5,7)]
Multiple_Chokwe_ffs_aoa  <- grep(aoa_names_without_seed[6], aoa_list, value = TRUE)
Chokwe_Chokwe_train_aoa  <- grep(aoa_names_without_seed[7], aoa_list, value = TRUE)
Chokwe_Manica_train_aoa  <- Chokwe_Chokwe_train_aoa [c(2,4,6,8)]
Chokwe_Chokwe_train_aoa <- Chokwe_Chokwe_train_aoa [c(1,3,5,7)]
Multiple_Manica_train_aoa  <- grep(aoa_names_without_seed[9], aoa_list, value = TRUE)
Manica_Manica_train_aoa  <- grep(aoa_names_without_seed[10], aoa_list, value = TRUE)
Manica_Chokwe_train_aoa  <- Manica_Manica_train_aoa [c(2,4,6,8)]
Manica_Manica_train_aoa  <- Manica_Manica_train_aoa [c(1,3,5,7)]
Multiple_Chokwe_train_aoa  <- grep(aoa_names_without_seed[12], aoa_list, value = TRUE)


selected_aoa_names <- grep(paste(aoa_names_without_seed, collapse = "|"), aoa_names, value = TRUE)

aoa_test <- aoa_list[1]

Chokwe_Chokwe_ffs_irri_aoa <- map(Chokwe_Chokwe_ffs_aoa, select_aoa_pixels)
Multiple_Manica_ffs_irri_aoa <- map(Multiple_Manica_ffs_aoa, select_aoa_pixels)
Manica_Manica_ffs_irri_aoa <- map(Manica_Manica_ffs_aoa, select_aoa_pixels)
Multiple_Chokwe_ffs_irri_aoa <- map(Multiple_Chokwe_ffs_aoa, select_aoa_pixels)
Chokwe_Chokwe_train_irri_aoa <- map(Chokwe_Chokwe_train_aoa, select_aoa_pixels)
Multiple_Manica_train_irri_aoa <- map(Multiple_Manica_train_aoa, select_aoa_pixels)
Manica_Manica_train_irri_aoa <- map(Manica_Manica_train_aoa, select_aoa_pixels)
Multiple_Chokwe_train_irri_aoa <- map(Multiple_Chokwe_train_aoa, select_aoa_pixels)
Chokwe_Manica_ffs_irri_aoa <- map(Chokwe_Manica_ffs_aoa, select_aoa_pixels)
Manica_Chokwe_ffs_irri_aoa <- map(Manica_Chokwe_ffs_aoa, select_aoa_pixels)
Chokwe_Manica_train_irri_aoa <- map(Chokwe_Manica_train_aoa, select_aoa_pixels)
Manica_Chokwe_train_irri_aoa <- map(Manica_Chokwe_train_aoa, select_aoa_pixels)

sum_Chokwe_Chokwe_ffs_aoa <- sum(rast(Chokwe_Chokwe_ffs_irri_aoa)) #%>%  classify(. , cbind(0, NA))
sum_Multiple_Manica_ffs_aoa <- sum(rast(Multiple_Manica_ffs_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Manica_Manica_ffs_aoa <- sum(rast(Manica_Manica_ffs_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Multiple_Chokwe_ffs_aoa <- sum(rast(Multiple_Chokwe_ffs_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Chokwe_Chokwe_train_aoa <- sum(rast(Chokwe_Chokwe_train_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Multiple_Manica_train_aoa <- sum(rast(Multiple_Manica_train_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Manica_Manica_train_aoa <- sum(rast(Manica_Manica_train_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Multiple_Chokwe_train_aoa <- sum(rast(Multiple_Chokwe_train_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Chokwe_Manica_ffs_aoa <- sum(rast(Chokwe_Manica_ffs_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Manica_Chokwe_ffs_aoa <- sum(rast(Manica_Chokwe_ffs_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Chokwe_Manica_train_aoa <- sum(rast(Chokwe_Manica_train_irri_aoa))#%>%  classify(. , cbind(0, NA))
sum_Manica_Chokwe_train_aoa <- sum(rast(Manica_Chokwe_train_irri_aoa))#%>%  classify(. , cbind(0, NA))

raster_list_chokwe <- c(
  sum_Chokwe_Chokwe_ffs,
  sum_Multiple_Chokwe_ffs,
  sum_Manica_Chokwe_ffs,
  sum_Chokwe_Chokwe_train,
  sum_Multiple_Chokwe_train,
  sum_Manica_Chokwe_train
)

raster_list_manica <- c(
  sum_Manica_Manica_ffs ,
  sum_Multiple_Manica_ffs,
  sum_Chokwe_Manica_ffs,
  sum_Manica_Manica_train,
  sum_Multiple_Manica_train ,
  sum_Chokwe_Manica_train
)

#some stats 
global(raster_list_chokwe, "sum")
global(raster_list_manica, "sum")

mean(global(raster_list_chokwe, "sum")$sum)
mean(global(raster_list_manica, "sum")$sum)

#### substracting aoa from irri map ####
Chokwe_Chokwe_ffs <- sum_Chokwe_Chokwe_ffs - sum_Chokwe_Chokwe_ffs_aoa 
writeRaster(sum_Chokwe_Chokwe_ffs, here("output", "Maps", "Final", "Chokwe_Chokwe_ffs.tif"))
writeRaster(sum_Chokwe_Chokwe_ffs_aoa, here("output", "Maps", "Final", "Chokwe_Chokwe_ffs_AOA.tif"))
writeRaster(Chokwe_Chokwe_ffs, here("output", "Maps", "Final", "Chokwe_Chokwe_ffs_difference.tif"))

Multiple_Manica_ffs <- sum_Multiple_Manica_ffs - sum_Multiple_Manica_ffs_aoa
writeRaster(sum_Multiple_Manica_ffs, here("output", "Maps", "Final", "Multiple_Manica_ffs.tif"))
writeRaster(sum_Multiple_Manica_ffs_aoa, here("output", "Maps", "Final", "Multiple_Manica_ffs_AOA.tif"))
writeRaster(Multiple_Manica_ffs, here("output", "Maps", "Final", "Multiple_Manica_ffs_difference.tif"))

Manica_Manica_ffs <- sum_Manica_Manica_ffs - sum_Manica_Manica_ffs_aoa
writeRaster(sum_Manica_Manica_ffs, here("output", "Maps", "Final", "Manica_Manica_ffs.tif"))
writeRaster(sum_Manica_Manica_ffs_aoa, here("output", "Maps", "Final", "Manica_Manica_ffs_AOA.tif"))
writeRaster(Manica_Manica_ffs, here("output", "Maps", "Final", "Manica_Manica_ffs_difference.tif"))

Multiple_Chokwe_ffs <- sum_Multiple_Chokwe_ffs - sum_Multiple_Chokwe_ffs_aoa
writeRaster(sum_Multiple_Chokwe_ffs, here("output", "Maps", "Final", "Multiple_Chokwe_ffs.tif"))
writeRaster(sum_Multiple_Chokwe_ffs_aoa, here("output", "Maps", "Final", "Multiple_Chokwe_ffs_AOA.tif"))
writeRaster(Multiple_Chokwe_ffs, here("output", "Maps", "Final", "Multiple_Chokwe_ffs_difference.tif"))

Chokwe_Chokwe_train <- sum_Chokwe_Chokwe_train - sum_Chokwe_Chokwe_train_aoa
writeRaster(sum_Chokwe_Chokwe_train, here("output", "Maps", "Final", "Chokwe_Chokwe_train.tif"))
writeRaster(sum_Chokwe_Chokwe_train_aoa, here("output", "Maps", "Final", "Chokwe_Chokwe_train_AOA.tif"))
writeRaster(Chokwe_Chokwe_train, here("output", "Maps", "Final", "Chokwe_Chokwe_train_difference.tif"))

Multiple_Manica_train <- sum_Multiple_Manica_train - sum_Multiple_Manica_train_aoa
writeRaster(sum_Multiple_Manica_train, here("output", "Maps", "Final", "Multiple_Manica_train.tif"))
writeRaster(sum_Multiple_Manica_train_aoa, here("output", "Maps", "Final", "Multiple_Manica_train_AOA.tif"))
writeRaster(Multiple_Manica_train, here("output", "Maps", "Final", "Multiple_Manica_train_difference.tif"))

Manica_Manica_train <- sum_Manica_Manica_train - sum_Manica_Manica_train_aoa
writeRaster(sum_Manica_Manica_train, here("output", "Maps", "Final", "Manica_Manica_train.tif"))
writeRaster(sum_Manica_Manica_train_aoa, here("output", "Maps", "Final", "Manica_Manica_train_AOA.tif"))
writeRaster(Manica_Manica_train, here("output", "Maps", "Final", "Manica_Manica_train_difference.tif"))

Multiple_Chokwe_train <- sum_Multiple_Chokwe_train - sum_Multiple_Chokwe_train_aoa
writeRaster(sum_Multiple_Chokwe_train, here("output", "Maps", "Final", "Multiple_Chokwe_train.tif"))
writeRaster(sum_Multiple_Chokwe_train_aoa, here("output", "Maps", "Final", "Multiple_Chokwe_train_AOA.tif"))
writeRaster(Multiple_Chokwe_train, here("output", "Maps", "Final", "Multiple_Chokwe_train_difference.tif"))

Chokwe_Manica_ffs <- sum_Chokwe_Manica_ffs - sum_Chokwe_Manica_ffs_aoa
writeRaster(sum_Chokwe_Manica_ffs, here("output", "Maps", "Final", "Chokwe_Manica_ffs.tif"))
writeRaster(sum_Chokwe_Manica_ffs_aoa, here("output", "Maps", "Final", "Chokwe_Manica_ffs_AOA.tif"))
writeRaster(Chokwe_Manica_ffs, here("output", "Maps", "Final", "Chokwe_Manica_ffs_difference.tif"))

Manica_Chokwe_ffs<- sum_Manica_Chokwe_ffs - sum_Manica_Chokwe_ffs_aoa
writeRaster(sum_Manica_Chokwe_ffs, here("output", "Maps", "Final", "Manica_Chokwe_ffs.tif"))
writeRaster(sum_Manica_Chokwe_ffs_aoa, here("output", "Maps", "Final", "Manica_Chokwe_ffs_AOA.tif"))
writeRaster(Manica_Chokwe_ffs, here("output", "Maps", "Final", "Manica_Chokwe_ffs_difference.tif"))

Chokwe_Manica_train<- sum_Chokwe_Manica_train - sum_Chokwe_Manica_train_aoa
writeRaster(sum_Chokwe_Manica_train, here("output", "Maps", "Final", "Chokwe_Manica_train.tif"))
writeRaster(sum_Chokwe_Manica_train_aoa, here("output", "Maps", "Final", "Chokwe_Manica_train_AOA.tif"))
writeRaster(Chokwe_Manica_train, here("output", "Maps", "Final", "Chokwe_Manica_train_difference.tif"))

Manica_Chokwe_train <- sum_Manica_Chokwe_train - sum_Manica_Chokwe_train_aoa
writeRaster(sum_Manica_Chokwe_train, here("output", "Maps", "Final", "Manica_Chokwe_train.tif"))
writeRaster(sum_Manica_Chokwe_train_aoa, here("output", "Maps", "Final", "Manica_Chokwe_train_AOA.tif"))
writeRaster(Manica_Chokwe_train, here("output", "Maps", "Final", "Manica_Chokwe_train_difference.tif"))


raster_list_chokwe <- c(
  Chokwe_Chokwe_ffs,
  Multiple_Chokwe_ffs,
  Manica_Chokwe_ffs,
  Chokwe_Chokwe_train,
  Multiple_Chokwe_train,
  Manica_Chokwe_train
)

raster_list_manica <- c(
  Manica_Manica_ffs ,
  Multiple_Manica_ffs,
  Chokwe_Manica_ffs,
  Manica_Manica_train,
  Multiple_Manica_train ,
  Chokwe_Manica_train
)
freq_chokwe<- freq(raster_list_chokwe) %>% filter(value != 0) %>% mutate(ha = count*100/10000,
                                                                          location = "Chokwe")
freq_manica <- freq(raster_list_manica) %>% filter(value != 0) %>% mutate(ha = count*100/10000,
                                                                          location = "Manica")
hectares_all <- rbind(freq_chokwe, freq_manica) %>% mutate(
  Scenario = case_when(
    layer == 1 ~ "Baseline",
    layer == 2 ~ "Multiple location transfer",
    layer == 3~  "Single location transfer",
    layer == 4 ~ "Baseline",
    layer == 5 ~ "Multiple location transfer",
    layer == 6 ~ "Single location transfer"),
  Feature = case_when(
    layer == 1 ~ "ffs",
    layer == 2 ~ "ffs",
    layer == 3~  "ffs",
    layer == 4 ~ "train",
    layer == 5 ~ "train",
    layer == 6 ~ "train") )  

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


accu_plot <- ggplot(hectares_all, aes(x = as.factor(value), y =ha, fill = Feature)) +
  # geom_point() +
  geom_col(position = "dodge")+
  facet_grid(location~Scenario, scales = "free_y") + 
  theme_bw()+
  scale_y_continuous(labels = scales::label_number(suffix = " k", scale = 1e-3)) +
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="bottom") +
  labs(x = "Hotspot value",
       y = "Area (hectares)")

ggsave(accu_plot, filename = here("output", "figs", "hectares.png"),
       units = "cm",
       width = 20,
       height = 18)


#### plotting ####
layer_names <-  c("Baseline (ffs)", "Multiple location transfer (ffs)","Single location transfer (ffs)", "Baseline (train)" , "Multiple location transfer (train)", "Single location transfer (train)")

Chokwe <- c(Chokwe_Chokwe_ffs, Multiple_Chokwe_ffs,  Manica_Chokwe_ffs, Chokwe_Chokwe_train, Multiple_Chokwe_train,  Manica_Chokwe_train )
names(Chokwe) <- layer_names

sum_Chokwe <- c(sum_Chokwe_Chokwe_ffs,  sum_Multiple_Chokwe_ffs,  sum_Manica_Chokwe_ffs,sum_Chokwe_Chokwe_train, sum_Multiple_Chokwe_train, sum_Manica_Chokwe_train )
names(sum_Chokwe) <- layer_names

Chokwe_AOA <- c(sum_Chokwe_Chokwe_ffs_aoa, sum_Multiple_Chokwe_ffs_aoa, sum_Manica_Chokwe_ffs_aoa, sum_Chokwe_Chokwe_train_aoa,sum_Multiple_Chokwe_train_aoa ,  sum_Manica_Chokwe_train_aoa)
names(Chokwe_AOA) <- layer_names

Manica <- c(Manica_Manica_ffs, Multiple_Manica_ffs, Chokwe_Manica_ffs, Manica_Manica_train,Multiple_Manica_train, Chokwe_Manica_train )
names(Manica) <- layer_names

sum_Manica <- c(sum_Manica_Manica_ffs, sum_Multiple_Manica_ffs, sum_Chokwe_Manica_ffs,sum_Manica_Manica_train,sum_Multiple_Manica_train,  sum_Chokwe_Manica_train )
names(sum_Manica) <- layer_names

Manica_AOA <- c(sum_Manica_Manica_ffs_aoa, sum_Multiple_Manica_ffs_aoa, sum_Chokwe_Manica_ffs_aoa, sum_Manica_Manica_train_aoa,sum_Multiple_Manica_train_aoa , sum_Chokwe_Manica_train_aoa)
names(Manica_AOA) <- layer_names



Chokwe <- c(Chokwe_Chokwe_ffs)
names(Chokwe) <- c("Chokwe_Chokwe_ffs")

layer_names_fct <-  c("Baseline (ffs)", "Baseline (train)" , 
                      "Multiple location transfer (ffs)", "Multiple location transfer (train)", 
                      "Single location transfer (ffs)", "Single location transfer (train)")

## Chokwe
g_chokwe <- ggplot() +
  geom_spatraster(data = as.factor(Chokwe),
                  maxcell = 1e+6,
                  show.legend = T) +
  scale_fill_whitebox_d(palette = "bl_yl_rd",
                        direction = -1,
                        na.value = "gray") +
  facet_wrap(~factor(lyr, levels = layer_names_fct), ncol = 2) +
  theme_bw() + labs(title = "Classification - AOA hotspot maps Chokwe")+
  ggthemes::theme_map() +
  theme(legend.position="bottom")
g_chokwe

ggsave(plot = g_chokwe, filename = here("output", "figs", "Chokwe combined.png"),
       units = "cm",
       width = 16,
       height = 25)

g_chokwe_sum <- ggplot() +
  geom_spatraster(data = as.factor(sum_Chokwe),
                  maxcell = 1e+6,
                  show.legend = T) +
  scale_fill_whitebox_d(palette = "bl_yl_rd",
                        direction = -1,
                        na.value = "gray") +
  facet_wrap(~factor(lyr, levels = layer_names_fct), ncol = 2) +
  theme_bw() + labs(title = "Classification Chokwe")+
  ggthemes::theme_map() +
  theme(legend.position="bottom")
g_chokwe_sum
ggsave(plot = g_chokwe_sum, filename = here("output", "figs", "Chokwe_irri.png"),
       units = "cm",
       width = 16,
       height = 25)

g_chokwe_aoa <- ggplot() +
  geom_spatraster(data = as.factor(Chokwe_AOA%>%  classify(. , cbind(0, NA))),
                  maxcell = 1e+6,
                  show.legend = T) +
  scale_fill_whitebox_d(palette = "bl_yl_rd",
                        na.value = "gray") +
  facet_wrap(~factor(lyr, levels = layer_names_fct), ncol = 2) +
  theme_bw() + labs(title = "AOA Chokwe")+
  ggthemes::theme_map() +
  theme(legend.position="bottom")
g_chokwe_aoa
ggsave(plot = g_chokwe_aoa, filename = here("output", "figs", "Chokwe_AOA.png"),
       units = "cm",
       width = 16,
       height = 25)

## Manica
g_manica <- ggplot() +
  geom_spatraster(data = as.factor(Manica),
                  maxcell = 1e+6,
                  show.legend = T) +
  scale_fill_whitebox_d(palette = "bl_yl_rd",
                        direction = -1,
                        na.value = "gray") +
  facet_wrap(~factor(lyr, levels = layer_names_fct), ncol = 2) +
  theme_bw()+ labs(title = "Classification - AOA hotspot maps Manica")+
  ggthemes::theme_map() +
  theme(legend.position="bottom")
g_manica
ggsave(plot = g_manica, filename = here("output", "figs", "Manica combined.png"),
       units = "cm",
       width = 16,
       height = 25)

g_manica_sum <- ggplot() +
  geom_spatraster(data = as.factor(sum_Manica),
                  maxcell = 1e+6,
                  show.legend = T) +
  scale_fill_whitebox_d(palette = "bl_yl_rd",
                        direction = -1,
                        na.value = "gray") +
  facet_wrap(~factor(lyr, levels = layer_names_fct), ncol = 2) +
  theme_bw()+ labs(title = "Classification Manica")+
  ggthemes::theme_map() +
  theme(legend.position="bottom")
g_manica_sum
ggsave(plot = g_manica_sum, filename = here("output", "figs", "Manica_irri.png"),
       units = "cm",
       width = 16,
       height = 25)

g_manica_aoa <- ggplot() +
  geom_spatraster(data = as.factor(Manica_AOA %>%  classify(. , cbind(0, NA))),
                  maxcell = 1e+6,
                  show.legend = T) +
  scale_fill_whitebox_d(palette = "bl_yl_rd",
                        na.value = "gray") +
  facet_wrap(~factor(lyr, levels = layer_names_fct), ncol = 2) +
  # theme_bw()+ 
  labs(title = "AOA Manica")+
  ggthemes::theme_map() +
  theme(legend.position="bottom")

g_manica_aoa
ggsave(plot = g_manica_aoa, filename = here("output", "figs", "Manica_AOA.png"),
       units = "cm",
       width = 16,
       height = 25)


#### accuracies ####

seed_vals <- c(2020, 2021, 2022, 2023)
algoos <- c("rf")
scenario_names <- c("Baseline_Chokwe", "Baseline_Manica","Multiple_Chokwe", "Multiple_Manica",  "Chokwe_transfer","Manica_transfer")
# scenario_names <- "Chokwe_transfer"

accus_train <- expand_grid(algorithm = algoos, seed_val = seed_vals, scenario_name = scenario_names) %>%
  pmap_df(accu_calc)
accus_ffs <- expand_grid(algorithm = algoos, seed_val = seed_vals, scenario_name = scenario_names, train = FALSE) %>%
  pmap_df(accu_calc)

vars_model <- rbind(accus_train, accus_ffs) %>% dplyr::select(seed_vals, algorithm, scenario, location_validated, features, number_of_vars, variables_used)

accus <- rbind(accus_train, accus_ffs) %>% dplyr::select(seed_vals, algorithm, oa_validation, scenario, location_validated,producer_irri_validation, user_irri_validation, features) %>%
  pivot_longer( cols = c("oa_validation", "producer_irri_validation", "user_irri_validation")) %>%
  mutate(
    Scenario = case_when(
      scenario == "Baseline_Chokwe" ~ "Baseline",
      scenario == "Baseline_Manica" ~ "Baseline",
      scenario == "Multiple_Chokwe" ~ "Multiple location transfer",
      scenario == "Multiple_Manica" ~ "Multiple location transfer",
      scenario == "Chokwe_transfer" ~ "Single location transfer",
      scenario == "Manica_transfer" ~ "Single location transfer"),
    Metric = case_when(
      name == "oa_validation" ~ "Overall accuracy",
      name == "producer_irri_validation" ~ "Producer accuracy",
      name == "user_irri_validation" ~ "User accuracy"
    )
  )

ggplot(accus, aes(x= Metric, y = value, col = Scenario)) +
  geom_point()+
  facet_grid(location_validated ~ features) +
  labs(x = "Accuracy metric", y = "Accuray value") +
  theme_bw() +   scale_colour_manual(values=cbPalette)+
  theme(legend.position="bottom")

ggsave(here("output", "figs", "accuracies.png"),
       units = "cm",
       width = 20,
       height = 18)

ggplot(accus, aes(x= name, y = value, col = scenario)) +
  geom_violin()+
  facet_grid(location_validated ~ features)

ggplot(accus, aes(x= name, y = value, fill = scenario)) +
  geom_col(position = "dodge")+
  facet_grid(location_validated ~ features)


#### confusion matrix ####

scenarios <- c("Baseline_Chokwe", "Baseline_Manica" ,"Multiple_Chokwe", "Multiple_Manica", "Chokwe_transfer", "Manica_transfer" )
seeds <- c(2020, 2021, 2022, 2023)
class_names_8classes = c("Built-up area", "Irrigated agriculture", "Rainfed agriculture", "Dense vegetation", "Grassland", "Light vegetation",  "Water", "Wetland")
accus_ffs$cm_data  <- plyr::mapvalues(accus_ffs$cm_data, from = c("1", "2", "3", "4", "5", "6","7" ,"8" ), to = class_names_8classes)
accus_ffs$cm_reference  <- plyr::mapvalues(accus_ffs$cm_reference , from = c("1", "2", "3", "4", "5", "6","7" ,"8" ), to = class_names_8classes)

# i was not able to get this into purrr format so change seeds[4] and scenarios[6] to other numbers to produce the plots.
accus_ffs   %>% filter(
  seed_vals == seeds[4],
  scenario == scenarios[6]
) %>% cm_plot_func(.)
