#' Targets plan
#' 
#' @author Liz Loutrage \email{liz.loutrage@gmail.com}
#' 
#' @date 2023/01/18

## Attach required packages ----
library(targets)

tar_source()

list(
  # Read isotope raw data ----
 tar_target(isotope_data, utils::read.csv(here::here("data","raw-data", "isotopic_data_evhoe_2021.csv"),
                                          sep = ";", header = T,dec = ",")),
 
 # Conserve only fish species ---- 
 tar_target(isotope_data_fish, subset(isotope_data, species != "Meganyctiphanes_norvegica")),
 
 # Create a matrix in SIBER format ----
 tar_target(SIBER_data, as.data.frame (prepare_siber_data(isotope_data_fish))),

 # Plotting the raw data ----
 tar_target(siber_plot, siber_plot_community (SIBER_data)),

 # Caculate group metrics
 tar_target(group_metrics, print_group_metrics (SIBER_data)),

 # Caculate community metrics
 tar_target(community_metrics, print_community_metrics (SIBER_data))

 )

