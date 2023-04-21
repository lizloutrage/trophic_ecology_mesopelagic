#' Targets plan
#' 
#' @author Liz Loutrage \email{liz.loutrage@gmail.com}
#' 
#' @date 2023/01/18

## Attach library packages ----
library(targets)
library(ggplot2)
library(geometry)
library(ape)
library(rcdd)

tar_source()

list(
  # SIBER ----
  
  # Read isotope raw data 
   tar_target(isotope_data, utils::read.csv(here::here("data","raw-data", "isotopic_data_2021.csv"),sep = ";", header = T,dec = ",")),

  # Conserve only fish species 
   tar_target(isotope_data_fish, subset(isotope_data, species != "Meganyctiphanes_norvegica")),

  # Create a matrix in SIBER format
   tar_target(siber_data, prepare_siber_data(isotope_data_fish)),
   tar_target(siber_data.df, as.data.frame(siber_data)),

  # Caculate group metrics ----
    tar_target(group_metrics, print_group_metrics(siber_data.df)),

  # Caculate community metrics ----
    tar_target(community_metrics, print_community_metrics(siber_data.df)),

  # Plot ellipses
   tar_target(ellipse_Z0492, plot_Z0492(siber_data.df)),
   tar_target(ellipse_Z0497, plot_Z0497(siber_data.df)),
   tar_target(ellipse_Z0503, plot_Z0503(siber_data.df)),
   tar_target(ellipse_Z0508, plot_Z0508(siber_data.df)),
   tar_target(ellipse_Z0512, plot_Z0512(siber_data.df)),
   tar_target(ellipse_Z0518, plot_Z0518(siber_data.df)),
   tar_target(ellipse_Z0524, plot_Z0524(siber_data.df)),
  
 # load derived data ----
 # Z0492
 tar_target(individuals_si_Z0492, utils::read.csv(here::here("data","derived-data", "individuals_si_Z0492.csv"), sep = ";")),
 tar_target(species_status_biomass_Z0492, utils::read.csv(here::here("data","derived-data", "species_status_biomass_Z0492.csv"), sep=";")),
 
 # Z0497
 tar_target(individuals_si_Z0497, utils::read.csv(here::here("data","derived-data", "individuals_si_Z0497.csv"), sep = ";")),
 tar_target(species_status_biomass_Z0497, utils::read.csv(here::here("data","derived-data", "species_status_biomass_Z0497.csv"), sep=";")),
 
 # Z0503
 tar_target(individuals_si_Z0503, utils::read.csv(here::here("data","derived-data", "individuals_si_Z0503.csv"), sep = ";")),
 tar_target(species_status_biomass_Z0503, utils::read.csv(here::here("data","derived-data", "species_status_biomass_Z0503.csv"), sep=";")),
 
 # Z0508
 tar_target(individuals_si_Z0508, utils::read.csv(here::here("data","derived-data", "individuals_si_Z0508.csv"), sep = ";")),
 tar_target(species_status_biomass_Z0508, utils::read.csv(here::here("data","derived-data", "species_status_biomass_Z0508.csv"), sep=";")),
 
 # Z0512
 tar_target(individuals_si_Z0512, utils::read.csv(here::here("data","derived-data", "individuals_si_Z0512.csv"), sep = ";")),
 tar_target(species_status_biomass_Z0512, utils::read.csv(here::here("data","derived-data", "species_status_biomass_Z0512.csv"), sep=";")),
 
 # Z0518
 tar_target(individuals_si_Z0518, utils::read.csv(here::here("data","derived-data", "individuals_si_Z0518.csv"), sep = ";")),
 tar_target(species_status_biomass_Z0518, utils::read.csv(here::here("data","derived-data", "species_status_biomass_Z0518.csv"), sep=";")),
 
 # Z0524
 tar_target(individuals_si_Z0524, utils::read.csv(here::here("data","derived-data", "individuals_si_Z0524.csv"), sep = ";")),
 tar_target(species_status_biomass_Z0524, utils::read.csv(here::here("data","derived-data", "species_status_biomass_Z0524.csv"), sep=";")),
 
 # compute trophic diversity indices ----
 tar_target(di_Z0492, diversity_indices_Z0492(individuals_si_Z0492, species_status_biomass_Z0492)),
 tar_target(di_Z0497, diversity_indices_Z0497(individuals_si_Z0497, species_status_biomass_Z0497)),
 tar_target(di_Z0503, diversity_indices_Z0503(individuals_si_Z0503, species_status_biomass_Z0503)),
 tar_target(di_Z0508, diversity_indices_Z0508(individuals_si_Z0508, species_status_biomass_Z0508)),
 tar_target(di_Z0512, diversity_indices_Z0512(individuals_si_Z0512, species_status_biomass_Z0512)),
 tar_target(di_Z0518, diversity_indices_Z0518(individuals_si_Z0518, species_status_biomass_Z0518)),
 tar_target(di_Z0524, diversity_indices_Z0524(individuals_si_Z0524, species_status_biomass_Z0524))
 )

# ## essai SIBER
# isotope_data <-  utils::read.csv(here::here("data","raw-data", "isotopic_data_2021.csv"),
#                                  sep = ";", header = T,dec = ",")
# isotope_data_fish <- subset(isotope_data, species != "Meganyctiphanes_norvegica")
# siber_data <- prepare_siber_data(isotope_data_fish)
# siber_data.df <- as.data.frame(siber_data)
# plot_Z0492(siber_data.df)

