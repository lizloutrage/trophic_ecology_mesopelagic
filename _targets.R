#' Targets plan
#'
#'@author Liz Loutrage, \email{liz.loutrage@gmail.com}
#'        Anik Brind'amour, \email {Anik.Brindamour@ifremer.fr}
#'

## Attach required packages ----
library(targets)
tar_option_set(packages = c("dplyr", "ggplot2"))

tar_source()

list(
  # 1. Isotopic niches ----
  
  # Load isotope data
  tar_target(
    isotope_data,
    utils::read.csv(
      here::here("data", "isotopic_data_2021.csv"),
      sep = ";",
      header = T,
      dec = ","
    ) %>%
      # remove outlier values
      filter(individual_code != "Arg-olf 42")
  ),
  
  # plot the isotopic niche of each species
  tar_target(species_niche, niche_plot_community(isotope_data)),
  
  # matrix overlap
  tar_target(overlap_mx, overlap_matrix(isotope_data)),
  
  # plot matrix overlap
  tar_target(plot_matrix, overlap_matrix_plot(overlap_mx)),
  
  # 2. Depth segregation----
  
  # Load trawling data
  tar_target(
    trawling_data,
    utils::read.csv(
      here::here("data", "trawling_data_evhoe_2021.csv"),
      sep = ";",
      header = T,
      dec = "."
    )
  ),
  
  # Load trawling data 2021
  tar_target(
    density_distribution_data,
    density_distrubtion(trawling_data)
  ),
  
  # median depth of species
  tar_target(median_depth, median_depth_sp(density_distribution_data)),
  
  # define number of cluster
  tar_target(nb_cluster_gs, nb_cluster(overlap_mx)),
  
  # define the cluster (which species belongs to which cluster)
  tar_target(cluster_definition, k_means_cluster(overlap_mx)),
  
  # plot cluster (dendrogram)
  tar_target(dendrogram, plot_densdrogram(overlap_mx)),
  
  # plot depth distribution
  tar_target(depth_distribution, depth_distribution_plot(density_distribution_data)),
  
  # 3. Isotopic diversity indices ----
  
  # Data formatting to match si_div function parameters
  tar_target(individuals_si, individuals_si_format(isotope_data)),

  tar_target(species_status_biomass, sp_status_biomass_format(trawling_data)),
  
  # Isotopic diversity index calculation
  tar_target(diversity_index, diversity_index_calculation (individuals_si, species_status_biomass)),
  
  #PCA
  tar_target(PCA, compute_PCA(diversity_index)),
  
  # 4.Null models ----
  
  # Niche area 
  tar_target(niche_area_sp, null_model_niche_area(isotope_data)),
  
  # Overlap
  tar_target(overlap_sp, null_model_overlap(isotope_data))
)

