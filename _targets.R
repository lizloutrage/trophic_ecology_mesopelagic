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
  # Load isotope data (DOI:)
  tar_target(
    isotope_data,
    readxl::read_excel(
      here::here(
        "data",
        "C_N_isotopes_deep_pelagic_fish_BayofBiscay_2021.xlsx"
      ),
      3
    ) %>%
      # Rename columns for simplicity
      rename(
        trawling_depth = `Trawling depth [m]`,
        d15n = `δ15N (untreated)`,
        species = Taxa,
        station = `Station label`
      ) %>%
      # creation of a d13c column (see mat & met), species with a C:N ratio:
      # >5: have undergone delipidation,
      # between 3.5 and 5: mathematically corrected (following Post et al. 2007) 
      # < 3.5: no corrected 
      mutate(
        d13c = case_when(
          !is.na(`δ13C (cyclohexane-delipidated)`) ~ `δ13C (cyclohexane-delipidated)`,
          `CN (untreated)` < 3.5 ~ `δ13C (untreated)`,
          TRUE ~ `δ13C (untreated)` - 3.32 + 0.99 * `CN (untreated)`
        )
      ) %>%
      # add taxon column
      mutate(
        taxon = case_when(Class == "Actinopteri" ~ "Fish",
                          Class == "Malacostraca" ~ "Krill")
      )
  ),
  
  # plot the isotopic niche of each species (all depth pooled)
  tar_target(species_niche, niche_plot_community(isotope_data)),
  
  # select only fish data 
  tar_target(isotope_data_fish, isotope_data %>% filter(taxon=="Fish")),
  
  # calculation of the matrix niche overlap
  tar_target(overlap_mx, overlap_matrix(isotope_data_fish)),
  
  # plot matrix niche overlap
  tar_target(plot_matrix, overlap_matrix_plot(overlap_mx)),
  
  # 2. Depth segregation----
  # Load trawling data
  tar_target(trawling_data,
             utils::read.csv(
               here::here("data", "trawling_data_evhoe_2021.csv"),
               sep = ";",
               header = T,
               dec = "."
             )),
  
  # Calculation of density depth distribution by species sampled 
  tar_target(
    density_distribution_data,
    density_distrubtion(trawling_data)
  ),
  
  # median depth of each species
  tar_target(median_depth, median_depth_sp(density_distribution_data)),
  
  # define number of cluster (trophic guilds)
  tar_target(nb_cluster_gs, nb_cluster(overlap_mx)),
  
  # define the cluster (which species belongs to which cluster)
  tar_target(cluster_definition, k_means_cluster(overlap_mx)),
  
  # plot cluster (dendrogram)
  tar_target(dendrogram, plot_densdrogram(overlap_mx)),
  
  # plot depth distribution
  tar_target(
    depth_distribution,
    depth_distribution_plot(density_distribution_data)
  ),
  
  # 3. Isotopic diversity indices ----
  
  # Data formatting to match si_div function parameters
  # species relative biomass in each depth layer 
    tar_target(
    species_status_biomass,
    sp_status_biomass_format(trawling_data)
  ),
  
  # individuals stable isotopes values
  tar_target(individuals_si, individuals_si_format(isotope_data_fish, species_status_biomass)),
  
  # Isotopic diversity index calculation
  tar_target(
    diversity_index,
    diversity_index_calculation (individuals_si, species_status_biomass)
  ),
  
  # PCA - isotopic diversity indices
  tar_target(PCA, compute_PCA(diversity_index)),
  
  # 4.Null models ----
  
  # Niche species area
  tar_target(niche_area_sp, null_model_niche_area(isotope_data_fish)),
  
  # Sum of species niche overlap
  tar_target(overlap_sp, null_model_overlap(isotope_data_fish))
)
