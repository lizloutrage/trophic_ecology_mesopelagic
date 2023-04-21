---
title: "Trophic ecology"
format: 
  html:
    self-contained: false
    code-fold: true
editor: source
theme: minty
keep-md: true
execute:
  warning: false
  message : false
toc: true
toc-title: Sections
toc-location: left
page-layout: full
---



__How does the trophic structure and competitive interactions within the meso-bathypelagic fish community of the Bay of Biscay evolve along the water column? __ 

__1- Isotopic diversity indices__  

- How isotopic richness evolves with depth? 

- The species with the highest biomass have contrasting isotopic values to limit competition?

__2- Focus on segregation/overlap of niches__  

- At the community level, is there significant overlap of isotopic niches between species? 

- Is this overlap reduced when species are separated according to their sampling depth? 

- Is there more or less overlap between species belonging to the same family? 

- At the intraspecific level, does the position of the isotopic niche of a species change according to its sampling depth? 

# 1. Representativeness of the sampling + baseline variation (krill) 

## Krill data 
-  Significant variability of krill isotope signatures with depth? 

::: {.cell}

```{.r .cell-code  code-fold="true"}
# Load library
library(dplyr)
library(ggplot2)

# Load data
isotope_data <-  utils::read.csv(here::here("data","raw-data", "isotopic_data_2021.csv"), sep = ";", header = T,dec = ",")
isotope_data_krill <- isotope_data %>% filter (species == "Meganyctiphanes_norvegica")

first_plot <- ggplot(data = isotope_data_krill, aes(x = d13c, y = d15n)) + 
  geom_point(aes(color= factor(trawling_depth)), size = 3) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=16)) + 
  theme_light()+
  paletteer::scale_color_paletteer_d("rcartocolor::Teal")

# Summarise By depth 
data_krill_sum <- isotope_data_krill %>% 
  group_by(trawling_depth) %>% 
  summarise(count = n(),
            mC = mean(d13c), 
            sdC = sd(d13c), 
            mN = mean(d15n), 
            sdN = sd(d15n))

second_plot <- first_plot +
  geom_errorbar(data = data_krill_sum,
                mapping = aes(x = mC, y = mN, ymin = mN - 1.96*sdN, ymax = mN + 1.96*sdN, col = factor(trawling_depth)), 
                width = 0, size=0.8) +
  geom_errorbarh(data = data_krill_sum, 
                 mapping = aes(x = mC, y = mN,xmin = mC - 1.96*sdC, xmax = mC + 1.96*sdC, col = factor(trawling_depth)),
                 height = 0, size=0.8) + 
  geom_point(data = data_krill_sum, 
             aes(x = mC, y = mN, fill = factor(trawling_depth),col = factor(trawling_depth), shape = factor(trawling_depth)),
             size = 5) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 8, 7))+
  paletteer::scale_fill_paletteer_d("rcartocolor::Teal")+
  paletteer::scale_color_paletteer_d("rcartocolor::Teal")+
  labs(shape="Trawling depth (m)", col="Trawling depth (m)", fill= "Trawling depth (m)" )

print(second_plot)
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/krill_data-1.png){width=672}
:::
:::


## Sampling
Percentage of the diversity sampled by depth layer : 

- Epipelagic : 45%
- Upper-mesopelagic: 33%
- Lower-mesopelagic : 28%
- Bathypelagic : 15%

Percentage of the biomass sampled by depth layer : 

- Epipelagic : 96%
- Upper-mesopelagic: 90% 
- Lower-mesopelagic : 54% ( _A.carbo_ 18%)
- Bathypelagic : 60% ( _A.carbo_ 10%)


# 2. Isotopic diversity index 

## Formatting of data and calculation of relative biomasses within each depth layer 


::: {.cell}

```{.r .cell-code  code-fold="true"}
# Load data 
isotope_data_fish <- isotope_data %>% filter (species != "Meganyctiphanes_norvegica")

# sourcing the R functions from 'si_div' R script
source("R/si_div.R")

# Format indiviudal_si
individuals_si <- isotope_data_fish %>%
  select(individual_code, station, d13c, d15n, species_code) %>%
  rename(indiv_ID=individual_code,
         d13C= d13c,
         d15N =d15n,
         Species_code= species_code)%>%
  mutate(depth_layer = case_when(station %in% c("Z0508") ~"epipelagic",
                                 station %in% c("Z0492", "Z0512") ~"upper_mesopelagic",
                                 station %in% c("Z0503", "Z0518") ~"lower_mesopelagic",
                                 station %in% c("Z0524","Z0497") ~"bathypelagic"))%>%
  select(-station)%>%
  arrange(Species_code)


# Format species_status_biomass with complete data of evhoe 2021
# use of all EVHOE 2021 catch data to calculate relative biomasses (not just individuals sampled for isotope)
catch_data_evhoe21 <-  utils::read.csv(here::here("data","raw-data", "data_catch_2021_mesopelagic_sp_sampled.csv"), sep = ";", header = T,
                                       dec = ".")

species_status_biomass <- catch_data_evhoe21%>%
  mutate(depth_layer = case_when(Code_Station %in% c("Z0508") ~"epipelagic",
                                 Code_Station %in% c("Z0492", "Z0512") ~"upper_mesopelagic",
                                 Code_Station %in% c("Z0503", "Z0518") ~"lower_mesopelagic",
                                 Code_Station %in% c("Z0524","Z0497") ~"bathypelagic"))%>%
  select(Code_Station, Tot_V_HV, Nom_Scientifique, depth_layer, Species_code, family)%>%
  distinct()%>%
  group_by(Nom_Scientifique, depth_layer)%>%
  mutate(biomass_sp=sum(Tot_V_HV))%>%
  select(-c(Code_Station, Tot_V_HV))%>%
  distinct()%>%
  group_by(depth_layer)%>%
  mutate(biomass_tot=sum(biomass_sp))%>%
  mutate(rel_Biomass=biomass_sp/biomass_tot*100)%>%
  select(-c(biomass_sp,  biomass_tot))%>%
  rename(Species_name= Nom_Scientifique,
         Status=depth_layer,
         Order=family)%>%
  relocate(Status, .after=Order)%>%
  relocate(rel_Biomass, .after=Species_code)%>%
  arrange(Species_code)
```
:::


## Definitions:

- __Isotopic richness__ : increases when the space occupied by the species (convex hull) is large 

- __Isotopic divergence__ : tends to 1 when all points (or their weights) are located at the edge of the convex hull, i.e. when the oragnisms with the most extreme isotopic values dominate the food web

- __Isotopic dispersion__: equal to 1 when most points (or their weights) are far from the center of gravity of the point group, i.e. when organisms tend to have contrasting isotopic values 

- __Isotopic evenness__: tends to 1 when all organisms are equally distributed in isotopic space

- __Isotopic uniqueness__: tends to 1 when most organisms (or their weights) are isolated in isotopic space, i.e. when most organisms (or those with the highest abundance) are isolated in isotopic space, their isotopic values are very different from the rest of the organisms 

## Calculation of indices within each depth layer 

- How to divide the depth layers? One station at 1000m and another at 1010, one of which is near the bottom: to be treated separately? 

- If percentage of sampled diversity low, use of  isotopic diversity indices relevant? % biomass sampled within each depth layer is high (especially if _A. carbo_ is excluded, and each most important species in terms of biomass have been sampled in each depth layer) and the indices are correlated to the biomass, so they remain relevant? 

- only the richness is not related to the biomass: to keep? 


### Epipelagic

- station at 25m, 5 species sampled 

- Low isotopic richness, the species with the highest biomasses have contrasted isotopic values and these species dominate the food web at this depth in terms of biomass ( _N. bolini_ and M. _muelleri_)


::: {.cell}

```{.r .cell-code  code-fold="true"}
# Epipelagic layer ----
individuals_si_epipelagic <- individuals_si%>%
  filter(depth_layer=="epipelagic")%>%
  select(-depth_layer)

status_biomass_epipelagic <- species_status_biomass%>%
  filter(Status=="epipelagic")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_ep<-data.frame(group=individuals_si_epipelagic[,"Species_code"], individuals_si_epipelagic)
mean_si_species_ep<-meanSI_group(individuals_si_ep)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_ep[,"sd_d13C"]/mean_si_species_ep[,"d13C"], CV_d15N=mean_si_species_ep[,"sd_d15N"]/mean_si_species_ep[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
                CV_d13C    CV_d15N
Lesti-sphy -0.009163991 0.03266115
Mau-mue    -0.005224151 0.05249590
Myct-pun   -0.009814716 0.04041982
Noto-bol   -0.009178283 0.02755544
Noto-kro   -0.012155251 0.02166277
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species)==status_biomass_epipelagic[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_ep<-data.frame(mean_si_species_ep[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=status_biomass_epipelagic[,"rel_Biomass"], Status=status_biomass_epipelagic[,"Status"], latin_name=status_biomass_epipelagic[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_ep<-scaleSI_range01(data_fish_ep)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_ep<-IDiversity(cons=data_fish_scl_ep, weight=data_fish_scl_ep[,c("rel_Biomass")], nm_plot="epipelagic")

# printing results
result <- as.data.frame(round(ID_scl_ab_ep,3)) 
knitr::kable(result)
```

::: {.cell-output-display}
|           | round(ID_scl_ab_ep, 3)|
|:----------|----------------------:|
|min_d13C   |                  0.000|
|min_d15N   |                  0.000|
|max_d13C   |                  1.000|
|max_d15N   |                  1.000|
|range_d13C |                  1.000|
|range_d15N |                  1.000|
|IPos_d13C  |                  0.541|
|IPos_d15N  |                  0.512|
|IRic       |                  0.306|
|IDiv       |                  0.953|
|IDis       |                  0.898|
|IEve       |                  0.446|
|IUni       |                  0.564|
:::
:::



![epipelagic](epipelagic_d13C_d15N.jpeg){width="550"}

### Upper-mesopelagic

-  2 stations : 370 & 555m, 6 species sampled

- Low isotopic richness but species with the highest biomass ( _N.kroyeri_ & _X. copei_) are located at the edge of the convex hull, with contrasting isotopic values. The species are rather evenly distributed and isolated in the isotopic space. 

::: {.cell}

```{.r .cell-code  code-fold="true"}
# upper_mesopelagic layer ----
individuals_si_upper_meso <- individuals_si%>%
  filter(depth_layer=="upper_mesopelagic")%>%
  select(-depth_layer)

status_biomass_upper_meso <- species_status_biomass%>%
  filter(Status=="upper_mesopelagic")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_um<-data.frame(group=individuals_si_upper_meso[,"Species_code"], individuals_si_upper_meso)
mean_si_species_um<-meanSI_group(individuals_si_um)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_um[,"sd_d13C"]/mean_si_species_um[,"d13C"], CV_d15N=mean_si_species_um[,"sd_d15N"]/mean_si_species_um[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
Arct-ris -0.010421638 0.03438256
Arg-olf  -0.008552011 0.04283576
Lamp-cro -0.015534945 0.07982239
Myct-pun -0.019927081 0.04074265
Noto-kro -0.013115883 0.02240457
Xeno-cop -0.012364479 0.07722547
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_um)==status_biomass_upper_meso[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_um <-data.frame(mean_si_species_um[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=status_biomass_upper_meso[,"rel_Biomass"], Status=status_biomass_upper_meso[,"Status"], latin_name=status_biomass_upper_meso[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_um<-scaleSI_range01(data_fish_um)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_um<-IDiversity(cons=data_fish_scl_um, weight=data_fish_scl_um[,c("rel_Biomass")], nm_plot="upper_mesopelagic")

# printing results
result <- as.data.frame(round(ID_scl_ab_um,3)) 
knitr::kable(result)
```

::: {.cell-output-display}
|           | round(ID_scl_ab_um, 3)|
|:----------|----------------------:|
|min_d13C   |                  0.000|
|min_d15N   |                  0.000|
|max_d13C   |                  1.000|
|max_d15N   |                  1.000|
|range_d13C |                  1.000|
|range_d15N |                  1.000|
|IPos_d13C  |                  0.519|
|IPos_d15N  |                  0.476|
|IRic       |                  0.287|
|IDiv       |                  0.867|
|IDis       |                  0.827|
|IEve       |                  0.755|
|IUni       |                  0.797|
:::
:::

![upper_mesopelagic](upper_mesopelagic_d13C_d15N.jpeg){width="550"}

### Lower-mesopelagic

-  2 stations : 715 & 1000m, 9 species sampled

- Isotopic richness slightly higher. In contrast, all other indices are rather low: the species with the largest biomasses (especially _L.crocodilus_) are located in the center of the isotopic space and do not have very contracted isotopic values. The species are not evenly distributed in the isotopic space. 


::: {.cell}

```{.r .cell-code  code-fold="true"}
#Lower mesopelagic layer ----
individuals_si_lower_meso <- individuals_si%>%
  filter(depth_layer=="lower_mesopelagic")%>%
  select(-depth_layer)

status_biomass_lower_meso <- species_status_biomass%>%
  filter(Status=="lower_mesopelagic")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_lm<-data.frame(group=individuals_si_lower_meso[,"Species_code"], individuals_si_lower_meso)
mean_si_species_lm<-meanSI_group(individuals_si_lm)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_lm[,"sd_d13C"]/mean_si_species_lm[,"d13C"], CV_d15N=mean_si_species_lm[,"sd_d15N"]/mean_si_species_lm[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
Arct-ris -0.013383792 0.02784671
Arg-olf  -0.009812119 0.03092443
Ben-gla  -0.015512985 0.06444602
Cyclo    -0.009264821 0.04946692
Lamp-cro -0.020344506 0.04819340
Mau-arg  -0.009632001 0.03191368
Ser-bea  -0.012549696 0.06183851
Ser-koef -0.022852986 0.05388211
Xeno-cop -0.012017717 0.06196185
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_lm)==status_biomass_lower_meso[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_lm <-data.frame(mean_si_species_lm[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=status_biomass_lower_meso[,"rel_Biomass"], Status=status_biomass_lower_meso[,"Status"], latin_name=status_biomass_lower_meso[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_lm<-scaleSI_range01(data_fish_lm)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_lm<-IDiversity(cons=data_fish_scl_lm, weight=data_fish_scl_lm[,c("rel_Biomass")], nm_plot="lower_mesopelagic")

# printing results
result <- as.data.frame(round(ID_scl_ab_lm,3)) 
knitr::kable(result)
```

::: {.cell-output-display}
|           | round(ID_scl_ab_lm, 3)|
|:----------|----------------------:|
|min_d13C   |                  0.000|
|min_d15N   |                  0.000|
|max_d13C   |                  1.000|
|max_d15N   |                  1.000|
|range_d13C |                  1.000|
|range_d15N |                  1.000|
|IPos_d13C  |                  0.627|
|IPos_d15N  |                  0.415|
|IRic       |                  0.477|
|IDiv       |                  0.561|
|IDis       |                  0.447|
|IEve       |                  0.535|
|IUni       |                  0.448|
:::
:::

![lower_mesopelagic](lower_mesopelagic_d13C_d15N.jpeg){width="550"}

### Bathypelagic

-  2 stations : 1010 (close to the bottom) & 1335m, 7 species sampled

- The species with the highest biomass, _L.crocodilus_, shows extreme isotopic values compared to the rest of the community (high divergence index and uniqueness)


::: {.cell}

```{.r .cell-code  code-fold="true"}
#Lower mesopelagic layer ----
individuals_si_bathypelagic <- individuals_si%>%
  filter(depth_layer=="bathypelagic")%>%
  select(-depth_layer)

status_biomass_bathypelagic <- species_status_biomass%>%
  filter(Status=="bathypelagic")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_b<-data.frame(group=individuals_si_bathypelagic[,"Species_code"], individuals_si_bathypelagic)
mean_si_species_b<-meanSI_group(individuals_si_b)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_b[,"sd_d13C"]/mean_si_species_b[,"d13C"], CV_d15N=mean_si_species_b[,"sd_d15N"]/mean_si_species_b[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
               CV_d13C    CV_d15N
Arg-olf   -0.011354958 0.04217724
Lamp-cro  -0.017123430 0.05265353
Lamp-mac  -0.022058396 0.02734756
Mel-atlan -0.009854421 0.04078758
Myct-pun  -0.024242014 0.04136229
Ser-bea   -0.013913445 0.05673029
Xeno-cop  -0.010997820 0.05399656
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_b)==status_biomass_bathypelagic[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_b <-data.frame(mean_si_species_b[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=status_biomass_bathypelagic[,"rel_Biomass"], Status=status_biomass_bathypelagic[,"Status"], latin_name=status_biomass_bathypelagic[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_b<-scaleSI_range01(data_fish_b)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_b<-IDiversity(cons=data_fish_scl_b, weight=data_fish_scl_b[,c("rel_Biomass")], nm_plot="bathypelagic")

# printing results
result <- as.data.frame(round(ID_scl_ab_lm,3)) 
knitr::kable(result)
```

::: {.cell-output-display}
|           | round(ID_scl_ab_lm, 3)|
|:----------|----------------------:|
|min_d13C   |                  0.000|
|min_d15N   |                  0.000|
|max_d13C   |                  1.000|
|max_d15N   |                  1.000|
|range_d13C |                  1.000|
|range_d15N |                  1.000|
|IPos_d13C  |                  0.627|
|IPos_d15N  |                  0.415|
|IRic       |                  0.477|
|IDiv       |                  0.561|
|IDis       |                  0.447|
|IEve       |                  0.535|
|IUni       |                  0.448|
:::
:::

![bathypelagic](bathypelagic_d13C_d15N.jpeg){width="550"}

## Evolution of trophic diversity indices with depth 

- The lower mesopelagic layer stands out with low indices for all measurements (except richness). Within this layer the species with the highest biomass (especially _L.crocodilus_) are located near the centre  of the convex hull with relatively similar isotopic values (no species with rare isotopic values). It is also the depth layer with the highest number of species sampled. 

- Within the other stations the divergence is high, i.e. the species that dominate the food web in terms of biomass have extreme isotopic values. 

- The upper mesopelagic layer also stands out with high values for all indices. It has the highest uniqueness and evenness values, which means that the species with the largest biomass are isolated in the isotopic space but also that the species are equally distributed. Indeed, the two species with the highest biomass ( _X.copei, N.kroyeri_) are located on opposite sides of the isotopic space. 


::: {.cell}

```{.r .cell-code}
# load library
library(ggplot2)
library(tidyr)

depth <- c("epipelagic", "upper_mesopelagic", "lower_mesopelagic", "bathypelagic" )
richness <- c(0.306, 0.287, 0.477, 0.441)
divergence <- c(0.953, 0.867, 0.561, 0.971)
disperion <- c(0.898, 0.827, 0.447, 0.516)
evenness <- c(0.446, 0.755, 0.535, 0.442)
uniqueness <- c(0.564, 0.797, 0.448, 0.765)

isotopic_indices <- 
  data.frame(depth, richness, divergence, disperion, evenness, uniqueness) %>%
  pivot_longer(
    cols = c(richness, divergence, disperion, evenness, uniqueness),
    names_to = "indices")

isotopic_indices$depth <- factor(isotopic_indices$depth, levels=c("epipelagic", "upper_mesopelagic",
                                                                 "lower_mesopelagic", "bathypelagic"))
ggplot(isotopic_indices, aes(x=depth, y=value))+
  geom_point(aes(col=depth), size=2.3)+
  facet_wrap(~indices)+
  scale_color_manual(values = c("#93c3ff","#74a8ff","#538def","#2c73d2"))+
  theme_light()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")+
  xlab("")
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/trophic_indices_df-1.png){width=960}
:::
:::


# 3. Isotopic niches: segregation vs overlap 


::: {.cell}

:::


## Community level

-  ellipses at 40%

::: {.cell}

```{.r .cell-code  code-fold="true"}
nichecol <- c("#E4A33A","#F67451","#D664BE", "#3DA5D9", "#94b3ae",
              "#18206F","#FD151B","#049A8F","#072AC8", "purple",
               "#d193f7","#d8c2ab","#678FCB","#A63A49","#00547A", "#6B54A0")


community_plot <- ggplot(data = isotope_data_fish, 
                     aes(x = d13c, 
                         y = d15n)) + 
  geom_point(aes(color = species, shape= family), size = 2.3) +
  scale_color_manual(values=nichecol)+
  scale_fill_manual(values=nichecol)+
  scale_shape_manual(values =c(15, 16, 17, 8, 19, 18, 3, 13, 6) )+
  scale_x_continuous(expression({delta}^13*C~'\u2030'), limits = c(-21, -18.5)) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'), limits = c(7, 14))+
  theme(text = element_text(size=16)) + 
  theme_light()

# Ellipses
# How big the ellipse 
p.ell <- 0.40
ellipse_community <- community_plot + 
  stat_ellipse(aes(group = species, 
                   fill = species, 
                   color = species), 
               alpha = 0.2, 
               level = p.ell,
               linewidth = 0.7,
               type = "norm",
               geom = "polygon")

print(ellipse_community)
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/community_ellipses-1.png){width=960}
:::
:::

### Overlap between species with ellipses at 40%


::: {.cell}

```{.r .cell-code  code-fold="true"}
# OVERLAP between species, all depth ----
test.elp_community <- rKIN::estEllipse(data= isotope_data_fish,  x="d13c", y="d15n", group="species", levels=40, smallSamp = TRUE)

# Extract the area of each polygon
elp.area_community <- rKIN::getArea(test.elp_community)

# determine polygon overlap for all polygons
elp.olp_community <- rKIN::calcOverlap(test.elp_community)
elp.olp_community_melt <- reshape2::melt(elp.olp_community, na.rm = TRUE)

ggplot(elp.olp_community_melt, aes(x=OverlapID, y=variable, fill=value)) + 
  geom_tile(alpha=0.85)+
  theme_minimal()+ 
  scale_fill_gradient(high = "darkblue", low="grey99", limits=c(0,1))+
  geom_text(aes(label = value), color = "black", size = 3.5) +
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 8, hjust = 1), 
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8))
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/community_overlaps-1.png){width=864}
:::
:::


## By depth layer 

### Epipelagic

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_epipelagic-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_epipelagic-1.png){fig-align='center' width=672}
:::
:::



### Upper-mesopelagic

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_upper_meso-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_upper_meso-1.png){fig-align='center' width=672}
:::
:::


### Lower-mesopelagic

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_lower_meso-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_lower_meso-1.png){fig-align='center' width=672}
:::
:::


### Bathypelagic

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_bathypelagic-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_bathypelagic-1.png){fig-align='center' width=672}
:::
:::


## By station 

### Station Z0492 - 370m

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_Z0492-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_z0492-1.png){fig-align='center' width=480}
:::
:::


### Station Z0512 - 555m

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_Z0512-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_z0512-1.png){fig-align='center' width=480}
:::
:::


### Station Z0503 - 715m

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_Z0503-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_z0503-1.png){fig-align='center' width=480}
:::
:::


### Station Z0518 - 1000m

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_Z0518-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_z0518-1.png){fig-align='center' width=480}
:::
:::


### Station Z0524 - 1010m

-  Station sampled near the bottom (<100m)

-  _A. olfersii_: only 3 samples, ellipse not performed 

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_Z0524-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_z0524-1.png){fig-align='center' width=480}
:::
:::


### Station Z0497 - 1335m

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_Z0497-1.png){fig-align='center' width=672}
:::
:::

::: {.cell layout-align="center"}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/overlap_z0497-1.png){fig-align='center' width=480}
:::
:::



## cluster

- cluster with the community-level overlap matrix as input (without depth division)

__cluster 1__ : 

-  S. koefoedi & M. argipalla, high overlap, same depth, same family
-  L. macdonaldi & S. koefoedi : different depth distribution

__cluster 2__: 

- N. kroyeri et N. bolini, same depth distribution, same family
- N.kroyeri hada high overlap with M.atlanticum and Cyclothone but different depth distribution (same for N. bolini and M. atlanticum)
- Cyclothone and M.atlanticum: not in the same depth layer (but same depth distribution)

__cluster3__: 

- A. olfersii had a high overlap with L. crocodilus and M.punctatum (station at 1000 and 370m)
- M.punctatum & L.crocodilus: in the upper-mesopelagic assemblage (not in station) 

__cluster4__: 

-  L. sphyrenoides & A.risso: not the same depth distribution, same family 

__cluter 5__:

- X. copei & M. muelleri: not the same depth distribution

::: {.cell layout-align="center"}

```{.r .cell-code}
elp.olp_community_mt <- elp.olp_community%>%
  tibble::column_to_rownames(var="OverlapID")

df <- as.data.frame(scale(elp.olp_community_mt)) 

# Elbow method
# factoextra::fviz_nbclust(df, kmeans, method = "wss") +
#   labs(subtitle = "Elbow method")

# Silhouette method
# factoextra::fviz_nbclust(df, kmeans, method = "silhouette")+
#   labs(subtitle = "Silhouette method")

# Gap statistic
# factoextra::fviz_nbclust(df, kmeans, nstart = 25, 
#                          method = "gap_stat", nboot = 500, 
#                          verbose = FALSE)+
#   labs(subtitle = "Gap statistic method")

res.km <- kmeans(scale(df), 5, nstart = 25)
# K-means clusters showing the group of each individuals
#res.km$cluster

factoextra::fviz_cluster(res.km, data = df, 
                         palette=c("#7b95cc", "#007A75", "#D96681", "#150578", "#BF9AD8"),
                         ellipse.type = "convex",
                         ggtheme = theme_minimal())
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/cluster-1.png){fig-align='center' width=768}
:::

```{.r .cell-code}
# # Dimension reduction using PCA
# res.pca <- prcomp(df,  scale = TRUE)
# # Coordinates of individuals
# ind.coord <- as.data.frame(factoextra::get_pca_ind(res.pca)$coord)
# # Add clusters obtained using the K-means algorithm
# ind.coord$cluster <- factor(res.km$cluster)
# # Add Species groups from the original data sett
# ind.coord$Species <- df$Species
# # Data inspection
# head(ind.coord)
# 
# # Percentage of variance explained by dimensions
# eigenvalue <- round(factoextra::get_eigenvalue(res.pca), 1)
# variance.percent <- eigenvalue$variance.percent
# head(eigenvalue)
```
:::

::: {.cell layout-align="center"}

```{.r .cell-code}
cluster_data <- isotope_data_fish%>%
  mutate(cluster= case_when(species%in% c("Lampanyctus_macdonaldi", "Searsia_koefoedi", "Maulisia_argipalla") ~"A",
                            species%in% c("Cyclothone", "Notoscopelus_bolini", "Notoscopelus_kroyeri", 
                                           "Melanostigma_atlanticum")~"B",
                            species%in% c("Lampanyctus_crocodilus","Argyropelecus_olfersii", 
                                          "Benthosema_glaciale", "Serrivomer_beanii",
                                          "Myctophum_punctatum")~"C",
                            species%in% c("Arctozenus_risso", "Lestidiops_sphyrenoides")~"D",
                            species%in% c("Xenodermichthys_copei", "Maurolicus_muelleri")~"E"))


cluster_plot <- ggplot(data = cluster_data, 
                         aes(x = d13c, 
                             y = d15n)) + 
  geom_point(aes(color = cluster, fill=cluster), size = 1.5, alpha=0.5) +
  scale_color_manual(values= c("#7b95cc", "#007A75", "#D96681", "#150578", "#BF9AD8"))+
  scale_fill_manual(values=c("#7b95cc", "#007A75", "#D96681", "#150578", "#BF9AD8"))+
  scale_x_continuous(expression({delta}^13*C~'\u2030'), limits = c(-21, -18.5)) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'), limits = c(7, 14))+
  theme(text = element_text(size=16)) + 
  theme_light()

# Ellipses
# How big the ellipse 
p.ell <- 0.40
ellipse_cluster <- cluster_plot + 
  stat_ellipse(aes(group = species, 
                   fill = cluster, 
                   color = cluster), 
               alpha = 0.1, 
               level = p.ell,
               linewidth = 0.8,
               type = "norm",
               geom = "polygon")
print(ellipse_cluster)
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/cluster_niche-1.png){fig-align='center' width=768}
:::
:::



## Distribution

::: {.cell}
::: {.cell-output-display}
![](trophic_ecology_files/figure-html/density_plot-1.png){width=1344}
:::
:::



## Intraspecific changes over the depth range ? Evolution of niche area (KUD) with depth
### Lampanyctus crocodilus 

::: {.cell layout-align="center"}

```{.r .cell-code}
lamp_cro_niche <- isotope_data_fish %>% filter(species=="Lampanyctus_crocodilus")
p.ell <- 0.40

ggplot(lamp_cro_niche, aes(x = d13c,y = d15n)) + 
  geom_point(aes(col=factor(trawling_depth), shape=factor(trawling_depth)), size = 2, alpha= 0.8) +
  scale_x_continuous(expression({delta}^13*C~'\u2030'), limits = c(-21, -18.5)) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'), limits = c(7, 14))+
  theme(text = element_text(size=16)) + 
  theme_light()+
  paletteer::scale_color_paletteer_d("LaCroixColoR::Pure")+
  paletteer::scale_fill_paletteer_d("LaCroixColoR::Pure")+
  stat_ellipse(aes(group = factor(trawling_depth), 
                   fill = factor(trawling_depth), 
                   color = factor(trawling_depth)), 
               alpha = 0.25, 
               level = p.ell,
               linewidth = 0.7,
               type = "norm",
               geom = "polygon")
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_lamp_cro-1.png){fig-align='center' width=960}
:::
:::


#### Calculate vector (centroids)

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}
```
Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model
```
:::

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_lamp_centroid-1.png){fig-align='center' width=1632}
:::
:::




### Xenodermichthys copei

::: {.cell layout-align="center"}

```{.r .cell-code}
xeno_cop_niche <- isotope_data_fish %>% filter(species=="Xenodermichthys_copei")
p.ell <- 0.40

ggplot(xeno_cop_niche, aes(x = d13c,y = d15n)) + 
  geom_point(aes(col=factor(trawling_depth), shape=factor(trawling_depth)), size = 2, alpha= 0.8) +
  scale_x_continuous(expression({delta}^13*C~'\u2030'), limits = c(-21, -18.5)) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'), limits = c(7, 14))+
  theme(text = element_text(size=16)) + 
  theme_light()+
  paletteer::scale_color_paletteer_d("LaCroixColoR::Pure")+
  paletteer::scale_fill_paletteer_d("LaCroixColoR::Pure")+
  stat_ellipse(aes(group = factor(trawling_depth), 
                   fill = factor(trawling_depth), 
                   color = factor(trawling_depth)), 
               alpha = 0.25, 
               level = p.ell,
               linewidth = 0.7,
               type = "norm",
               geom = "polygon")
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_xeno_cop-1.png){fig-align='center' width=960}
:::
:::


#### Calculate vector (centroids)

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}
```
Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 22
   Unobserved stochastic nodes: 3
   Total graph size: 37

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 20
   Unobserved stochastic nodes: 3
   Total graph size: 35

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 15
   Unobserved stochastic nodes: 3
   Total graph size: 30

Initializing model
```
:::

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_xeno_cop_centroid-1.png){fig-align='center' width=1632}
:::
:::




### Argyropelecus olfersii

::: {.cell layout-align="center"}

```{.r .cell-code}
argy_olf_niche <- isotope_data_fish %>% filter(species=="Argyropelecus_olfersii")
p.ell <- 0.40

ggplot(argy_olf_niche, aes(x = d13c,y = d15n)) + 
  geom_point(aes(col=factor(trawling_depth), shape=factor(trawling_depth)), size = 2, alpha= 0.8) +
  scale_x_continuous(expression({delta}^13*C~'\u2030'), limits = c(-21, -18.5)) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'), limits = c(7, 14))+
  theme(text = element_text(size=16)) + 
  theme_light()+
  paletteer::scale_color_paletteer_d("LaCroixColoR::Pure")+
  paletteer::scale_fill_paletteer_d("LaCroixColoR::Pure")+
  stat_ellipse(aes(group = factor(trawling_depth), 
                   fill = factor(trawling_depth), 
                   color = factor(trawling_depth)), 
               alpha = 0.25, 
               level = p.ell,
               linewidth = 0.7,
               type = "norm",
               geom = "polygon")
```

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_argy_olf-1.png){fig-align='center' width=960}
:::
:::


#### Calculate vector (centroids)

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}
```
Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 11
   Unobserved stochastic nodes: 3
   Total graph size: 26

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 11
   Unobserved stochastic nodes: 3
   Total graph size: 26

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 8
   Unobserved stochastic nodes: 3
   Total graph size: 23

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 3
   Unobserved stochastic nodes: 3
   Total graph size: 18

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 8
   Unobserved stochastic nodes: 3
   Total graph size: 23

Initializing model
```
:::

::: {.cell-output-display}
![](trophic_ecology_files/figure-html/niche_argy_olf_centroid-1.png){fig-align='center' width=1632}
:::
:::
