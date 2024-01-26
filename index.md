---
title: "index"
author: "Liz Loutrage"
format:
  html:
    code-fold: true
    code-summary: "Show the code"
    self-contained: true
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



# 1. Data

::: {.cell}

```{.r .cell-code  code-fold="true"}
# Library
library(dplyr)
library(ggplot2)

# load data 
## Isotope data 
isotope_data <-  utils::read.csv(here::here("data", "isotopic_data_2021.csv"), sep = ";", header = T,dec = ",")%>%
    # remove outlier values 
    filter(individual_code!="Arg-olf 42")

isotope_data_fish <- isotope_data %>%
  # only fish 
  filter(taxon == "Fish")%>%
  arrange(species)

## trawling data 
trawling_data_evhoe21 <-  utils::read.csv(here::here("data", "trawling_data_evhoe_2021.csv"), sep = ";", header = T, dec = ".")

# density distribution from biomass value 
density_distribution <- trawling_data_evhoe21%>%
  select(Nom_Scientifique, Tot_V_HV, Code_Station)%>%
  #selection of pelagic trawling 
  filter(Code_Station%in% c("Z0524", "Z0518", "Z0512", "Z0508", 
                            "Z0503", "Z0497", "Z0492"))%>%
  #selection of species sampled for isotope 
  filter(Nom_Scientifique%in%c("Arctozenus risso", "Argyropelecus olfersii", "Benthosema glaciale",
                               "Cyclothone", "Lampanyctus crocodilus", "Lampanyctus macdonaldi",
                               "Lestidiops sphyrenoides", "Maulisia argipalla", "Maurolicus muelleri",
                               "Melanostigma atlanticum", "Myctophum punctatum", "Notoscopelus bolini",
                               "Notoscopelus kroyeri", "Searsia koefoedi", "Serrivomer beanii",
                               "Xenodermichthys copei"))%>%
  mutate(Nom_Scientifique=recode(Nom_Scientifique, "Cyclothone"="Cyclothone spp."))%>%
  mutate(trawling_depth= case_when(Code_Station %in% c("Z0508") ~25,
                                   Code_Station %in% c("Z0492") ~370,
                                   Code_Station%in% c("Z0512") ~555,
                                   Code_Station %in% c("Z0503") ~715,
                                   Code_Station %in% c("Z0518") ~1000,
                                   Code_Station %in% c("Z0524") ~1010,
                                   Code_Station %in% c("Z0497") ~1335))%>%
  distinct()%>%
  group_by(Nom_Scientifique)%>%
  mutate(sum_sp=sum(Tot_V_HV))%>%
  ungroup()%>%
  group_by(trawling_depth, Nom_Scientifique)%>%
  mutate(pourcentage_bio=sum(Tot_V_HV/sum_sp*100))%>%
  # Selection of trawling depth 
  select(Nom_Scientifique, trawling_depth, pourcentage_bio)%>%
  # to have a round number to be able to multiply it afterwards 
  mutate(across(pourcentage_bio, round, 0)) %>%
  mutate(n_bio = as.integer(pourcentage_bio))%>%
  select(-c(pourcentage_bio))%>%
  tidyr::uncount(n_bio)
```
:::


# 2. Isotopic niches

## Ellipses

-   ellipses at 40%
-   Δ $\delta$<sup>13</sup>C = 2.36‰
-   Δ $\delta$<sup>15</sup>N = 5.94‰

::: {.cell}

```{.r .cell-code  code-fold="true"}
niche_plot_community <- isotope_data%>%
  mutate(species= gsub("_"," ", species))%>%
  mutate(species=recode(species, "Cyclothone"="Cyclothone spp."))

# order by taxonomy 
niche_plot_community$species <- factor(niche_plot_community$species, 
                                       levels = c("Serrivomer beanii",
                                                  "Xenodermichthys copei",
                                                  "Maulisia argipalla",
                                                  "Searsia koefoedi",
                                                  "Cyclothone spp.",
                                                  "Argyropelecus olfersii",
                                                  "Maurolicus muelleri",
                                                  "Lestidiops sphyrenoides",
                                                  "Arctozenus risso",
                                                  "Benthosema glaciale",
                                                  "Lampanyctus crocodilus",
                                                  "Lampanyctus macdonaldi",
                                                  "Myctophum punctatum",
                                                  "Notoscopelus bolini",
                                                  "Notoscopelus kroyeri",
                                                  "Melanostigma atlanticum",
                                                  "Meganyctiphanes norvegica"))

# plot
ggplot(data = niche_plot_community, 
       aes(x = d13c, 
           y = d15n)) + 
  geom_point(aes(color = species, shape= taxon)) +
   paletteer:: scale_color_paletteer_d("pals::alphabet")+ 
   paletteer:: scale_fill_paletteer_d("pals::alphabet")+
  scale_shape_manual(values= c(19, 3))+
  scale_x_continuous(expression({delta}^13*C~'\u2030')) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'))+
  stat_ellipse(aes(group = species, fill = species, color = species), 
               alpha = 0.2, level = 0.40,linewidth = 0.7, type = "norm", geom = "polygon")+
  theme_bw()+
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15))+
  labs(shape="Taxon", col= "Species", fill="Species")+
  theme(aspect.ratio = 1)
```

::: {.cell-output-display}
![](index_files/figure-html/community_ellipses-1.png){width=960}
:::

```{.r .cell-code  code-fold="true"}
ggsave("niches_community.png", path = "figures", dpi = 700, height = 10, width = 12)
```
:::


## Overlaps


::: {.cell}

```{.r .cell-code  code-fold="true"}
# Prepare data 
mat_overlap_data <- isotope_data%>%
  filter(taxon=="Fish")%>%
  mutate(species= gsub("_"," ", species))%>%
  mutate(species=recode(species, "Cyclothone"="Cyclothone spp."))

# Arrange species by their d15n values 
mat_overlap <- mat_overlap_data%>%
  group_by(species)%>%
  mutate(mean_d15n=mean(d15n))%>%
  arrange(mean_d15n)%>%
  as.data.frame()

# ellipse at 40%
test.elp_community <- rKIN::estEllipse(data= mat_overlap,  x="d13c", y="d15n", group="species", levels=40, smallSamp = TRUE)
```

::: {.cell-output .cell-output-stdout}
```
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
[1] "data.frame"
```
:::

```{.r .cell-code  code-fold="true"}
# Extract the area of each polygon
elp.area_community <- rKIN::getArea(test.elp_community)

# determine polygon overlap for all polygons
elp.olp_community <- rKIN::calcOverlap(test.elp_community)%>%
  #renames row names
  mutate(OverlapID= gsub("_"," ", OverlapID))%>%
  mutate(OverlapID= gsub("40","", OverlapID))%>%
  mutate(across(where(is.numeric), round, 2))%>%
  tibble::column_to_rownames("OverlapID")%>%
  as.data.frame()

#renames colnames
colnames(elp.olp_community)<- gsub(x =colnames(elp.olp_community), pattern = "_", replacement = " ")
colnames(elp.olp_community)<- gsub(x =colnames(elp.olp_community), pattern = "40", replacement = "")

#plot 
ggcorrplot::ggcorrplot(elp.olp_community, lab = T, outline.color = "white", lab_size = 3, tl.cex = 10)+
  scale_fill_gradient2(limit = c(0,1), low = "white", high = "grey50", mid = "grey80", midpoint = 0.5)+
  labs(fill="Overlap value")+
  theme(axis.text = element_text(face="italic", size = 13),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        plot.background = element_rect(colour = "white"))
```

::: {.cell-output-display}
![](index_files/figure-html/community_overlaps-1.png){width=864}
:::

```{.r .cell-code  code-fold="true"}
ggsave("matrix_overlap.png", path = "figures", dpi = 700, width = 8, height = 6)
```
:::


# 3. Depth segregation

## Cluster

-   input data for cluster = overlap matrix
-   depth distribution = from complete 2021 trawling data (not only individuals sampled for isotope)


::: {.cell layout-align="center"}

```{.r .cell-code  code-fold="true"}
# calculate median depth by species with trawling data set 2021
mean_depth_sp <- density_distribution %>%
  group_by(Nom_Scientifique) %>%
  mutate(median_depth = median(trawling_depth)) %>%
  select(Nom_Scientifique, median_depth) %>%
  distinct() %>%
  ungroup() %>%
  arrange(Nom_Scientifique) %>%
  rename(species = Nom_Scientifique)

# Gap statistic
factoextra::fviz_nbclust(
  elp.olp_community ,
  kmeans,
  nstart = 25,
  method = "gap_stat",
  nboot = 100,
  verbose = FALSE) +
  labs(subtitle = "Gap statistic method")
```

::: {.cell-output-display}
![](index_files/figure-html/cluster-1.png){fig-align='center' width=672}
:::

```{.r .cell-code  code-fold="true"}
res.km <- kmeans(scale(elp.olp_community), 5, nstart = 25)

# Dimension reduction using PCA
res.pca <- prcomp(elp.olp_community [,-5],  scale = TRUE)

# Individual coordinates
ind.coord <- as.data.frame(factoextra::get_pca_ind(res.pca)$coord)

# Add the clusters obtained using the k-means algorithm
ind.coord$cluster <- factor(res.km$cluster)

# Percentage of variance explained by dimensions
eigenvalue <- round(factoextra::get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent

# Add species names and median depth
ind.coord_depth <- cbind(ind.coord, mean_depth_sp)
ind.coord_depth$median_depth <- as.factor(ind.coord_depth$median_depth)
```
:::


## Cluster and depth distribution


::: {.cell}

```{.r .cell-code  code-fold="true"}
# dendrogram ----
dend <- elp.olp_community %>%
  dist() %>%
  hclust() %>%
  as.dendrogram()

#png("figures/dendrogram.png", units="in", width=6, height=4, res=700)
par(mar = c(1, 1, 1, 10))
dend %>%
  dendextend::set("labels_col",
      value = c("#86BBBD", "#ECA72C", "#4D85A8", "#9BABE8", "#D35D4A"),
      k = 5) %>%
  dendextend::set("branches_k_color",
      value = c("#86BBBD", "#ECA72C", "#4D85A8", "#9BABE8", "#D35D4A"),
      k = 5) %>%
  dendextend::set("labels_cex", 0.8) %>%
  dendextend::set("branches_lty", 2) %>%
  dendextend::set("leaves_bg") %>%
  dendextend::set("leaves_pch", 19) %>%
  dendextend::set("leaves_col",
    c(
      "#86BBBD",
      "#86BBBD",
      "#86BBBD",
      "#86BBBD",
      "#86BBBD",
      "#ECA72C",
      "#ECA72C",
      "#ECA72C",
      "#ECA72C",
      "#4D85A8",
      "#4D85A8",
      "#4D85A8",
      "#9BABE8",
      "#9BABE8",
      "#D35D4A",
      "#D35D4A"
    )
  ) %>%
  plot(horiz = TRUE, axes = FALSE)
```

::: {.cell-output-display}
![](index_files/figure-html/density_cluster_plot-1.png){width=960}
:::

```{.r .cell-code  code-fold="true"}
#dev.off()

# Density plot----
# assign each species to a cluster 
density_distribution_cluster <- density_distribution %>%
  mutate(
    cluster = case_when(
      Nom_Scientifique %in% c(
        "Argyropelecus olfersii",
        "Lampanyctus crocodilus",
        "Benthosema glaciale",
        "Myctophum punctatum",
        "Serrivomer beanii"
      ) ~ 4,
      Nom_Scientifique %in% c(
        "Lampanyctus macdonaldi",
        "Maulisia argipalla",
        "Searsia koefoedi"
      ) ~ 5,
      Nom_Scientifique %in% c(
        "Cyclothone spp.",
        "Notoscopelus bolini",
        "Notoscopelus kroyeri",
        "Melanostigma atlanticum"
      ) ~ 3,
      Nom_Scientifique %in% c("Xenodermichthys copei", "Maurolicus muelleri") ~
        1,
      Nom_Scientifique %in% c("Arctozenus risso", "Lestidiops sphyrenoides") ~
        2
    )
  ) %>%
  group_by(Nom_Scientifique) %>%
  arrange(desc(trawling_depth))

# Order in function of median depth
density_distribution_cluster$Nom_Scientifique = with(density_distribution_cluster, reorder(Nom_Scientifique, cluster, max))  

# plot
ggplot(density_distribution_cluster,
       aes(x = trawling_depth, y = Nom_Scientifique, group = Nom_Scientifique, 
           col=factor(cluster), fill=factor(cluster)))+ 
  scale_fill_manual(values = c("#D35D4A", "#9BABE8", "#ECA72C", "#86BBBD","#4D85A8"))+
  scale_color_manual(values = c("#D35D4A", "#9BABE8", "#ECA72C", "#86BBBD","#4D85A8"))+
  ggridges::stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5 , alpha=0.4, size=0.7,
                                rel_min_height = 0.002, scale=1.2)+
  theme_bw()+
  scale_y_discrete(position = "left")+
  scale_x_reverse(limits = c( 1400,0))+
  coord_flip()+
  ylab(label = "")+ xlab("Depth (m)")+
  theme(axis.text.y = element_text(size=15),
        axis.text.x = element_text(face="italic", size=13, angle=80, vjust = 0.5, hjust=0.5),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))+
  guides(fill="none", col="none", alpha="none")
```

::: {.cell-output-display}
![](index_files/figure-html/density_cluster_plot-2.png){width=960}
:::

```{.r .cell-code  code-fold="true"}
ggsave("density_plot.png", path = "figures", dpi = 600, height = 8, width = 10)
```
:::


# 4. Isotopic diversity index

## Representativeness of sampling

-   Percentage of species biomass in each station


::: {.cell}

```{.r .cell-code  code-fold="true"}
# Species % biomass and abundance by station
species_abundance <- trawling_data_evhoe21 %>%
  # selection of mesopelagic trawl
  filter(Code_Station %in% c("Z0524", "Z0518", "Z0512", "Z0508",
                             "Z0503", "Z0497", "Z0492")) %>%
  #deletion when only genus name and genus already sampled in isotopy + A. carbo
  filter(
    !Nom_Scientifique %in% c(
      "Cyclothone braueri",
      "Cyclothone microdon",
      "Myctophidae",
      "Aphanopus carbo"
    )
  ) %>%
  mutate(
    depth_layer = case_when(
      Code_Station %in% c("Z0508") ~ "epipelagic",
      Code_Station %in% c("Z0492", "Z0512") ~ "upper-mesopelagic",
      Code_Station %in% c("Z0503", "Z0518") ~ "lower-mesopelagic",
      Code_Station %in% c("Z0497") ~ "bathypelagic",
      Code_Station %in% c("Z0524") ~ "bottom-proximity"
    )
  ) %>%
  select(depth_layer, Nbr, Nom_Scientifique, Tot_V_HV) %>%
  group_by(Nom_Scientifique, depth_layer) %>%
  mutate(nb_ind = sum(Nbr)) %>%
  select(-Nbr) %>%
  distinct() %>%
  group_by(depth_layer) %>%
  mutate(sum_biomass_station = sum(Tot_V_HV)) %>%
  ungroup() %>%
  group_by(depth_layer, Nom_Scientifique) %>%
  mutate(pourcentage_biomass_sp = Tot_V_HV / sum_biomass_station * 100) %>%
  select(Nom_Scientifique, pourcentage_biomass_sp, nb_ind, depth_layer) %>%
  mutate(across(where(is.numeric), round, 2))
  
htmltools::tagList(DT::datatable(species_abundance))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-ad815e7ebc76f2b5d1da" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-ad815e7ebc76f2b5d1da">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148"],["Xenodermichthys copei","Argyropelecus olfersii","Maurolicus muelleri","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus crocodilus","Notoscopelus kroyeri","Arctozenus risso","Melanostomias bartonbeani","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Maurolicus muelleri","Borostomias antarcticus","Chauliodus sloani","Stomias boa boa","Nansenia oblita","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus crocodilus","Nannobrachium atrum","Lampanyctus macdonaldi","Lobianchia gemellarii","Notoscopelus bolini","Notoscopelus kroyeri","Paralepis coregonoides","Lestidiops sphyrenoides","Arctozenus risso","Derichthys serpentinus","Synaphobranchus kaupii","Melanostigma atlanticum","Bathylagus euryops","Sigmops bathyphilus","Malacosteus niger","Melanostomias bartonbeani","Serrivomer beanii","Bolinichthys supralateralis","Photostylus pycnopterus","Eurypharynx pelecanoides","Searsia koefoedi","Mentodus rostratus","Xenodermichthys copei","Cyclothone","Argyropelecus hemigymnus","Argyropelecus olfersii","Maurolicus muelleri","Chauliodus sloani","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus crocodilus","Notoscopelus bolini","Notoscopelus kroyeri","Lestidiops sphyrenoides","Arctozenus risso","Melanostigma atlanticum","Melanostomias bartonbeani","Serrivomer beanii","Sudis hyalina","Sagamichthys schnakenbecki","Searsia koefoedi","Pseudoscopelus altipinnis","Maurolicus muelleri","Chauliodus sloani","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus","Notoscopelus bolini","Notoscopelus kroyeri","Notoscopelus kroyeri","Lestidiops sphyrenoides","Melanostomias bartonbeani","Dolicholagus longirostris","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus","Lampanyctus crocodilus","Nannobrachium atrum","Notoscopelus kroyeri","Evermannella balbo","Arctozenus risso","Melanostigma atlanticum","Nansenia","Dolicholagus longirostris","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Maurolicus muelleri","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Diaphus holti","Lampanyctus crocodilus","Nannobrachium atrum","Lampanyctus macdonaldi","Notoscopelus bolini","Notoscopelus kroyeri","Evermannella balbo","Lestidiops sphyrenoides","Arctozenus risso","Melanostigma atlanticum","Malacosteus niger","Maulisia argipalla","Normichthys operosus","Serrivomer beanii","Parabrotulidae","Photostylus pycnopterus","Sagamichthys schnakenbecki","Searsia koefoedi","Alepocephalus bairdii","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Chauliodus sloani","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Lampanyctus crocodilus","Nannobrachium atrum","Lampanyctus macdonaldi","Notoscopelus kroyeri","Evermannella balbo","Lestidiops sphyrenoides","Arctozenus risso","Melanostigma atlanticum","Malacosteus niger","Maulisia argipalla","Normichthys operosus","Serrivomer beanii","Nannobrachium lineatum","Taaningichthys paurolychnus","Aphyonidae","Parabrotulidae","Photostylus pycnopterus","Gonostoma elongatum","Sagamichthys schnakenbecki","Searsia koefoedi","Neonesthes capensis"],[13.14,0.5600000000000001,0.07000000000000001,4.98,1.48,0.13,0.09,5.41,31.73,3.61,2.5,4.86,5.62,1.27,2.69,4.02,2.26,3.47,0.12,4.92,2.95,0.41,13.86,1.48,18.78,0.29,1.62,5.73,0.2,0.23,1.91,0.41,0.03,0.06,3.07,0.32,0.43,1.33,15.77,0.84,0.32,0.29,0.17,0.26,6.36,2.55,0.01,0.66,0.37,0.6,3.37,1.98,0.36,0.34,14.62,0.77,5.39,0.24,1.68,0.01,0.43,2.04,0.92,1.24,0.39,0.39,38.54,0.21,6.59,1.33,0.07000000000000001,0.42,30.06,8.199999999999999,7.43,3.92,0.98,2.24,20.61,0.02,1.42,1.77,0.04,0.22,0.02,6.43,0.36,3.01,0.11,1.62,0.02,0.15,0.51,6.7,3.34,1,5.72,0.77,1.46,0.95,0.04,0.04,15.14,1.71,0.4,0.09,4.17,0.28,0.1,1.24,0.25,1.56,2.12,0.64,3.07,0.01,0.01,0.03,4.4,0.2,5.99,1.9,0.63,2.63,0.96,0.61,8.09,56.18,0.77,1.16,3.41,0.33,0.11,0.8100000000000001,3.48,2.62,0.41,0.07000000000000001,4.72,0.5,0.18,0.02,0.02,0.02,1.47,0.02,1.51,1.2],[211,24,7,5,43,14,4,132,200,41,4,21,582,11,532,3,2,5,1,43,102,3,50,5,31,1,10,24,2,2,8,2,2,2,9,3,1,2,12,1,3,1,1,5,109,1091,1,13,2547,1,5,62,157,7,209,10,232,8,23,14,1,10,1,11,17,1,3360,1,26,58,1,76,73,198,198,16,1,3,211,2,24,43,14,4,2,132,2,200,1,41,2,2,1,109,1091,13,2547,5,62,157,7,1,209,9,1,10,232,2,8,23,14,1,16,4,10,1,1,11,17,1,36,262,4,3,1,11,617,84,4,3,28,2,2,6,81,3,3,1,4,1,3,1,3,1,1,1,5,1],["upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","bathypelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","epipelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","upper-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","lower-mesopelagic","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity","bottom-proximity"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Nom_Scientifique<\/th>\n      <th>pourcentage_biomass_sp<\/th>\n      <th>nb_ind<\/th>\n      <th>depth_layer<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Nom_Scientifique","targets":1},{"name":"pourcentage_biomass_sp","targets":2},{"name":"nb_ind","targets":3},{"name":"depth_layer","targets":4}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


## Calculation

-   Formatting of data and calculation of relative biomass within each station


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
  arrange(Species_code)

# species_status_biomass ----
# with all species sampled (not only species sampled for isotope)

# load total trawling data evhoe 2021
trawling_data_evhoe21 <-  utils::read.csv(here::here("data", "trawling_data_evhoe_2021.csv"), sep = ";", header = T, dec = ".")

species_status_biomass <- trawling_data_evhoe21 %>%
  # selection of mesopelagic trawl
  filter(Code_Station %in% c("Z0524", "Z0518", "Z0512", "Z0508",
                             "Z0503", "Z0497", "Z0492")) %>%
  #deletion when only genus name and genus already sampled in isotope + A. carbo
  filter(
    !Nom_Scientifique %in% c(
      "Cyclothone braueri",
      "Cyclothone microdon",
      "Myctophidae",
      "Aphanopus carbo",
      "Lampanyctus"
    )
  ) %>%
  # group by depth layer 
  mutate(
    Status = case_when(
      Code_Station %in% c("Z0508") ~ "epipelagic",
      Code_Station %in% c("Z0492", "Z0512") ~ "upper-mesopelagic",
      Code_Station %in% c("Z0503", "Z0518") ~ "lower-mesopelagic",
      Code_Station %in% c("Z0497") ~ "bathypelagic",
      Code_Station %in% c("Z0524") ~ "bottom-proximity"
    )
  ) %>%
  select(Status,
         Tot_V_HV,
         Nom_Scientifique,
         Code_Espece_Campagne) %>%
  distinct() %>%
  # sum species biomass by depth layer
  group_by(Nom_Scientifique, Status) %>%
  mutate(biomass_sp = sum(Tot_V_HV)) %>%
  select(-Tot_V_HV) %>%
  distinct() %>%
  # sum of total biomass by depth layer
  group_by(Status) %>%
  mutate(biomass_tot = sum(biomass_sp)) %>%
  # relative biomass of each species by station
  mutate(rel_biomass = biomass_sp / biomass_tot * 100) %>%
  select(-c(biomass_sp, biomass_tot)) %>%
  # selection of species sampled for isotopy in each depth
  filter(
    Status == "epipelagic" &
      Nom_Scientifique %in% c(
        "Lestidiops sphyrenoides",
        "Maurolicus muelleri",
        "Myctophum punctatum",
        "Notoscopelus kroyeri",
        "Notoscopelus bolini"
      ) |
      Status == "upper-mesopelagic" &
      Nom_Scientifique %in% c(
        "Arctozenus risso",
        "Argyropelecus olfersii",
        "Lampanyctus crocodilus",
        "Myctophum punctatum",
        "Notoscopelus kroyeri",
        "Xenodermichthys copei"
      ) |
      Status == "lower-mesopelagic" &
      Nom_Scientifique %in% c(
        "Arctozenus risso",
        "Argyropelecus olfersii",
        "Cyclothone",
        "Benthosema glaciale",
        "Lampanyctus crocodilus",
        "Maulisia argipalla",
        "Searsia koefoedi",
        "Serrivomer beanii",
        "Xenodermichthys copei"
      ) |
      Status == "bathypelagic" &
      Nom_Scientifique %in% c(
        "Argyropelecus olfersii",
        "Lampanyctus crocodilus",
        "Lampanyctus macdonaldi",
        "Myctophum punctatum",
        "Serrivomer beanii",
        "Xenodermichthys copei"
      ) |
      Status == "bottom-proximity" &
      Nom_Scientifique %in% c(
        "Argyropelecus olfersii",
        "Lampanyctus crocodilus",
        "Melanostigma atlanticum",
        "Serrivomer beanii",
        "Xenodermichthys copei"
      )
  ) %>%
  mutate(Species_code = tolower(Code_Espece_Campagne)) %>%
  rename(Species_name = Nom_Scientifique) %>%
  select(-Code_Espece_Campagne) %>%
  relocate(Status, .after = rel_biomass) %>%
  relocate(Species_code, .after = Species_name) %>%
  arrange(Species_name) %>%
  distinct()

htmltools::tagList(DT::datatable(species_status_biomass))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-e15eb63548883c55be3d" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-e15eb63548883c55be3d">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"],["Arctozenus risso","Arctozenus risso","Argyropelecus olfersii","Argyropelecus olfersii","Argyropelecus olfersii","Argyropelecus olfersii","Benthosema glaciale","Cyclothone","Lampanyctus crocodilus","Lampanyctus crocodilus","Lampanyctus crocodilus","Lampanyctus crocodilus","Lampanyctus macdonaldi","Lestidiops sphyrenoides","Maulisia argipalla","Maurolicus muelleri","Melanostigma atlanticum","Myctophum punctatum","Myctophum punctatum","Myctophum punctatum","Notoscopelus bolini","Notoscopelus kroyeri","Notoscopelus kroyeri","Searsia koefoedi","Serrivomer beanii","Serrivomer beanii","Serrivomer beanii","Xenodermichthys copei","Xenodermichthys copei","Xenodermichthys copei","Xenodermichthys copei"],["noto-ris","noto-ris","argy-olf","argy-olf","argy-olf","argy-olf","bent-gla","cycl-otz","lamp-cro","lamp-cro","lamp-cro","lamp-cro","lamp-mac","lest-sph","maul-arg","maur-mue","mela-atl","myct-pun","myct-pun","myct-pun","noto-bol","noto-kro","noto-kro","sear-koe","serr-bea","serr-bea","serr-bea","xeno-cop","xeno-cop","xeno-cop","xeno-cop"],[5.231498359460446,2.920143027413588,1.986875683558148,1.273516642547033,1.653754469606675,0.626266347393627,1.311084624553039,5.884982121573302,11.84834123222749,13.86396526772793,29.75268176400477,56.17977528089888,18.78437047756874,3.940886699507389,2.115613825983313,38.70513722730472,3.481304107570456,3.244622675902296,4.92040520984081,6.61505981703026,30.19000703729768,34.74298213634707,15.69317382125264,4.782479141835519,15.77424023154848,5.110250297973778,4.715417203904956,33.75865840320817,4.862518089725036,13.06615017878427,5.986369497144963],["upper-mesopelagic","lower-mesopelagic","upper-mesopelagic","bathypelagic","lower-mesopelagic","bottom-proximity","lower-mesopelagic","lower-mesopelagic","upper-mesopelagic","bathypelagic","lower-mesopelagic","bottom-proximity","bathypelagic","epipelagic","lower-mesopelagic","epipelagic","bottom-proximity","upper-mesopelagic","bathypelagic","epipelagic","epipelagic","upper-mesopelagic","epipelagic","lower-mesopelagic","bathypelagic","lower-mesopelagic","bottom-proximity","upper-mesopelagic","bathypelagic","lower-mesopelagic","bottom-proximity"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Species_name<\/th>\n      <th>Species_code<\/th>\n      <th>rel_biomass<\/th>\n      <th>Status<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":3},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Species_name","targets":1},{"name":"Species_code","targets":2},{"name":"rel_biomass","targets":3},{"name":"Status","targets":4}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


- percentage of biomass sampled for isotopy in each depth layer 

::: {.cell}

```{.r .cell-code  code-fold="true"}
biomass_sampled <- species_status_biomass%>%
  group_by(Status)%>%
  mutate(sum_biomass = sum(rel_biomass))%>%
  select(sum_biomass, Status)%>%
  distinct()

htmltools::tagList(DT::datatable(biomass_sampled))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-5409fa19bf8bbfab1f38" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-5409fa19bf8bbfab1f38">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5"],[90.81297849070361,66.59713945172825,59.47901591895803,70.98913243691288,95.14426460239268],["upper-mesopelagic","lower-mesopelagic","bathypelagic","bottom-proximity","epipelagic"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sum_biomass<\/th>\n      <th>Status<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":1},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"sum_biomass","targets":1},{"name":"Status","targets":2}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


### Epipelagic 
_ 25m (Z0508)


::: {.cell}

```{.r .cell-code  code-fold="true"}
# 25m Z0508 ----
individuals_si_epi <- individuals_si%>%
  filter(station =="Z0508")%>%
  select(-station)

status_biomass_epi <- species_status_biomass%>%
  filter(Status=="epipelagic")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_epi<-data.frame(group=individuals_si_epi[,"Species_code"], individuals_si_epi)
mean_si_species_epi<-meanSI_group(individuals_si_epi)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_epi[,"sd_d13C"]/mean_si_species_epi[,"d13C"], CV_d15N=mean_si_species_epi[,"sd_d15N"]/mean_si_species_epi[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
               CV_d13C    CV_d15N
lesti-sph -0.010766438 0.03266115
maur-mue  -0.005224151 0.05249590
myct-pun  -0.009814716 0.04047466
noto-bol  -0.009178283 0.02755544
noto-kro  -0.012155251 0.02163464
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
row.names(mean_si_species_epi)==status_biomass_epi[,"Species_code"] # OK
```

::: {.cell-output .cell-output-stdout}
```
     Species_code
[1,]        FALSE
[2,]         TRUE
[3,]         TRUE
[4,]         TRUE
[5,]         TRUE
```
:::

```{.r .cell-code  code-fold="true"}
# building a single dataframe with all data for computing isotopic diversity indices
data_fish_epi <-data.frame(mean_si_species_epi[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_epi[,"rel_biomass"], Status=status_biomass_epi[,"Status"], latin_name=status_biomass_epi[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_epi<-scaleSI_range01(data_fish_epi)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_epi<-IDiversity(cons=data_fish_scl_epi, weight=data_fish_scl_epi[,c("rel_biomass")], nm_plot="1_epipelagic")

# printing results
result <- as.data.frame(round(ID_scl_ab_epi,3)) 
```
:::


![epipelagic_d13C_d15N](1_epipelagic_d13C_d15N.jpeg){width="550"}

### Upper mesopelagic
-  370m (Z0492) & 555m (Z0512)

::: {.cell}

```{.r .cell-code  code-fold="true"}
individuals_si_upm <- individuals_si%>%
  filter(station%in%c("Z0492", "Z0512"))%>%
  select(-station)

status_biomass_upm <- species_status_biomass%>%
  filter(Status=="upper-mesopelagic")

# computing mean Stable Isotope values for each species
individuals_si_upm<-data.frame(group=individuals_si_upm[,"Species_code"], individuals_si_upm)
mean_si_species_upm<-meanSI_group(individuals_si_upm)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_upm[,"sd_d13C"]/mean_si_species_upm[,"d13C"], CV_d15N=mean_si_species_upm[,"sd_d15N"]/mean_si_species_upm[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
arct-ris -0.010421638 0.03438256
argy-olf -0.008706125 0.04283576
lamp-cro -0.015534945 0.07982239
myct-pun -0.019927081 0.04074265
noto-kro -0.013115883 0.02239182
xeno-cop -0.012364479 0.07722547
```
:::

```{.r .cell-code  code-fold="true"}
# building a single dataframe with all data for computing isotopic diversity indices
data_fish_upm <-data.frame(mean_si_species_upm[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_upm[,"rel_biomass"], Status=status_biomass_upm[,"Status"], latin_name=status_biomass_upm[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_upm<-scaleSI_range01(data_fish_upm)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_upm<-IDiversity(cons=data_fish_scl_upm, weight=data_fish_scl_upm[,c("rel_biomass")], nm_plot="2_upper_meso")

# printing results
result <- as.data.frame(round(ID_scl_ab_upm,3)) 
```
:::


![370m](2_upper_meso_d13C_d15N.jpeg){width="550"}

### Lower mesopelagic
- 715m (Z0503)
- 1000m (Z0518)


::: {.cell}

```{.r .cell-code  code-fold="true"}
individuals_si_lwm <- individuals_si%>%
  filter(station%in%c("Z0503", "Z0518"))%>%
  select(-station)

status_biomass_lwm<- species_status_biomass%>%
  filter(Status=="lower-mesopelagic")

# computing mean Stable Isotope values for each species
individuals_si_lwm<-data.frame(group=individuals_si_lwm[,"Species_code"], individuals_si_lwm)
mean_si_species_lwm<-meanSI_group(individuals_si_lwm)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_lwm[,"sd_d13C"]/mean_si_species_lwm[,"d13C"], CV_d15N=mean_si_species_lwm[,"sd_d15N"]/mean_si_species_lwm[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
arct-ris -0.013383792 0.02784671
argy-olf -0.010245107 0.03092443
bent-gla -0.015512985 0.06444602
cycl-otz -0.009264821 0.04946692
lamp-cro -0.020344506 0.04819340
maul-arg -0.009604780 0.03191368
sear-koe -0.022852986 0.05388211
serr-bea -0.012134196 0.06183851
xeno-cop -0.012017717 0.06196185
```
:::

```{.r .cell-code  code-fold="true"}
# building a single dataframe with all data for computing isotopic diversity indices
data_fish_lwm <-data.frame(mean_si_species_lwm[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_lwm[,"rel_biomass"], Status=status_biomass_lwm[,"Status"], latin_name=status_biomass_lwm[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_lwm<-scaleSI_range01(data_fish_lwm)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_lwm<-IDiversity(cons=data_fish_scl_lwm, weight=data_fish_scl_lwm[,c("rel_biomass")], nm_plot="3_lower-mesopelagic")

# printing results
result <- as.data.frame(round(ID_scl_ab_lwm,3)) 
```
:::


![715m](3_lower-mesopelagic_d13C_d15N.jpeg){width="550"}

### Bathypelagic
- 1335m (Z0497)


::: {.cell}

```{.r .cell-code  code-fold="true"}
individuals_si_bathy <- individuals_si%>%
  filter(station=="Z0497")%>%
  select(-station)

status_biomass_bathy <- species_status_biomass%>%
  filter(Status=="bathypelagic")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_bathy<-data.frame(group=individuals_si_bathy[,"Species_code"], individuals_si_bathy)
mean_si_species_bathy<-meanSI_group(individuals_si_bathy)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_bathy[,"sd_d13C"]/mean_si_species_bathy[,"d13C"], CV_d15N=mean_si_species_bathy[,"sd_d15N"]/mean_si_species_bathy[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
argy-olf -0.009889227 0.01043410
lamp-cro -0.018677334 0.05649516
lamp-mac -0.022058396 0.02734756
myct-pun -0.024242014 0.04136229
serr-bea -0.014132768 0.05883833
xeno-cop -0.012585074 0.05902294
```
:::

```{.r .cell-code  code-fold="true"}
# building a single dataframe with all data for computing isotopic diversity indices
data_fish_bathy <-data.frame(mean_si_species_bathy[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_bathy[,"rel_biomass"], Status=status_biomass_bathy[,"Status"], latin_name=status_biomass_bathy[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_bathy<-scaleSI_range01(data_fish_bathy)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_bathy<-IDiversity(cons=data_fish_scl_bathy, weight=data_fish_scl_bathy[,c("rel_biomass")], nm_plot="4_bathypelagic")

# printing results
result <- as.data.frame(round(ID_scl_ab_bathy,3)) 
```
:::


![bathypelagic](4_bathypelagic_d13C_d15N.jpeg){width="550"}

### Near bottom
- Z0524


::: {.cell}

```{.r .cell-code  code-fold="true"}
# 1010 Z0524 ----
individuals_si_nb <- individuals_si%>%
  filter(station=="Z0524")%>%
  select(-station)

status_biomass_nb <- species_status_biomass%>%
  filter(Status=="bottom-proximity")

# computing mean Stable Isotope values for each species
individuals_si_nb<-data.frame(group=individuals_si_nb[,"Species_code"], individuals_si_nb)
mean_si_species_nb<-meanSI_group(individuals_si_nb)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_nb[,"sd_d13C"]/mean_si_species_nb[,"d13C"], CV_d15N=mean_si_species_nb[,"sd_d15N"]/mean_si_species_nb[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
argy-olf -0.016533711 0.03529939
lamp-cro -0.015886548 0.04471856
mela-atl -0.009854421 0.04078758
serr-bea -0.012700557 0.05654679
xeno-cop -0.009943149 0.04105076
```
:::

```{.r .cell-code  code-fold="true"}
# building a single dataframe with all data for computing isotopic diversity indices
data_fish_nb <-data.frame(mean_si_species_nb[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_nb[,"rel_biomass"], Status=status_biomass_nb[,"Status"], latin_name=status_biomass_nb[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_nb<-scaleSI_range01(data_fish_nb)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_nb<-IDiversity(cons=data_fish_scl_nb, weight=data_fish_scl_nb[,c("rel_biomass")], nm_plot="5_near_bottom")

# printing results
result <- as.data.frame(round(ID_scl_ab_nb,3)) 
```
:::


![bathypelagic](5_near_bottom_d13C_d15N.jpeg){width="550"}

## PCA


::: {.cell}

```{.r .cell-code  code-fold="true"}
# crate a data frame with all index value
trophic_indices <- data.frame(
  group = c(
    "Epipelagic",
    "Upper-mesopelagic",
    "Lower-Mesopelagic",
    "Bathypelagic",
    "Bottom proximity"
  ),
  IDiv = c(0.953, 0.862, 0.536, 0.932, 0.994),
  IDis = c(0.898, 0.823 , 0.491, 0.755, 0.318),
  IEve = c(0.448, 0.870 , 0.619, 0.633, 0.543),
  IUni = c(0.564, 0.800, 0.576, 0.713, 0.758)
)%>%
  tibble::column_to_rownames(var="group")

res.pca <- FactoMineR::PCA(trophic_indices, graph = FALSE)

factoextra::fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#00778E", 
                col.ind = "gray50", 
                arrowsize = 1,
                title = ""
)
```

::: {.cell-output-display}
![](index_files/figure-html/trophic_indices_df-1.png){width=672}
:::

```{.r .cell-code  code-fold="true"}
ggsave("PCA.png", path = "figures", dpi = 700)
```
:::

::: {.cell}

```{.r .cell-code}
htmltools::tagList(DT::datatable(trophic_indices))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-f61436f79c2f792accee" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-f61436f79c2f792accee">{"x":{"filter":"none","vertical":false,"data":[["Epipelagic","Upper-mesopelagic","Lower-Mesopelagic","Bathypelagic","Bottom proximity"],[0.953,0.862,0.536,0.9320000000000001,0.994],[0.898,0.823,0.491,0.755,0.318],[0.448,0.87,0.619,0.633,0.543],[0.5639999999999999,0.8,0.576,0.713,0.758]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>IDiv<\/th>\n      <th>IDis<\/th>\n      <th>IEve<\/th>\n      <th>IUni<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"IDiv","targets":1},{"name":"IDis","targets":2},{"name":"IEve","targets":3},{"name":"IUni","targets":4}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::
