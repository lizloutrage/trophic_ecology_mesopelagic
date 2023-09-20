---
title: "index"
author: "Liz Loutrage"
format:
  html:
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

:::

::: {.cell}
::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-33f3c13fd179d9af4042" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-33f3c13fd179d9af4042">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"],["Arctozenus_risso","Argyropelecus_olfersii","Benthosema_glaciale","Cyclothone","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Maulisia_argipalla","Maurolicus_muelleri","Meganyctiphanes_norvegica","Melanostigma_atlanticum","Myctophum_punctatum","Notoscopelus_bolini","Notoscopelus_kroyeri","Searsia_koefoedi","Serrivomer_beanii","Xenodermichthys_copei"],[-20.01,-19.77,-19.46,-19.6,-19.65,-19.67,-20.04,-19.46,-20.53,-20.57,-19.69,-19.99,-19.83,-19.73,-19.61,-19.99,-20.32],[0.23,0.22,0.3,0.18,0.43,0.43,0.26,0.19,0.11,0.23,0.19,0.39,0.18,0.27,0.45,0.26,0.24],[10.52,10.18,9.91,10.98,10.42,11.52,10.72,12.01,9.869999999999999,8.5,11.28,9.92,11.13,11.17,11.85,9.470000000000001,9.800000000000001],[0.35,0.43,0.64,0.54,0.67,0.32,0.35,0.38,0.52,0.33,0.46,0.42,0.31,0.25,0.64,0.55,0.67],[43,41,20,20,120,20,12,14,20,35,20,57,20,60,14,26,97],[1,1,3,2,1,1,1,1,4,5,1,1,1,1,1,1,1],["Paralepididae","Sternoptychidae","Myctophidae","Gonostomatidae","Myctophidae","Myctophidae","Lestidiidae","Platytroctidae","Sternoptychidae","Euphausiidae","Zoarcidae","Myctophidae","Myctophidae","Myctophidae","Platytroctidae","Serrivomeridae","Alepocephalidae"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>mean_d13c<\/th>\n      <th>sd_d13c<\/th>\n      <th>mean_d15n<\/th>\n      <th>sd_d15n<\/th>\n      <th>n<\/th>\n      <th>number_ind_pool<\/th>\n      <th>family<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


# 2. Isotopic niches 

## Ellipses
-  ellipses at 40%
-  Δ  $\delta$<sup>13</sup>C = 2.36‰
-  Δ  $\delta$<sup>15</sup>N = 5.94‰

::: {.cell}

```{.r .cell-code  code-fold="true"}
niche_plot_community <- isotope_data%>%
  mutate(species= gsub("_"," ", species))%>%
    mutate(species=recode(species, "Cyclothone"="Cyclothone spp."))

# colors selection 
nichecol_krill <- c("#E4A33A", "#F67451", "#D664BE", "#3DA5D9", "#94b3ae",
                    "#18206F", "#FD151B", "#049A8F", "#072AC8", "black", "purple", 
                    "#d193f7", "#d8c2ab", "#678FCB", "#A63A49", "#00547A", "#6B54A0")

# size of the ellipse 
p.ell<-0.40
 
# plot
ggplot(data = niche_plot_community, 
                         aes(x = d13c, 
                             y = d15n)) + 
  geom_point(aes(color = species, shape= taxon), size = 2) +
  scale_color_manual(values=nichecol_krill)+  
  scale_fill_manual(values=nichecol_krill)+
  scale_shape_manual(values= c(19, 3))+
  scale_x_continuous(expression({delta}^13*C~'\u2030'), limits = c(-21, -18.5)) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'), limits = c(7, 14))+
  stat_ellipse(aes(group = species, fill = species, color = species), 
               alpha = 0.2, level = p.ell,linewidth = 0.7, type = "norm", geom = "polygon")+
  theme_light()+
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15))+
  labs(shape="Taxon", col= "Species", fill="Species")
```

::: {.cell-output-display}
![](index_files/figure-html/community_ellipses-1.png){width=960}
:::

```{.r .cell-code  code-fold="true"}
#ggsave("niches_community.png", path = "figures", dpi = 700, height = 10, width = 12)
```
:::


## Niche areas

::: {.cell layout-align="center"}
::: {.cell-output .cell-output-stdout}
```
Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 14
   Unobserved stochastic nodes: 3
   Total graph size: 29

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 120
   Unobserved stochastic nodes: 3
   Total graph size: 135

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 57
   Unobserved stochastic nodes: 3
   Total graph size: 72

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
   Observed stochastic nodes: 97
   Unobserved stochastic nodes: 3
   Total graph size: 112

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
   Observed stochastic nodes: 26
   Unobserved stochastic nodes: 3
   Total graph size: 41

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
   Observed stochastic nodes: 41
   Unobserved stochastic nodes: 3
   Total graph size: 56

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
   Observed stochastic nodes: 43
   Unobserved stochastic nodes: 3
   Total graph size: 58

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 14
   Unobserved stochastic nodes: 3
   Total graph size: 29

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 60
   Unobserved stochastic nodes: 3
   Total graph size: 75

Initializing model

Compiling model graph
   Resolving undeclared variables
   Allocating nodes
Graph information:
   Observed stochastic nodes: 12
   Unobserved stochastic nodes: 3
   Total graph size: 27

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
![](index_files/figure-html/niche_area_community-1.png){fig-align='center' width=1152}
:::
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
  #renames rownames
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
  theme(axis.text = element_text(face="italic", size = 10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        plot.background = element_rect(colour = "white"))
```

::: {.cell-output-display}
![](index_files/figure-html/community_overlaps-1.png){width=864}
:::

```{.r .cell-code  code-fold="true"}
#ggsave("matrix_overlap.png", path = "figures", dpi = 700, width = 8, height = 6)
```
:::



# 3. Depth segregation
## Cluster 
- input data for cluster = overlap matrix 
- depth distribution = from complete 2021 trawling data (not only individuals sampled for isotope)

::: {.cell layout-align="center"}

```{.r .cell-code  code-fold="true"}
# calculate median depth by species with trawling data set 2021
mean_depth_sp <- density_distribution%>%
  group_by(Nom_Scientifique)%>%
  mutate(median_depth=median(trawling_depth))%>%
  select(Nom_Scientifique, median_depth)%>%
  distinct()%>%
  ungroup()%>%
  arrange(Nom_Scientifique)%>%
  rename(species=Nom_Scientifique)

# calcul overlap ellipse 40%
test.elp_community <- rKIN::estEllipse(data= isotope_data_fish,  x="d13c", y="d15n", group="species", levels=40, smallSamp = TRUE)
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
elp.olp_community <- rKIN::calcOverlap(test.elp_community)

df <- elp.olp_community%>%
  tibble::column_to_rownames(var="OverlapID")%>%
  as.data.frame()

# Gap statistic
factoextra::fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50, verbose = FALSE)+
  labs(subtitle = "Gap statistic method")
```

::: {.cell-output-display}
![](index_files/figure-html/cluster-1.png){fig-align='center' width=672}
:::

```{.r .cell-code  code-fold="true"}
res.km <- kmeans(scale(df),5, nstart = 25)

# Réduction de dimension en utilisant l'ACP
res.pca <- prcomp(df[, -5],  scale = TRUE)
# Coordonnées des individus
ind.coord <- as.data.frame(factoextra::get_pca_ind(res.pca)$coord)
# Ajouter les clusters obtenus à l'aide de l'algorithme k-means
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
# Cluster plot ----
cluster_plot <- ggpubr::ggscatter(
  ind.coord_depth, x = "Dim.1", y = "Dim.2",
  color = "cluster", shape = "median_depth", 
  ellipse = TRUE, ellipse.type = "convex",
  palette = c("#007A75","#150578", "#B80050", "#845EC2", "#E77823"),
  size = 2.5, ggtheme = theme_minimal(),
  add.params = list(size=0.1),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )) +
  scale_shape_manual(values = c(15, 8, 16, 2, 17, 18, 4))+
  labs(shape="Median depth (m)")+
  theme(plot.background = element_rect(color = "white"),
        legend.text = element_text(size=10.5),
        legend.title = element_text(size=10.5),
        axis.text = element_text(size=10.5),
        axis.title =element_text(size=10.5))+
  guides(col="none", fill="none")+
  ggrepel::geom_text_repel(label= ind.coord_depth$species, aes(col=cluster), size=4, box.padding = 0.5) +
  scale_color_manual(values = c("#007A75","#150578", "#B80050", "#845EC2", "#E77823"))

# Density plot----
# assigne each species to a cluster 
density_distribution_cluster <- density_distribution%>%
  mutate(cluster=case_when(Nom_Scientifique%in% c("Argyropelecus olfersii", "Lampanyctus crocodilus",
                                                  "Benthosema glaciale","Myctophum punctatum","Serrivomer beanii")~4,
                           Nom_Scientifique%in% c("Lampanyctus macdonaldi", "Maulisia argipalla",
                                                  "Searsia koefoedi")~5,
                           Nom_Scientifique%in% c("Cyclothone spp.","Notoscopelus bolini", "Notoscopelus kroyeri",
                                                  "Melanostigma atlanticum")~3,
                           Nom_Scientifique%in% c("Xenodermichthys copei", "Maurolicus muelleri")~1,
                           Nom_Scientifique%in% c("Arctozenus risso","Lestidiops sphyrenoides")~2))%>%
  group_by(Nom_Scientifique)%>%
  arrange(desc(trawling_depth))

# Order in function of median depth
density_distribution_cluster$Nom_Scientifique = with(density_distribution_cluster, reorder(Nom_Scientifique, cluster, min))  

density_plot <-ggplot(density_distribution_cluster,
                      aes(x = trawling_depth, y = Nom_Scientifique, group = Nom_Scientifique, 
                          col=factor(cluster), fill=factor(cluster)))+ 
  scale_fill_manual(values = c("#150578", "#E77823", "#007A75","#845EC2", "#B80050"))+
  scale_color_manual(values = c("#150578", "#E77823", "#007A75","#845EC2", "#B80050"))+
  ggridges::stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5 , alpha=0.4, size=0.4)+
  theme_classic()+
  theme(axis.text.y = element_text(size=7))+
  scale_y_discrete(position = "right")+
  scale_x_reverse()+
  coord_flip()+
  ylab(label = "")+ xlab("Depth (m)")+
  theme(axis.text.y = element_text(size=10.5),
        axis.text.x = element_text(face="italic", size=10.5, angle=80, vjust = 0.5, hjust=0),
        axis.title.x = element_text(size=10.5),
        axis.title.y = element_text(size=10.5))+
  guides(fill="none", col="none", alpha="none")

ggpubr::ggarrange(cluster_plot, density_plot, ncol=1, labels = c("A", "B"), heights = c(1, 1.3))
```

::: {.cell-output-display}
![](index_files/figure-html/density_cluster_plot-1.png){width=960}
:::

```{.r .cell-code  code-fold="true"}
#ggsave("cluster_density.png", path = "figures", dpi = 700, height = 8, width = 10)
```
:::


# 4. Isotopic diversity index 
## Definitions

- __Isotopic divergence__ : tends to 1 when all points (or their weights) are located at the edge of the convex hull, i.e. when the oragnisms with the most extreme isotopic values dominate the food web

- __Isotopic dispersion__: equal to 1 when most points (or their weights) are far from the center of gravity of the point group, i.e. when organisms tend to have contrasting isotopic values 

- __Isotopic evenness__: tends to 1 when all organisms are equally distributed in isotopic space

- __Isotopic uniqueness__: tends to 1 when most organisms (or their weights) are isolated in isotopic space, i.e. when most organisms (or those with the highest abundance) are isolated in isotopic space, their isotopic values are very different from the rest of the organisms 

## Representativeness of sampling 
- Percentage of species biomass in each station 

::: {.cell}

```{.r .cell-code  code-fold="true"}
# Species % biomass and abundance by station 
species_abundance <- trawling_data_evhoe21%>%
  # selection of mesopelagic trawl 
  filter(Code_Station%in% c("Z0524", "Z0518", "Z0512", "Z0508", 
                            "Z0503", "Z0497", "Z0492"))%>%
  #deletion when only genus name and genus already sampled in isotopy + A. carbo
  filter(!Nom_Scientifique%in%c("Cyclothone braueri","Cyclothone microdon", 
                                "Myctophidae", "Aphanopus carbo"))%>%
  select(Code_Station, Nbr, Nom_Scientifique, Tot_V_HV)%>%
  group_by(Nom_Scientifique, Code_Station)%>%
  mutate(nb_ind= sum(Nbr))%>%
  select(-Nbr)%>%
  distinct()%>%
  group_by(Code_Station)%>%
  mutate(sum_biomass_station=sum(Tot_V_HV))%>%
  ungroup()%>%
  group_by(Code_Station, Nom_Scientifique)%>%
  mutate(pourcentage_biomass_sp= Tot_V_HV/sum_biomass_station*100)%>%
  select(Nom_Scientifique, pourcentage_biomass_sp, nb_ind, Code_Station)%>%
  mutate(across(where(is.numeric), round, 2))
  
htmltools::tagList(DT::datatable(species_abundance))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-840dc0da72908cb846a5" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-840dc0da72908cb846a5">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148"],["Xenodermichthys copei","Argyropelecus olfersii","Maurolicus muelleri","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus crocodilus","Notoscopelus kroyeri","Arctozenus risso","Melanostomias bartonbeani","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Maurolicus muelleri","Borostomias antarcticus","Chauliodus sloani","Stomias boa boa","Nansenia oblita","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus crocodilus","Nannobrachium atrum","Lampanyctus macdonaldi","Lobianchia gemellarii","Notoscopelus bolini","Notoscopelus kroyeri","Paralepis coregonoides","Lestidiops sphyrenoides","Arctozenus risso","Derichthys serpentinus","Synaphobranchus kaupii","Melanostigma atlanticum","Bathylagus euryops","Sigmops bathyphilus","Malacosteus niger","Melanostomias bartonbeani","Serrivomer beanii","Bolinichthys supralateralis","Photostylus pycnopterus","Eurypharynx pelecanoides","Searsia koefoedi","Mentodus rostratus","Xenodermichthys copei","Cyclothone","Argyropelecus hemigymnus","Argyropelecus olfersii","Maurolicus muelleri","Chauliodus sloani","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus crocodilus","Notoscopelus bolini","Notoscopelus kroyeri","Lestidiops sphyrenoides","Arctozenus risso","Melanostigma atlanticum","Melanostomias bartonbeani","Serrivomer beanii","Sudis hyalina","Sagamichthys schnakenbecki","Searsia koefoedi","Pseudoscopelus altipinnis","Maurolicus muelleri","Chauliodus sloani","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus","Notoscopelus bolini","Notoscopelus kroyeri","Notoscopelus kroyeri","Lestidiops sphyrenoides","Melanostomias bartonbeani","Dolicholagus longirostris","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Lampanyctus","Lampanyctus crocodilus","Nannobrachium atrum","Notoscopelus kroyeri","Evermannella balbo","Arctozenus risso","Melanostigma atlanticum","Nansenia","Dolicholagus longirostris","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Maurolicus muelleri","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Ceratoscopelus maderensis","Diaphus holti","Lampanyctus crocodilus","Nannobrachium atrum","Lampanyctus macdonaldi","Notoscopelus bolini","Notoscopelus kroyeri","Evermannella balbo","Lestidiops sphyrenoides","Arctozenus risso","Melanostigma atlanticum","Malacosteus niger","Maulisia argipalla","Normichthys operosus","Serrivomer beanii","Parabrotulidae","Photostylus pycnopterus","Sagamichthys schnakenbecki","Searsia koefoedi","Alepocephalus bairdii","Xenodermichthys copei","Cyclothone","Argyropelecus olfersii","Chauliodus sloani","Stomias boa boa","Myctophum punctatum","Benthosema glaciale","Lampanyctus crocodilus","Nannobrachium atrum","Lampanyctus macdonaldi","Notoscopelus kroyeri","Evermannella balbo","Lestidiops sphyrenoides","Arctozenus risso","Melanostigma atlanticum","Malacosteus niger","Maulisia argipalla","Normichthys operosus","Serrivomer beanii","Nannobrachium lineatum","Taaningichthys paurolychnus","Aphyonidae","Parabrotulidae","Photostylus pycnopterus","Gonostoma elongatum","Sagamichthys schnakenbecki","Searsia koefoedi","Neonesthes capensis"],[20.63,0.89,0.11,7.81,2.32,0.2,0.14,8.5,49.81,5.67,3.92,4.86,5.62,1.27,2.69,4.02,2.26,3.47,0.12,4.92,2.95,0.41,13.86,1.48,18.78,0.29,1.62,5.73,0.2,0.23,1.91,0.41,0.03,0.06,3.07,0.32,0.43,1.33,15.77,0.84,0.32,0.29,0.17,0.26,14.22,5.7,0.03,1.47,0.83,1.33,7.53,4.43,0.8,0.77,32.68,1.73,12.06,0.53,3.76,0.03,0.97,4.56,2.07,2.76,0.87,0.87,38.54,0.21,6.59,1.33,0.07000000000000001,0.42,30.06,8.199999999999999,7.43,3.92,0.98,2.24,56.78,0.05,3.92,4.87,0.1,0.6,0.05,17.72,1,8.279999999999999,0.3,4.47,0.05,0.4,1.41,12.13,6.04,1.81,10.35,1.4,2.64,1.73,0.08,0.08,27.39,3.1,0.73,0.16,7.55,0.51,0.19,2.24,0.46,2.83,3.83,1.16,5.55,0.03,0.03,0.05,7.95,0.2,5.99,1.9,0.63,2.63,0.96,0.61,8.09,56.18,0.77,1.16,3.41,0.33,0.11,0.8100000000000001,3.48,2.62,0.41,0.07000000000000001,4.72,0.5,0.18,0.02,0.02,0.02,1.47,0.02,1.51,1.2],[105,11,7,5,17,11,1,63,170,26,4,21,582,11,532,3,2,5,1,43,102,3,50,5,31,1,10,24,2,2,8,2,2,2,9,3,1,2,12,1,3,1,1,5,57,451,1,5,147,1,4,37,61,6,89,9,73,6,14,2,1,4,1,10,2,1,3360,1,26,58,1,76,73,198,198,16,1,3,106,2,13,26,3,3,2,69,2,30,1,15,2,2,1,52,640,8,2400,1,25,96,1,1,120,9,1,1,159,2,2,9,12,1,16,4,6,1,1,1,15,1,36,262,4,3,1,11,617,84,4,3,28,2,2,6,81,3,3,1,4,1,3,1,3,1,1,1,5,1],["Z0492","Z0492","Z0492","Z0492","Z0492","Z0492","Z0492","Z0492","Z0492","Z0492","Z0492","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0497","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0503","Z0508","Z0508","Z0508","Z0508","Z0508","Z0508","Z0508","Z0508","Z0508","Z0508","Z0508","Z0508","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0512","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0518","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524","Z0524"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Nom_Scientifique<\/th>\n      <th>pourcentage_biomass_sp<\/th>\n      <th>nb_ind<\/th>\n      <th>Code_Station<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::



## Calculation
-  Formatting of data and calculation of relative biomass within each station


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

species_status_biomass <- trawling_data_evhoe21%>%
 # selection of mesopelagic trawl 
  filter(Code_Station%in% c("Z0524", "Z0518", "Z0512", "Z0508", 
                            "Z0503", "Z0497", "Z0492"))%>%
  #deletion when only genus name and genus already sampled in isotope + A. carbo
  filter(!Nom_Scientifique%in%c("Cyclothone braueri","Cyclothone microdon", "Myctophidae",
                                "Aphanopus carbo","Lampanyctus"))%>%
  select(Code_Station, Tot_V_HV, Nom_Scientifique, Code_Espece_Campagne)%>%
  distinct()%>%
  # sum species biomass by depth 
  group_by(Nom_Scientifique, Code_Station)%>%
  mutate(biomass_sp=sum(Tot_V_HV))%>%
  select(-Tot_V_HV)%>%
  distinct()%>%
  # sum of total biomass by station
  group_by(Code_Station)%>%
  mutate(biomass_tot=sum(biomass_sp))%>%
  # relative biomass of each species by station
  mutate(rel_biomass=biomass_sp/biomass_tot*100)%>%
  select(-c(biomass_sp, biomass_tot))%>%
  # selection of species sampled for isotopy in each depth 
  filter(Code_Station== "Z0492"& 
           Nom_Scientifique%in% c("Arctozenus risso", "Argyropelecus olfersii",
                                  "Lampanyctus crocodilus", "Myctophum punctatum",
                                  "Notoscopelus kroyeri", "Xenodermichthys copei")|
           Code_Station== "Z0497"& 
           Nom_Scientifique%in% c("Argyropelecus olfersii", "Lampanyctus crocodilus", 
                                  "Lampanyctus macdonaldi", "Myctophum punctatum",
                                  "Serrivomer beanii", "Xenodermichthys copei")|
           Code_Station== "Z0503"& 
           Nom_Scientifique%in% c("Arctozenus risso","Cyclothone","Lampanyctus crocodilus",
                                  "Serrivomer beanii", "Xenodermichthys copei")|
           Code_Station== "Z0508"& 
           Nom_Scientifique%in% c("Lestidiops sphyrenoides", "Maurolicus muelleri", 
                                   "Myctophum punctatum","Notoscopelus kroyeri",
                                  "Notoscopelus bolini")|
           Code_Station== "Z0512"& 
           Nom_Scientifique%in% c("Arctozenus risso","Argyropelecus olfersii",
                                  "Lampanyctus crocodilus","Notoscopelus kroyeri",
                                  "Xenodermichthys copei")|
           Code_Station== "Z0518"& 
           Nom_Scientifique%in% c("Argyropelecus olfersii","Lampanyctus crocodilus",
                                  "Benthosema glaciale", "Maulisia argipalla",
                                  "Searsia koefoedi", "Serrivomer beanii")|
           Code_Station== "Z0524"& 
           Nom_Scientifique%in% c("Argyropelecus olfersii","Lampanyctus crocodilus",
                                  "Melanostigma atlanticum",  "Serrivomer beanii",
                                  "Xenodermichthys copei"))%>%
  mutate(Species_code= tolower(Code_Espece_Campagne))%>%
  rename(Species_name= Nom_Scientifique,
         Status=Code_Station)%>%
  select(-Code_Espece_Campagne)%>%
  relocate(Status, .after=rel_biomass)%>%
  relocate(Species_code,.after = Species_name)%>%
  arrange(Species_name)
```
:::



### 25m (Z0508)

::: {.cell}

```{.r .cell-code  code-fold="true"}
# 25m Z0508 ----
individuals_si_25 <- individuals_si%>%
  filter(station =="Z0508")%>%
  select(-station)

status_biomass_25 <- species_status_biomass%>%
  filter(Status=="Z0508")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_25<-data.frame(group=individuals_si_25[,"Species_code"], individuals_si_25)
mean_si_species_25<-meanSI_group(individuals_si_25)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_25[,"sd_d13C"]/mean_si_species_25[,"d13C"], CV_d15N=mean_si_species_25[,"sd_d15N"]/mean_si_species_25[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
               CV_d13C    CV_d15N
lesti-sph -0.012948117 0.03266115
maur-mue  -0.005224151 0.05249590
myct-pun  -0.009814716 0.04047466
noto-bol  -0.009178283 0.02755544
noto-kro  -0.012155251 0.02163464
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_25)==status_biomass_25[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_25 <-data.frame(mean_si_species_25[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_25[,"rel_biomass"], Status=status_biomass_25[,"Status"], latin_name=status_biomass_25[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_25<-scaleSI_range01(data_fish_25)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_25<-IDiversity(cons=data_fish_scl_25, weight=data_fish_scl_25[,c("rel_biomass")], nm_plot="25m_Z0508")

# printing results
result <- as.data.frame(round(ID_scl_ab_25,3)) 
#knitr::kable(result)
```
:::



![Z0508_25m_d13C_d15N](25m_Z0508_d13C_d15N.jpeg){width="550"}

### 370m (Z0492)

::: {.cell}

```{.r .cell-code  code-fold="true"}
# 370m (Z0492) ----
individuals_si_370 <- individuals_si%>%
  filter(station=="Z0492")%>%
  select(-station)

status_biomass_370 <- species_status_biomass%>%
  filter(Status=="Z0492")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_370<-data.frame(group=individuals_si_370[,"Species_code"], individuals_si_370)
mean_si_species_370<-meanSI_group(individuals_si_370)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_370[,"sd_d13C"]/mean_si_species_370[,"d13C"], CV_d15N=mean_si_species_370[,"sd_d15N"]/mean_si_species_370[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
arct-ris -0.006981948 0.01886755
argy-olf -0.011380016 0.03779400
lamp-cro -0.004821553 0.04473307
myct-pun -0.019927081 0.04074265
noto-kro -0.012294978 0.02480797
xeno-cop -0.011716566 0.07257123
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_370)==status_biomass_370[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_370 <-data.frame(mean_si_species_370[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_370[,"rel_biomass"], Status=status_biomass_370[,"Status"], latin_name=status_biomass_370[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_370<-scaleSI_range01(data_fish_370)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_370<-IDiversity(cons=data_fish_scl_370, weight=data_fish_scl_370[,c("rel_biomass")], nm_plot="370m_Z0492")

# printing results
result <- as.data.frame(round(ID_scl_ab_370,3)) 
#knitr::kable(result)
```
:::

![370m](370m_Z0492_d13C_d15N.jpeg){width="550"}

### 555m (Z0512)

::: {.cell}

```{.r .cell-code  code-fold="true"}
# 555m (Z0512) ----
individuals_si_555 <- individuals_si%>%
  filter(station=="Z0512")%>%
  select(-station)

status_biomass_555 <- species_status_biomass%>%
  filter(Status=="Z0512")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_555<-data.frame(group=individuals_si_555[,"Species_code"], individuals_si_555)
mean_si_species_555<-meanSI_group(individuals_si_555)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_555[,"sd_d13C"]/mean_si_species_555[,"d13C"], CV_d15N=mean_si_species_555[,"sd_d15N"]/mean_si_species_555[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
arct-ris -0.012564587 0.03643136
argy-olf -0.008743823 0.04560865
lamp-cro -0.017196928 0.05084072
noto-kro -0.014008937 0.01957619
xeno-cop -0.010054994 0.04599149
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_555)==status_biomass_555[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_555 <-data.frame(mean_si_species_555[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_555[,"rel_biomass"], Status=status_biomass_555[,"Status"], latin_name=status_biomass_555[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_555<-scaleSI_range01(data_fish_555)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_555<-IDiversity(cons=data_fish_scl_555, weight=data_fish_scl_555[,c("rel_biomass")], nm_plot="555m_Z0512")

# printing results
result <- as.data.frame(round(ID_scl_ab_555,3)) 
#knitr::kable(result)
```
:::


![555m](555m_Z0512_d13C_d15N.jpeg){width="550"}

### 715m (Z0503)

::: {.cell}

```{.r .cell-code  code-fold="true"}
#715m Z0503 ----
individuals_si_715 <- individuals_si%>%
  filter(station=="Z0503")%>%
  select(-station)

status_biomass_715<- species_status_biomass%>%
  filter(Status=="Z0503")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_715<-data.frame(group=individuals_si_715[,"Species_code"], individuals_si_715)
mean_si_species_715<-meanSI_group(individuals_si_715)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_715[,"sd_d13C"]/mean_si_species_715[,"d13C"], CV_d15N=mean_si_species_715[,"sd_d15N"]/mean_si_species_715[,"d15N"] )
```

::: {.cell-output .cell-output-stdout}
```
              CV_d13C    CV_d15N
arct-ris -0.013383792 0.02784671
cycl-otz -0.009264821 0.04946692
lamp-cro -0.013572275 0.03144289
serr-bea -0.008721937 0.06133097
xeno-cop -0.012017717 0.06196185
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_715)==status_biomass_715[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_715 <-data.frame(mean_si_species_715[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_715[,"rel_biomass"], Status=status_biomass_715[,"Status"], latin_name=status_biomass_715[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_715<-scaleSI_range01(data_fish_715)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_715<-IDiversity(cons=data_fish_scl_715, weight=data_fish_scl_715[,c("rel_biomass")], nm_plot="715m_Z0503")

# printing results
result <- as.data.frame(round(ID_scl_ab_715,3)) 
#knitr::kable(result)
```
:::

![715m](715m_Z0503_d13C_d15N.jpeg){width="550"}

### 1000m (Z0518)

::: {.cell}

```{.r .cell-code  code-fold="true"}
#1000m Z0518 ----
individuals_si_1000 <- individuals_si%>%
  filter(station=="Z0518")%>%
  select(-station)

status_biomass_1000<- species_status_biomass%>%
  filter(Status=="Z0518")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_1000<-data.frame(group=individuals_si_1000[,"Species_code"], individuals_si_1000)
mean_si_species_1000<-meanSI_group(individuals_si_1000)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_1000[,"sd_d13C"]/mean_si_species_1000[,"d13C"], CV_d15N=mean_si_species_1000[,"sd_d15N"]/mean_si_species_1000[,"d15N"])
```

::: {.cell-output .cell-output-stdout}
```
             CV_d13C    CV_d15N
argy-olf -0.01024511 0.03092443
bent-gla -0.01551299 0.06444602
lamp-cro -0.02274635 0.05835428
maul-arg -0.00960478 0.03191368
sear-koe -0.02285299 0.05388211
serr-bea -0.01480479 0.06708122
```
:::

```{.r .cell-code  code-fold="true"}
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_1000)==status_biomass_1000[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_1000 <-data.frame(mean_si_species_1000[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_1000[,"rel_biomass"], Status=status_biomass_1000[,"Status"], latin_name=status_biomass_1000[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_1000<-scaleSI_range01(data_fish_1000)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_1000<-IDiversity(cons=data_fish_scl_1000, weight=data_fish_scl_1000[,c("rel_biomass")], nm_plot="1000m_Z0518")

# printing results
result <- as.data.frame(round(ID_scl_ab_1000,3)) 
#knitr::kable(result)
```
:::

![1000m](1000m_Z0518_d13C_d15N.jpeg){width="550"}

### 1335m (Z0497)

::: {.cell}

```{.r .cell-code  code-fold="true"}
#1335 Z0497 ----
individuals_si_1335 <- individuals_si%>%
  filter(station=="Z0497")%>%
  select(-station)

status_biomass_1335 <- species_status_biomass%>%
  filter(Status=="Z0497")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si_1335<-data.frame(group=individuals_si_1335[,"Species_code"], individuals_si_1335)
mean_si_species_1335<-meanSI_group(individuals_si_1335)

# computing coefficent of variation within each species to assess intraspecific variability
cbind(CV_d13C=mean_si_species_1335[,"sd_d13C"]/mean_si_species_1335[,"d13C"], CV_d15N=mean_si_species_1335[,"sd_d15N"]/mean_si_species_1335[,"d15N"] )
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
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_1335)==status_biomass_1335[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_1335 <-data.frame(mean_si_species_1335[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_1335[,"rel_biomass"], Status=status_biomass_1335[,"Status"], latin_name=status_biomass_1335[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_1335<-scaleSI_range01(data_fish_1335)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_1335<-IDiversity(cons=data_fish_scl_1335, weight=data_fish_scl_1335[,c("rel_biomass")], nm_plot="1335m_Z0497")

# printing results
result <- as.data.frame(round(ID_scl_ab_1335,3)) 
#knitr::kable(result)
```
:::

![bathypelagic](1335m_Z0497_d13C_d15N.jpeg){width="550"}

### near_bottom (Z0524)

::: {.cell}

```{.r .cell-code  code-fold="true"}
# 1010 Z0524 ----
individuals_si_nb <- individuals_si%>%
  filter(station=="Z0524")%>%
  select(-station)

status_biomass_nb <- species_status_biomass%>%
  filter(Status=="Z0524")

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
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
# -> intraspecific variability is overall low (<20%)

# checking that species codes are the same in the two tables
#row.names(mean_si_species_nb)==status_biomass_nb[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish_nb <-data.frame(mean_si_species_nb[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_biomass=status_biomass_nb[,"rel_biomass"], Status=status_biomass_nb[,"Status"], latin_name=status_biomass_nb[,"Species_name"])

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl_nb<-scaleSI_range01(data_fish_nb)

# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab_nb<-IDiversity(cons=data_fish_scl_nb, weight=data_fish_scl_nb[,c("rel_biomass")], nm_plot="near_bottom_Z0524")

# printing results
result <- as.data.frame(round(ID_scl_ab_nb,3)) 
#knitr::kable(result)
```
:::

![bathypelagic](near_bottom_Z0524_d13C_d15N.jpeg){width="550"}

## Spider web

::: {.cell}

```{.r .cell-code  code-fold="true"}
# load library
library(tidyr)

# crate a data frame with all index value
trophic_indices <- data.frame(
  group = c("25m", "370m", "555m", "715m", "1000m", "1335m", "1010m-bottom proximity"),
  IDiv = c(0.952, 0.918, 0.845, 0.928, 0.721, 0.932, 0.994),
  IDis = c(0.898, 0.620, 0.466, 0.621, 0.451, 0.755, 0.318),
  IEve= c(0.46, 0.713, 0.789, 0.644, 0.58, 0.633, 0.543), 
  IUni =c(0.566,0.884, 0.833, 0.599, 0.73, 0.713, 0.758))%>%
  mutate(across(group, factor, levels=c("25m", "370m", "555m", "715m", "1000m", "1335m", "1010m-bottom proximity")))

# plot 
ggradar::ggradar(trophic_indices,
                 values.radar = c("0", "0.5", "1"),
                 group.colours ="#004291", group.line.width = 1,
                 axis.label.size= 4, group.point.size = 3,
                 grid.label.size = 4)+
  facet_wrap(~group)+
  theme_minimal()+
  theme(strip.text.x = element_text(size=12, face = "bold"),
        plot.background = element_rect(colour = "white"))+
  guides(col="none")
```

::: {.cell-output-display}
![](index_files/figure-html/trophic_indices_df-1.png){width=960}
:::

```{.r .cell-code  code-fold="true"}
#ggsave("spider_web.png", path = "figures", dpi = 700)

# spider web for each station 
# for (i in 1:7) {
#  print(ggradar::ggradar(trophic_indices[i,],
#                  values.radar = c("0", "0.5", "1"),
#                  group.colours ="#0c3d97",  group.line.width = 1.3,
#                  axis.label.size= 4, group.point.size = 5,
#                  grid.label.size = 4)+
#   facet_wrap(~group)+
#   theme_light()+
#   theme(strip.text.x = element_text(size=12, face = "bold"))+
#   guides(col="none"))
# }
```
:::

::: {.cell}

```{.r .cell-code}
htmltools::tagList(DT::datatable(trophic_indices))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-2d621a9dc2d51aaf9e1a" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-2d621a9dc2d51aaf9e1a">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7"],["25m","370m","555m","715m","1000m","1335m","1010m-bottom proximity"],[0.952,0.918,0.845,0.928,0.721,0.9320000000000001,0.994],[0.898,0.62,0.466,0.621,0.451,0.755,0.318],[0.46,0.713,0.789,0.644,0.58,0.633,0.543],[0.5659999999999999,0.884,0.833,0.599,0.73,0.713,0.758]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>group<\/th>\n      <th>IDiv<\/th>\n      <th>IDis<\/th>\n      <th>IEve<\/th>\n      <th>IUni<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::



# 5.additional analyses 
## Isotopic niches by sations 

::: {.cell}

```{.r .cell-code  code-fold="true"}
# colors selection 
nichecol <- c("#E4A33A", "#F67451", "#D664BE", "#3DA5D9", "#94b3ae",
              "#18206F", "#FD151B", "#049A8F", "#072AC8", "purple", 
              "#d193f7", "#d8c2ab", "#678FCB", "#A63A49", "#00547A", "#6B54A0")

# plot
ggplot(data = isotope_data_fish, 
                         aes(x = d13c, 
                             y = d15n)) + 
  facet_wrap(~factor(trawling_depth))+
  geom_point(aes(color = species), alpha=0.7, size=1) +
  scale_color_manual(values=nichecol)+  
  scale_fill_manual(values=nichecol)+
  scale_x_continuous(expression({delta}^13*C~'\u2030')) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'))+
  stat_ellipse(aes(group = species, fill = species, color = species), 
               alpha = 0.2, level = p.ell,linewidth = 0.7, type = "norm", geom = "polygon")+
  theme_light()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10))+
  labs(shape="Taxon", col= "Species", fill="Species")
```

::: {.cell-output-display}
![](index_files/figure-html/ncihes_stations-1.png){width=960}
:::

```{.r .cell-code  code-fold="true"}
#ggsave("niches_stations.png", path = "figures", dpi = 700, height = 8, width = 10)
```
:::



## Krill data 
- Total $\delta$<sup>15</sup>N variability  = 0.76‰
- Total $\delta$<sup>13</sup>C variability = 1.1‰


::: {.cell}

```{.r .cell-code  code-fold="true"}
# Load data
isotope_data <-  utils::read.csv(here::here("data", "isotopic_data_2021.csv"), sep = ";", header = T,dec = ",")
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
![](index_files/figure-html/krill_data-1.png){width=672}
:::
:::


### Isotope variability

::: {.cell}

```{.r .cell-code  code-fold="true"}
boxplot_d13c <- ggpubr::ggboxplot(isotope_data_krill , x = "trawling_depth", y = "d13c", 
          color = "trawling_depth",fill = "trawling_depth", alpha=0.3,
          ylab = "d13c", xlab = "trawling_depth")+
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  xlab("Trawling depth (m)")+
  paletteer::scale_fill_paletteer_d("rcartocolor::Teal")+
  paletteer::scale_color_paletteer_d("rcartocolor::Teal")+
  guides(fill="none", col="none" ,alpha="none")+
  theme_light()

boxplot_d15n <- ggpubr::ggboxplot(isotope_data_krill , x = "trawling_depth", y = "d15n", 
                  color = "trawling_depth", fill = "trawling_depth",
                  alpha=0.3) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab("Trawling depth (m)")+
  paletteer::scale_fill_paletteer_d("rcartocolor::Teal")+
  paletteer::scale_color_paletteer_d("rcartocolor::Teal")+
  guides(fill="none", col="none" ,alpha="none")+
  theme_light()

ggpubr::ggarrange(boxplot_d13c, boxplot_d15n)
```

::: {.cell-output-display}
![](index_files/figure-html/krill_boxplot-1.png){width=1152}
:::
:::


### Tests 

::: {.cell}

```{.r .cell-code  code-fold="true"}
pairwise.wilcox.test(isotope_data_krill$d13c, isotope_data_krill$trawling_depth,
                 p.adjust.method = "BH")
```

::: {.cell-output .cell-output-stdout}
```

	Pairwise comparisons using Wilcoxon rank sum exact test 

data:  isotope_data_krill$d13c and isotope_data_krill$trawling_depth 

     25    370   555   715   1000  1010 
370  0.359 -     -     -     -     -    
555  0.033 0.122 -     -     -     -    
715  0.042 0.125 0.497 -     -     -    
1000 0.033 0.033 0.303 0.179 -     -    
1010 0.033 0.033 0.541 0.125 0.883 -    
1335 0.107 0.433 0.833 1.000 0.454 0.433

P value adjustment method: BH 
```
:::

```{.r .cell-code  code-fold="true"}
pairwise.wilcox.test(isotope_data_krill$d15n, isotope_data_krill$trawling_depth,
                 p.adjust.method = "BH")
```

::: {.cell-output .cell-output-stdout}
```

	Pairwise comparisons using Wilcoxon rank sum exact test 

data:  isotope_data_krill$d15n and isotope_data_krill$trawling_depth 

     25    370   555   715   1000  1010 
370  0.025 -     -     -     -     -    
555  0.025 0.143 -     -     -     -    
715  0.025 0.243 0.030 -     -     -    
1000 0.025 0.274 0.585 0.025 -     -    
1010 0.025 0.120 0.629 0.025 0.467 -    
1335 0.025 0.306 0.037 0.750 0.025 0.025

P value adjustment method: BH 
```
:::
:::
