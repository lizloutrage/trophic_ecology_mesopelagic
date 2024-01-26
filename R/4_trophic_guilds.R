#' Determine number of cluster with Gap statistic method 
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples

nb_cluster <- function(data){
  
# Gap statistic
factoextra::fviz_nbclust(
  data,
  kmeans,
  nstart = 25,
  method = "gap_stat",
  nboot = 100,
  verbose = FALSE) +
  labs(subtitle = "Gap statistic method")
  
  ggsave("nb_cluster.png", path = "figures", dpi = 700, width = 8, height = 6) 
}


#' Definition of the cluster with kmeans algorithm
#'
#' @param overlap_matrix 

k_means_cluster <- function(overlap_matrix){
 res_km <-  kmeans(scale(overlap_matrix), 5, nstart = 25)
 return(res_km$cluster)
} 


#' Representation of the trophic guilds (cluster)
#'
#' @param overlap_mx 
#'

plot_densdrogram <- function(overlap_mx){
  
dend <- overlap_mx %>%
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
#dev.off()
}

#' plot species depth distribution 
#'
#' @param density_distribution 
#'

depth_distribution_plot <- function(density_distribution){
  
  # assign each species to a cluster (define above)
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

ggsave("density_plot.png", path = "figures", dpi = 600, height = 8, width = 10)
}


