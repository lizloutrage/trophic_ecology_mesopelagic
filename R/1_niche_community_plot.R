#' niche_plot_community
#'
#' @param data : file of isotope values
#'
#' @return
#' @export
#'
#' @examples

niche_plot_community <- function(data) {
  
    data_plot <- data %>%
    mutate(species = gsub("_", " ", species)) %>%
    mutate(species = recode(species, "Cyclothone" = "Cyclothone spp."))
    
    # order by taxonomy 
    data_plot$species <- factor(data_plot$species, 
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
  
  colors_sp <- c(
    "#6B3777", "#00218F", "#1E78FF", "#78C8F4", "#8E050B", "#FF5064",
    "#FF87A6", "#E5D61D", "#C5AA1A", "#8FDCB6", "#00564E", "#54C797",
    "#5F7F57", "#6CA086", "#344B47", "#9256DD", "black"
  )
  
  ggplot(data = data_plot, 
         aes(x = d13c, 
             y = d15n)) + 
    geom_point(aes(color = species, shape= taxon), size =1) +
    scale_color_manual(values = colors_sp)+
    scale_fill_manual(values = colors_sp)+
    scale_shape_manual(values= c(19, 3))+
    scale_x_continuous(expression({delta}^13*C~'\u2030')) +
    scale_y_continuous(expression({delta}^15*N~'\u2030'))+
    stat_ellipse(aes(group = species, fill = species, color = species), 
                 alpha = 0.12, level = 0.40,linewidth = 0.5, type = "norm", geom = "polygon")+
    theme_bw()+
    theme(legend.text = element_text(size=13),
          legend.title = element_text(size=13),
          axis.title = element_text(size=15),
          axis.text = element_text(size=15))+
    labs(shape="Taxon", col= "Species", fill="Species")+
    theme(aspect.ratio = 1)
  
  ggsave("figures/niches_community_col.png", dpi = 700, height = 10, width = 12)
}

