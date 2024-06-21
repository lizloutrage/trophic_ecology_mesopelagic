#' plot_niche_cluster
#'
#' @param data : file of isotope values
#'
#' @return
#' @export
#'
#' @examples
plot_niche_cluster <- function(data) {
  # assign each species to a cluster
  niche_cluster <- data %>%
    mutate(
      cluster = case_when(
        species %in% c(
          "Argyropelecus olfersii",
          "Lampanyctus crocodilus",
          "Benthosema glaciale"
        ) ~ 1,
        species %in% c(
          "Lampanyctus macdonaldi",
          "Maulisia argipalla",
          "Searsia koefoedi"
        ) ~ 3,
        species %in% c(
          "Cyclothone",
          "Notoscopelus bolini",
          "Notoscopelus kroyeri",
          "Melanostigma atlanticum"
        ) ~ 2,
        species %in% c(
          "Xenodermichthys copei",
          "Serrivomer beanii",
          "Maurolicus muelleri",
          "Myctophum punctatum"
        ) ~ 5,
        species %in% c("Arctozenus risso", "Lestidiops sphyrenoides") ~ 4
      )
    )
  
  niche_cluster$cluster <- as.factor(niche_cluster$cluster)
  # plot
  ggplot(data = niche_cluster,
         aes(x = d13c,
             y = d15n)) +
    geom_point(aes(color = factor(cluster))) +
    scale_color_manual(values = c("#86BBBD", "#ECA72C", "#4D85A8", "#9BABE8", "#D35D4A")) +
    scale_fill_manual(values = c("#86BBBD", "#ECA72C", "#4D85A8", "#9BABE8", "#D35D4A")) +
    scale_x_continuous(expression({
      delta
    } ^ 13 * C ~ '\u2030')) +
    scale_y_continuous(expression({
      delta
    } ^ 15 * N ~ '\u2030')) +
    stat_ellipse(
      aes(
        group = cluster,
        fill = cluster,
        color = cluster
      ),
      alpha = 0.2,
      level = 0.40,
      linewidth = 0.5,
      type = "norm",
      geom = "polygon"
    ) +
    theme_bw() +
    theme(
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16)
    ) +
    labs (col = "Trophic guilds", fill = "Trophic guilds") +
    theme(aspect.ratio = 1)
  
  ggsave(
    "niches_cluster.png",
    path = "figures",
    dpi = 700,
    height = 8,
    width = 10
  )
}
