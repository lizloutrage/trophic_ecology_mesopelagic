plotKIN2 <- function (estObj, scaler = 1, alpha = 0.3, title = "", xlab = "x", 
                      ylab = "y", xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, nichecol) 
{
  requireNamespace("maptools")
  if (!inherits(estObj$estObj, "estObj")) 
    stop("estObj must be of class estObj created from estEllipse, estKIN, or estMCP functions!")
  if (!inherits(scaler, "numeric")) 
    stop("scaler must be numeric!")
  if (!inherits(alpha, "numeric")) 
    stop("alpha must be numeric!")
  if (alpha > 1 | alpha < 0) 
    stop("alpha must be a numeric value between 0 and 1!")
  if (!inherits(title, "character")) 
    stop("title must be a character!")
  if (!inherits(xlab, "character")) 
    if (!inherits(xlab, "expression")) 
      stop("xlab must be a character or an expression!")
  if (!inherits(ylab, "character")) 
    if (!inherits(ylab, "expression")) 
      stop("ylab must be a character or an expression!")
  if (!inherits(xmin, "numeric")) 
    if (!is.null(xmin)) 
      stop("xmin must be numeric or NULL!")
  if (!inherits(xmax, "numeric")) 
    if (!is.null(xmax)) 
      stop("xmax must be numeric or NULL!")
  if (!inherits(ymin, "numeric")) 
    if (!is.null(ymin)) 
      stop("ymin must be numeric or NULL!")
  if (!inherits(ymax, "numeric")) 
    if (!is.null(ymax)) 
      stop("ymax must be numeric or NULL!")
  ord <- unique(estObj$estObj[[1]]@data$ConfInt)[order(unique(estObj$estObj[[1]]@data$ConfInt), 
                                                       decreasing = TRUE)]
  xs <- numeric()
  ys <- numeric()
  df <- list()
  for (i in 1:length(estObj$estObj)) {
    xs <- c(xs, sp::bbox(estObj$estObj[[i]])[1, ])
    ys <- c(ys, sp::bbox(estObj$estObj[[i]])[2, ])
    for (j in 1:length(ord)) {
      estObj$estObj[[i]]@data$PlotOrder[estObj$estObj[[i]]@data$ConfInt == 
                                          ord[j]] <- j
    }
    gdf <- ggplot2::fortify(estObj$estObj[[i]], region = "PlotOrder")
    gdf <- merge(gdf, estObj$estObj[[i]]@data, by.x = "id", 
                 by.y = "PlotOrder")
    gdf$Group_ConfInt <- paste(gdf$Group, gdf$ConfInt, sep = "_")
    df <- c(df, list(gdf))
  }
  if (length(df) > 20) 
    stop("You have more than 6 Groups, this is quite a few and plotKIN will currently fail with that many due\n         to the number of discernable color pallettes. Perhaps try reducing your data to fewer groups?")
  pts <- list()
  for (i in 1:length(estObj$estInput)) {
    pts <- c(pts, list(estObj$estInput[[i]]@data))
    xs <- c(xs, estObj$estInput[[i]]@data[, 3])
    ys <- c(ys, estObj$estInput[[i]]@data[, 4])
  }
  ifelse(is.null(xmin) & !is.numeric(xmin), xmin <- (min(xs) - 
                                                       scaler), xmin)
  ifelse(is.null(xmax) & !is.numeric(xmax), xmax <- (max(xs) + 
                                                       scaler), xmax)
  ifelse(is.null(ymin) & !is.numeric(ymin), ymin <- (min(ys) - 
                                                       scaler), ymin)
  ifelse(is.null(ymax) & !is.numeric(ymax), ymax <- (max(ys) + 
                                                       scaler), ymax)
  kin.plot <- ggplot2::ggplot() + 
    lapply(df, function(x) ggplot2::geom_polygon(data = x, alpha = alpha, ggplot2::aes_string(x = "long", y = "lat", fill = "Group_ConfInt", group = "group", col="Group"))) + 
    ggplot2::scale_fill_manual(values = nichecol) +
    lapply(pts, function(x) ggplot2::geom_point(data = x, ggplot2::aes_string(x = names(x)[3], y = names(x)[4],colour = "Group"))) + 
    ggplot2::scale_color_manual(values = nichecol) + 
    ggplot2::coord_fixed(ratio = (xmax - xmin)/(ymax -  ymin), xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
    ggplot2::scale_x_continuous(breaks = seq(from = round(xmin), 
                                             to = round(xmax), by = scaler)) +
    ggplot2::scale_y_continuous(breaks = seq(from = round(ymin), to = round(ymax), by = scaler)) +
    ggplot2::labs(title = title,  x = xlab, y = ylab) +
    ggplot2::guides(colour = "none", shape="none") + 
    ggplot2::theme_light() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                   plot.title = element_text(hjust = 0.5))
  return(kin.plot)
}