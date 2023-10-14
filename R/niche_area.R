#' calculating species ellipse area in a habitat
#'
#' @param table_iso: has to have these column names species, d15N, d13C, habitat
#' @param alea: if true caculate convex hull
#'
#'@author Liz Loutrage, \email{liz.loutrage@gmail.com}
#'        Anik Brind'amour, \email {Anik.Brindamour@ifremer.fr}
#'
#' @examples ellipse_size(table_iso, alea = TRUE)

niche_area <- function(table_iso, alea) {
  table_iso <- data.frame(na.omit(table_iso))
  SpNames <- table_iso$species
  d13C <- table_iso$d13C
  d15N <- table_iso$d15N
  X <- table_iso$d13C
  Y <- table_iso$d15N
  PID <- 1:nrow(table_iso)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  # Calculation ellipse size with rKIN package
  sea <-
    rKIN::estEllipse(
      data = table_iso,
      x = "d13C",
      y = "d15N",
      group = "species",
      levels = 40,
      smallSamp = TRUE
    )
  ELP <-  sea$estObj[, c('ShapeArea', 'Group')][[1]]
  
  if (alea == F) {
    lt <-
      list(ELP = ELP)
  } else {
    lt <-
      list(ELP = ELP,
           ConvHull = PBSmapping::calcConvexHull(xydata))
  }
  return(lt)
}
