#' sum of isotopic niches overlap
#'
#' @param table_iso : table with species, d15n, d13c and habitat columns
#' @param alea : if true calculate convex hull
#'
#' @examples sum_overlap_std(table_iso, alea=TRUE)

sum_overlap <- function(table_iso, alea) {
  
  # preparation of the file
  table_iso <- data.frame(na.omit(table_iso))
  d13C <- table_iso$d13C
  d15N <- table_iso$d15N
  X <- table_iso$d13C
  Y <- table_iso$d15N
  PID <- 1:nrow(table_iso)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  
  # Calculation of ellipse area
  overlap <-
    rKIN::estEllipse(
      data = table_iso,
      x = "d13C",
      y = "d15N",
      group = "species",
      levels = 40,
      smallSamp = TRUE
    )
  
  # Calculation of overlap
  overlap_matrix <- rKIN::calcOverlap(overlap) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    tibble::column_to_rownames("OverlapID") %>%
    as.data.frame()
  
  # Sum of all overlaps in the habitat
  OVER <- sum(overlap_matrix)
  
  if (alea == F) {
    lt <- list(OVER = OVER)
  } else {
    lt <- list(OVER = OVER,
               ConvHull = PBSmapping::calcConvexHull(xydata))
  }
  return(lt)
}
