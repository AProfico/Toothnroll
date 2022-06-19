#' ToothDF
#' 
#' Tool to build a data.frame suitable for morphometric maps
#' @param morphomap.thickness list: morphomap.Thickness object
#' @param rem.out logical: if TRUE the outlier will be removed 
#' @param fac.out numeric: parameter to set the threshold in outliers detection
#' @param smooth logical: if TRUE the smooth algorithm is applied
#' @param scale logical: if TRUE the thichkness matrix is scaled from 0 to 1 
#' @param smooth.iter numeric: number of smoothing iterations 
#' @param method character: if set on "equiangular" the cortical thickness is meant as the distance of the segment intersecting the external and internal outline starting from the centroid of the section. If set on "closest" the cortical thickness is calculated at each point as the closest distance between external and internal outlines
#' @param labels character vector: names for x labels in the morphometric map
#' @return XYZ data.frame for morphometric map
#' @return labels character vector for x labels in the morphometric map
#' @author Antonio Profico
#' @export

ToothDF<-function (morphomap.thickness, rem.out = TRUE, fac.out = 0.5, 
                        smooth = TRUE, scale = TRUE, smooth.iter = 5, method = "equiangular",labels=c("Li","Mes", "Bu", "D","Li")) {
  morphomap.shape <- morphomap.thickness$morphomap.shape
  if (method == "equiangular") {
    thicks <- morphomap.thickness$sect_thickness_eq
  }
  if (method == "closest") {
    thicks <- morphomap.thickness$sect_thickness_cp
  }
  X <- NULL
  Y <- NULL
  Z <- NULL
  summary <- dim(thicks)
  num.sect <- summary[3]
  num.land <- summary[1]
  if (scale == TRUE) {
    x <- as.vector(thicks)
    thickvector <- (x - min(x))/(max(x) - min(x))
    Thick_arr <- array(thickvector, dim = c(num.land, 1, 
                                            num.sect))
    thicks <- Thick_arr
  }
  if (rem.out == TRUE) {
    x <- as.vector(thicks)
    qnt <- stats::quantile(x, probs = c(0.25, 0.75))
    H <- fac.out * IQR(x)
    y <- x
    y[x < (qnt[1] - H)] <- min(y[x > (qnt[1] - H)])
    y[x > (qnt[2] + H)] <- max(y[x < (qnt[2] + H)])
    thick_values <- y
    Thick_arr <- array(thick_values, dim = c(num.land, 1, 
                                             num.sect))
    thicks <- Thick_arr
    if (scale == TRUE) {
      x <- as.vector(thicks)
      thickvector <- (x - min(x))/(max(x) - min(x))
      Thick_arr <- array(thickvector, dim = c(num.land, 
                                              1, num.sect))
      thicks <- Thick_arr
    }
  }
  if (smooth == TRUE) {
    thick_smoo <- apply(thicks, 2, function(x) (matrixSmooth(x, 
                                                             passes = smooth.iter)))
    thick_out <- array(thick_smoo, dim = c(num.land, 1, num.sect))
    thicks <- thick_out
  }
  for (i in 1:num.sect) {
    X_i <- seq(1, 100, length.out = num.land)
    Y_i <- morphomap.shape$`3D_out`[, 3, i]
    Z_i <- thicks[, , i]
    X <- c(X, X_i)
    Y <- c(Y, Y_i)
    Z <- c(Z, Z_i)
  }
  
  labels <- labels
  
  XYZ <- data.frame(X, Y, Z)
  return(list(XYZ = XYZ, labels = labels))
}
