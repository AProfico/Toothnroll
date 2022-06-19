#' ToothShape
#' 
#' Tool for the extraction of equiangular landmarks from conical structures 
#' @param morphomap.core list: morphomap.core object 
#' @param num.land numeric: number of landmarks defining each section
#' @param sects_vector numeric: number of sections
#' @param cent.out how to define the center of each section. The method allowed are "CCA" (center of cortical area), "E" (barycenter of the external outline) and "I" (barycenter of the internal outline)
#' @param delta pixel size used to calculate the CCA
#' @param direction character: specify the desired direction; "c" = clockwise, "a" = anticlockwise 
#' @return 3D_out num.pointsx3xnum.sect array in which the external outlines are stored
#' @return 3D_inn num.pointsx3xnum.sect array in which the internal outlines are stored
#' @return 2D_out num.pointsx2xnum.sect array in which the external outlines are stored
#' @return 2D_inn num.pointsx2xnum.sect array in which the interal outlines are stored
#' @return ALPM_inn array with the coordinates of ALPM coordinates on the external outline
#' @return ALPM_out array with the coordinates of ALPM coordinates on the internal outline
#' @return mech_length mechanical length of the long bone
#' @return start percentage of the mechanical length from which the first section is defined
#' @return end percentage of the mechanical length from which the last section is defined
#' @author Antonio Profico
#' @export
ToothShape<-function (morphomap.core, num.land, sects_vector, cent.out = "E", 
                       delta = 0.1,direction="c") {
    
  mech.len <- morphomap.core$mech_length
  start <- morphomap.core$start
  end <- morphomap.core$end
  if (is.null(sects_vector) == TRUE) {
    sects_vector <- 1:dim(morphomap.core$`3D_out`)[3]
  }
  num.sect <- length(sects_vector)
  sect_poi <- seq(mech.len * start, mech.len * end, length = num.sect)
  out_coo_3D <- array(NA, dim = c(num.land, 3, num.sect))
  inn_coo_3D <- array(NA, dim = c(num.land, 3, num.sect))
  out_coo_2D <- array(NA, dim = c(num.land, 2, num.sect))
  inn_coo_2D <- array(NA, dim = c(num.land, 2, num.sect))
  ALPM_i_tot <- array(NA, dim = c(4, 2, num.sect))
  ALPM_o_tot <- array(NA, dim = c(4, 2, num.sect))
  for (m in sects_vector) {
    # m<-1
    out_outline <- morphomap.core$`2D_out`[, , m]
    inn_outline <- morphomap.core$`2D_inn`[, , m]
    if (cent.out == "CCA") {
      centroid <- morphomapCentroid(out_outline, inn_outline, 
                                    delta)
      ids_o <- ToothRegradius(out_outline, centroid, 
                                  num.land,direction=direction)
      ALPM_o <- ToothRegradius(out_outline, centroid, 
                                   4,direction=direction)
      
      fho <- ids_o[which.min(abs(ids_o - ALPM_o[1])):(which.min(abs(ids_o - ALPM_o[3])) - 1)]
      sho <- ids_o[which.min(abs(ids_o - ALPM_o[3])):length(ids_o)]
      
      ids_i <- ToothRegradius(inn_outline, centroid, num.land,direction=direction)
      ALPM_i <- ToothRegradius(inn_outline, centroid, 4,direction=direction)
      
      fhi <- ids_i[which.min(abs(ids_i - ALPM_i[1])):(which.min(abs(ids_i - ALPM_i[3])) - 1)]
      shi <- ids_i[which.min(abs(ids_i - ALPM_i[3])):length(ids_i)]
      
      out_coo_2D[, , m] <- out_outline[c(sho,fho), ]
      out_coo_3D[, , m] <- cbind(out_coo_2D[, , m],sect_poi[m])
      ALPM_o_tot[, , m] <- out_outline[ALPM_o[c(1, 2, 3, 4)], ]
      
      inn_coo_2D[, , m] <- inn_outline[c(shi, fhi), ]
      inn_coo_3D[, , m] <- cbind(inn_coo_2D[, , m], sect_poi[m])
      ALPM_i_tot[, , m] <- inn_outline[ALPM_i[c(1, 2, 3, 4)], ]
      }
    if (cent.out == "E") {
      
      ids_o <- ToothRegradius(out_outline, colMeans(out_outline), num.land,direction=direction)
      ALPM_o <- ToothRegradius(out_outline, colMeans(out_outline), 4,direction=direction)
      
      fho <- ids_o[which.min(abs(ids_o - ALPM_o[1])):(which.min(abs(ids_o - ALPM_o[3])) - 1)]
      sho <- ids_o[which.min(abs(ids_o - ALPM_o[3])):length(ids_o)]
  
      out_coo_2D[, , m] <- out_outline[c(sho,fho), ]
      out_coo_3D[, , m] <- cbind(out_coo_2D[, , m],sect_poi[m])
      ALPM_o_tot[, , m] <- out_outline[ALPM_o[c(1, 2, 3, 4)], ]
      
         
      ids_i <- ToothRegradius(inn_outline, colMeans(out_outline), num.land,direction=direction)
      ALPM_i <- ToothRegradius(inn_outline, colMeans(out_outline), 4,direction=direction)
      
      fhi <- ids_i[which.min(abs(ids_i - ALPM_i[1])):(which.min(abs(ids_i - ALPM_i[3])) - 1)]
      shi <- ids_i[which.min(abs(ids_i - ALPM_i[3])):length(ids_i)]
      
      inn_coo_2D[, , m] <- inn_outline[c(shi, fhi), ]
      inn_coo_3D[, , m] <- cbind(inn_coo_2D[, , m], sect_poi[m])
      ALPM_i_tot[, , m] <- inn_outline[ALPM_i[c(1, 2, 3, 4)], ]
      
    }
    if (cent.out == "I") {
      ids_o <- ToothRegradius(out_outline, colMeans(inn_outline), num.land,direction=direction)
      ALPM_o <- ToothRegradius(out_outline, colMeans(inn_outline), 4,direction=direction)
      
      fho <- ids_o[which.min(abs(ids_o - ALPM_o[1])):(which.min(abs(ids_o - ALPM_o[3])) - 1)]
      sho <- ids_o[which.min(abs(ids_o - ALPM_o[3])):length(ids_o)]
      
      
      ALPM_o_tot[, , m] <- out_outline[ALPM_o, ]
      out_coo_2D[, , m] <- out_outline[c(sho,fho), ]
      out_coo_3D[, , m] <- cbind(out_coo_2D[, , m],sect_poi[m])
  
      ids_i <- ToothRegradius(inn_outline, colMeans(inn_outline), 
                                  num.land,direction=direction)
      ALPM_i <- ToothRegradius(inn_outline, colMeans(inn_outline), 4,direction=direction)
      ALPM_i_tot[, , m] <- inn_outline[ALPM_i, ]
      
      fhi <- ids_i[which.min(abs(ids_i - ALPM_i[1])):(which.min(abs(ids_i - ALPM_i[3])) - 1)]
      shi <- ids_i[which.min(abs(ids_i - ALPM_i[3])):length(ids_i)]
      
      inn_coo_2D[, , m] <- inn_outline[c(shi, fhi), ]
      inn_coo_3D[, , m] <- cbind(inn_coo_2D[, , m], sect_poi[m])
    }
  }
  out <- list("3D_out" = round(out_coo_3D, 2), "3D_inn" = round(inn_coo_3D, 2), 
              "2D_out" = round(out_coo_2D, 2), "2D_inn" = round(inn_coo_2D, 2), 
              ALPM_inn = ALPM_i_tot, ALPM_out = ALPM_o_tot, start = start, 
              end = end, mech.len = mech.len)
  return(out)
}
