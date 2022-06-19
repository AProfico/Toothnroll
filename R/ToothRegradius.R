#' ToothRegradius
#' 
#' Wrapper of the function regularradius written by Julien Claude (Morphometrics with R)
#' @param mat a kx2 matrix 
#' @param center coordinates of the center from which the calculation of regular radius started 
#' @param n number of points
#' @param direction specify direction of equiangular landmarks: "c"=clockwise; "a"=anticlockwise
#' @return V2 position of landmarks equiangular spaced
#' @author Julien Claude, Antonio Profico
#' @export

ToothRegradius<-function (mat, center, n,direction=c("c","a")) 
{
  Rx <- mat[, 1]
  Ry <- mat[, 2]
  le <- length(Rx)
  M <- matrix(c(Rx, Ry), le, 2)
  M1 <- matrix(c(Rx - center[1], Ry - center[2]), le, 2)
  V1 <- complex(real = M1[, 1], imaginary = M1[, 2])
  M2 <- matrix(c(Arg(V1), Mod(V1)), le, 2)
  V2 <- NA
  for (i in 0:(n - 1)) {
    if(direction=="c"){
    V2[i + 1] <- which.max((cos(M2[, 1] + 2 * i * pi/n)))}
    if(direction=="a"){
    V2[i + 1] <- which.max((cos(M2[, 1] - 2 * i * pi/n)))}
    }

  return(V2)
}