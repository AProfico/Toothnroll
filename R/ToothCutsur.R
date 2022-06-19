#' ToothCutsur
#' 
#' Create a 3D mesh from the coordinates of the cervical outline and its barycenter
#' @param outline 3D matrix: cervical outline
#' @param scale numeric: magnification factor
#' @return mesh cutting surface
#' @author Antonio Profico
#' @export

ToothCutsur<-function(outline,scale=1.2){
  linemean<-colMeans(outline)
  Lines <- outline * scale
  Liness <- translate3d(Lines, -linemean[1], -linemean[2],
                        -linemean[3])
  cutsur<-morphomapTri2sects(equidistantCurve(equidistantCurve(morphomapSort(Liness),100),100),repmat(t(colMeans(Liness)),100,1))
  mesh<-list("vb"=t(cbind(cutsur$matrix,1)),"it"=t(cutsur$tri))
  class(mesh)<-"mesh3d"
  return(mesh)
}