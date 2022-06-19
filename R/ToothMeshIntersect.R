#' ToothMeshIntersect
#' 
#' Remove intersection between two aligned meshes 
#' @param refmesh object of class mesh3d: reference mesh (cutting surface)
#' @param tarmesh object of class mesh3d: target mesh (surface to be cut)
#' @return meshUp upper region of the cut mesh
#' @return meshLo lower region of the cut mesh
#' @author Antonio Profico
#' @examples
#' data("ToothExData")
#' require(rgl)
#' require(Rvcg)
#' Enamel<-ToothExData$enamel
#' Dentin<-ToothExData$dentin
#' Pulp<-ToothExData$pulp
#' outline<-ToothExData$outline
#' set<-ToothExData$set
#' AlignMeshes<-ToothAlignment(mesh1=Enamel,mesh2=Dentin,mesh3=Pulp,set,outline)
#' cutSurface<-ToothCutsur(AlignMeshes$outline,scale=1.2)
#' cutSurface2<-vcgClean(vcgUniformRemesh(vcgClean(cutSurface),voxelSize=0.1,multiSample = TRUE))
#' InterMeshes<-ToothMeshIntersect(cutSurface2,AlignMeshes$almesh2$mesh)
#' if(interactive()){
#'  wire3d(cutSurface,col=1)
#'  wire3d(InterMeshes$meshUp,col=2)
#'  wire3d(InterMeshes$meshLo,col=3)
#' }
#' @export


ToothMeshIntersect<-function(refmesh,tarmesh){
  meshInters<-mcNNindex(refmesh,tarmesh,k=1)
  sel<-as.numeric(which(vert2points(tarmesh)[,3]>vert2points(refmesh)[meshInters[,1],3]))
  meshUp<-rmVertex(tarmesh,index = sel,keep = FALSE)
  meshLo<-rmVertex(tarmesh,index = sel,keep = TRUE)
  
  out<-list("meshUp"=meshUp,"meshLo"=meshLo)
  return(out)
}