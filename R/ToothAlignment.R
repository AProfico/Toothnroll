#' ToothAlignment
#'
#' Align dental meshes using as reference the cervical outline and five landmarks
#' @param mesh1 3D mesh: dental mesh (enamel)
#' @param mesh2 3D mesh: dental mesh (dentin)
#' @param mesh3 3D mesh: dental mesh (dental pulp)
#' @param set matrix: 5 landmarks acquired on the mesh (see details)
#' @param outline matrix: set of coordinates along the cerical outline
#' @return almesh1: mesh of the aligned mesh1
#' @return almesh2: mesh of the aligned mesh2
#' @return almesh3: mesh of the aligned mesh3
#' @return alset: coordinates of the aligned landmark configuration 
#' @return blength: biomechanical lenght of the root (see details)
#' @return margins: coordinates of the landmarks in correspondence of the four margins  
#' @return margins_sel: position of the margine along the outline 
#' @return aloutline: coordinates of the aligned cervical outline 
#' @details The function needs five landmarks to align the dental meshes. Usually landmarks from 1 to 4 define the Linguale, Mesial, Buccal and Distal margins.
#' The fifth landmark defines the end of the z axis (biomechanical length). The centroid of the cervical outline defines the origin of axes.  
#' @author Antonio Profico
#' @examples
#' data("ToothExData")
#' Enamel<-ToothExData$enamel
#' Dentin<-ToothExData$dentin
#' Pulp<-ToothExData$pulp
#' outline<-ToothExData$outline
#' set<-ToothExData$set
#' AlignMeshes<-ToothAlignment(mesh1=Enamel,mesh2=Dentin,mesh3=Pulp,set,outline)
#' if(interactive()){
#' require(rgl)
#' open3d()
#' shade3d(AlignMeshes$almesh1$mesh,col="white",alpha=0.5)
#' shade3d(AlignMeshes$almesh2$mesh,col="pink",alpha=0.5,add=TRUE)
#' shade3d(AlignMeshes$almesh3$mesh,col="orange",alpha=0.5,add=TRUE)
#' spheres3d(AlignMeshes$alset,radius=0.25)
#' spheres3d(AlignMeshes$outline,radius=0.1,col="blue")
#' lines3d(AlignMeshes$outline)
#' text3d(rbind(AlignMeshes$outline[AlignMeshes$margins_sel,],AlignMeshes$alset[4,]),texts=1:5,cex=4)
#' spheres3d(AlignMeshes$alset[2,],radius=0.3,col="red")
#' arrow3d(colMeans(AlignMeshes$outline),AlignMeshes$alset[4,],lwd=3,col="green",type="lines",s=1/10)
#' lines3d(rbind(AlignMeshes$alset[2,],AlignMeshes$alset[1,]),lwd=3,col="green")
#' axes3d()
#' }
#' @export


ToothAlignment<-function(mesh1,mesh2,mesh3=NULL,set,outline){
  
  too_L_M_B_D<-set[1:4,]
  too_L_M_B_D[1:4,]<-too_L_M_B_D[c(1,2,3,4),]
  apex<-set[5,]
  
  posor<-c(aro.clo.points(outline,too_L_M_B_D)$position)
  cur_pca<-prcomp(outline)
  if(cur_pca$x[posor[2],2]<cur_pca$x[posor[4],2]){
  cur_pca$x[,2]<-cur_pca$x[,2]*-1
  }
  pca_line<-cur_pca$x
  newbar<-colMeans(pca_line)
  
  if(pca_line[posor[1],2]<0){
    Angle<-angle.calc(c(-300000,0),c(pca_line[posor[1],1:2]))
    Rotmat<-rotaxis3d(pca_line,c(0,0,0),c(0,0,-1000),Angle)
  }
  
  if(pca_line[posor[1],2]>0){
    Angle<-angle.calc(c(-300000,0),c(pca_line[posor[1],1:2]))
    Rotmat<-rotaxis3d(pca_line,c(0,0,0),c(0,0,1000),Angle)
  }
  
  sel<-ToothRegradius(Rotmat[,1:2],center = colMeans(Rotmat[,1:2]),n = 4,direction="a")
  newbarot<-colMeans(Rotmat)
  
  sel2<-aro.clo.points(Rotmat[posor,1:3],Rotmat[sel,1:3])
  new_lands<-Rotmat[sel,1:3][sel2$position,]
  tobe<-rbind(apex)
  rotobe<-rotonmat(tobe,too_L_M_B_D,new_lands,scale = FALSE,reflection = FALSE)
  rotmesh1<-rotmesh.onto(mesh1,too_L_M_B_D,new_lands,scale = FALSE,reflection = FALSE)
  if(!is.null(mesh2)){
    rotmesh2<-rotmesh.onto(mesh2,too_L_M_B_D,new_lands,scale = FALSE,reflection = FALSE)
  }
  if(!is.null(mesh3)){
    rotmesh3<-rotmesh.onto(mesh3,too_L_M_B_D,new_lands,scale = FALSE,reflection = FALSE)
  }
  rotobecur<-rotonmat(outline,too_L_M_B_D,new_lands,scale = FALSE,reflection = FALSE)
  
  set<-rbind(rotmesh1$yrot,newbar,rotobe)
  
  lin<-set[1,]
  lab<-set[3,]
  ape<-set[6,]
  
  Mea<-set[5,] 
  
  biomech<-sqrt(sum((set[5,]-ape)^2))
  distlin<-sqrt(sum((set[5,]-lin)^2))
  distlab<-sqrt(sum((set[5,]-lab)^2))
  
  settar<-rbind(lin,Mea,lab,ape)
  setref<-rbind(c(lin[1],0,0),c(0,0,0),c(lab[1],0,0),c(0,0,biomech))
  aligMesh1<-rotmesh.onto(rotmesh1$mesh,settar,setref,scale=FALSE,reflection = FALSE)
  
  Li_Me_Bu_D_rot<-rotonmat(rotobecur,settar,setref,scale = FALSE,reflection = FALSE)
  Li_Me_Bu_D_rot_sel<-Li_Me_Bu_D_rot[posor,]
  
  object1<-aligMesh1
  if(!is.null(mesh2)){
    aligMesh2<-rotmesh.onto(rotmesh2$mesh,settar,setref,scale=FALSE,reflection = FALSE)
    object2<-aligMesh2
    
  }
  if(!is.null(mesh3)){
    aligMesh3<-rotmesh.onto(rotmesh3$mesh,settar,setref,scale=FALSE,reflection = FALSE)
    object3<-aligMesh3
  }
  rotobeset<-rotonmat(set,settar,setref,scale = FALSE,reflection = FALSE)
  
  if(is.null(mesh2)){
    object2<-NULL
  }
  if(is.null(mesh3)){
    object3<-NULL
  }
  
  
  out<-list("almesh1"=object1,"almesh2"=object2,"almesh3"=object3,"alset"=object1$yrot,"blength"=biomech,"margins"=Li_Me_Bu_D_rot_sel,"margins_sel"=aro.clo.points(Li_Me_Bu_D_rot,Li_Me_Bu_D_rot_sel)$position,"outline"=Li_Me_Bu_D_rot)
  return(out)
  
  
}
