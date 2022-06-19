#' ToothImport
#'
#' Import the output from ToothAlignement 
#' @param file character: name of input file
#' @return Mesh1: mesh of the aligned mesh1
#' @return Mesh2: mesh of the aligned mesh2
#' @return Mesh3: mesh of the aligned mesh3
#' @return B.Length: biomechanical lenght 
#' @return Landmarks: landmark coordinates 
#' @return Outline: outline coordinates 
#' @return Margins: position of the margins along the outline 
#' @author Antonio Profico
#' @export

ToothImport<-function(file){
  A <- readLines(file)
  e1<-which(A == "@1");e2<-which(A == "@2");e3<-which(A == "@3")
  e4<-which(A == "@4");e5<-which(A == "@5");e6<-which(A == "@6")
  e7<-which(A == "@7");e8<-which(A == "@8");e9<-which(A == "@9")
  e10<-which(A == "@10")
  
  vb1<-read.table(file, skip = e1, nrows =(e2-e1-2))
  it1<-read.table(file, skip = e2, nrows =(e3-e2-2))
  mesh1<-list(vb=t(cbind(as.matrix(vb1),1)),it=t(as.matrix(it1)));class(mesh1)<-"mesh3d"
  vb2<-read.table(file, skip = e3, nrows =(e4-e3-2))
  it2<-read.table(file, skip = e4, nrows =(e5-e4-2))
  mesh2<-list(vb=t(cbind(as.matrix(vb2),1)),it=t(as.matrix(it2)));class(mesh2)<-"mesh3d"
  vb3<-read.table(file, skip = e5, nrows =(e6-e5-2))
  it3<-read.table(file, skip = e6, nrows =(e7-e6-2))
  mesh3<-list(vb=t(cbind(as.matrix(vb2),1)),it=t(as.matrix(it2)));class(mesh3)<-"mesh3d"
  B.Length<-as.numeric(read.table(file, skip = e7, nrows =(e8-e7-2)))
  Landmarks<-as.matrix(read.table(file, skip = e8, nrows =(e9-e8-2)))
  Outline<-as.matrix(read.table(file, skip = e9, nrows =(e10-e9-2)))
  Margins<-as.numeric(read.table(file, skip = e10, nrows = 1))
  
  out<-list("Mesh1"= mesh1,"Mesh2"=mesh2,"Mesh3"=mesh3,
             "B.Length"=B.Length,"Landmarks"=Landmarks,"Outline"=Outline)
  
  return(out)
}
  

