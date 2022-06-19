#' ToothExport
#'
#' Export the output from ToothAlignement 
#' @param input list: output from ToothAlignement
#' @param id character: label name
#' @param file character: name the output file
#' @author Antonio Profico
#' @export

ToothExport<-function(input,id,file){
  
  
cat(paste("# Toothnroll ","version ",packageVersion("morphomap"), "\n", 
          sep = ""), file = file, append = FALSE, sep = "")
cat(paste("# ",id, "\n", 
          sep = ""), file = file, append = TRUE, sep = "")
cat(paste("# ","List of contents", "\r\n", 
          sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Mesh 1 ", "{ 3D mesh vb}", " @1", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Mesh 1 ", "{ 3D mesh it}", " @2", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Mesh 2 ", "{ 3D mesh vb}", " @3", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Mesh 2 ", "{ 3D mesh it}", " @4", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Mesh 3 ", "{ 3D mesh vb}", " @5", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Mesh 3 ", "{ 3D mesh it}", " @6", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("B.Length ", "{ numeric }", " @7", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Landmarks ", "{ 3D coordinates }", " @8", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Outline ", "{ 3D coordinates }", " @9", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("Margins ", "{ 3D coordinates }", " @10", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("### Data ###", "\n", sep = ""), file = file, append = TRUE, sep = "")
if(!is.null(input$almesh1)){
vb1<-vert2points(input$almesh1$mesh)
it1<-t(input$almesh1$mesh$it)
}else {vb1<-NULL;it1<-NULL}
if(!is.null(input$almesh2)){
vb2<-vert2points(input$almesh2$mesh)
it2<-t(input$almesh2$mesh$it)
}else {vb2<-NULL;it2<-NULL}
if(!is.null(input$almesh3)){
vb3<-vert2points(input$almesh3$mesh)
it3<-t(input$almesh3$mesh$it)
}else {vb3<-NULL;it3<-NULL}

cat(paste("@1", "\n", sep = ""), file = file, append = TRUE,sep = "")
write.table(format(vb1, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@2", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(it1, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@3", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(vb2, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@4", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(it2, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@5", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(vb3, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@6", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(it3, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@7", "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste(input$blength, "\n", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@8", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(input$alset, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@9", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(input$outline, scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\n")
cat(paste("\r", sep = ""), file = file, append = TRUE, sep = "")
cat(paste("@10", "\n", sep = ""), file = file, append = TRUE, sep = "")
write.table(format(t(input$margins_sel), scientific = F, trim = T), 
            file = file, sep = " ", append = TRUE, quote = FALSE, 
            row.names = FALSE, col.names = FALSE, na = "", 
            eol = "\r\n")
}

