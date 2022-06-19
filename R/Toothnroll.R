#' @name Toothnroll-package
#' @docType package
#' @aliases Toothnroll
#' @title Equiangular semilandmarks, virtual unrolling, 2D and 3D maps of enamel and dentine thickess
#' @author Antonio Profico
#' @description Tool to process dental meshes (shape data, virtual unrolling, and morphometric maps)
#' @import Arothron
#' @import DescTools
#' @import Morpho
#' @import morphomap
#' @import Rvcg
#' @import colorRamps
#' @import geometry
#' @import grDevices
#' @import graphics
#' @import lattice
#' @import mgcv
#' @import rgdal
#' @import utils
#' @import rgl
#' @importFrom rgl open3d layout3d triangles3d next3d translate3d rotate3d rotationMatrix wire3d axis3d title3d rgl.bbox par3d view3d planes3d spheres3d lines3d
#' @importFrom colorRamps blue2green2red
#' @importFrom Arothron aro.clo.points ext.int.mesh
#' @importFrom morphomap morphomapSort morphomapThickness morphomapCentroid morphomapTri2sects
#' @importFrom Morpho angle.calc vert2points equidistantCurve mcNNindex rmVertex rotmesh.onto rotonmat
#' @importFrom grDevices dev.off tiff 
#' @importFrom graphics points polygon text
#' @importFrom oce matrixSmooth 
#' @importFrom sp CRS point.in.polygon
#' @importFrom stats IQR quantile sd prcomp weighted.mean
#' @importFrom utils setTxtProgressBar txtProgressBar packageVersion
NULL

