% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ToothShape.R
\name{ToothShape}
\alias{ToothShape}
\title{ToothShape}
\usage{
ToothShape(
  morphomap.core,
  num.land,
  sects_vector,
  cent.out = "E",
  delta = 0.1,
  direction = "c"
)
}
\arguments{
\item{morphomap.core}{list: morphomap.core object}

\item{num.land}{numeric: number of landmarks defining each section}

\item{sects_vector}{numeric: number of sections}

\item{cent.out}{how to define the center of each section. The method allowed are "CCA" (center of cortical area), "E" (barycenter of the external outline) and "I" (barycenter of the internal outline)}

\item{delta}{pixel size used to calculate the CCA}

\item{direction}{character: specify the desired direction; "c" = clockwise, "a" = anticlockwise}
}
\value{
3D_out num.pointsx3xnum.sect array in which the external outlines are stored

3D_inn num.pointsx3xnum.sect array in which the internal outlines are stored

2D_out num.pointsx2xnum.sect array in which the external outlines are stored

2D_inn num.pointsx2xnum.sect array in which the interal outlines are stored

ALPM_inn array with the coordinates of ALPM coordinates on the external outline

ALPM_out array with the coordinates of ALPM coordinates on the internal outline

mech_length mechanical length of the long bone

start percentage of the mechanical length from which the first section is defined

end percentage of the mechanical length from which the last section is defined
}
\description{
Tool for the extraction of equiangular landmarks from conical structures
}
\author{
Antonio Profico
}
