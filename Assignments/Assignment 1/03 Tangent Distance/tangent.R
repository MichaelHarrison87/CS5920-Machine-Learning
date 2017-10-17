# MH Note: Edited to work with Windows
# dyn.load points to a .dll file, rather than .so

dyn.load("distance.dll")
# tangent distance
distance <- function(imageOne,imageTwo) {
  out <- .C("distance",
    img1=as.double(imageOne),
    img2=as.double(imageTwo),
    dist=as.double(0))
  return(out$dist)
}

USPS <- read.table("USPSsubset.txt")