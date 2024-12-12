# points for radii
#' Calculates radii of circle through
#' points with z + and z -
#'
#' @param  xmat  Matrix X
#'
#' @return radii Radii1 for z>0 and  Radii2 for z < 0
#'
gcalcrz=function(xmat)
{
  xmat1=subset(xmat,xmat[,3] > 0.0001)
  bigx1=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy1=xmat1[,2]/(1+abs(xmat1[,3]))
  bigmat1=as.matrix(cbind(bigy1,-bigx1))
  radii1=sqrt(rowSums(bigmat1*bigmat1))
  xmat2=subset(xmat,xmat[,3]< -0.0001)
  bigx2=xmat2[,1]/(1+abs(xmat2[,3]))
  bigy2=xmat2[,2]/(1+abs(xmat2[,3]))
  bigmat2=as.matrix(cbind(bigy2,-bigx2))
  radii2=sqrt(rowSums(bigmat2*bigmat2))
  radii=rbind(radii1,radii2)
  return(radii)
}
