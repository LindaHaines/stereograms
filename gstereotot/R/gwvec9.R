#' Weights for Euclidean 9-design on
#' Yamamoto, Hirao, Sawa (2019)
#'
#' @param A   Parameter for calculating weights
#'            a=square root of A
#' @param r2  Radius of concentric circle
#'
#' @return wvec Vector of weights
#'              (w2,w3,wint,v1,v2,v3)
#' @export
#'
#' @examples
wvvector=function(A,r2)
{
# J1
w2min = 8*(A-5)*(A-1)^2/(5*(A+2)^3)
w3min = 9*A*(A^2-2*A-29)/(10*(A+2)^3)
totj1 = 12*w2min + 8*w3min + 24
# J2
R2=r2^2
v1min = (13-A)*(A-1)^2*A/((A+2)^4*R2^4)
v2min = 8*(13-A)*(A-1)^2*A/(5*(A+2)^4*R2^4)
v3min = 9*(13-A)*(A-1)^2*A/(10*(A+2)^4*R2^4)
totj2=(6*v1min+12*v2min+8*v3min)
# total weights =1
tot = totj1 + totj2
gw = 1/tot
# J1 weights
w2=12*w2min*gw
w3=8*w3min*gw
wint=24*gw
# J2 weight2
v1=6*v1min*gw
v2=12*v2min*gw
v3=8*v3min*gw
# weights vector
wvvec=c(w2,w3,wint,v1,v2,v3)
return(wvvec)
}
