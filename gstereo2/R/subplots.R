# r=radius
# nda= 0,1,2
# ngc= 1,2
# basic stereographic projection
gstereo=function(r,nda,ngc)
{gcircle(r,nbb)
  if(nda > 0)
  {gaxes(nda)}
  if(ngc > 0)
  {greats(ngc)}
}

# --------------------------------------

# plots a circle
# r= radius
# annotates x1 x2 x3 and for nbb=0 NO 110s
gcircle=function(r,nbb)
{
  par(pty="s")
  cp1=c(-1.15,1.15)
  cp2=c(-1.15,1.15)
  plot(cp1,cp2,type="p",axes="F",xlab="",ylab="",cex=0)
  dt=(1/sqrt(2))+0.07
  text(0,1.075,expression(paste(x[2])),cex=1.25)
  text(1.075,0.0,expression(paste(x[1])),cex=1.25)
  text(0.075,0.065,expression(paste(x[3])),cex=1.25)
  if(nbb==1)
    {text(dt,dt,expression(paste("[110]")),cex=0.65)
  text(dt,-dt,expression(paste("[1",bar(1),"0]")),cex=0.65)
  text(-dt,dt,expression(paste("[",bar(1),"10]")),cex=0.65)
  text(-dt,-dt,expression(paste("[",bar(1),bar(1),"0]")),
       cex=0.65)}
  n=1000
  cx=r*cos(seq(0,2*pi,by=pi/n))
  cy=r*sin(seq(0,2*pi,by=pi/n))
  lines(cx,cy,type="l",lwd=1.5)
}



#draw axes
# nda=0 none
# nda=1 main only
# nda=2 main + diagonal axes
gaxes=function(nda)
{
  if(nda>0)
  {ptx=c(0,0)
  pty=c(-1,1)
  lines(ptx,pty,lwd=0.25)
  ptx=c(-1,1)
  pty=c(0,0)
  lines(ptx,pty,lwd=0.25)}
  if(nda==2)
  {a=1/sqrt(2)
  ptx=c(-a,a)
  pty=c(-a,a)
  lines(ptx,pty,lwd=0.25)
  ptx=c(a,-a)
  pty=c(-a,a)
  lines(ptx,pty,lwd=0.25)
  }
}

# --------------------------------------

# draw great circles
# ngc=1 to main axes only
# ngc=2 to main and diagonal axes
greats=function(ngc)
{
  if(ngc==1||2)
  {r=sqrt(2)
  n=1000
  cx=r*cos(seq(-pi/4,pi/4,by=pi/n))-1
  cy=r*sin(seq(-pi/4,pi/4,by=pi/n))
  lines(cx,cy,type="l",lwd=0.25)
  lines(-cx,-cy,type="l",lwd=0.25)
  cx=r*cos(seq(pi/4,3*pi/4,by=pi/n))
  cy=r*sin(seq(pi/4,3*pi/4,by=pi/n))-1
  lines(cx,cy,type="l",lwd=0.25)
  lines(-cx,-cy,type="l",lwd=0.25)
  }
  if(ngc==2)
  {a=1/sqrt(2)
  xc=rbind(c(a,a,0),c(a,0,a),c(0,-a,a))
  bigx=xc[,1]/(1+abs(xc[,3]))
  bigy=xc[,2]/(1+abs(xc[,3]))
  xymat=cbind(bigx,bigy)
  cr=gccalc(xymat)
  cmat=c(1,1)
  r=cr[3]
  n=1000
  da=asin((1-1/sqrt(2))/sqrt(3))
  cx=r*cos(seq(pi+da,3*pi/2-da,by=pi/n))+cmat[1]
  cy=r*sin(seq(pi+da,3*pi/2-da,by=pi/n))+cmat[2]
  lines(cx,cy,type="l",lwd=0.25)
  cx1=cx
  cy1=cy
  cx=-cx1
  cy=-cy1
  lines(cx,cy,type="l",lwd=0.25)
  cx=-cx1
  cy=cy1
  lines(cx,cy,type="l",lwd=0.25)
  cx=cx1
  cy=-cy1
  lines(cx,cy,type="l",lwd=0.25)
  }
}

# --------------------------------------

# plot points with color using bigx bigy
# xmat= (x1, x2, x3) (can select)
# clr= color of points
gplot=function(xmat,cx,clr)
{# set up bigx bigy z < 0
  xmat1=subset(xmat,xmat[,3]< -0.0001)
  bigx=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy=xmat1[,2]/(1+abs(xmat1[,3]))
  points(bigx,bigy,type="p",lwd=1.5,pch=1,cex=1.5*cx,col=clr)
# set up bigx bigy z > 0
  xmat1=subset(xmat,xmat[,3]>=0)
  bigx=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy=xmat1[,2]/(1+abs(xmat1[,3]))
  points(bigx,bigy,type="p",pch=16,cex=cx,col=clr)
}

# calculates moment matrix
# xmat = (x,y,z) matrix given
gmmcalc=function(xmat)
{
  npoints=nrow(xmat)
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # set up x^2 y^2 z^2
  xmat2=xmat*xmat
  # interactions
  xy=as.matrix(x*y)
  xz=as.matrix(x*z)
  yz=as.matrix(y*z)
  xint=cbind(xy,xz,yz)
  # complete matrix X
  onevec=matrix(1,npoints,1)
  xtot=cbind(onevec,xmat,xint,xmat2)
  mmat=t(xtot)%*%xtot
  return(mmat)
}



# ---------------------

# draw circle perpendicular to x3 axis
# r = radius of circle
# lwd=line width
gcadd=function(r,lw)
{
  n=1000
  cx=r*cos(seq(0,2*pi,by=pi/n))
  cy=r*sin(seq(0,2*pi,by=pi/n))
  lines(cx,cy,type="l",lwd=lw,col="blue")
}

# -------------------------

# returns centre and radius thru 3 points
# specify in xymat
gccalc=function(xymat)
{x1=xymat[1,1]
y1=xymat[1,2]
x2=xymat[2,1]
y2=xymat[2,2]
x3=xymat[3,1]
y3=xymat[3,2]
a=rbind(c(2*(x2-x1),2*(y2-y1)),
        c(2*(x3-x1),2*(y3-y1)))
b=rbind(y2^2-y1^2+x2^2-x1^2,
        y3^2-y1^2+x3^2-x1^2)
gc=solve(a,b)
r=sqrt((x1-gc[1])^2+(y1-gc[2])^2)
return(c(gc,r))
}

# ----------

# circle through points (xc1, xc2, xc3)
# on stereogram
gplotcircle=function(xc)
{bigx=xc[,1]/(1+abs(xc[,3]))
bigy=xc[,2]/(1+abs(xc[,3]))
xymat=cbind(bigx,bigy)
cr=gccalc(xymat)
# draw a circle
cmat=rbind(c(1,1),c(1,-1),c(-1,1),c(-1,-1))
cmat=cmat*cr[1:2]
r=cr[3]
n=1000
for(j in 1:4)
{cx=r*cos(seq(0,2*pi,by=pi/n))+cmat[j,1]
cy=r*sin(seq(0,2*pi,by=pi/n))+cmat[j,2]
lines(cx,cy,type="l",lwd=1,col="red")}
return(n)
}

# -----------------------------

#function to draw trigonal axes
gtrig=function(lw,clr)
{a=pi/3
ptx=c(sin(a), -sin(a))
pty=c(cos(a), -cos(a))
lines(ptx,pty,lwd=lw,col=clr)
ptx=c(-sin(a), sin(a))
pty=c(cos(a), -cos(a))
lines(ptx,pty,lwd=lw,col=clr)
ptx=c(0,0)
pty=c(-1,1)
lines(ptx,pty,lwd=lw,col=clr)
}

# -----------------------------

#function to draw pentagonal axes
gpenta=function(lw,clr)
{a=4*pi/5
ptx=c(sin(a), -sin(a))
pty=c(cos(a), -cos(a))
lines(ptx,pty,lwd=lw,col=clr)
ptx=c(-sin(a), sin(a))
pty=c(cos(a), -cos(a))
lines(ptx,pty,lwd=lw,col=clr)
a=2*pi/5
ptx=c(sin(a), -sin(a))
pty=c(cos(a), -cos(a))
lines(ptx,pty,lwd=lw,col=clr)
ptx=c(-sin(a), sin(a))
pty=c(cos(a), -cos(a))
lines(ptx,pty,lwd=lw,col=clr)
ptx=c(0,0)
pty=c(-1,1)
lines(ptx,pty,lwd=lw,col=clr)
}

# -----------------------------

# points for bigx bigy with z < 0
gpointsmz=function(xmat)
{
  xmat1=as.matrix(subset(xmat,xmat[,3]< -0.0001))
  bigx=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy=xmat1[,2]/(1+abs(xmat1[,3]))
  bigmat=as.matrix(cbind(bigy,-bigx))
  return(bigmat)
}

# points for bigx bigy with z > 0
gcircrzfunction(xmat)
{
  xmat1=subset(xmat,xmat[,3]> 0.0001)
  bigx=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy=xmat1[,2]/(1+abs(xmat1[,3]))
  bigmat=as.matrix(cbind(bigy,-bigx))
  radii=sqrt(rowSums(bigmat*bigmat))
  return(radii)
}

# -----------------------------

gpointsz0=function(xmat)
{
  # set up bigx bigy z == 0
  xmat1=subset(xmat,abs(xmat[,3])< 0.0001)
  bigx=xmat1[,1]/(1+abs(xmat1[,3]))
  bigy=xmat1[,2]/(1+abs(xmat1[,3]))
  bigmat=as.matrix(cbind(bigy,-bigx))
  return(bigmat)
}
