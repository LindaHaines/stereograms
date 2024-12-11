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

# -------------------------------------

# tests for t in spherical t-design

# -------------------------------------
# t=2

gdeg2=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree
  dvec22=cbind(x^2,y^2,z^2)
  dvec211=cbind(x*y,x*z,y*z)
  dvectot2=cbind(dvec22,dvec211)
  g=colSums(dvectot2)/nx
  return(g)
}

# -------------------------------------

# t=3
gdeg3=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree 5
  dvec3=cbind(x^3,y^3,z^3)
  dvec21=cbind(x^2*y,x^2*z,y^2*x,y^2*z,z^2*x,z^2*y)
  dvec111=cbind(x*y*z)
  dvectot3=cbind(dvec3,dvec21,dvec111)
  g=colSums(dvectot3)/nx
  return(g)
}

# t=4
gdeg4=function(xmat)
{
nx=nrow(xmat)
# polynomials in x,y,z
x=xmat[,1]
y=xmat[,2]
z=xmat[,3]
# degree 4
dvec4=cbind(x^4,y^4,z^4)
dvec31=cbind(x^3*y,x^3*z,y^3*x,y^3*z,z^3*x,z^3*y)
dvec22=cbind(x^2*y^2,x^2*z^2,y^2*z^2)
dvec211=cbind(x^2*y*z,x*y^2*z,x*y*z^2)
dvectot4=cbind(dvec4,dvec31,dvec22,dvec211)
g=colSums(dvectot4)/nx
return(g)
}

# t=5
gdeg5=function(xmat)
{
nx=nrow(xmat)
# polynomials in x,y,z
x=xmat[,1]
y=xmat[,2]
z=xmat[,3]
# degree 5
dvec5=cbind(x^5,y^5,z^5)
dvec41=cbind(x^4*y,x^4*z,y^4*x,y^4*z,z^4*x,z^4*y)
dvec32=cbind(x^3*y^2,x^3*z^2,y^3*x^2,y^3*z^2,z^3*x^2,z^3*y^2)
dvec311=cbind(x^3*y*z,x*y^3*z,x*y*z^3)
dvec221=cbind(x^2*y^2*z,x^2*y*z^2,x*y^2*z^2)
dvectot5=cbind(dvec5,dvec41,dvec32,dvec311,dvec221)
g=colSums(dvectot5)/nx
return(g)
}

# t=6
gdeg6=function(xmat)
{
nx=nrow(xmat)
# polynomials in x,y,z
x=xmat[,1]
y=xmat[,2]
z=xmat[,3]
# degree 6
dvec6=cbind(x^6,y^6,z^6)
dvec51=cbind(x^5*y,x^5*z,y^5*x,y^5*z,z^5*x,z^5*y)
dvec42=cbind(x^4*y^2,x^4*z^2,y^4*x^2,y^4*z^2,z^4*x^2,z^4*y^2)
dvec33=cbind(x^3*y^3,x^3*z^3,y^3*z^3)
dvec411=cbind(x^4*y*z,x*y^4*z,x*y*4^3)
dvec321=cbind(x^3*y^2*z,x^2*y^3*z,x^3*y*z^2,x^2*y*z^3,x*y^3*z^2,x*y^2*z^3)
dvec222=cbind(x^2*y^2*z^2,x^2*y^2*z^2,x^2*y^2*z^2)
dvectot6=cbind(dvec6,dvec51,dvec42,dvec33,dvec411,dvec321,dvec222)
g=colSums(dvectot6)/nx
return(g)
}

# t=7
gdeg7=function(xmat)
{
nx=nrow(xmat)
# polynomials in x,y,z
x=xmat[,1]
y=xmat[,2]
z=xmat[,3]
# degree 7
dvec7=cbind(x^7,y^7,z^7)
dvec61=cbind(x^6*y,x^6*z,y^6*x,y^6*z,z^6*x,z^6*y)
dvec52=cbind(x^5*y^2,x^5*z^2,y^5*x^2,y^5*z^2,z^5*x^2,z^5*y^2)
dvec43=cbind(x^4*y^3,x^4*z^3,y^4*x^3,y^4*z^3,z^4*x^3,z^4*y^3)
dvec511=cbind(x^5*y*z,x*y^5*z,x*y*z^5)
dvec421=cbind(x^4*y^2*z,x^2*y^4*z,x^4*y*z^2,x^2*y^4*z,x*y^4*z^2,x*y^2*z^4)
dvec331=cbind(x^3*y^3*z,x^3*y^3*z,x^3*y*z^3,x^3*y*z^3,x*y^3*z^3,x*y^3*z^3)
dvec322=cbind(x^3*y^2*z^2,x^2*y^3*z^2,x^2*y^2*z^3)
dvectot7=cbind(dvec7,dvec61,dvec52,dvec43,dvec511,dvec421,dvec331,dvec322)
g=colSums(dvectot7)/nx
return(g)
}

# ------------------------

gdeg8=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree 8 even
  dvec8=cbind(x^8)
  dvec62=cbind(x^6*y^2)
  dvec44=cbind(x^4*y^4)
  dvec422=cbind(x^4*y^2*z^2)
  # degree 8 odd
  dvec71=cbind(x^7*y)
  dvec611=cbind(x^6*y*z)
  dvec53=cbind(x^5*y^3)
  dvec521=cbind(x^5*y^2*z)
  dvec431=cbind(x^4*y^3*z)
  dvec332=cbind(x^3*y^3*z^2)
  dvectot8=cbind(dvec8,dvec62,dvec44,dvec422,
                 dvec71,dvec611,dvec53,dvec521,
                 dvec431,dvec332)
  g=colSums(dvectot8)/nx
  return(g)
}

# ----------------------

gdeg9=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree 9
  dvec9=cbind(x^9)
  dvec81=cbind(x^8*y^1)
  dvec72=cbind(x^7*y^2)
  dvec711=cbind(x^7*y*z)
  dvec63=cbind(x^6*y^3)
  dvec621=cbind(x^6*y^2*z)
  dvec54=cbind(x^5*y^4)
  dvec531=cbind(x^5*y^3*x)
  dvec522=cbind(x^5*y^2*x^2)
  dvec441=cbind(x^4*y^4*x)
  dvec432=cbind(x^4*y^3*x^2)
  dvectot9=cbind(dvec9,dvec81,dvec72,dvec711,
                 dvec63,dvec621,dvec54,dvec531,dvec522,
                 dvec441,dvec432)
  g=colSums(dvectot9)/nx
  return(g)
}

# ----------------------

gdeg10=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree 10 even
  dvec10=cbind(x^10)
  dvec82=cbind(x^8*y^2)
  dvec64=cbind(x^6*y^4)
  dvec622=cbind(x^6*y^2*z^2)
  dvec442=cbind(x^4*y^4*z^2)
  # degree 10 odd
  dvec91=cbind(x^9*y)
  dvec811=cbind(x^8*y*z)
  dvec73=cbind(x^7*y^3)
  dvec721=cbind(x^7*y^2*z)
  dvec631=cbind(x^6*y^3*z)
  dvec541=cbind(x^5*y^4*z)
  dvec532=cbind(x^5*y^3*z^2)
  dvectot10=cbind(dvec10,dvec82,dvec64,dvec622,
                  dvec442,dvec91,dvec811,dvec73,dvec721,dvec631,
                  dvec541, dvec532)
  g=colSums(dvectot10)/nx
  return(g)
}

# ----------------------

gdeg11=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree 11
  dvec11=cbind(x^11)
  dvec101=cbind(x^10*y^1)
  dvec92=cbind(x^9*y^2)
  dvec911=cbind(x^9*y*z)
  dvec83=cbind(x^8*y^3)
  dvec821=cbind(x^8*y^2*z)
  dvec74=cbind(x^7*y^4)
  dvec731=cbind(x^7*y^3*z)
  dvec722=cbind(x^7*y^2*z^2)
  dvec65=cbind(x^6*y^5)
  dvec641=cbind(x^6*y^4*z)
  dvec632=cbind(x^6*y^3*z^2)
  dvec551=cbind(x^5*y^5*x)
  dvec542=cbind(x^5*y^4*x^2)
  dvec533=cbind(x^5*y^3*x^3)
  dvectot11=cbind(dvec11,dvec101,dvec92,dvec911,
                  dvec83,dvec821,dvec74,dvec731,dvec722,dvec65,
                  dvec641,dvec632,dvec551,dvec542,dvec533)
  g=colSums(dvectot11)/nx
  return(g)
}

# -----------------------

gdeg12=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree 12 even
  dvec12=cbind(x^12)
  dvec102=cbind(x^10*y^2)
  dvec84=cbind(x^8*y^4)
  dvec822=cbind(x^8*y^2*z^2)
  dvec66=cbind(x^6*y^6)
  dvec642=cbind(x^6*y^4*z^2)
  dvec444=cbind(x^4*y^4*z^4)
  # degree 12 odd
  dvec111=cbind(x^11*y)
  dvec1011=cbind(x^10*y*z)
  dvec93=cbind(x^9*y^3)
  dvec921=cbind(x^9*y^2*z)
  dvec831=cbind(x^8*y^3*z)
  dvec75=cbind(x^7*y^5)
  dvec741=cbind(x^7*y^4*z)
  dvec732=cbind(x^7*y^3*z^2)
  dvec651=cbind(x^6*y^3*z^3)
  dvec633=cbind(x^6*y^3*z^3)
  dvectot12=cbind(dvec12,dvec102,dvec84,dvec822,
                  dvec66,dvec642,dvec444,dvec111,dvec1011,
                  dvec93,dvec921,dvec831,dvec75,dvec741,
                  dvec732,dvec651,dvec633)
  g=colSums(dvectot12)/nx
  return(g)
}

# -----------------------

gdeg13=function(xmat)
{
  nx=nrow(xmat)
  # polynomials in x,y,z
  x=xmat[,1]
  y=xmat[,2]
  z=xmat[,3]
  # degree 13
  dvec13=cbind(x^13)
  dvec121=cbind(x^12*y^1)
  dvec112=cbind(x^11*y^2)
  dvec1111=cbind(x^11*y*z)
  dvec103=cbind(x^10*y^3)
  dvec1021=cbind(x^10*y^2*z)
  dvec94=cbind(x^9*y^4)
  dvec931=cbind(x^9*y^3*z)
  dvec922=cbind(x^9*y^2*z^2)
  dvec85=cbind(x^8*y^5)
  dvec841=cbind(x^8*y^4*z)
  dvec832=cbind(x^8*y^3*z^2)
  dvec76=cbind(x^7*y^6)
  dvec751=cbind(x^7*y^5*z)
  dvec733=cbind(x^7*y^3*z^3)
  dvec661=cbind(x^6*y^6*x)
  dvec652=cbind(x^6*y^5*x^2)
  dvec643=cbind(x^6*y^4*x^3)
  dvec553=cbind(x^5*y^5*x^3)
  dvectot13=cbind(dvec13,dvec121,dvec112,dvec1111,
                  dvec103,dvec1021,dvec94,dvec931,dvec922,
                  dvec85,dvec841,dvec832,dvec76,dvec751,dvec733,
                  dvec661,dvec652,dvec643,dvec553)
  g=colSums(dvectot13)/nx
  return(g)
}
