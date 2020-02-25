# Based on the article: 
  # Menge, D. N. L., A. C. MacPherson, T. A. Bytnerowicz, A. W. Quebbeman, N. B. Schwartz, B. N. Taylor, and A. A. Wolf. 2018. Logarithmic scales in ecological data presentation may cause misinterpretation. Nature Ecology & Evolution:1–13.
# https://stackoverflow.com/questions/6955440/displaying-minor-logarithmic-ticks-in-x-axis-in-r 

log10.axis <- function(side, at, ...) {
  at.minor <- log10(outer(1:9, 10^(min(at):max(at))))
  lab <- sapply(at, function(i) as.expression(bquote(10^ .(i))))
  axis(side=side, at=at.minor, labels=NA, tcl=par("tcl")*0.5, ...)
  axis(side=side, at=at, labels=lab, ...)
}
log10.axis2 <- function(side, at, ...) {
  at.minor <- log10(outer(1:9, 10^(min(at):max(at))))
  lab <- sapply(at, function(x) 10^x)
  axis(side=side, at=at.minor, labels=NA, tcl=par("tcl")*0.5, ...)
  axis(side=side, at=at, labels=lab, ...)
}

# As you can see in this example, when you take the loragithm of the Ys, you can see that the multiplicative factor is what makes the difference between the 2 equations 
# y1 = .5*x => log10(y1) = log10(.5)+log10(x)
# y2 = 1.5*x => log10(y2) = log10(1.5)+log10(x)
# So the difference between the 2 equation is JUST that the line is 1 unit above the other for all X 

x=0:100
y1=.5*x
y2=1.5*x
log10(y1)
# y1=.5*x
# head(log10(y1))
# log10(.5)+log10(0:5)

par(mfrow=c(3,3))
plot(x,y1,
     xlim=c(0,100),ylim=c(0,100),type ="l")
points(x,y2,type ="l", lty = 3)

plot(log10(x),log10(y1),
     xlim=c(0,2),ylim=c(-1,2),type ="l",yaxt="n",xaxt="n")
points(log10(x),log10(y2),type ="l", lty = 3)
log10.axis2(1, at=seq(0, 2, 1))
log10.axis2(2, at=seq(-1, 2, 1))

plot(log10(x),log10(y1),
     xlim=c(0,2),ylim=c(0,2),type ="l")
points(log10(x),log10(y2),type ="l", lty = 3)



# -------------------------------------------------------------------------



maxx=100
x=0:maxx
y1=9*x^(1/4)
y2=0.02*x^2
maxy=round(max(y1,y2),-1)

plot(x,y1,
     xlim=c(0,maxx),ylim=c(0,100),type ="l")
points(x,y2,type ="l", lty = 3)
# Solution to get to the interception of the 2 lines 
abline(h=9*(10^((4*log10(9/0.02))/7))^(1/4), 
       v=10^((4*log10(9/0.02))/7))

plot(log10(x),log10(y1),
     xlim=c(0,2),ylim=c(-1,2),type ="l",yaxt="n",xaxt="n")
points(log10(x),log10(y2),type ="l", lty = 3)
log10.axis2(1, at=seq(0, 2, 1))
log10.axis2(2, at=seq(-1, 2, 1))

plot(log10(x),log10(y1),
     xlim=c(0,2),ylim=c(-1,2.5),type ="l")
points(log10(x),log10(y2),type ="l", lty = 3)


# -------------------------------------------------------------------------
maxx=100
x=0:maxx
y1=.5*x+20
y2=1.5*x+20
maxy=round(max(y1,y2),-2)

plot(x,y1,
     xlim=c(0,maxx),ylim=c(0,maxy),type ="l")
points(x,y2,type ="l", lty = 3)

plot(log10(x),log10(y1),
     xlim=c(0,2),ylim=c(-1,2),type ="l",yaxt="n",xaxt="n")
points(log10(x),log10(y2),type ="l", lty = 3)
log10.axis2(1, at=seq(0, 2, 1))
log10.axis2(2, at=seq(-1, 2, 1))

plot(log10(x),log10(y1),
     xlim=c(0,2),ylim=c(0,2),type ="l")
points(log10(x),log10(y2),type ="l", lty = 3)




par(mfrow = c(1,1))
x= c(0,5.8,7,9.0,9.5)
y = rep(0, length(x))
plot(y~x, pch =21, bg = "black")
# How many powers of 10 are the points from one another 
# In logarithmic scale: a fixed DISTANCE is a SCALING factor 
# Thus the distance between 2 points is the power of 10 rasied by that distance
# For example, the distance on the logarithmic scale between 7 and 5.8 is 1.2. 
7-5.8
# But the HOW LARGE or HOW MANY TIMES bigger was that earthquake? 
10^(7-5.8) # or 10^(1.2)
# So the number of times the largest earthquate was is 
10^(9.5-5.8)
# more than 5000 TIMES BIGGER!!!!!!!!!!!
