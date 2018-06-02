## --------------------- K Functions in SPATSTAT
##
## Shaun Keegan
## University of Liverpool
## Course Details: Glasgow 02 Jun 2018
## 


## ----------------------------------------------------------- LOAD DATA 

setwd ("~/Documents")
data <- read.csv("BU_data.csv", header = T)

## --------------------------------------------------------- LOAD PACKAGE

library("spatstat")

## ------------------------------------------------- SUBSET INFECTION DATA

head(data)
data$Infec <- ifelse(data$BU_TOT > 0, 1, 0)
datInf <- data[which (data$Infec == 1),]

## ----------------------------------------------------- PLOT POINT PATTERN

win <- as.owin(c(1.7,2.6,6.5,7.2))
coords <- cbind(data$Long,data$Lat)
pp <- as.ppp(coords, win)


plot(win)
points(pp, pch = 20)

coordsINF <- cbind(datInf$Long,datInf$Lat)
pp.inf <- as.ppp(coordsINF, win)
points(pp.inf, col = "red", pch = 20)


## ---------------------------------------------------------- PLOT DENSITY

dens.loc <- density(pp)
dens.inf <- density(pp.inf)
par(mfrow=c(2,1))
plot(dens.loc)
plot(dens.inf)

plot(dens.loc)
points(pp, col = "lightblue", pch = "*")
plot(dens.inf)
points(pp.inf, col = "lightblue", pch = "*")


dens.inf.sigma1 <- density(pp.inf)
plot(dens.inf.sigma1)
points(pp, col = "lightblue", pch = "*")
dens.inf.sigma2 <- density(pp.inf, sigma = bw.ppl(pp.inf))
plot(dens.inf.sigma2)
points(pp, col = "lightblue", pch = "*")

## ---------------------------------------------------------- PLOT K FUNC

par(mfrow = c(1,1))
K <- (Kest(pp.inf))
plot(K)

Krmax <- (Kest(pp.inf, rmax = 0.5))
plot(Krmax)

Kenv <- (envelope(pp.inf, fun = Kest, correction = "iso"))
plot(Kenv)

Kenvrmax <- (envelope(pp.inf, fun = Kest, correction = "iso", rmax = 0.4))
plot(Kenv)

par(mfrow = c(1,2))
Kenv <- (envelope(pp.inf, fun = Kest, correction = "iso"))
plot(Kenv)

Kloh <- (lohboot(pp.inf, fun = Kest, correction = "iso"))
plot(Kloh)



## ---------------------------------------------------------- PLOT L FUNC

Lenvrmax <- (envelope(pp.inf, fun = Lest, correction = "iso", rmax = 0.4))
plot(Lenv)

## -------------------------------------------------------- PLOT ALL STATS

Allstats <- allstats(pp.inf)
plot(Allstats)
