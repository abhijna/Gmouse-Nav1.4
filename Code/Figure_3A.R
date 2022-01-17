###### Figure 3A #######
require(sciplot)
require(tidyverse)
require(plyr)
require(optimx)
library(bbmle)

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_3/Figure_3A.pdf")

par(mfrow = c(1,1), mar = c(5, 5, 3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))

# Wildtype
beta <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/activation.csv", header = T)

beta$X <- NULL
beta$X <- NULL
head(beta, 5)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
str(beta)
require(sciplot)
require(tidyverse)
require(plyr)
require(optimx)
library(bbmle)

## Subset only voltage range we need
beta <- subset(beta, Voltage < 10 & Voltage > -80)

### Enas using the fuction given in Smith and Goldin 1998:
Rev_Pot <- function(V, V50, z, g, Vr) {
  (1+ exp(-0.039937 * z * (V-V50)))^-1 * g * (V-Vr)}

grab_enas <- function(dats) {
  gdat <- data.frame(cbind(dats$Voltage, dats$Current))
  colnames(gdat) <- c("Vm","I")
  
  V50.data <- -10 
  z.data <- 6
  g.data <- 10
  Vr.data <- 80
  
  MLE.fit <- try(fit <- mle2(I ~ dnorm(mean = Rev_Pot(Vm, V50=V50, z=z, g=g, Vr=Vr), 
                                       sd = 0.5), start = list(V50 = V50.data, z= z.data, g= g.data, Vr = Vr.data), 
                             control=list(maxit=10000000), data = gdat, method = "Nelder-Mead"))
  
  ena_one <- as.numeric(coef(MLE.fit)[4])
  dats$Ena <- as.numeric(rep(ena_one, nrow(dats)))
  dats}

split_by <- c("Species", "replicate", "Venom", "Date")

beta <- dlply(.data = beta, .variables = split_by, .fun = grab_enas) ## this function does the actual splitting
beta <- do.call(rbind, beta) ## merge the data back into one data frame 



## Calculate conductance: gna = I/E-Ena
beta$Conductance <- (beta$Current)/(beta$Voltage - beta$Ena)

## Function for calculating normalized conductance
Norm_C <- function(beta1) {
  beta1$Conductance_Norm <- beta1$Conductance/max(beta1$Conductance)
  beta1
}
beta1 <- dlply(.data = beta, .variables = split_by, .fun = Norm_C)
beta <- do.call(rbind, beta1)


### Fitting the Boltzmann equation:

## Single Boltzmann 
SingleBoltz <- function(V, V50, k) {
  1/(1+ exp((V50 - V)/k))}


grab_V50s <- function(dats, color, pointshape, half_voltage, slope, linetype, yaxislab, xaxislab) {
  gdat <- data.frame(cbind(dats$Voltage, dats$Current, dats$Conductance_Norm))
  colnames(gdat) <- c("Vm","I", "G")
  
  V50.data <- half_voltage 
  k.data <- slope  
  
  MLE.fit <- try(fit <- mle2(G ~ dnorm(mean = SingleBoltz(Vm, V50=V50, k=k), 
                                       sd = s), start = list(V50 = V50.data, k = k.data, s = 0.5), 
                             control=list(maxit=10000000), data = gdat, method = "Nelder-Mead"))
  
  V50_one <- as.numeric(coef(MLE.fit)[1])
  lineplot.CI(dats$Voltage, dats$Conductance_Norm,type = "p",
              col = color, lwd = 1.5, bty = 'n', xlab = xaxislab, 
              ylab = yaxislab, x.cont = T, xaxt = "n", yaxt = "n",
              x.leg = -80, y.leg = 0.6, pch = pointshape, ylim = c(0,1),
              xlim = c(-70, 10), cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", 
              cex = 2, err.col = color)
  
  curve(SingleBoltz(x,coef(MLE.fit)[[1]],coef(MLE.fit)[[2]]), from = -75, to = 5, add = T, 
        lwd = 1.5, col = color, lty = linetype )
  summary(MLE.fit)
} 

split_by_2 <- c("Species", "Venom")
beta1 <- split(beta, beta[,split_by_2]) 


## Assigning species data to variables"

gmouse_yes <-beta1[[1]]
mouse_yes <-beta1[[2]]
rat_yes <-beta1[[3]]
gmouse_no <-beta1[[4]]
mouse_no <-beta1[[5]]
rat_no <-beta1[[6]]


#### Double Boltzmann equation ####

DoubleBoltz <- function(V, P, V50a, V50b, k1, k2){
  ((P/(1 + exp((V50a-V)/k1)) + ((1-P)/(1 + exp((V50b-V)/k2)))))
}


Double_Boltzmann <- function(dats, color, pointshape, half_voltage1, half_voltage2, slope1, slope2, linetype, yaxislab, xaxislab) {
  gdat <- data.frame(cbind(dats$Voltage, dats$Current, dats$Conductance_Norm))
  colnames(gdat) <- c("Vm","I", "G")
  V50a <- half_voltage1 
  V50b <- half_voltage2 
  k1 <- slope1  
  k2 <- slope2 
  P <- 0 # this need to be between 0-1, however this function does not constrain it!
  
  MLE.fit <- try(fit <- mle2(G ~ dnorm(mean = DoubleBoltz(Vm, P=P, V50a=V50a, V50b=V50b, 
                                                          k1=k1, k2=k2), sd = s), start = list(P=P, V50a=V50a, V50b=V50b, k1=k1, 
                                                                                               k2=k2, s = 0.5),control=list(maxit=4000000),method = "Nelder-Mead",  data = gdat))
  V50_one <- as.numeric(coef(MLE.fit)[1])
  
  lineplot.CI(dats$Voltage, dats$Conductance_Norm,type = "p",
              col = color, lwd = 1.5, bty = 'n', xlab = xaxislab, 
              ylab = yaxislab, x.cont = T, 
              x.leg = -80, y.leg = 0.6, pch = pointshape, ylim = c(0,1),
              xlim = c(-70, 10), cex.axis = 1.7, cex.lab = 2, err.width = 0.025, 
              bg = "white", cex = 1.7, err.col = color, xaxt = "n", yaxt = "n")
  
  curve(DoubleBoltz(x,coef(MLE.fit)[[1]],coef(MLE.fit)[[2]],coef(MLE.fit)[[3]],
                    coef(MLE.fit)[[4]],coef(MLE.fit)[[5]]),from = -75, to = 5, add = T, lwd = 1.5,
        col = color, lty = linetype)
  
  return(summary(MLE.fit))}

gmouse_yes <-beta1[[1]]
mouse_yes <- beta1[[2]]
rat_yes <-beta1[[3]]


thing <- expression(paste("Norm Conductance (G" ["Na"],")"))

grab_V50s(gmouse_no, "grey3", 18, 0, 1, 1, thing, "Voltage (mV)")
par(new = T)
grab_V50s(rat_no,"green3",16, 0, 1, 1, "", "")
par(new = T)
Double_Boltzmann(gmouse_yes, "grey3", 23, -8, -20, 6,6,2, "", "")
par(new = T)
Double_Boltzmann(rat_yes,"green3",1, 1, -20, 6,6,2, "", "")

axis(side = 1, at = seq(-80, 90, 10), lwd = 1.5, cex.axis = 1.7)
axis(side = 2, at = seq(-1, 1, 0.1), lwd = 1.5, cex.axis = 1.7)

legend(x = -74, y = 0.9 - 0.01, pch = 18, legend = "Gmouse", col = "grey3", bty = "n", cex = 1.8,  x.intersp = 0.5)
legend(x = -74, y = 1.1 - 0.01, pch = 23, legend = "Gmouse+Venom", col = "grey3", bty = "n", cex = 1.8,  x.intersp = 0.5)
legend(x = -74, y = 0.8 - 0.01, pch = 16, legend = "Rat", col = "green3", bty = "n", cex = 1.8,  x.intersp = 0.5)
legend(x = -74, y = 1.0 - 0.01, pch = 1, legend = "Rat+Venom", col = "green3", bty = "n", cex = 1.8,  x.intersp = 0.5)

dev.off() 
