## Figure 5A ## saving dim = 10x10

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_5/Figure_5A.pdf")


thing <- expression(paste("Nomalized Conductance (G" ["Na"],")"))
par(mfrow = c(1,1), mar = c(5, 5, 0.3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))



###### PANNEL A #######
## Combining different datasets to get the triple mutant dataset
## Diii
Diii <- read.csv(file = "/Users/abhijnaparigi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Rowe_lab/Electrophys_data/Data/Venom/Activation/High_Venom/DIII_Mutant_Activation.csv", header = T)

## NoC
NoC <- read.csv(file = "/Users/abhijnaparigi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Rowe_lab/Electrophys_data/Data/Venom/Activation/High_Venom/NoC_Mutant_Activation.csv", header = T)
NoC$X <- NULL
NoC$X.1 <- NULL
## Di
Di <- read.csv(file = "/Users/abhijnaparigi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Rowe_lab/Electrophys_data/Data/Venom/Activation/High_Venom/Di_Mutant_Activation.csv", header = T)
Di$Quality <- rep("Good", nrow(Di))
beta <- rbind(Di,Diii,NoC)
beta <- subset(beta, Species == "Gmouse" | Species == "DiDiiiNC")


# ## Diii
# Diii <- read.csv(file = "/Users/abhijnaparigi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Rowe_lab/Electrophys_data/Data/Venom/Activation/High_Venom/DIII_Mutant_Activation.csv", header = T)
# 
# ## NoC
# NoC <- read.csv(file = "/Users/abhijnaparigi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Rowe_lab/Electrophys_data/Data/Venom/Activation/High_Venom/NoC_Mutant_Activation.csv", header = T)
#  
# ## Di
# Di <- read.csv(file = "/Users/abhijnaparigi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Rowe_lab/Electrophys_data/Data/Venom/Activation/High_Venom/Di_Mutant_Activation.csv", header = T)
# Di$Quality <- rep("Good", nrow(Di))
# 
# beta <- rbind(Di,Diii,NoC)
# 
# beta <- subset(beta, Species == "Gmouse" | Species == "DiDiiiNC")

beta$X <- NULL
beta$X <- NULL
head(beta, 5)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
str(beta)
require(sciplot)
require(plyr)
require(optimx)
require(bbmle)
require(tidyverse)
beta <- subset(beta, Voltage < 10 & Voltage > -80)

# First lets estimate Enas:

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
                                       sd = 0.5), start = list(V50 = V50.data,z= z.data, g= g.data, Vr = Vr.data), 
                             control=list(maxit=10000000), data = gdat, method = "Nelder-Mead"))
  ena_one <- as.numeric(coef(MLE.fit)[4])
  dats$Ena <- as.numeric(rep(ena_one, nrow(dats)))
  dats}

split_by <- c("Species", "replicate", "Venom", "Date")

beta <- dlply(.data = beta, .variables = split_by, .fun = grab_enas) ## this function does the actual splitting
beta <- do.call(rbind, beta) ## merge the data back into one data frame 



## Calculate conductance: gna = I/E-Ena

beta$Conductance <- (beta$Current)/(beta$Voltage - beta$Ena)

## I don't need any data for voltage higher than 30mV
## Function for calculating normalized conductance

Norm_C <- function(beta1) {
  beta1$Conductance_Norm <- beta1$Conductance/max(beta1$Conductance)
  beta1
}
beta1 <- dlply(.data = beta, .variables = split_by, .fun = Norm_C)
beta <- do.call(rbind, beta1)


### Fitting the Boltzman equation:

SingleBoltz <- function(V, V50, k) {
  1/(1+ exp((V50 - V)/k))}


grab_V50s <- function(dats, color, pointshape, half_voltage, slope, linetype, yaxislab, xaxislab) {
  gdat <- data.frame(cbind(dats$Voltage, dats$Current, dats$Conductance_Norm))
  colnames(gdat) <- c("Vm","I", "G")
  
  V50.data <- half_voltage 
  k.data <- slope  
  
  MLE.fit <- try(fit <- mle2(G ~ dnorm(mean = SingleBoltz(Vm, V50=V50, k=k), 
                                       sd = s), start = list(V50 = V50.data, k = k.data, s = 0.5), 
                             control=list(maxit=10000000), data = gdat,
                             method = "Nelder-Mead"))
  
  V50_one <- as.numeric(coef(MLE.fit)[1])
  
  lineplot.CI(dats$Voltage, dats$Conductance_Norm,type = "p",
              col = color, lwd = 1.5, bty = 'n', xlab = xaxislab, 
              ylab = yaxislab, x.cont = T, xaxt = "n", yaxt = "n",
              x.leg = -80, y.leg = 0.6, pch = pointshape, ylim = c(0,1),
              xlim = c(-70, 10), cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", 
              cex = 1.7, err.col = color)
  
  curve(SingleBoltz(x,coef(MLE.fit)[[1]],coef(MLE.fit)[[2]]), from = -75, to = 5, add = T, lwd = 1.5, col = color, lty = linetype )
  summary(MLE.fit)
} 


split_by_2 <- c("Species", "Venom")
beta1 <- split(beta, beta[,split_by_2]) 


# ### Triple mutant experiment 
DiDiiiNC_yes <-beta1[[2]]
gmouse_yes <-beta1[[3]]
DiDiiiNC_no <- beta1[[7]]
gmouse_no <- beta1[[8]]


### Fitting a two-state Boltzmann equation (also called a double Boztmann)
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
  P <- 0 
  
  MLE.fit <- try(fit <- mle2(G ~ dnorm(mean = DoubleBoltz(Vm, P=P, V50a=V50a, V50b=V50b, 
                                                          k1=k1, k2=k2), sd = s),start = list(P=P, V50a=V50a, V50b=V50b, k1=k1, 
                                                                                              k2=k2, s = 0.5),control=list(maxit=4000000), method = "Nelder-Mead",  
                             data = gdat))
  
  V50_one <- as.numeric(coef(MLE.fit)[1])
  
  lineplot.CI(dats$Voltage, dats$Conductance_Norm,type = "p",
              col = color, lwd = 1.5, bty = 'n', xlab = xaxislab, 
              ylab = yaxislab, x.cont = T, 
              x.leg = -80, y.leg = 0.6, pch = pointshape, ylim = c(0,1),
              xlim = c(-70, 10), cex.axis = 1.7, cex.lab = 2, err.width = 0.025, 
              bg = "white", 
              cex = 1.7, err.col = color, xaxt = "n", yaxt = "n")
  
  curve(DoubleBoltz(x,coef(MLE.fit)[[1]],coef(MLE.fit)[[2]],coef(MLE.fit)[[3]],
                    coef(MLE.fit)[[4]],coef(MLE.fit)[[5]]), 
        from = -75, to = 5, add = T, lwd = 1.5, col = color, lty = linetype)
  
  return(summary(MLE.fit))}


### Combined triple mutant stuff
grab_V50s(DiDiiiNC_no, "goldenrod3", 17, 1, 1, 1, thing, "Voltage (mV)")
par(new = T)
grab_V50s(gmouse_no, "grey3", 18, 0, 1, 1, "", "")

axis(side = 1, at = seq(-80, 90, 10), lwd = 1.5, cex.axis = 1.7)
axis(side = 2, at = seq(-1, 1, 0.2), lwd = 1.5, cex.axis = 1.7)


dev.off() 