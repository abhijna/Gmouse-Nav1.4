#### Figure 6 C ####

require(sciplot)
require(plyr)
library(bbmle)
require(nlme)
require(tidyverse)


pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_6/Figure_6C.pdf")
par(mfrow = c(1,1), mar = c(5, 5, 0.3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))
thing <- expression(paste("Norm Conductance (G" ["Na"],")"))

#### PANNEL E ####

beta <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/diii_activation.csv", header = T)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)

beta <- subset(beta, Voltage < 10 & Voltage > -80 )

# First lets try the equation for estimate Enas:

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

beta$Conductance <- (beta$Current)/(beta$Voltage - beta$Ena)

## Function for calculating normalized conductance
Norm_C <- function(beta1) {
  beta1$Conductance_Norm <- beta1$Conductance/max(beta1$Conductance)
  beta1
}
beta1 <- dlply(.data = beta, .variables = split_by, .fun = Norm_C)
beta <- do.call(rbind, beta1)


### giving each recording a unique identifier: 
split_by <- c("Species", "replicate", "Date")

beta <- dlply(.data = beta, .variables = split_by) ## this function does the actual splitting

for(i in 1:length(beta)){
  beta[[i]]$ID <- rep(i, nrow(beta[[i]]))
}

beta <- do.call(rbind, beta)


species1 <- "DiDiiiNC"
color <- "deepskyblue3"
species3 <- "Gmouse"
species2 <- "Diii"


beta2 <- subset(beta, Venom != "Yes", select = c(Voltage, Conductance, Conductance_Norm,  
                                                 Current, Species, Venom, Date, replicate, Ena))

for(i in (1: length(beta2$Species))) {
  if(beta2$Venom[i] == "Late") {beta2$Venom[i] <- "Yes"}
}

lineplot.CI(beta2$Voltage[beta2$Species == species1], beta2$Conductance_Norm[beta2$Species == species1], 
            beta2$Venom[beta2$Species == species1], 
            col = "goldenrod3", lwd = 1.5, bty = 'n', xlab = "Voltage (mV)", xaxt = "n", yaxt = "n",
            ylab = thing , x.cont = T, type = "b",
            x.leg = -80, y.leg = 0.8,  
            pch = c(17,2), ylim = c(0,0.6), xlim = c(-50, 0),
            cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", legend =F,
            cex = 1.7)
axis(side = 1, at = seq(-90,90,5), lwd = 1.5, cex.axis = 1.7)
axis(side = 2, at = seq(-1,1.2,0.1), lwd = 1.5, cex.axis = 1.7)

par(new = T)

# Gmouse
lineplot.CI(beta2$Voltage[beta2$Species == species3], beta2$Conductance_Norm[beta2$Species == species3], 
            beta2$Venom[beta2$Species == species3], 
            col = "grey3", lwd = 1.5, bty = 'n', xlab = "", type = "b",
            ylab = "", x.cont = T, xaxt = "n", yaxt = "n",
            x.leg = -80, y.leg = 0.8,  
            pch = c(18,23), ylim = c(0,0.6), xlim = c(-50, 0),
            cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", legend =F,
            cex = 1.7)
par(new = T)

lineplot.CI(beta2$Voltage[beta2$Species == species2], beta2$Conductance_Norm[beta2$Species == species2], 
            beta2$Venom[beta2$Species == species2], 
            col = "deepskyblue3", lwd = 1.5, bty = 'n', xlab = "", type = "b",
            ylab = "", x.cont = T, xaxt = "n", yaxt = "n",
            x.leg = -80, y.leg = 0.8,  
            pch = c(9,8), ylim = c(0,0.6), xlim = c(-50, 0),
            cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", legend =F,
            cex = 1.7)

legend(x = -52, y = 0.6 + 0.05, pch = 9, legend = "mDIII", col = color, bty = "n", cex = 1.8,  x.intersp = 0.4)
legend(x = -52, y = 0.55 + 0.05, pch = 8, legend = "mDIII + Venom", col = color, bty = "n", cex = 1.8,  x.intersp = 0.4)

# mtext("C", 3, adj = -1.75, cex = 2, font = 2)
# mtext("D", 3, adj = -0.36, cex = 2, font = 2)


dev.off()
