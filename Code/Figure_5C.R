#### Panel 5C #### 

require(sciplot)
require(tidyverse)
require(plyr)
require(optimx)
library(bbmle)

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_5/Figure_5C.pdf")

thing <- expression(paste("Nomalized Conductance (G" ["Na"],")"))
par(mfrow = c(1,1), mar = c(5, 5, 0.3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))


## Combining different datasets to get the triple mutant dataset
## Diii
Diii <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/diii_activation.csv", header = T)

## NoC
NoC <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/noc_activation.csv", header = T)
NoC$X <- NULL
NoC$X.1 <- NULL
## Di
Di <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/di_activation.csv", header = T)
Di$Quality <- rep("Good", nrow(Di))
beta <- rbind(Di,Diii,NoC)
beta <- subset(beta, Species == "Gmouse" | Species == "DiDiiiNC")


beta <- subset(beta, Venom != "Late")

head(beta, 5)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
str(beta)
require(sciplot)
require(plyr)
library(bbmle)

split_by <- c("Species", "replicate", "Venom", "Date")
beta <- dlply(.data = beta, .variables = split_by) 

for(i in 1:length(beta)){
  beta[[i]]$ID <- rep(i, nrow(beta[[i]]))
}

beta <- do.call(rbind, beta)


## Function for calculating normalized current by peak of venom trace
Norm_C <- function(beta1) {
  thing <- -min(beta1$Current[beta1$Venom == "No"])
  beta1$Current_Norm <- beta1$Current/thing
  beta1
}
split_by <- c("Species", "replicate", "Date")

beta2 <- dlply(.data = beta, .variables = split_by, .fun = Norm_C)
beta <- do.call(rbind, beta2)


species1 <- "DiDiiiNC" #green3
species3 <- "Gmouse" #dodgerblue

beta2 <- subset(beta , select = c(Voltage,Current_Norm, Species, Venom, Date, replicate))

lineplot.CI(beta2$Voltage[beta2$Species == species3], beta2$Current_Norm[beta2$Species == species3], 
            beta2$Venom[beta2$Species == species3], col = "grey3", lwd = 3, bty = 'n', xlab = "Voltage (mV)", 
            xaxt = "n", yaxt = "n", ylab = "Normalized Current", x.cont = T, x.leg = -80, y.leg = 0.8, 
            pch = c(18,23), ylim = c(-1.2,0), cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", 
            legend =F, cex = 1.7)

par(new = T)
lineplot.CI(beta2$Voltage[beta2$Species == species1], beta2$Current_Norm[beta2$Species == species1], beta2$Venom[beta2$Species == species1], 
            col = "goldenrod3", lwd = 3, bty = 'n', xlab = NA, 
            ylab = "", x.cont = T, x.leg = -80, y.leg = 0.8, pch = c(17,2), ylim = c(-1.2,0),
            cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", legend =F, , xaxt = "n", yaxt = "n",
            cex = 1.7)

axis(side = 1, at = seq(-90,90,10), lwd = 1.5, cex.axis = 1.7)
axis(side = 2, at = seq(-1.4,0,0.2), lwd = 1.5, cex.axis = 1.7)


## pulling out the numbers:
means <- beta2 %>% 
  dplyr::filter(Voltage == 5 & Venom != "Late" & Species != "Mouse") %>% 
  dplyr::group_by(Species, Venom) %>% 
  dplyr::summarize(avg = mean(Current_Norm), 
                   SEM = sd(Current_Norm)/sqrt(n())) %>% 
  ungroup()

means

dev.off()