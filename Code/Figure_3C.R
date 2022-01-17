###### Figure 3C #######
require(sciplot)
require(tidyverse)
require(plyr)
require(optimx)
library(bbmle)

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_3/Figure_3C.pdf")

par(mfrow = c(1,1), mar = c(5, 5, 3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))

## Wildtype
beta <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/activation.csv", header = T)
beta <- subset(beta, Venom != "Late")

head(beta, 5)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
str(beta)


## Unique IDs ##
split_by <- c("Species", "replicate", "Venom", "Date")
beta <- dlply(.data = beta, .variables = split_by) ## this function does the actual splitting

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

### WILDTYPE

species1 <- "Rat" #green3
species2 <- "Mouse" # purple3
species3 <- "Gmouse" #dodgerblue
species4 <- "MouseA_RatB"

beta2 <- subset(beta , select = c(Voltage,Current_Norm, Species, Venom, Date, replicate))
lineplot.CI(beta2$Voltage[beta2$Species == species3], beta2$Current_Norm[beta2$Species == species3], beta2$Venom[beta2$Species == species3], 
            col = "grey3", lwd = 1.5, bty = 'n', xlab = "Voltage (mV)", yaxt = "n", xaxt = "n", ylim = c(-1.2, 0),
            ylab = "Norm Current (I/Imax)", x.cont = T, x.leg = -80, y.leg = 0.8, pch = c(18,23),
            cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", legend =F,
            cex = 2)

par(new = T)
lineplot.CI(beta2$Voltage[beta2$Species == species1], beta2$Current_Norm[beta2$Species == species1], beta2$Venom[beta2$Species == species1], 
            col = "green3", lwd = 1.5, bty = 'n', xlab = NA,
            ylab = NA, x.cont = T, x.leg = -80, y.leg = 0.8, pch = c(16,1), 
            cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", legend =F, yaxt = "n", xaxt = "n",ylim = c(-1.2, 0),
            cex = 1.7)

axis(side = 1, at = seq(-90,90,20), lwd = 1.5, cex.axis = 1.7)
axis(side = 2, at = seq(-2,0,0.2), lwd = 1.5, cex.axis = 1.7)

# mtext("A", 3, adj = -1.9, cex = 1.5, font = 2)
# mtext("B", 3, adj = -0.3, cex = 1.5, font = 2)
# mtext("C", 3, adj = 1.15, cex = 1.5, font = 2)

### Statistics for the paper

#beta2 <- subset(beta, Venom != "Late" & Species != "Mouse" & Voltage == 5)
beta2 <- subset(beta, Venom != "Late" & Species != "Mouse")

beta2$Species <- relevel(beta2$Species, ref = "Gmouse")
beta2$Voltage <- as.factor(as.character(beta2$Voltage))

model1_lme <- lme(Current_Norm ~ Species *Venom*Voltage , random = ~ 1 | ID/Venom,  data = beta2)
summary(model1_lme)
anova(model1_lme)

## Pulling out the numbers:
means <- beta2 %>%
  dplyr::filter(Voltage == 5 & Venom != "Late" & Species != "Mouse") %>%
  dplyr::group_by(Species, Venom) %>%
  dplyr::summarize(avg = mean(Current_Norm),
                   SEM = sd(Current_Norm)/sqrt(n())) %>%
  ungroup()

means

dev.off() 