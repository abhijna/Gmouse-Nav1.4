### Figure 4C ####
require(effects)
require(car)
require(sciplot)
require(plyr)
library(bbmle)
library(nlme)
library(lme4)

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_4/Figure_4B.pdf")

beta <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/wt_decay_constant.csv", header = T)
par(mfrow = c(1,1), mar = c(5, 5, 0.3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))

beta$X <- NULL
head(beta, 5)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
beta$Date <- as.factor(as.character(beta$Date))
beta$Decay_Time <- as.numeric(as.character(beta$Decay_Time))
beta$Decay_Slope <- as.numeric(as.character(beta$Decay_Slope))
beta$Tau_Fast <- as.numeric(as.character(beta$Tau_Fast))
beta$Tau_Slow <- as.numeric(as.character(beta$Tau_Slow))


### Unique identifier for oocytes ###

split_by <- c("Species", "replicate", "Date")

beta <- dlply(.data = beta, .variables = split_by) ## this function does the actual splitting

for(i in 1:length(beta)){
  beta[[i]]$ID <- rep(i, nrow(beta[[i]]))
}

beta <- do.call(rbind, beta)
beta$ID <- as.character(beta$ID)


beta2 <- subset(beta, Species == "Gmouse" & Voltage > -20 & Voltage < 30)
Decay_Time_Diff <- (beta2$Decay_Time[beta2$Venom == "No"] - beta2$Decay_Time[beta2$Venom == "Yes"])/beta2$Decay_Time[beta2$Venom == "No"]
Voltage <- beta2$Voltage[beta2$Venom == "Yes"]
Species <- rep("Gmouse", length(Voltage))
ID <- beta2$ID[beta2$Venom == "No"]
Date <- beta2$Date[beta2$Venom == "No"]
Set <- beta2$Set[beta2$Venom == "No"]
Gmouse <- data.frame(Decay_Time_Diff, Voltage, Species, ID, Date, Set)

lineplot.CI(Voltage, Decay_Time_Diff, col ="grey3", x.cont = T, xlim = c(-15,35), 
            ylim = c(-0.5, 0.1), pch =18, cex = 2, cex.axis = 2, cex.lab = 2, bty = "n", xlab = "Voltage (mV)", ylab = "Prop. change in time constant",  xaxt = "n", yaxt = "n", err.width = 0.025)

par(new = T)


beta2 <- subset(beta, Species == "Rat" & Voltage > -20 & Voltage < 35)
Decay_Time_Diff <- (beta2$Decay_Time[beta2$Venom == "No"] - beta2$Decay_Time[beta2$Venom == "Yes"])/beta2$Decay_Time[beta2$Venom == "No"]
Voltage <- beta2$Voltage[beta2$Venom == "Yes"]
Species <- rep("Rat", length(Voltage))
ID <- beta2$ID[beta2$Venom == "No"]
Date <- beta2$Date[beta2$Venom == "No"]
Set <- beta2$Set[beta2$Venom == "No"]
Rat <- data.frame(Decay_Time_Diff, Voltage, Species, ID, Date, Set)

lineplot.CI(Voltage, Decay_Time_Diff, col ="green3", x.cont = T, xlim = c(-15,35), ylim = c(-0.5, 0.1), pch =16, cex = 2, cex.axis = 2, cex.lab = 2.3, bty = "n", xlab = NA, ylab = NA,  xaxt = "n", yaxt = "n", err.width = 0.025)


axis(side = 1, at = seq(-20,70, 10), pos = -0.5, lwd = 1.5, cex.axis = 2)
axis(side = 2, at = seq(-0.5,1, 0.1), lwd = 1.5, cex.axis = 2)
legend(x = 10, y = 0.14, pch = 16, legend = "Rat", col = "green3", bty = "n", cex = 1.8,  x.intersp = 0.35)
legend(x = 10, y = 0.1, pch = 18, legend = "Gmouse", col = "grey3", bty = "n", cex = 1.8,  x.intersp = 0.35)



# Stats
beta3 <- rbind(Gmouse, Rat)
#beta3 <- subset(beta, Species != "Mouse" & Voltage > -20 & Voltage < 30)

beta3$Voltage <- as.factor(beta3$Voltage)
beta3 <- na.omit(beta3)

model6 <- lme(Decay_Time_Diff ~ Voltage+Species, random = ~ 1 | ID/Voltage, data = beta3)
summary(model6)
anova(model6)

dev.off()