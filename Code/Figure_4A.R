#### Figure 4A ####

require(effects)
require(car)
require(sciplot)
require(plyr)
library(bbmle)
library(nlme)
library(lme4)

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_4/Figure_4A.pdf")
par(mfrow = c(1,1), mar = c(5, 5, 0.3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))

beta <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/wt_decay_constant.csv", header = T)

beta$X <- NULL
head(beta, 5)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
beta$Date <- as.factor(as.character(beta$Date))
str(beta)


### Unique identifier for oocytes ###

split_by <- c("Species", "replicate", "Date")
beta <- dlply(.data = beta, .variables = split_by)
for(i in 1:length(beta)){
  beta[[i]]$ID <- rep(i, nrow(beta[[i]]))
}

beta <- do.call(rbind, beta)
beta$ID <- as.character(beta$ID)

beta2 <- subset(beta, Species == "Rat")
lineplot.CI(Voltage, Decay_Time, Venom, data = beta2, 
            col = "green3", lwd = 1.5, 
            err.width = 0.025, xlab = "", pch = c(16,1), ylab = "",  xlim = c(-15,30), ylim = c(0,5), xaxt = "n", yaxt = "n",
            cex = 1.7, cex.axis = 1.7, cex.lab =2, bty = "n", x.cont = T, legend = F, lty = 1, x.leg = 0)

legend(x = 5, y = 5+0.3, pch = 16, legend = "Rat", col = "green3", bty = "n", cex = 1.8,  x.intersp = 0.35)
legend(x = 5, y = 4.6+0.3, pch = 1, legend = "Rat+Venom", col = "green3", bty = "n", cex = 1.8,  x.intersp = 0.35)


par(new = T)

beta2 <- subset(beta, Species == "Gmouse")
lineplot.CI(Voltage, Decay_Time, Venom, data = beta2, 
            col = "grey3", lwd = 1.5, 
            err.width = 0.025, xlab = "Voltage(mV)", pch = c(18,23), ylab = "Current decay time constant (ms)",xlim = c(-15,30), ylim = c(0,5),xaxt = "n", yaxt = "n",
            cex = 1.7, cex.axis = 1.7, cex.lab =2, bty = "n", x.cont = T, legend = F, lty = 1, x.leg = 0)

legend(x = 5, y = 4.2+0.3, pch = 18, legend = "Gmouse", col = "grey3", bty = "n", cex = 1.8,  x.intersp = 0.35)
legend(x = 5, y = 3.8+0.3, pch = 23, legend = "Gmouse+Venom", col = "grey3", bty = "n", cex = 1.8,  x.intersp = 0.35)
axis(side = 1, at = seq(-90,30, 10), pos = 0, lwd = 1.5, cex.axis = 2)
axis(side = 2, at = seq(0,5, 0.5), lwd = 1.5, cex.axis = 2)



## STATS

## No Venom
beta3 <- select(beta, Voltage, Decay_Time, Species, replicate, Venom, ID)
beta3 <- dplyr::filter(beta3, Voltage > -20 & Voltage < 35 & Venom == "No" & Species != "Mouse")
beta3$Voltage <- as.factor(beta3$Voltage)
beta3 <- na.omit(beta3)
model6 <- lme(Decay_Time ~ Voltage * Species, random = ~ 1 | ID/Voltage, data = beta3)
summary(model6)
anova(model6)


## IN the presence of  Venom
beta3 <- select(beta, Voltage, Decay_Time, Species, replicate, Venom, ID)
beta3 <- dplyr::filter(beta3, Voltage > -20 & Voltage < 35  & Species != "Mouse" & Venom == "Yes")
beta3$Voltage <- as.factor(beta3$Voltage)
beta3 <- na.omit(beta3)
model6 <- lme(Decay_Time ~ Voltage * Species, random = ~ 1 | ID/Voltage, data = beta3)
summary(model6)
anova(model6)

## Finding the mean at 0mV in the absence of venom
beta3 <- select(beta, Voltage, Decay_Time, Species, replicate, Venom, ID)
means <- beta3 %>% 
  group_by(Venom, Species) %>% 
  dplyr::filter(Voltage == 0 & Species != "Mouse") %>% 
  dplyr::summarize(avg = mean(Decay_Time), 
                   SEM = sd(Decay_Time)/sqrt(n())) %>% 
  ungroup()

means

dev.off()
