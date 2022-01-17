#### Figure 5B ####
pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_5/Figure_5B.pdf")

thing <- expression(paste("Nomalized Conductance (G" ["Na"],")"))
par(mfrow = c(1,1), mar = c(5, 5, 0.3, 2) + 0.1, omi = c(bottom = 0, left=0.5, top=0, right=0.3))


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



beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
require(sciplot)
require(plyr)
library(bbmle)
require(nlme)

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
color <- "goldenrod3"
species3 <- "Gmouse"

beta2 <- subset(beta, (Species == "Gmouse" | Species == "DiDiiiNC") & Venom != "Late", 
                select = c(Voltage, Conductance, Conductance_Norm, Current, Species, 
                           Venom, Date, replicate, Ena))


lineplot.CI(beta2$Voltage[beta2$Species == species1], beta2$Conductance_Norm[beta2$Species == species1], 
            beta2$Venom[beta2$Species == species1], 
            col = color, lwd = 1.5, bty = 'n', xlab = "Voltage (mV)", xaxt = "n", yaxt = "n",
            ylab = thing , x.cont = T, type = "b",
            x.leg = -80, y.leg = 0.8,  
            pch = c(17,2), ylim = c(0,0.6), xlim = c(-50, 0),
            cex.axis = 1.7, cex.lab = 2, err.width = 0.025, bg = "white", legend =F,
            cex = 2.3)
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


legend(x = -52, y = 0.5 + 0.05, pch = 18, legend = "Gmouse", col = "grey3", bty = "n", cex = 1.8,  x.intersp = 0.23)
legend(x = -52, y = 0.6 + 0.05, pch = 23, legend = "Gmouse+Venom", col = "grey3", bty = "n", cex = 1.8,  x.intersp = 0.23)
legend(x = -52, y = 0.45 + 0.05, pch = 17, legend = "Triple", col = "goldenrod3", bty = "n", cex = 1.8, x.intersp = 0.23)
legend(x = -52, y = 0.55 + 0.05, pch = 2, legend = "Triple+Venom", col = "goldenrod3", bty = "n", cex = 1.8,  x.intersp = 0.23)

mtext("A", 3, adj = -1.8, cex = 2, font = 2, line = -1.5)
mtext("B", 3, adj = -0.38, cex = 2, font = 2, line = 3)


## comparing mean difference in coductance
#t.test(peaks$Peak_Diff ~ peaks$Species)


### Statistics: repeated measures anova ###

beta2 <- subset(beta, Venom != "Late" & Voltage == -20)

beta2$Species <- relevel(beta2$Species, ref = "Gmouse")


beta2$Voltage <- as.factor(as.character(beta2$Voltage))

model1_lme <- lme(Conductance_Norm ~ Species *Venom , random = ~ 1 | ID/Venom,  data = beta2)
summary(model1_lme)
anova(model1_lme)

## Summary statistics

means <- beta %>% 
  dplyr::filter(Voltage == -20 & Venom != "Yes") %>% 
  dplyr::group_by(Species, Venom) %>% 
  dplyr::summarize(avg = mean(Conductance_Norm), 
                   SEM = sd(Conductance_Norm)/sqrt(n())) %>% 
  ungroup()

means

dev.off() 
