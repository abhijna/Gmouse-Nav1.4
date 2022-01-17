###### Figure 3B #######

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_3/Figure_3B.pdf")
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
require(optimx)
library(bbmle)

## Subset only voltage range we need
#beta <- subset(beta, Voltage < -5 & Voltage > -55)
beta <- subset(beta, Voltage < 10 & Voltage > -80 )

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

### giving each recording a unique identifier: 
split_by <- c("Species", "replicate", "Date")
beta <- dlply(.data = beta, .variables = split_by) 
for(i in 1:length(beta)){
  beta[[i]]$ID <- rep(i, nrow(beta[[i]]))
}
beta <- do.call(rbind, beta)



current <- subset(beta, Species == "Gmouse" | Species == "Rat")

## Graphs:

split_it <- c("Species", "Date", "replicate")

peak_diff <- function(thing){
  thing$Species <- as.character(thing$Species)
  thing$Date <- as.character(thing$Date)
  thing$replicate <- as.character(thing$replicate)
  a <- thing$Conductance_Norm[thing$Voltage == -20 & thing$Venom == "Late"]
  b <- thing$Conductance_Norm[thing$Voltage == -20 & thing$Venom == "No"]
  difference <- -(b-a)
  return(c(thing$Species[1], thing$replicate[1], thing$Date[1], difference))
}

thing2 <- dlply(current, split_it, peak_diff)
peaks <- data.frame(do.call(rbind, thing2)) ## merge the data back into one data frame 

colnames(peaks) <- c("Species", "replicate", "Date", "Peak_Diff")
peaks$Peak_Diff <- as.numeric(as.character(peaks$Peak_Diff))


ggplot(peaks, aes(x = Species, y = Peak_Diff, fill = Species)) +
  stat_summary(fun="mean", geom="bar",
               width = 0.75, alpha = 0.66, position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               size = 0.66, width = 0.1, position = position_dodge(0.72)) +
  geom_jitter(height = 0, width = 0.15) +
  scale_x_discrete(name = "", labels= c("Gmouse", "Rat")) +
  coord_cartesian(ylim = c(0,0.5)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey3", "green3")) +
  guides(fill = guide_legend(reverse = F, title=""),
         color = guide_legend(reverse = F)) +
  ylab("Diff in Norm Conductance at -20mV") +
  theme_classic(base_size = 22) +
  theme(legend.position="none", element_line(colour = 'black', size = 0.5), axis.title=element_text(size=26), axis.text.x =element_text(size=25))

dev.off() 


### Statistics: repeated measures anova ###
### These stats went in the manuscript
beta2 <- subset(beta, Venom != "Yes" & Species != "Mouse" & Voltage == -20)
beta2$Species <- relevel(beta2$Species, ref = "Gmouse")
beta2$Voltage <- as.factor(as.character(beta2$Voltage))

model1_lme <- lme(Conductance_Norm ~ Species *Venom , random = ~ 1 | ID/Venom,  data = beta2)
summary(model1_lme)
anova(model1_lme)

## Simple t test for panel b
t.test(peaks$Peak_Diff ~ peaks$Species)
peaks <- na.omit(peaks)
means <- peaks %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarize(avg = mean(Peak_Diff), 
                   SEM = sd(Peak_Diff)/sqrt(n())) %>% 
  ungroup()

means
