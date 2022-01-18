###### PANNEL B #######


pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_5/Figure_5C_NEW.pdf")

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

beta$X <- NULL
beta$X <- NULL
head(beta, 5)
beta$replicate <- as.factor(beta$replicate)
beta$Voltage <- as.numeric(beta$Voltage)
beta$Set <- as.factor(beta$Set)
str(beta)


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

beta <- dlply(.data = beta, .variables = split_by) ## this function does the actual splitting

for(i in 1:length(beta)){
  beta[[i]]$ID <- rep(i, nrow(beta[[i]]))
}

beta <- do.call(rbind, beta)


current <- subset(beta, Species == "DiDiiiNC" | Species == "Gmouse")

## Make it wide, remove stuff you don't need
current_wide <- current %>% 
  dplyr::select(Voltage, Conductance_Norm, ID, Venom, Species) %>% 
  spread(Venom, Conductance_Norm) %>% 
  mutate(Peak_Diff = -(No-Late))

current_wide <- na.omit(current_wide)
head(current_wide)

# current_again <- gather(current_wide, Venom, Conductance_Norm, Late:Yes)
# head(current_again)

## Only -20 and the species we want!
peaks <- current_wide %>% 
  dplyr::filter( Voltage == -20) %>% 
  dplyr::filter(Species == "Gmouse" | Species == "DiDiiiNC")

peaks$Species <- factor(peaks$Species,levels = c("Gmouse", "DiDiiiNC"))

ggplot(peaks, aes(x = Species, y = Peak_Diff, fill = Species)) +
  stat_summary(fun="mean", geom="bar",
               width = 0.75, alpha = 0.66, position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               size = 0.66, width = 0.1, position = position_dodge(0.72)) +
  geom_jitter(height = 0, width = 0.15) +
  scale_x_discrete(name = "", labels= c("Gmouse", "Triple Mutant")) +
  coord_cartesian(ylim = c(0,0.5)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey3", "goldenrod")) +
  guides(fill = guide_legend(reverse = F, title=""),
         color = guide_legend(reverse = F)) +
  ylab("Diff in Norm Conductance at -20mV") +
  theme_classic(base_size = 22) +
  theme(legend.position="none", element_line(colour = 'black', size = 0.5), axis.title=element_text(size=26), axis.text.x =element_text(size=25))


dev.off()



## STATS ##
##Simple t test
t.test(peaks$Peak_Diff ~ peaks$Species)

means <- peaks %>% 
  #dplyr::filter(Voltage == -20 & Venom != "Yes") %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarize(avg = mean(Peak_Diff), 
                   SEM = sd(Peak_Diff)/sqrt(n())) %>% 
  ungroup()

means


beta
beta2 <- subset(beta, Venom != "Yes" & Voltage == -20)
beta3 <- subset(beta3, Species == "Gmouse" | Species == "DiDiiiNC")
beta3$Species <- relevel(beta3$Species, ref = "Gmouse")

beta3$Voltage <- as.factor(as.character(beta3$Voltage))

model1_lme <- lme(Conductance_Norm ~ Species *Venom , random = ~ 1 | ID/Venom,  data = beta3)
summary(model1_lme)
anova(model1_lme)


means <- beta %>% 
  dplyr::filter(Voltage == -20 & Venom != "Yes") %>% 
  dplyr::group_by(Species, Venom) %>% 
  dplyr::summarize(avg = mean(Conductance_Norm), 
                   SEM = sd(Conductance_Norm)/sqrt(n())) %>% 
  ungroup()

means