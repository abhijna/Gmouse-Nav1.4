#### PANNEL D ####
require(tidyverse)
require(sciplot)
require(plyr)
#### PANNEL D ####

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_5/Figure_5D.pdf", height = 10)

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


#beta <- subset(beta, Venom != "Late")
beta$replicate <- factor(beta$replicate)


split_by <- c("Species", "replicate", "Date")
beta <- dlply(.data = beta, .variables = split_by) 
for(i in 1:length(beta)){
  beta[[i]]$ID <- rep(i, nrow(beta[[i]]))
}

beta <- do.call(rbind, beta)

current <- subset(beta, Species == "Gmouse" | Species == "DiDiiiNC")
current$X <- NULL
current$ID <- as.factor(current$ID)


split_it <- c("Species", "Date", "replicate")
peak_diff <- function(thing){
  thing$Species <- as.character(thing$Species)
  thing$Date <- as.character(thing$Date)
  thing$replicate <- as.character(thing$replicate)
  a <- thing$Current[thing$Voltage == 5 & thing$Venom == "Yes"]
  b <- thing$Current[thing$Voltage == 5 & thing$Venom == "No"]
  difference <- -(b-a)
  return(c(thing$Species[1], thing$replicate[1], thing$Date[1], difference, thing$Current[1], thing$Voltage[1]))
}

thing2 <- dlply(current, split_it, peak_diff)
peaks <- data.frame(do.call(rbind, thing2)) ## merge the data back into one data frame 

colnames(peaks) <- c("Species", "replicate", "Date", "Peak_Diff", "Current", "Voltage")
peaks$Peak_Diff <- as.numeric(as.character(peaks$Peak_Diff))
head(peaks)


ggplot(peaks, aes(x = Species, y = Peak_Diff , fill = Species)) +
  stat_summary(fun="mean", geom="bar",
               width = 0.75, alpha = 0.66, position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               size = 0.66, width = 0.1, position = position_dodge(0.72)) +
  geom_jitter(height = 0, width = 0.15) +
  scale_x_discrete(name = "", labels= c("Gmouse", "Triple Mutant")) +
  coord_cartesian(ylim = c(0,1.5)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey3", "goldenrod")) +
  guides(fill = guide_legend(reverse = F, title=""),
         color = guide_legend(reverse = F)) +
  ylab("Diff in Norm Conductance at -20mV") +
  theme_classic(base_size = 22) +
  theme(legend.position="none", element_line(colour = 'black', size = 0.5), axis.title=element_text(size=26), axis.text.x =element_text(size=25))


## Stats to test diff
t.test(peaks$Peak_Diff ~ peaks$Species)

means <- peaks %>% 
  #dplyr::filter(Voltage == 5 & Venom != "Late" & Species != "Mouse") %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarize(avg = mean(Peak_Diff), 
                   SEM = sd(Peak_Diff)/sqrt(n())) %>% 
  ungroup()

means

## Pulling out the means
means <- current %>% 
  dplyr::filter(Voltage == 5 & Venom != "Late" & Species != "Mouse") %>% 
  dplyr::group_by(Species, Venom) %>% 
  dplyr::summarize(avg = mean(Current), 
                   SEM = sd(Current)/sqrt(n())) %>% 
  ungroup()

means


