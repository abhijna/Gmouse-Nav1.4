#### Ashlee's muscle twitch peak data ####

library(cowplot)
require(tidyverse)
require(sciplot)
require(plyr)

#### Treatment ####
muscle <- read.csv("/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/muscle_peak_tension.csv", h = T)
head(muscle)

## Converting some variables into the right classes
muscle$sample <- as.factor(as.character(muscle$sample))
muscle$experiment <- as.factor(as.character(muscle$experiment))

## Converting voltage into force and adding a new column

muscle$Contraction_Force <- (muscle$Twitch_Peak_Volts/0.5)*2

# bargraph.CI(response = Contraction_Force, x.factor = Species, group = Venom, data = muscle, 
#             col = "grey40", err.width = 0.05, cex.axis = 1.7, 
#             cex.lab = 2, cex = 1.5, angle = -135, lwd = 1.5, density = c(1000,25,1000,20), 
#             ylim = c(0,2.5), xaxt = "n", legend = T, leg.lab = c("Pre", "Post"), x.leg = 1, y.leg = 2.6)
# mtext("Gmouse", side = 1, cex = 1.8, at = 2, line = 1.1)
# mtext("Mouse", side = 1, cex = 1.8, at = 5, line = 1.1)

## GG-Plot

muscle <- muscle %>%
  mutate(Venom = recode(Venom, No = "BPT", Yes = "VPT"))

treatment <- ggplot(muscle, aes(x = Species, y = Contraction_Force, fill = Venom)) +
  stat_summary(fun="mean", geom="bar",
               width = 0.75, alpha = 0.66, position = "dodge", color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "black", size = 0.66, width = 0.3, 
               position = position_dodge(0.72)) +
  geom_jitter(position = position_jitterdodge(0.1)) +
  scale_x_discrete(name = "", labels= c("Gmouse", "Hmouse")) +
  coord_cartesian(ylim = c(0,3)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey40", "white")) +
  guides(fill = guide_legend(reverse = F, title=""),
         color = guide_legend(reverse = TRUE)) +
  #annotate("text", x = 2.18, y = 0.5, label = "*", size = 8) +
  ylab("Peak Twitch Tension (grams)") +
  theme_classic(base_size = 22) +
  theme(legend.position="top", element_line(colour = 'black', size = 0.5), 
        axis.title=element_text(size=26), axis.text.x = element_text(size=25))+
  ggtitle("Treatment")


#### Control ####

muscle <- read.csv("/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/muscle_peak_tension_control.csv", h = T)
head(muscle)
str(muscle)


## Converting some variables into the right classes
muscle$sample <- as.factor(as.character(muscle$sample))
muscle$Hour <- as.factor(as.character(muscle$Hour))

## Converting voltage into force and adding a new column
muscle$Contraction_Force <- (muscle$Twitch_Peak_Volts/0.5)*2

## GG-Plot

control <- ggplot(muscle, aes(x = Species, y = Contraction_Force, fill = Hour)) +
  stat_summary(fun="mean", geom="bar",
               width = 0.75, alpha = 0.66, position = "dodge", color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "black", size = 0.66, width = 0.2, 
               position = position_dodge(0.72)) +
  geom_jitter(position = position_jitterdodge(0.1)) +
  scale_x_discrete(name = "", labels= c("Gmouse", "Hmouse")) +
  coord_cartesian(ylim = c(0,3)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey40", "white")) +
  guides(fill = guide_legend(reverse = F, title=""),
         color = guide_legend(reverse = F)) +
  ylab("Peak Twitch Tension (grams)") +
  theme_classic(base_size = 22) +
  theme(legend.position = "top", element_line(colour = 'black', size = 0.5), 
        axis.title=element_text(size=26), axis.text.x =element_text(size=25)) +
  ggtitle("Control")



figure1 <- plot_grid(treatment, control, labels = "AUTO", label_size = 21)

ggsave("Figure_1.pdf", device = "pdf", path = "~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_1/", width = 10, height = 7)

