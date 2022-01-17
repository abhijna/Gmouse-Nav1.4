#### Figure 3D ####

pdf(file="~/Dropbox/Current_Biology/Gmouse-Nav1.4/Figures/Figure_3/Figure_3D.pdf")

## Wildtype
beta <- read.csv(file = "/Users/abhijnaparigi/Dropbox/Current_Biology/Gmouse-Nav1.4/CSV/activation.csv", header = T)

beta <- subset(beta, Venom != "Late")
beta$replicate <- factor(beta$replicate)
current <- subset(beta, Species == "Gmouse" | Species == "Rat")

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

ggplot(peaks, aes(x = Species, y = Peak_Diff, fill = Species)) +
  stat_summary(fun="mean", geom="bar",
               width = 0.75, alpha = 0.66, position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               size = 0.66, width = 0.1, position = position_dodge(0.72)) +
  geom_jitter(height = 0, width = 0.15) +
  scale_x_discrete(name = "", labels= c("Gmouse", "Rat")) +
  coord_cartesian(ylim = c(0,1.5)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey3", "green3")) +
  guides(fill = guide_legend(reverse = F, title=""),
         color = guide_legend(reverse = F)) +
  ylab("Diff in Peak I at 5mV") +
  theme_classic(base_size = 22) +
  theme(legend.position="none", element_line(colour = 'black', size = 0.5), axis.title=element_text(size=26), axis.text.x =element_text(size=25))

dev.off()


### Stats
head(peaks)
## Use this in the paper because it's a simple t test comparing the differences in differences
t.test(peaks$Peak_Diff ~ peaks$Species)

### Let's get the means
means <- peaks %>% 
  #dplyr::filter(Voltage == 5 & Venom != "Late" & Species != "Mouse") %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarize(avg = mean(Peak_Diff), 
                   SEM = sd(Peak_Diff)/sqrt(n())) %>% 
  ungroup()

means


## Pulling out the numbers: can't use previous panel to pull out means because we're showing non-normalized currents in this panel

means <- current %>% 
  dplyr::filter(Voltage == 5 & Venom != "Late" & Species != "Mouse") %>% 
  dplyr::group_by(Species, Venom) %>% 
  dplyr::summarize(avg = mean(Current), 
                   SEM = sd(Current)/sqrt(n())) %>% 
  ungroup()

means