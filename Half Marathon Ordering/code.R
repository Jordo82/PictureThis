library(tidyverse)
library(ggthemes)
library(scales)


#read in the race results, taken from http://www.mychiptime.com/searchevent.php?id=11569
race <- read_csv("Half Marathon Ordering/Race Results.csv", col_types = "dddccdc") %>% 
  #get an approximate starting order based on the difference between chip time and gun time
  mutate(Start_Order = rank(Gun - Chip, ties.method = "min"),
         Finish_Order = rank(Chip, ties.method = "min"))


#plot start order vs finish order
race %>% 
  ggplot(aes(Finish_Order, Start_Order)) + 
  geom_point(alpha = 0.2, color = "red") + 
  
  #highlight and label some areas on the plot based on start vs finish
  annotate("polygon", x = c(0, 500, 6016, 6016, 5000, 0), y = c(0, 0, 5000, 6016, 6016, 500), 
           fill = hue_pal()(5)[4], alpha = 0.3, size = 1)+
  annotate("text", x = c(3000, 500, 5500), y = c(3000, 5500, 500),
           label = c("Just Right", "I overslept!", "I'm clueless!"), color = "grey20",
           angle = 20, fontface = "bold", size = c(8, 4, 4)) +
  
  #add arrows to assist with axis interpretation
  annotate("segment", x = 0, xend = 500, y = -500, yend = -500, 
           arrow = arrow(end = "first", length = unit(0.3, "cm")), color = "grey50", size = 1.2)+
  annotate("text", x = 500, y = -500, label = "Finished first", color = "grey50", hjust = 0)+
  annotate("segment", x = 5500, xend = 6000, y = -500, yend = -500, 
           arrow = arrow(end = "last", length = unit(0.3, "cm")), color = "grey50", size = 1.2)+
  annotate("text", x = 5500, y = -500, label = "Finished last", color = "grey50", hjust = 1)+
  annotate("segment", x = -100, xend = -100, y = 0, yend = 500, 
           arrow = arrow(end = "first", length = unit(0.3, "cm")), color = "grey50", size = 1.2)+
  annotate("text", x = -100, y = 500, label = "Started first", color = "grey50", hjust = 0, angle = 90)+
  annotate("segment", x = -100, xend = -100, y = 5500, yend = 6000, 
           arrow = arrow(end = "last", length = unit(0.3, "cm")), color = "grey50", size = 1.2)+
  annotate("text", x = -100, y = 5500, label = "Started last", color = "grey50", hjust = 1, angle = 90)+
  
  #make some general changes to the theme to make everything look nice
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm")) + 
  labs(title = "Where to start?",
       subtitle = "In what order did runners start and finish the race?",
       x = "Order at the Finish",
       y = "Order at the Start") + 
  #manually assign the limits so that we can add another annotation at the bottom
  coord_cartesian(xlim = c(0,6016), ylim = c(0,6016), clip = "off") + 
  annotate("segment", x = -500, xend = 6200, y = -1100, yend = -1100, color = "gray50", size = 1)+
  #cite the source
  annotate("text", x = 6200, y = -1400, 
           label = "Source : Austin 3M Half Marathon 2019 Results",
           hjust = 1, color = "grey50", fontface = "bold", size = 3) + 
  annotate("text", x = -500, y = -1400, 
           label = "github.com/Jordo82/PictureThis",
           hjust = 0, color = "grey50", fontface = "bold", size = 3)


#output plot as png
ggsave("Half Marathon Ordering/Race_Ordering.png", width = 8, height = 4.5, dpi = 600)
