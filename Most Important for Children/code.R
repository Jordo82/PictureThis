library(tidyverse)
library(foreign)
library(ggthemes)
library(scales)

#download the GSS dataset into a temporary directory
td <- tempdir()
tf <- paste0(tempfile(tmpdir = td), ".zip")
download.file("https://gssdataexplorer.norc.org/system/data/GSS/1972_2018/GSS_spss.zip", tf)

#extract the zip file and read it from SPSS format into R
unzip(tf, exdir = td)
GSS <- read.spss(paste0(td, "\\GSS7218_R1.sav"), 
                 use.value.labels = TRUE, to.data.frame = TRUE)

#summarise the data to get the %of respondents ranking each attribute as most important by year
most_important <- GSS %>% 
  select(YEAR, OBEY, POPULAR, THNKSELF, WORKHARD, HELPOTH) %>% 
  gather(-YEAR, key = "quality", value = "importance") %>% 
  #exclude non-responses
  filter(!is.na(importance)) %>% 
  group_by(YEAR, quality) %>% 
  summarise(n = n(),
            p = sum(importance == "MOST IMPORTANT") / n)

#from the suammry data, generate a plot showing the trend over time
most_important %>% 
  ggplot(aes(YEAR, p, color = quality)) + 
  geom_line(size = 1.5) + 
  #add custom labels to identify each quality
  annotate("text", x = c(2014, 1988, 2000, 1990, 2015),
           y = c(.21, .27, .04, .47, .29),
           label = c("To help others", "To obey", "To be well liked or popular",
                     "To think for one's self", "To work hard"),
           color = hue_pal()(5), fontface = "bold", size = 4) + 
  theme_fivethirtyeight() + 
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, .5, by = 0.1)) + 
  scale_x_continuous(breaks = seq(1986, 2018, by = 4)) +
  theme(axis.text = element_text(face = "bold", size = 12),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 2, 1), "cm"),
        panel.grid.major.x = element_blank()) + 
  labs(title = "What's most important for a child to learn?",
       subtitle = "%of respondents selecting each attribute as 'the most important for a child to learn\nto prepare him or her for life'") + 
  #manually assign the limits so that we can add another annotation at the bottom
  coord_cartesian(ylim = c(0, .55), xlim = c(1986, 2018), clip = "off") + 
  annotate("segment", x = 1983, xend = 2020, y = -.1, yend = -.1, color = "gray50", size = 1)+
  #cite the source
  annotate("text", x = 2020, y = -.15, 
           label = "Source : 2018 General Social Survey",
           hjust = 1, color = "grey50", fontface = "bold", size = 3) + 
  annotate("text", x = 1983, y = -.15, 
           label = "github.com/Jordo82/PictureThis",
           hjust = 0, color = "grey50", fontface = "bold", size = 3)

#output plot as png
ggsave("Most Important for Children/Most_Important.png", width = 8, height = 4.5, dpi = 600)
