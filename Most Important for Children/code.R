library(tidyverse)
library(foreign)
library(ggthemes)
library(scales)
library(grid)
library(gridExtra)

#grab a custom function for captioning plots
source("functions/functions.R")


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

#from the summary data, generate a plot showing the trend over time
p <- most_important %>% 
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
        panel.grid.major.x = element_blank()) + 
  labs(title = "What's most important for a child to learn?",
       subtitle = "%of respondents selecting each attribute as 'the most important for a child to learn\nto prepare him or her for life'")



#output plot as png
ggsave("Most Important for Children/Most_Important.png", 
       caption_plot(p, "github.com/jordanRupton/PictureThis", "Source: 2018 General Social Survey"),
       width = 8, height = 4.5, dpi = 300)
