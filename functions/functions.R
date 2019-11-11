require(grid)
require(gridExtra)

#function to create custom captions, in the style of fivethirtyeight.com.
caption_plot <- function(plot, left_caption = "", right_caption = "", fontsize = 12){
  grobTree(
    #make the background of the entire plot area match the color of ggthemes::theme_fivethirtyeight
    rectGrob(gp=gpar(fill="grey94", lwd = NA)),
    
    #Combine the plot, a line break and a caption
    arrangeGrob(plot,
                #line break
                rectGrob(height = 0, gp=gpar(fill="grey94", lwd = 3, col = "grey80")),
                
                #left caption; typically used for the site URL
                textGrob(left_caption, just = "left", x = 0.05, y = .5, gp = gpar(fontface = "bold",
                                                                                  fontsize = fontsize,
                                                                                  col = "grey40",
                                                                                  fontfamily = "sans")),
                
                #right caption, typically used to cite the source
                textGrob(right_caption, just = "right", x = 0.95, y = .5, gp = gpar(fontface = "bold",
                                                                                    fontsize = fontsize,
                                                                                    col = "grey40",
                                                                                    fontfamily = "sans")),
                
                #layout the plot, line break and captions, with the plot using 95% of the area
                layout_matrix = rbind(c(1, 1),
                                      c(2, 2),
                                      c(3, 4)),
                heights = c(.95, .01, .04)
    )
  )
}