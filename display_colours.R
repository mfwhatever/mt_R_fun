# taken from https://r-graph-gallery.com/42-colors-names.html

library(ggplot2)
library(tidyverse)

par(mar=c(0,0,0,0))

# Empty chart
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")

# Settings
line <- 14
col <- 5

# Add color background
rect(  
  rep((0:(col - 1)/col),line) ,  
  sort(rep((0:(line - 1)/line),col),decreasing=T),   
  rep((1:col/col),line) , 
  sort(rep((1:line/line),col),decreasing=T),  
  border = "white" , 
  #col=colors()[seq(1,line*col)])
  col=colors()[str_detect(colours(), "blue")])

# Color names
text(  
  rep((0:(col - 1)/col),line)+0.1 ,  
  sort(rep((0:(line - 1)/line),col),decreasing=T)+0.015 , 
  #colors()[seq(1,line*col)]  , 
  colors()[str_detect(colours(), "blue")]  ,
  cex=1)
