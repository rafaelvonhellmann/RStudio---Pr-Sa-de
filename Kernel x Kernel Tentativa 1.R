# Comparar PA1S com PA2S
library(sm)
attach(df)

# create value labels 
p4pa1sis <- factor(p4pa1sis, levels= c(p4pa1sis,p4pa2sis),
                labels = c("p4pa2sis")) 

# plot densities 
sm.density.compare(p4pa1sis, p4pa2sis, xlab="Pressão Sistólica 1")
title(main="p4pa1sis X p4pa2sis")

# add legend via mouse click
colfill<-c(2:(2+length(levels(p4pa1sis))) 
legend(locator(1), levels(p4pa1sis), fill=colfill)