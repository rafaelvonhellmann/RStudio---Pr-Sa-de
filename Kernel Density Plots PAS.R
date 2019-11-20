## Kernel Density Plots

d <- density(df$p4pa1sis) # returns the density data 
plot(d, main="Frequêncica PAS")
polygon(d, col="green", border="blue")