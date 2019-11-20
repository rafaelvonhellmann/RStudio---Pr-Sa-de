Arquivo princípio ativo medicamentos Fase 4

## ARQUIVO DO SPPS.

install.packages("memisc")

library(memisc)

data <- as.data.set(spss.system.file("medicamentos_fase4a_pro_saude.sav"))





getwd()

setwd("C:/Users/Rafael/Documents/Rstudio")

df <- read.csv2("banco_pa_hg3.csv")

PASM <- rowMeans(cbind(df$p4pa2sis,df$p4pa3sis), na.rm=TRUE)

PADM <- rowMeans(cbind(df$p4pa2dia,df$p4pa3dia), na.rm=TRUE)

PAM <- (PASM + 2*PADM)/3

dfPA <- cbind(PASM,PADM,PAM)  

dfPA <- as.data.frame(dfPA)

library(ggplot2)

mean(PASM)

mean(PADM)

mean(PAM)

sd(PASM)

sd(PADM)

sd(PAM)


ggplot(data.frame(x = c(10, 250)), aes(x))
mapply(function(mean, sd, col) {
        stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = col)
}, 
mean = c(122.4416, 74.8982, 90.74601), 
sd = c(27.20965, 24.54236, 24.86601), 
col = c('red', 'blue', 'green')
)

#### legend ("topright", legend = c("PASM","PADM","PAM"), lty=1:2, xjust = 1, yjust = 1, title: "Pressões")
#####
