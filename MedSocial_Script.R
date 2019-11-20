library(survival)
library(readr)

medsocial <- read_csv("C:/Users/Rafael/Documents/MedSocial/MedSocial.csv")

table(medsocial$tratamento)

Tratamento <- ifelse(medsocial$tratamento == "TTOA", "Convencional",
               ifelse(medsocial$tratamento == "TTOB", "Experimental",
                             NA  ))
medsocial <- cbind(medsocial, Tratamento)

medsocial <- subset(medsocial, select= c(morte, Tratamento, status))

table(medsocial$Tratamento)

with(medsocial, tapply(morte[status==1], Tratamento[status==1],mean))

library(survival)

plot(survfit(Surv(morte,status)~1, data=medsocial))

fit=survfit(Surv(morte,status)~Tratamento, data=medsocial)

plot(fit)

plot(fit, col=c(1,2), xlab="Tempo", ylab="Probabilidade de sobrevivência")

legend(30,1, c("Convencional", "Experimental"), lwd=0.5, col=c(1,2))

library(survminer)

survdiff(Surv(medsocial$morte, medsocial$status)~medsocial$Tratamento, rho=1.5)

survdiff(Surv(medsocial$morte, medsocial$status)~medsocial$Tratamento, rho=1)

survdiff(Surv(medsocial$morte, medsocial$status)~medsocial$Tratamento, rho=0)

library(ggthemes)

G <- ggsurvplot( fit,                     
        data = medsocial,  
        risk.table = TRUE,       
        pval = TRUE,             
        conf.int = TRUE,          
        xlim = c(0,24),        
        break.time.by = 2,     
        risk.table.y.text.col = T,
        risk.table.y.text = FALSE)

G + xlab("Tempo") + ylab("Probabilidade") + 
        ggtitle("Curva de Sobrevivência")
        