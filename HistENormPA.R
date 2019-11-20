##Histograma com Curva normal da PA Sistólica 1

x <- df$p4pa1sis
h<-hist(x, breaks=20, col="green", xlab="Pressão Arterial Sistólica 1", ylab="Frequência",
        main="Histograma e Curva PA1S") 
xfit<-seq(min(x),max(x),length=100) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)