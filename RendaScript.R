## Ler banco de dados renda:


library(readr)
Renda <- read.csv2("~/Rstudio/rafael_renda.csv",
                  header = TRUE, 
                  quote="\"", 
                  stringsAsFactors= TRUE, 
                  strip.white = TRUE)


##Apagar dados faltantes:

library(stringr)
Renda[] <- lapply(Renda, str_trim)
is.na(Renda) <- Renda==''
Renda

Renda <- Renda[complete.cases(Renda), ]

Renda<-as.data.frame(Renda)

##Transformando as variáveis

P1R <- ifelse(Renda$p1e33 == "Ate R$ 500", 1,
              ifelse(Renda$p1e33 =="R$ 501-R$ 1000", 2,
                     ifelse(Renda$p1e33 == "R$ 1001-R$ 1500",3,
                            ifelse(Renda$p1e33 =="R$ 1501-R$ 2000", 4,
                                   ifelse(Renda$p1e33 =="R$ 2001-R$ 2500", 5,
                                          ifelse(Renda$p1e33 =="R$ 2501-R$ 3000", 6,
                                                 ifelse(Renda$p1e33 =="R$ 3001-R$ 4000", 7,
                                                        ifelse(Renda$p1e33 =="R$ 4001-R$ 5000", 8,
                                                               ifelse(Renda$p1e33 =="+ R$ 5000", 9,
                                                        NA  )))))))))


Renda <- cbind(Renda, P1R)

P2R <- ifelse(Renda$p2g3 == "Ate R$ 500", 1,
              ifelse(Renda$p2g3 =="R$ 501-R$ 1000", 2,
                     ifelse(Renda$p2g3 == "R$ 1001-R$ 1500",3,
                            ifelse(Renda$p2g3 =="R$ 1501-R$ 2000", 4,
                                   ifelse(Renda$p2g3 =="R$ 2001-R$ 2500", 5,
                                          ifelse(Renda$p2g3 =="R$ 2501-R$ 3000", 6,
                                                 ifelse(Renda$p2g3 =="R$ 3001-R$ 4000", 7,
                                                        ifelse(Renda$p2g3 =="R$ 4001-R$ 5000", 8,
                                                               ifelse(Renda$p2g3 =="+ R$ 5000", 9,
                                                                      NA  )))))))))



Renda <- cbind(Renda, P2R)


### Há um erro na tabela original com os valores, de 3000-4000 como seria o esperado, há 3001-3500, 4501-4000, Mais de 50000
P3R <- ifelse(Renda$p3f == "Ate R$ 500", 1,
              ifelse(Renda$p3f =="R$ 501-R$ 1000", 2,
                     ifelse(Renda$p3f == "R$ 1001-R$ 1500",3,
                            ifelse(Renda$p3f =="R$ 1501-R$ 2000", 4,
                                   ifelse(Renda$p3f =="R$ 2001-R$ 2500", 5,
                                          ifelse(Renda$p3f =="R$ 2501-R$ 3000", 6,
                                                 ifelse(Renda$p3f =="R$ 3001-R$ 3500", 7,
                                                        ifelse(Renda$p3f =="R$ 4501-R$ 4000", 8,
                                                               ifelse(Renda$p3f =="Mais de R$5000 reais", 9,
                                                                      NA  )))))))))



Renda <- cbind(Renda, P3R)


P4R <- ifelse(Renda$p4g5 == "Até 1000 reais", 1,
              ifelse(Renda$p4g5 =="Entre 1001 e 1500 reais", 2,
                     ifelse(Renda$p4g5 == "Entre 1501 e 2000 reais",3,
                            ifelse(Renda$p4g5 =="Entre 2001 e 2500 reais", 4,
                                   ifelse(Renda$p4g5 =="Entre 2501 e 3000 reais", 5,
                                          ifelse(Renda$p4g5 =="Entre 3001 e 4000 reais", 6,
                                                 ifelse(Renda$p4g5 =="Entre 4001 e 5000 reais", 7,
                                                        ifelse(Renda$p4g5 =="Entre 5001 e 6000 reais", 8,
                                                               ifelse(Renda$p4g5 =="Entre 6001 e 7000 reais", 9,
                                                                      ifelse(Renda$p4g5 =="Mais de 7000 reais", 10,
                                                                      NA  ))))))))))


Renda <- cbind(Renda, P4R)


##Trocando o nome das colunas de dependentes:

names(Renda)[names(Renda) == 'p1e34'] <- 'P1D'

names(Renda)[names(Renda) == 'p2g4'] <- 'P2D'

names(Renda)[names(Renda) == 'p3g'] <- 'P3D'

names(Renda)[names(Renda) == 'p4g6'] <- 'P4D'

##Alterando nome colunas questionário:

names(Renda)[names(Renda) == 'id'] <- 'ID'

names(Renda)[names(Renda) == 'p1quest'] <- 'P1Q'

names(Renda)[names(Renda) == 'p2quest'] <- 'P2Q'

names(Renda)[names(Renda) == 'p3quest'] <- 'P3Q'

names(Renda)[names(Renda) == 'p4quest'] <- 'P4Q'


##Juntando as novas variáveis:

Renda <- subset(Renda, select=-c(6,8,10,12))

View(Renda)


### Renda per capita: usar a média dos extremos? Usar os dois valores para renda per capita máxima e mínima?
##Criando valores inferiores, médios e superiores para fazer três rendas per capitas por fase do Pró-Saúde

P1RI <- ifelse(Renda$p1e33 == "Ate R$ 500", 0,
              ifelse(Renda$p1e33 =="R$ 501-R$ 1000", 501,
                     ifelse(Renda$p1e33 == "R$ 1001-R$ 1500",1001,
                            ifelse(Renda$p1e33 =="R$ 1501-R$ 2000", 1501,
                                   ifelse(Renda$p1e33 =="R$ 2001-R$ 2500", 2001,
                                          ifelse(Renda$p1e33 =="R$ 2501-R$ 3000", 2501,
                                                 ifelse(Renda$p1e33 =="R$ 3001-R$ 4000",3000,
                                                        ifelse(Renda$p1e33 =="R$ 4001-R$ 5000", 4001,
                                                               ifelse(Renda$p1e33 =="+ R$ 5000", 5000,
                                                                      NA  )))))))))

Renda <- cbind(Renda, P1RI)


P2RI <- ifelse(Renda$p2g3 == "Ate R$ 500", 0,
              ifelse(Renda$p2g3 =="R$ 501-R$ 1000", 501,
                     ifelse(Renda$p2g3 == "R$ 1001-R$ 1500",1001,
                            ifelse(Renda$p2g3 =="R$ 1501-R$ 2000", 1501,
                                   ifelse(Renda$p2g3 =="R$ 2001-R$ 2500", 2001,
                                          ifelse(Renda$p2g3 =="R$ 2501-R$ 3000",2501,
                                                 ifelse(Renda$p2g3 =="R$ 3001-R$ 4000", 3001,
                                                        ifelse(Renda$p2g3 =="R$ 4001-R$ 5000", 4001,
                                                               ifelse(Renda$p2g3 =="+ R$ 5000", 5000,
                                                                      NA  )))))))))

Renda <- cbind(Renda, P2RI)


P3RI <- ifelse(Renda$p3f == "Ate R$ 500", 0,
              ifelse(Renda$p3f =="R$ 501-R$ 1000", 501,
                     ifelse(Renda$p3f == "R$ 1001-R$ 1500",1001,
                            ifelse(Renda$p3f =="R$ 1501-R$ 2000", 1501,
                                   ifelse(Renda$p3f =="R$ 2001-R$ 2500", 2001,
                                          ifelse(Renda$p3f =="R$ 2501-R$ 3000", 2501,
                                                 ifelse(Renda$p3f =="R$ 3001-R$ 3500", 3001,
                                                        ifelse(Renda$p3f =="R$ 4501-R$ 4000", 4000,
                                                               ifelse(Renda$p3f =="Mais de R$5000 reais", 5000,
                                                                      NA  )))))))))


Renda <- cbind(Renda, P3RI)


P4RI <- ifelse(Renda$p4g5 == "Até 1000 reais", 0,
              ifelse(Renda$p4g5 =="Entre 1001 e 1500 reais", 1001,
                     ifelse(Renda$p4g5 == "Entre 1501 e 2000 reais",1501,
                            ifelse(Renda$p4g5 =="Entre 2001 e 2500 reais", 2001,
                                   ifelse(Renda$p4g5 =="Entre 2501 e 3000 reais", 2501,
                                          ifelse(Renda$p4g5 =="Entre 3001 e 4000 reais", 3001,
                                                 ifelse(Renda$p4g5 =="Entre 4001 e 5000 reais", 4001,
                                                        ifelse(Renda$p4g5 =="Entre 5001 e 6000 reais", 5001,
                                                               ifelse(Renda$p4g5 =="Entre 6001 e 7000 reais", 6001,
                                                                      ifelse(Renda$p4g5 =="Mais de 7000 reais", 7000,
                                                                             NA  ))))))))))
Renda <- cbind(Renda, P4RI)


P1RM <- ifelse(Renda$p1e33 == "Ate R$ 500", 250,
               ifelse(Renda$p1e33 =="R$ 501-R$ 1000", 750,
                      ifelse(Renda$p1e33 == "R$ 1001-R$ 1500",1250,
                             ifelse(Renda$p1e33 =="R$ 1501-R$ 2000", 1750,
                                    ifelse(Renda$p1e33 =="R$ 2001-R$ 2500", 2250,
                                           ifelse(Renda$p1e33 =="R$ 2501-R$ 3000", 2750,
                                                  ifelse(Renda$p1e33 =="R$ 3001-R$ 4000",3500,
                                                         ifelse(Renda$p1e33 =="R$ 4001-R$ 5000", 4500,
                                                                ifelse(Renda$p1e33 =="+ R$ 5000", 7500,
                                                                       NA  )))))))))
Renda <- cbind(Renda, P1RM)


P2RM <- ifelse(Renda$p2g3 == "Ate R$ 500", 250,
               ifelse(Renda$p2g3 =="R$ 501-R$ 1000", 750,
                      ifelse(Renda$p2g3 == "R$ 1001-R$ 1500",1250,
                             ifelse(Renda$p2g3 =="R$ 1501-R$ 2000", 1750,
                                    ifelse(Renda$p2g3 =="R$ 2001-R$ 2500", 2250,
                                           ifelse(Renda$p2g3 =="R$ 2501-R$ 3000",2750,
                                                  ifelse(Renda$p2g3 =="R$ 3001-R$ 4000", 3500,
                                                         ifelse(Renda$p2g3 =="R$ 4001-R$ 5000", 4500,
                                                                ifelse(Renda$p2g3 =="+ R$ 5000", 7500,
                                                                       NA  )))))))))
Renda <- cbind(Renda, P2RM)


P3RM <- ifelse(Renda$p3f == "Ate R$ 500", 250,
               ifelse(Renda$p3f =="R$ 501-R$ 1000", 750,
                      ifelse(Renda$p3f == "R$ 1001-R$ 1500",1250,
                             ifelse(Renda$p3f =="R$ 1501-R$ 2000", 1750,
                                    ifelse(Renda$p3f =="R$ 2001-R$ 2500", 2250,
                                           ifelse(Renda$p3f =="R$ 2501-R$ 3000", 2750,
                                                  ifelse(Renda$p3f =="R$ 3001-R$ 3500", 3500,
                                                         ifelse(Renda$p3f =="R$ 4501-R$ 4000", 4500,
                                                                ifelse(Renda$p3f =="Mais de R$5000 reais", 7500,
                                                                       NA  )))))))))


Renda <- cbind(Renda, P3RM)

P4RM <- ifelse(Renda$p4g5 == "Até 1000 reais", 500,
               ifelse(Renda$p4g5 =="Entre 1001 e 1500 reais", 1250,
                      ifelse(Renda$p4g5 == "Entre 1501 e 2000 reais",1750,
                             ifelse(Renda$p4g5 =="Entre 2001 e 2500 reais", 2250,
                                    ifelse(Renda$p4g5 =="Entre 2501 e 3000 reais", 2750,
                                           ifelse(Renda$p4g5 =="Entre 3001 e 4000 reais", 3500,
                                                  ifelse(Renda$p4g5 =="Entre 4001 e 5000 reais", 4500,
                                                         ifelse(Renda$p4g5 =="Entre 5001 e 6000 reais", 5500,
                                                                ifelse(Renda$p4g5 =="Entre 6001 e 7000 reais", 6500,
                                                                       ifelse(Renda$p4g5 =="Mais de 7000 reais", 8500,
                                                                              NA  ))))))))))

Renda <- cbind(Renda, P4RM)

P1RS <- ifelse(Renda$p1e33 == "Ate R$ 500", 500,
               ifelse(Renda$p1e33 =="R$ 501-R$ 1000", 1000,
                      ifelse(Renda$p1e33 == "R$ 1001-R$ 1500",1500,
                             ifelse(Renda$p1e33 =="R$ 1501-R$ 2000", 2000,
                                    ifelse(Renda$p1e33 =="R$ 2001-R$ 2500", 2500,
                                           ifelse(Renda$p1e33 =="R$ 2501-R$ 3000", 3000,
                                                  ifelse(Renda$p1e33 =="R$ 3001-R$ 4000",400,
                                                         ifelse(Renda$p1e33 =="R$ 4001-R$ 5000", 5000,
                                                                ifelse(Renda$p1e33 =="+ R$ 5000", 10000,
                                                                       NA  )))))))))

Renda <- cbind(Renda, P1RS)


P2RS <- ifelse(Renda$p2g3 == "Ate R$ 500", 500,
               ifelse(Renda$p2g3 =="R$ 501-R$ 1000", 1000,
                      ifelse(Renda$p2g3 == "R$ 1001-R$ 1500",1500,
                             ifelse(Renda$p2g3 =="R$ 1501-R$ 2000", 2000,
                                    ifelse(Renda$p2g3 =="R$ 2001-R$ 2500", 2500,
                                           ifelse(Renda$p2g3 =="R$ 2501-R$ 3000",3000,
                                                  ifelse(Renda$p2g3 =="R$ 3001-R$ 4000", 4000,
                                                         ifelse(Renda$p2g3 =="R$ 4001-R$ 5000", 5000,
                                                                ifelse(Renda$p2g3 =="+ R$ 5000", 10000,
                                                                       NA  )))))))))
Renda <- cbind(Renda, P2RS)



P3RS <- ifelse(Renda$p3f == "Ate R$ 500", 500,
               ifelse(Renda$p3f =="R$ 501-R$ 1000", 1000,
                      ifelse(Renda$p3f == "R$ 1001-R$ 1500",1500,
                             ifelse(Renda$p3f =="R$ 1501-R$ 2000", 2000,
                                    ifelse(Renda$p3f =="R$ 2001-R$ 2500", 2500,
                                           ifelse(Renda$p3f =="R$ 2501-R$ 3000", 3000,
                                                  ifelse(Renda$p3f =="R$ 3001-R$ 3500", 4000,
                                                         ifelse(Renda$p3f =="R$ 4501-R$ 4000", 5000,
                                                                ifelse(Renda$p3f =="Mais de R$5000 reais", 10000,
                                                                       NA  )))))))))


Renda <- cbind(Renda, P3RS)


P4RS <- ifelse(Renda$p4g5 == "Até 1000 reais", 1000,
               ifelse(Renda$p4g5 =="Entre 1001 e 1500 reais", 1500,
                      ifelse(Renda$p4g5 == "Entre 1501 e 2000 reais",2000,
                             ifelse(Renda$p4g5 =="Entre 2001 e 2500 reais", 2500,
                                    ifelse(Renda$p4g5 =="Entre 2501 e 3000 reais", 3000,
                                           ifelse(Renda$p4g5 =="Entre 3001 e 4000 reais", 4000,
                                                  ifelse(Renda$p4g5 =="Entre 4001 e 5000 reais", 5000,
                                                         ifelse(Renda$p4g5 =="Entre 5001 e 6000 reais", 6000,
                                                                ifelse(Renda$p4g5 =="Entre 6001 e 7000 reais", 7000,
                                                                       ifelse(Renda$p4g5 =="Mais de 7000 reais", 10000,
                                                                              NA  ))))))))))
Renda <- cbind(Renda, P4RS)


##Cálculo da renda per capita:


##1) Usando os valores inferiores:


RPC1I <- P1RI/P1D

RPC2I <- P2RI/P2D

RPC3I <- P3RI/P3D

RPC4I <- P4RI/P4D

Renda <- cbind(Renda, RPC1I)

Renda <- cbind(Renda, RPC2I)

Renda <- cbind(Renda, RPC3I)

Renda <- cbind(Renda, RPC4I)




##2) Usando os valores médios:


RPC1M <- P1RM/P1D

RPC2M <- P2RM/P2D

RPC3M <- P3RM/P3D

RPC4M <- P4RM/P4D

Renda <- cbind(Renda, RPC1M)

Renda <- cbind(Renda, RPC2M)

Renda <- cbind(Renda, RPC3M)

Renda <- cbind(Renda, RPC4M)


##3) Usando os valores superiores:


RPC1S <- P1RS/P1D

RPC2S <- P2RS/P2D

RPC3S <- P3RS/P3D

RPC4S <- P4RS/P4D

Renda <- cbind(Renda, RPC1S)

Renda <- cbind(Renda, RPC2S)

Renda <- cbind(Renda, RPC3S)

Renda <- cbind(Renda, RPC4S)

