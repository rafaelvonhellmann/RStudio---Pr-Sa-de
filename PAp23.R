###Dados Pró-Saúde Fase 2, 3 e 4

DFPA2 <- read.csv2("~/Rstudio/rafael_pa2.csv",
                   header = TRUE, 
                   quote="\"", 
                   stringsAsFactors= TRUE, 
                   strip.white = TRUE)

DFPA3 <- read.csv2("~/Rstudio/rafael_pa3.csv",
                   header = TRUE, 
                   quote="\"", 
                   stringsAsFactors= TRUE, 
                   strip.white = TRUE)

###Transformando em NA os dados colocados como faltantes

DFPA2[DFPA2 == 888|DFPA2 == 8888|DFPA2 == 88888|DFPA2 == 88880|
              DFPA2 == 7777|DFPA2 == 88|DFPA2 == 88/88/8888|
              DFPA2 == 77/77/7777|DFPA2 == 88/88/88|DFPA2 == 77/77/77] <- NA

DFPA3[DFPA3 == 888|DFPA3 == 8888|DFPA3 == 88888|DFPA3 == 88880|
              DFPA3 == 7777|DFPA3 == 88|DFPA3 == 88/88/8888|
              DFPA3 == 77/77/7777|DFPA3 == 88/88/88|DFPA3 == 77/77/77] <- NA


### Excluindo os NA's

DFPA2 <- DFPA2[complete.cases(DFPA2), ]

DFPA3 <- DFPA32[complete.cases(DFPA3), ]


###Análise PA

###Pressão arterial sistólica média Fase 2

PASM <- rowMeans(cbind(DFPA2$p2pa1sis,DFPA2$p2pa2sis), na.rm=TRUE) 

PASM <- round(PASM)

#### Pressão arterial diastólica média Fase 2

PADM <- rowMeans(cbind(DFPA2$p2pa1dia,DFPA2$p2pa2dia), na.rm=TRUE)

PADM <- round(PADM)


#### Pressão Arterial Média Fase 2

PAM <- (PASM + 2*PADM)/3

PAM <- round(PAM)


#### Pressão de Pulso Fase 2

PP <- PASM-PADM

### Juntando P4PA, PASM, PADM, PP, PAM Fase 2

DFPA2 <- cbind(DFPA2, PASM, PADM, PAM, PP)

DFPA2[DFPA2 == 888|DFPA2 == 8888|DFPA2 == 88888|DFPA2 == 88880 ] <- NA

###Excluir valores absurdos Fase 2


###Pressão arterial sistólica média Fase 3

PASM <- rowMeans(cbind(DFPA3$p3pa1sis,DFPA3$p3pa2sis), na.rm=TRUE) 

PASM <- round(PASM)

#### Pressão arterial diastólica média Fase 3

PADM <- rowMeans(cbind(DFPA3$p3pa1dia,DFPA3$p3pa2dia), na.rm=TRUE)

PADM <- round(PADM)


#### Pressão Arterial Média Fase 3

PAM <- (PASM + 2*PADM)/3

PAM <- round(PAM)


#### Pressão de Pulso Fase 3

PP <- PASM-PADM

### Juntando P4PA, PASM, PADM, PP, PAM Fase 3

P4PA2 <- cbind(P4PA, PASM, PADM, PAM, PP)

DFPA3[DFPA3 == 888|DFPA3 == 8888|DFPA3 == 88888|DFPA3 == 88880 ] <- NA

###Excluir valores absurdos Fase 3
