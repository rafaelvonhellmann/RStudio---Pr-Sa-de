#Transformando os dados para conseguir achar as medidas sumárias:
summary(DFPA)

attach(DFPA)
cols.num <- c("PASM", "PADM")

cols.num1 <- c("CGI", "Sexo", "CCor", "FEtária")

DFPA[cols.num] <- sapply(DFPAA[cols.num],as.numeric)
sapply(DFPA, class)


DFPAA[cols.numAA] <- sapply(DFPAA[cols.numAA],as.numeric)
sapply(DFPAA, class)

summary(DFPAA)


attach(DFPAA)
cols.numA <- c("PASM", "PADM")

cols.numAA <- c("CGI", "Sexo", "CCor", "FEtária")

DFPAA[cols.numA] <- sapply(DFPAA[cols.numA],as.numeric)
sapply(DFPAA, class)


DFPAA[cols.numAA] <- sapply(DFPAA[cols.numAA],as.numeric)
sapply(DFPAA, class)

summary(DFPAA)

attach(DFPAB)
cols.numB <- c("PASM", "PADM")

cols.numBB <- c("CGI", "Sexo", "CCor", "FEtária")


DFPAB[cols.numB] <- sapply(DFPAB[cols.numB],as.numeric)
sapply(DFPAB, class)

DFPAB[cols.numBB] <- sapply(DFPAB[cols.numBB],as.factor)
sapply(DFPAB, class)

summary(DFPAB)

attach(DFPAC)

cols.numC <- c("PASM", "PADM")

cols.numCC<- c("CGI", "Sexo", "CCor", "FEtária")


DFPAC[cols.numC] <- sapply(DFPAC[cols.numC],as.numeric)
sapply(DFPAC, class)

DFPAC[cols.numCC] <- sapply(DFPAC[cols.numCC],as.factor)
sapply(DFPAC, class)

summary(DFPAC)

attach(DFPAD)

cols.numD <- c("PASM", "PADM")

cols.numDD <- c("CGI", "Sexo", "CCor", "FEtária")


DFPAD[cols.numD] <- sapply(DFPAD[cols.numD],as.numeric)
sapply(DFPAD, class)

DFPAD[cols.numDD] <- sapply(DFPAD[cols.numDD],as.factor)
sapply(DFPAD, class)

summary(DFPAD)