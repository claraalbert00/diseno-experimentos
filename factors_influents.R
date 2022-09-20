dad# Treball disseny d'experiments
# Clara Albert. NIU: 1530466
# Realització de les combinacions de forma aleatòria
sample(c("A","B","C","D","E"),5,replace=F)
sample(c("A","B","C","D","E"),5,replace=F)


sample(c('ext','int'),5,replace=T,prob=c(0.5,0.5))
sample(c('ext','int'),5,replace=T,prob=c(0.5,0.5))

# Carregar la base de dades
library(readxl)
dades <- read_excel("dades.xlsx")
dades=dades[,-12]
View(dades)

# #######################
# Comprovació base de dades

# No hi ha cap valor NA
anyNA(dades)

# Les variables respostes són entre 0 i 10
any(dades$`Abans - Estrés`>10 | dades$`Abans - Estrés`<0)
any(dades$`Abans - Avorriment`>10 | dades$`Abans - Avorriment`<0)
any(dades$`Abans - Felicitat`>10 | dades$`Abans - Felicitat`<0)

any(dades$`Despres - Estrés`>10 | dades$`Despres - Estrés`<0)
any(dades$`Despres - Avorriment`>10 | dades$`Despres - Avorriment`<0)
any(dades$`Despres - Felicitat`>10 | dades$`Despres - Felicitat`<0)

# El factor dia pren valors del 1 al 5
any(dades$Dia > 5 | dades$Dia<1)
any(dades$Individu>10 | dades$Individu<1)
any(dades$Horari<1 | dades$Horari>2)
any(dades$Lloc<1 | dades$Lloc>2)
as.factor(dades$Activitat)

# Combinacions equilibrades
nrow(subset(dades, Lloc==1))
nrow(subset(dades, Lloc==2))

nrow(subset(dades, Activitat == 'A'))
nrow(subset(dades, Activitat == 'B'))
nrow(subset(dades, Activitat == 'C'))
nrow(subset(dades, Activitat == 'D'))
nrow(subset(dades, Activitat == 'E'))

nrow(subset(dades, Horari==1))
nrow(subset(dades, Horari==2))


# ################
# Estadísitica descriptiva

#Activitat-Lloc-Horari
AME=nrow(subset(dades, Activitat == 'A' & Lloc==1 & Horari==1))
ATE=nrow(subset(dades, Activitat == 'A' & Lloc==1 & Horari==2))
AMI=nrow(subset(dades, Activitat == 'A' & Lloc==2 & Horari==1))
ATI=nrow(subset(dades, Activitat == 'A' & Lloc==2 & Horari==2))

BME=nrow(subset(dades, Activitat == 'B' & Lloc==1 & Horari==1))
BTE=nrow(subset(dades, Activitat == 'B' & Lloc==1 & Horari==2))
BMI=nrow(subset(dades, Activitat == 'B' & Lloc==2 & Horari==1))
BTI=nrow(subset(dades, Activitat == 'B' & Lloc==2 & Horari==2))

CME=nrow(subset(dades, Activitat == 'C' & Lloc==1 & Horari==1))
CTE=nrow(subset(dades, Activitat == 'C' & Lloc==1 & Horari==2))
CMI=nrow(subset(dades, Activitat == 'C' & Lloc==2 & Horari==1))
CTI=nrow(subset(dades, Activitat == 'C' & Lloc==2 & Horari==2))

DME=nrow(subset(dades, Activitat == 'D' & Lloc==1 & Horari==1))
DTE=nrow(subset(dades, Activitat == 'D' & Lloc==1 & Horari==2))
DMI=nrow(subset(dades, Activitat == 'D' & Lloc==2 & Horari==1))
DTI=nrow(subset(dades, Activitat == 'D' & Lloc==2 & Horari==2))

EME=nrow(subset(dades, Activitat == 'E' & Lloc==1 & Horari==1))
ETE=nrow(subset(dades, Activitat == 'E' & Lloc==1 & Horari==2))
EMI=nrow(subset(dades, Activitat == 'E' & Lloc==2 & Horari==1))
ETI=nrow(subset(dades, Activitat == 'E' & Lloc==2 & Horari==2))

NUMact.lloc.horari=c(AME,ATE,AMI,ATI,BME,BTE,BMI,BTI,CME,CTE,CMI,CTI,DME,DTE,DMI,DTI,EME,ETE,EMI,ETI)
act.lloc.horari=c('AME','ATE','AMI','ATI','BME','BTE','BMI','BTI','CME','CTE','CMI','CTI','DME','DTE','DMI','DTI','EME','ETE','EMI','ETI')
freq2=data.frame(act.lloc.horari,NUMact.lloc.horari)
par (mfrow=c(1,1))
plot(freq2,main="Freqüències Activitat-Lloc-Horari",ylab="Freqüència", xlab="Combinació")

#Número d'activitats per dia
Dia1=subset(dades, Dia==1)
Dia2=subset(dades, Dia==2)
Dia3=subset(dades, Dia==3)
Dia4=subset(dades, Dia==4)
Dia5=subset(dades, Dia==5)

install.packages('dplyr')
library(dplyr)
grup1 <- group_by(Dia1, Activitat)
summarise(grup1,
          num = n()
)
grup2 <- group_by(Dia2, Activitat)
summarise(grup2,
          num = n()
)
grup3 <- group_by(Dia3, Activitat)
summarise(grup3,
          num = n()
)
grup4 <- group_by(Dia4, Activitat)
summarise(grup4,
          num = n()
)
grup5 <- group_by(Dia5, Activitat)
summarise(grup5,
          num = n()
)

# Anàlisi descriptiva bàsica
summary(dades)

# Boxplots dels estats
par(mfrow=c(3,2))

boxplot(dades$`Abans - Estrés`,main="Abans-Estrès",ylim=c(0,10),ylab="Nivell d'estrès")
boxplot(dades$`Despres - Estrés`,main="Després-Estrès",ylim=c(0,10))

boxplot(dades$`Abans - Avorriment`,main="Abans-Avorriment",ylim=c(0,10), ylab="Nivell d'avorriment")
boxplot(dades$`Despres - Avorriment`,main="Després-Avorriment",ylim=c(0,10))

boxplot(dades$`Abans - Felicitat`,main="Abans-Felicitat",ylim=c(0,10), ylab="Nivell de felicitat")
boxplot(dades$`Despres - Felicitat`,main="Després-Felicitat",ylim=c(0,10))



# #################
# Actualització base de dades

estres=dades$`Despres - Estrés`-dades$`Abans - Estrés`
avo=dades$`Despres - Avorriment`-dades$`Abans - Avorriment`
feli=dades$`Despres - Felicitat`-dades$`Abans - Felicitat`


data=data.frame(dades$Individu,dades$Dia,dades$Horari,dades$Lloc,dades$Activitat,estres,avo,feli)
names(data)=c("ind","dia","horari","lloc","act","estres","avorriment","felicitat")
head(data)


# ############
# Anàlisi requisits ANOVA

# Estres
mod.estres=lm(estres~act+lloc+horari,data=data)

# Igualtat de variàncies del model
par (mfrow=c(1,1))
library(lmtest)
bptest(mod.estres)
plot (mod.estres,which=3)

# Normalitat del test
par (mfrow=c(1,3))
plot (mod.estres, which=2)
boxplot (mod.estres$residuals, main="Boxplot dels residus del model")
require (car); qqPlot (mod.estres,distribution = "t",main="qqPlot")
ks.test(mod.estres$residuals,"pnorm",mean(mod.estres$residuals),sd(mod.estres$residuals))

# Avorriment
# Model sense interaccions ni blocs
mod.avo=lm(avorriment~act+lloc+periode,data=data)

# Igualtat de variàncies del model
par (mfrow=c(1,1))
bptest(mod.avo)
plot (mod.avo,which=3)

# Normalitat del test
par (mfrow=c(1,3))
plot (mod.avo, which=2)
boxplot (mod.avo$residuals, main="Boxplot dels residus del model")
require (car); qqPlot (mod.avo,distribution = "t",main="qqPlot")
ks.test(mod.avo$residuals,"pnorm",mean(mod.avo$residuals),sd(mod.avo$residuals))

# Felicitat
# Model sense interaccions ni blocs
mod.feli=lm(felicitat~act+lloc+periode,data=data)

# Igualtat de variàncies del model
par (mfrow=c(1,1))
bptest(mod.feli)
plot (mod.feli,which=3)

# Normalitat del test
par (mfrow=c(1,3))
plot (mod.feli, which=2)
boxplot (mod.feli$residuals, main="Boxplot dels residus del model")
require (car); qqPlot (mod.feli,distribution = "t",main="qqPlot")
ks.test(mod.feli$residuals,"pnorm",mean(mod.feli$residuals),sd(mod.feli$residuals))

# ###############
# Analisi principal
require(emmeans)
require(lmerTest)
require (lmtest)
require (nortest)

par (mfrow=c(1,1))
# Estres
m1.1=lmer(estres~act*lloc*horari+dia+(1|ind), data=data)
m1=lm(estres~act*lloc*horari, data=data)


summary(m1)
model1=summary(m1.1)
model1

anova(m1)
anova(m1.1)

means1=emmeans(m1,~act)
means1
pairs(means1)
emmeans(m1,~act+lloc)
emmeans(m1,~act+horari)
emmeans(m1,~act+lloc+horari)

# Avorriment
m2=lm(avo~act*lloc*horari+dia, data=data)
m2.1=lmer(avo~act*lloc*horari+dia+(1|ind), data=data)

summary(m2)
model2=summary(m2.1)
model2

anova(m2)
anova(m2.1)


means2=emmeans(m2,~act)
means2
pairs(means2)
emmeans(m2,~act+lloc)
emmeans(m2,~act+horari)
emmeans(m2,~act+lloc+horari)


# Felicitat
m3=lm(feli~act*lloc*horari+dia+ind, data=data)
m3.1=lmer(feli~act*lloc*horari+dia+(1|ind), data=data)

summary(m3)
model3=summary(m3.1)
model3

anova(m3)
anova(m3.1)

means3=emmeans(m3,~act)
means3
pairs(means3)
emmeans(m3,~act+lloc)
emmeans(m3,~act+horari)
emmeans(m3,~act+lloc+horari)

# Residus dels models
plot(model1$residuals); summary(model1$residuals)
plot(model2$residuals); summary(model2$residuals)
plot(model3$residuals); summary(model3$residuals)


