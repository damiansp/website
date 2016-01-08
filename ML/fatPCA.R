rm(list = ls())
search()
detach()
library(FactoMineR)

#60
fat60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/fatOutFinal60FATranspose.csv')

fat60[1:10, 1:10]
rownames(fat60) <- fat60[,1]
fat60 <- fat60[,-1]
attach(fat60)
COLS = dim(fat60)[2]

#All
allPCA <- PCA(fat60[, 2:COLS], quali.sup=c(1,2))
barplot(allPCA$eig[,1], main='Eigenvalues', names.arg=paste('dim', 1:nrow(allPCA$eig)))
plot(allPCA, habillage=1, main="Principal Components Map")
plot(allPCA, habillage=2, axes=c(1, 3))


#Prey only
prey60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/prey60transpose.csv')

fat60[1:10, 1:10]
detach(fat60)
attach(prey60)
COLS = dim(prey60)[2]

#use: ?plot.PCA to see documentation
preyPCA <- PCA(prey60[, 2:COLS], quali.sup=c(1,2))
barplot(preyPCA$eig[,1], main='Eigenvalues', names.arg=paste('dim', 1:nrow(preyPCA$eig)))
plot(preyPCA, habillage=2, label='', title='', ylab='Second Principal Component \n(12.9% of the total variance)')
plot(preyPCA, axes=c(3,4), habillage=2, label='', title='', ylab='Second Principal Component \n(12.9% of the total variance)')
par(xpd = NA)	#change default setting to allow text anywhere in device
rect(0,-10, 5, -11.5, col='white', border=F)
rect(-10,3, -8.5, -3, col='white', border=F)
text(2.5, -10.5, 'First Principal Component\n(accounts for 14.9% of the total variance)')
text(-9, 0, 'Second Principal Component\n(accounts for 12.9% of the total variance)', srt=90)
par(xpd = F) #reset to default
plot(preyPCA, habillage=2, axes=c(1, 3))

#extract information from the model
preyPCA	#see accessible values
preyPCA$var

#Otters
otter60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/otter60transpose.csv')

fat60[1:10, 1:10]
detach(prey60)
attach(otter60)
COLS = dim(otter60)[2]
head(otter60)

otterPCA <- PCA(otter60[, c(2, (4:COLS))], quali.sup=c(1))
barplot(otterPCA$eig[,1], main='Eigenvalues', names.arg=paste('dim', 1:nrow(otterPCA$eig)))

plot(otterPCA, habillage=1, label='', title='')
par(xpd = NA)	#change default setting to allow text anywhere in device
rect(-3,-14, 4, -16, col='white', border=F)
rect(-17, 3, -15, -4, col='white', border=F)
text(0, -14.5, 'First Principal Component\n(accounts for 33.7% of the total variance)')
text(-15.7, -1, 'Second Principal Component\n(accounts for 9.8% of the total variance)', srt=90)
par(xpd = F) #reset to default
rect(-12.96,10.523, -10,6, col='white', border=F)
text(-10, 9.5, 'otter footpads')
text(-10, 8.5, 'otter tails', col=2)
rect(-15,11, -7, 7.7)

#extract info:
otterPCA$var

#Molluscs
mollusc60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/mollusc60transpose.csv')

fat60[1:10, 1:10]
detach(otter60)
detach(mollusc60)
attach(mollusc60)
COLS = dim(mollusc60)[2]
head(mollusc60)

molluscPCA <- PCA(mollusc60[, c(2, (4:COLS))], quali.sup=c(1))
barplot(molluscPCA$eig[,1], main='Eigenvalues', names.arg=paste('dim', 1:nrow(molluscPCA$eig)))

plot(molluscPCA, habillage=1, label='', title='')
plot(molluscPCA, habillage=1, label='', title='', axes=c(3,4))
plot(molluscPCA, habillage=1, label='', title='', axes=c(4,5))

par(xpd = NA)	#change default setting to allow text anywhere in device
rect(-3,-9, 4, -12, col='white', border=F)
rect(-15, 4, -13, -4, col='white', border=F)
text(0, -10, 'First Principal Component\n(accounts for 22.1% of the total variance)')
text(-13, 1.5, 'Second Principal Component\n(accounts for 18.5% of the total variance)', srt=90)
par(xpd = F) #reset to default
rect(-12.96,10.523, -7,6, col='white', border=F)
text(-8.5, 9.3, 'Asian clam')
text(-8.5, 8.6, 'Fat mucket', col=2)
text(-8.5, 7.9, 'Three-ridge', col=3)
text(-8.5, 7.2, 'Round pigtoe', col=4)
text(-8.5, 6.5, 'Wabash pigtoe', col=5)
rect(-10.8,9.93, -6, 5.7)

#extract info
molluscPCA$var

plot(molluscPCA, habillage=1, axes=c(3, 4))


#Fishes
fish60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/fish60transpose.csv')

fat60[1:10, 1:10]
detach(otter60)
detach(mollusc60)
attach(fish60)
COLS = dim(fish60)[2]
head(fish60)

fishPCA <- PCA(fish60[, c(2, (4:COLS))], quali.sup=c(1))
barplot(fishPCA$eig[,1], main='Eigenvalues', names.arg=paste('dim', 1:nrow(fishPCA$eig)))

plot(fishPCA, habillage=1, label='', title='')
plot(fishPCA, habillage=1, label='', title='', axes=c(3,4))
plot(fishPCA, habillage=1, label='', title='', axes=c(4,5))

par(xpd = NA)	#change default setting to allow text anywhere in device
rect(-3,-9, 4, -12, col='white', border=F)
rect(-15, 4, -13, -4, col='white', border=F)
text(0, -10, 'First Principal Component\n(accounts for 22.1% of the total variance)')
text(-13, 1.5, 'Second Principal Component\n(accounts for 18.5% of the total variance)', srt=90)
par(xpd = F) #reset to default
rect(-12.96,10.523, -7,6, col='white', border=F)
text(-8.5, 9.3, 'Asian clam')
text(-8.5, 8.6, 'Fat mucket', col=2)
text(-8.5, 7.9, 'Three-ridge', col=3)
text(-8.5, 7.2, 'Round pigtoe', col=4)
text(-8.5, 6.5, 'Wabash pigtoe', col=5)
rect(-10.8,9.93, -6, 5.7)

#Everything
fat60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/fatOutFinal60FATranspose.csv')

fat60[1:10, 1:10]
rownames(fat60) <- fat60[,1]
fat60 <- fat60[,-1]
attach(fat60)
COLS = dim(fat60)[2]
fat60 <- subset(fat60, Species != 'ob')
fat60 <- subset(fat60, Species != 'of')

#All
allPCA <- PCA(fat60[, 2:COLS], quali.sup=c(1,2))
barplot(allPCA$eig[,1], main='Eigenvalues', names.arg=paste('dim', 1:nrow(allPCA$eig)))
plot(allPCA, habillage=1, main="Principal Components Map")

plot(allPCA, habillage=1, label='', title='')
#plot(allPCA, habillage=1, label='', title='', axes=c(3,4))
#plot(allPCA, habillage=1, label='', title='', axes=c(4,5))

par(xpd = NA)	#change default setting to allow text anywhere in device
rect(0,-13, 6, -10.5, col='white', border=F)
rect(-15, -4, -9, 4, col='white', border=F)
text(4, -11.5, 'First Principal Component\n(accounts for 15.1% of the total variance)')
text(-9.5, 0, 'Second Principal Component\n(accounts for 11.6% of the total variance)', srt=90)
par(xpd = F) #reset to default
rect(-12.96,10.523, -7,6, col='white', border=F)
text(-8.5, 9.3, 'Asian clam')
text(-8.5, 8.6, 'Fat mucket', col=2)
text(-8.5, 7.9, 'Three-ridge', col=3)
text(-8.5, 7.2, 'Round pigtoe', col=4)
text(-8.5, 6.5, 'Wabash pigtoe', col=5)
rect(-10.8,9.93, -6, 5.7)










#Stats
#large taxonomic divisions
loadings <- read.csv('~/Desktop/FAT Working/loadings.csv')
tax.lm <- lm(loadings$Dim.1 ~ loadings$tax)
summary(tax.lm)
plot(tax.lm)
TukeyHSD(aov(tax.lm))
boxplot(loadings$Dim.1 ~ loadings$tax, ylab='PC1')

tax2.lm <- lm(loadings$Dim.2 ~ loadings$tax)
summary(tax2.lm)
plot(tax2.lm)
TukeyHSD(aov(tax2.lm))
boxplot(loadings$Dim.2 ~ loadings$tax, ylab='PC2')

tax3.lm <- lm(loadings$Dim.3 ~ loadings$tax)
summary(tax3.lm)
TukeyHSD(aov(tax3.lm))
boxplot(loadings$Dim.2 ~ loadings$tax, ylab='PC2')


#otter depots
otterLoad <- read.csv('~/Desktop/FAT Working/otterLoad.csv')
otterPC.lm <- lm(otterLoad$Dim.1 ~ otterLoad$depot)
summary(otterPC.lm)

#By species
#Crayfish
crayfish60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/crayfish60transpose.csv')
crayfish60[1:10, 1:10]
COLS = dim(crayfish60)[2]
crayfishPCA <- PCA(crayfish60[, 4:COLS])
crayfishLoad <- as.data.frame(crayfishPCA$ind$coord)
crayfishLoad <- cbind(crayfishLoad, crayfish60$Species)
names(crayfishLoad)[6] <- 'species'
crayfishPC.lm <- lm(crayfishLoad$Dim.1 ~ crayfishLoad$species)
summary(crayfishPC.lm)

crayfishPC.lm <- lm(crayfishLoad$Dim.2 ~ crayfishLoad$species)
summary(crayfishPC.lm)

crayfishPC.lm <- lm(crayfishLoad$Dim.3 ~ crayfishLoad$species)
summary(crayfishPC.lm)


#Frogs
frog60 <- read.csv('~/Desktop/FAT Working/FINAL FILES/frog60transpose.csv')
frog60[1:10, 1:10]
COLS = dim(frog60)[2]
frogPCA <- PCA(frog60[, 4:COLS])
frogLoad <- as.data.frame(frogPCA$ind$coord)
frogLoad <- cbind(frogLoad, frog60$Species)
names(frogLoad)[6] <- 'species'
frogPC.lm <- lm(frogLoad$Dim.1 ~ frogLoad$species)
summary(frogPC.lm)
TukeyHSD(aov(frogPC.lm))

frogPC2.lm <- lm(frogLoad$Dim.2 ~ frogLoad$species)
summary(frogPC2.lm)
TukeyHSD(aov(frogPC2.lm))

frogPC3.lm <- lm(frogLoad$Dim.3 ~ frogLoad$species)
summary(frogPC3.lm)
TukeyHSD(aov(frogPC3.lm))

frogPC4.lm <- lm(frogLoad$Dim.4 ~ frogLoad$species)
summary(frogPC4.lm)
TukeyHSD(aov(frogPC4.lm))

frogPC5.lm <- lm(frogLoad$Dim.5 ~ frogLoad$species)
summary(frogPC5.lm)
TukeyHSD(aov(frogPC5.lm))


#molluscs
molluscLoad <- as.data.frame(molluscPCA$ind$coord)
molluscLoad <- cbind(molluscLoad, mollusc60$Species)
names(molluscLoad)[6] <- 'species'
molluscPC.lm <- lm(molluscLoad$Dim.1 ~ molluscLoad$species)
summary(molluscPC.lm)
TukeyHSD(aov(molluscPC.lm))

molluscPC2.lm <- lm(molluscLoad$Dim.2 ~ molluscLoad$species)
summary(molluscPC2.lm)
TukeyHSD(aov(molluscPC2.lm))

molluscPC3.lm <- lm(molluscLoad$Dim.3 ~ molluscLoad$species)
summary(molluscPC3.lm)
TukeyHSD(aov(molluscPC3.lm))

molluscPC4.lm <- lm(molluscLoad$Dim.4 ~ molluscLoad$species)
summary(molluscPC4.lm)
TukeyHSD(aov(molluscPC4.lm))

molluscPC5.lm <- lm(molluscLoad$Dim.5~ molluscLoad$species)
summary(molluscPC5.lm)
TukeyHSD(aov(molluscPC5.lm))



#fish (Families)
fishLoad <- as.data.frame(fishPCA$ind$coord)
fishLoad <- cbind(fishLoad, fish60$Taxon)
names(fishLoad)[6] <- 'family'
fishPC.lm <- lm(fishLoad$Dim.1 ~ fishLoad$family)
summary(fishPC.lm)
TukeyHSD(aov(fishPC.lm))

fishPC2.lm <- lm(fishLoad$Dim.2 ~ fishLoad$family)
summary(fishPC2.lm)
TukeyHSD(aov(fishPC2.lm))

fishPC3.lm <- lm(fishLoad$Dim.3 ~ fishLoad$family)
summary(fishPC3.lm)
TukeyHSD(aov(fishPC3.lm))

fishPC4.lm <- lm(fishLoad$Dim.4 ~ fishLoad$family)
summary(fishPC4.lm)
TukeyHSD(aov(fishPC4.lm))


#(Species)
fishLoad2 <- as.data.frame(fishPCA$ind$coord)
fishLoad2 <- cbind(fishLoad2, fish60$Species)
names(fishLoad2)[6] <- 'species'
fish2PC.lm <- lm(fishLoad2$Dim.1 ~ fishLoad2$species)
summary(fish2PC.lm)
TukeyHSD(aov(fish2PC.lm))

fish2PC2.lm <- lm(fishLoad2$Dim.2 ~ fishLoad2$species)
summary(fish2PC2.lm)
TukeyHSD(aov(fish2PC2.lm))

fish2PC3.lm <- lm(fishLoad2$Dim.3 ~ fishLoad2$species)
summary(fish2PC3.lm)
TukeyHSD(aov(fish2PC3.lm))

fish2PC4.lm <- lm(fishLoad2$Dim.4 ~ fishLoad2$species)
summary(fish2PC4.lm)
TukeyHSD(aov(fish2PC4.lm))

fish2PC5.lm <- lm(fishLoad2$Dim.5 ~ fishLoad2$species)
summary(fish2PC5.lm)
TukeyHSD(aov(fish2PC5.lm))


#ALL (otter tails only --per fig. 5)
#(Species)
allLoad <- as.data.frame(allPCA$ind$coord)
allLoad <- cbind(allLoad, fat60$Taxon)
names(allLoad)[6] <- 'taxon'
allPC.lm <- lm(allLoad$Dim.1 ~ allLoad$taxon)
summary(allPC.lm)
TukeyHSD(aov(allPC.lm))

allPC2.lm <- lm(allLoad$Dim.2 ~ allLoad$taxon)
summary(allPC2.lm)
TukeyHSD(aov(allPC2.lm))
