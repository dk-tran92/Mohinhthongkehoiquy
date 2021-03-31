#https://archive.ics.uci.edu/ml/datasets/Tennis+Major+Tournament+Match+Statistics

setwd('E:/z_Study/CH/HP1/Thongke/Tieu-luan/Tennis-Major-Tournaments')

install.packages('psych')
#
wimData <- read.csv('Wimbledon-men-2013.csv', header = TRUE)
attach(wimData)
dim(wimData)
summary(wimData)

clData <- subset(wimData, select = -c(Player1, Player2, TPW.1, TPW.2))
summary(clData)

clData[is.na(clData)] <- 0
summary(clData)
dim(clData)

#Cach1##############################
library(psych)
fa.parallel(clData[,-1], 
            n.obs=114, 
            fa="pc", n.iter=100,
            show.legend=FALSE, 
            main="Scree plot with parallel analysis")


pc <- principal(clData[,-1],
                nfactors = 3,
                rotate = "none")

pc
plot(pc)
####################################


#Cach2##############################
wim.pca <- prcomp(clData[,c(3:38)], center = TRUE,scale. = TRUE)

summary(wim.pca)

str(wim.pca)

install.packages('devtools')
install.packages('ggbiplot')
library(devtools)
install_github("vqv/ggbiplot")


 library(ggbiplot)

ggbiplot(mtcars.pca)

####################################
