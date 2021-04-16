setwd('E:/z_Study/CH/HP1/Thongke/Tieu-luan/data')

housePriceFull <- read.csv('data2.csv', header = TRUE)

housePrice <- subset(housePriceFull, select = -c(id, date ) )
logprice <-log(housePrice['price'])

names(housePrice)[names(housePrice) == 'price'] <- 'logprice'
housePrice['logprice'] <- logprice

modelFull <- lm(logprice~., data = housePrice)
summary(modelFull)

housePriceX <- housePrice[,-1]

housePriceX.pca <- prcomp(housePriceX, center = TRUE, scale. =  TRUE)

plot(housePriceX.pca, type = c("lines"))
summary(housePriceX.pca)

print(housePriceX.pca$rotation)


#Tri rieng X va duong cheo PCA giong nhau
eigen(cor(housePriceX))$value
diag(cov(housePriceX.pca$x[,]))

cor(housePriceX.pca$x)

housePrice.pca <- cbind(housePrice[,1], data.frame(housePriceX.pca$x))
colnames(housePrice.pca)[1] <- 'logPrice' 

pairs(logPrice~PC1, data = housePrice.pca) #Khang

#check correlation matrix, show correlation of logPrice to each PC
cor(housePrice.pca)[,1]


housePrice.pca.modelF <- lm(logPrice ~ ., data = housePrice.pca)
summary(housePrice.pca.modelF)


housePrice.pca.modelS <- lm(logPrice ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8, data = housePrice.pca)
summary(housePrice.pca.modelS)

housePrice.pca.modelS1 <- lm(logPrice ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9, data = housePrice.pca)
summary(housePrice.pca.modelS1)
anova(housePrice.pca.modelS, housePrice.pca.modelS1)


betas1 <- housePriceX.pca$rotation %*% housePrice.pca.modelF$coefficients[-1]
betas1


summary(housePrice.pca.modelS)
vif(housePrice.pca.modelS)
par(mfrow=c(2,2))
plot(housePrice.pca.modelS)

anova(housePrice.pca.modelF, housePrice.pca.modelS)

##########################
setwd('E:/z_Study/CH/HP1/Thongke/Tieu-luan/Tennis-Major-Tournaments')


#
wimData <- read.csv('Wimbledon-men-2013.csv', header = TRUE)
attach(wimData)
dim(wimData)
summary(wimData)


