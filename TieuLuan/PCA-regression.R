
#Data tu chon
##################################################

setwd('K:/0-Caohoc/HP1/Thongke/Tieu-luan/data')

Concrete <- readxl::read_xls('Concrete_Data.xls')

#Doi ten bien de don gian trong qua trinh phan tich
colnames(Concrete)[1] <- 'Cement'
colnames(Concrete)[2] <- 'BlastFurnaceSlag'
colnames(Concrete)[3] <- 'FlyAsh'
colnames(Concrete)[4] <- 'Water'
colnames(Concrete)[5] <- 'Superplasticizer'
colnames(Concrete)[6] <- 'CoarseAgg'
colnames(Concrete)[7] <- 'FineAgg'
colnames(Concrete)[8] <- 'AgeDay'
colnames(Concrete)[9] <- 'ConcreteCS'

attach(Concrete)
#Danh gia mo hinh Full
pairs(ConcreteCS~., data = Concrete)

fullModel <- lm(log(ConcreteCS) ~ ., data = Concrete)
summary(fullModel)

par(mfrow=c(2,2))
plot(fullModel)
car::vif(fullModel)

detach(Concrete)
###########################################################
#               Principal Component Analyis               #
###########################################################
#Loai ra bien phu thuoc
ConcreteX <- Concrete[,-9]

#PCA cho cac bien doc lap
ConcreteX.PCA <- prcomp(ConcreteX, center = TRUE,scale. = TRUE)

#Ket qua PC tinh toan boi cac bien
par(mfrow=c(1,1))
plot(ConcreteX.PCA, type = c("lines"))
summary(ConcreteX.PCA)

#Matran he so tuong quan
corrplot::corrplot(cor(ConcreteX.PCA$x), addCoef.col = "gray")

#He so tinh toan cac PC
print(ConcreteX.PCA$rotation)

#Tri rieng ma tran he so tuong quan X va
#duong cheo ma tran hiep phuong sai PCA giong nhau
eigen(cor(ConcreteX))$value
diag(cov(ConcreteX.PCA$x[,]))

#Bieu do tuong quan giua cac PC va cac bien goc
par(mfrow=c(1,2))
biplot(ConcreteX.PCA, cex = 0.75)
biplot(ConcreteX.PCA, choices = c(1, 3))
biplot(ConcreteX.PCA, choices = c(2, 3))


###########################################################
#                     PCA Regression                      #
###########################################################
#Ket hop bien phu thuoc vao du lieu PCA va thuc hien hoi quy

data.PCA <- cbind(Concrete[,9], data.frame(ConcreteX.PCA$x))
attach(data.PCA)
ModelPCA <- lm(ConcreteCS ~ PC1+PC2+PC3+PC4+PC5, data = data.PCA)
summary(ModelPCA)

par(mfrow=c(1,1))
plot(ModelPCA$fitted.values,ConcreteCS,xlab="Fitted Values")
abline(lsfit(ModelPCA$fitted.values,ConcreteCS), col = "red")

par(mfrow=c(2,2))
plot(ModelPCA)

#stepwise AIC de chon bien PC
ModelPCABase <- lm(ConcreteCS ~ 1 , data = data.PCA)
ModelPCAFull <- lm(ConcreteCS ~ . , data = data.PCA)

ModelPCA.AIC <- MASS::stepAIC(ModelPCA, direction = "both",
            scope = list(lower = ModelPCABase, upper = ModelPCAFull))


summary(ModelPCA.AIC)

par(mfrow=c(1,1))
plot(ModelAIC$fitted.values,ConcreteCS,xlab="Fitted Values")
abline(lsfit(ModelAIC$fitted.values,ConcreteCS), col = "red")

par(mfrow=c(2,2))
plot(ModelAIC)

par(mfrow=c(1,1))
hist(ModelAIC$residuals)
car::vif(ModelAIC)
