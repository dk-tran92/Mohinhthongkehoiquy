####Khoi tao du lieu

# Y: muc do ben deo cua nhua
Y <- c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)

# X1: do day cua vat lieu
X1 <- c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)

# X2: mat do cua vat lieu
X2 <- c(4.0, 6.3, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3., 2.8)

###Cac mo hinh hoi quy tuyen tinh
model1 <- lm(Y~X1)
summary(model1)

model2 <- lm(Y~X2)
summary(model2)

model3 <- lm(Y~X1+X2)
summary(model3)

###He so xac dinh R-square
summary(model1)$r.squared
summary(model3)$r.squared

###He so xac dinh hieu chinh Adj-R-square
summary(model1)$adj.r.squared
summary(model3)$adj.r.squared

###Bang ANOVA cho mo hinh 2 bien
anova(model3)



####Kiem dinh gia thuyet Beta1 = Beta2 = 0
H0_Beta1 <- 0 #H0
H0_Beta2 <- 0 #H0

SYY <- 0
for (y in Y){
  SYY <- SYY + (y - mean(Y))^2
}

SST <- SYY
SSE <- anova(model3)[3,2]
SSR <- SST - SSE

#p = 3 he so hoi quy
#n = 12 so quan sat trong mau
F_obs <- (SSR/2)/(SSE/9) #Gia tri thong ke
F_obs

#So sanh lai voi gia tri thong ke F tren R:
summary(model3)$fstatistic[1] - F_obs

isFail_H0 <- F_obs > qf(0.05, 2, 9)
isFail_H0
#Co y nghia moi quan he tuyen tinh

###Khoang tin cay voi muc y nghia 5% cho Beta1 cho mo hinh chi co bien 'do day vat lieu'
confint(model1, level = 0.95)[2,]

confint(model2, level = 0.95)[2,]

confint(model3, level = 0.95)


















pairs(Y~X1+X2,gap=0.5, cex.labels=1.5)

par(mfrow = c(2, 2))
plot(X1,Y, xlab='do day', ylab='muc do ben deo')
abline(model1, col = 'red')
#abline(lsfit(X1,Y), col = 'blue')

plot(X2,Y,xlab="m???t d???", ylab="d??? b???n d???o")
abline(model2, col = 'blue')

plot(model3)

anova(model3)
anova(model1)
