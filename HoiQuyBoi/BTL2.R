# Y: muc do ben deo cua nhua
Y <- c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
# X1: do day cua vat lieu
X1 <- c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
# X2: mat do cua vat lieu
X2 <- c(4.0, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)

#Cac mo hinh hoi quy tuyen tinh
model1 <- lm(Y~X1)
summary(model1)

model2 <- lm(Y~X2)
summary(model2)

model3 <- lm(Y~X1+X2)
summary(model3)

#He so xac dinh R-square
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared

#He so xac dinh hieu chinh Adj-R-square
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
summary(model3)$adj.r.squared

#Bang ANOVA cho mo hinh 2 bien
anova(model3)

#Kiem dinh gia thuyet H0: Beta1 = Beta2 = 0
SSE <- anova(model3)[3,2]
SSR <- anova(model3)[1,2] + anova(model3)[2,2]
  
  #p = 3 so luong he so hoi quy
  #n = 12 tong so quan sat trong mau
F_obs <- (SSR/2)/(SSE/9) #Gia tri thong ke
F_obs

isFail_H0 <- F_obs > qf(0.05, 2, 9)
isFail_H0
#True. => Bac bo H0

#Khoang tin cay voi muc y nghia 5% cho Beta1
#tren mo hinh chi co bien 'do day vat lieu'
confint(model1, level = 0.95)[2,]

#Khoang tin cay voi muc y nghia 5% cho cac he so hoi quy
#tren mo hinh 2 bien doc lap
confint(model3, level = 0.95)

