
#Kh???i t???o data

# Y: m???c d??? b???n d???o c???a nh???a
Y <- c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)

# X1: d??? dày v???t li???u
X1 <- c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)

#X2: m???t d??? v???t li???u
X2 <- c(4.0, 6.3, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3., 2.8)


model1 <- lm(Y~X1)
summary(model1)
#Y = 3.523 + 6.036*X1

model2 <- lm(Y~X2)
summary(model2)
#Y = 10.678 + 3.235*X2

model3 <- lm(Y~X1+X2)
summary(model3)
#Y = 2.8991 + 5.9945*X1 + 0.2121*X2

pairs(Y~X1+X2,gap=0.5, cex.labels=1.5)

par(mfrow = c(4, 2))
plot(X1,Y,xlab="d??? dày", ylab="d??? b???n d???o")
abline(model1, col = 'red')
#abline(lsfit(X1,Y), col = 'blue')

plot(X2,Y,xlab="m???t d???", ylab="d??? b???n d???o")
abline(model2, col = 'blue')

plot(model3)

anova(model3)
anova(model1)

#############
#4
#############
H0_Beta1 <- 0 #H0
H0_Beta2 <- 0 #H0


SYY <- 0
for (y in Y){
  SYY <- SYY + (y - mean(Y))^2
}

SST <- SYY
SSE <- anova(model3)[3,2]
SSR <- SST - SSE

p <- 2
n <- length(Y)
F_obs <- (SSR/(p-1))/(SSE/(n-p)) #Gia tri thong ke


alpha <- 5/100

isFail_H0 <- F_obs > qf(alpha, p-1, n-p)
#isFail_H0 = TRUE so fail H0: Beta1 = Beta2 = 0
#Co y nghia moi quan he tuyen tinh

#############
#5
#############
confint(model1, level = 0.95)[2,]

confint(model2, level = 0.95)[2,]

confint(model3, level = 0.95)
