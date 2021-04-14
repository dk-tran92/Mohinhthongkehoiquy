#Tieu luan
#Data2
#install MASS and plm
#install.packages("plm")
install.packages("ggplot2")


setwd('K:/0-Caohoc/HP1/Thongke/Tieu-luan/data')
housePrice <- read.csv('data2.csv', header = TRUE)
summary(housePrice)
dim(housePrice)
housePrice[1:5,]

housePrice.prepro <- subset( housePrice, select = -id)

housePrice.prepro[,'date'] <- as.Date(housePrice.prepro[,'date'], format = "%m/%d/%Y")
housePrice.prepro[,'date'] <- as.numeric(housePrice.prepro[,'date'])
housePrice.prepro[1:5,'date']


attach(housePrice.prepro)


pairs(price~., data = housePrice.prepro)


attach(housePrice.prepro)
housePrice.prepro[["price"]] <- log(price)
colnames(housePrice.prepro)[colnames(housePrice.prepro) == "price"] <- "log.price"


round(cor(housePrice.prepro), 2)
corrplot::corrplot(cor(housePrice.prepro), addCoef.col = "grey")

#BIC both
interModel <-lm(log.price ~ date + bedrooms + bathrooms + sqft_living + 
  sqft_lot + floors + view + condition + sqft_above)

baseModel = lm(log.price~1)
fullModel <- lm(log.price~., data = housePrice.prepro)

BIC.both <- MASS::stepAIC(interModel, direction = "both",
              scope = list(lower = baseModel, upper = fullModel), k = log(nrow(housePrice)))

summary(BIC.both)
vif(BIC.both)
par(mfrow=c(2,2))
plot(BIC.both)


a <- lm(formula = log.price ~ date + bedrooms + bathrooms + sqft_living15 + 
          sqft_lot + floors + view + condition + lat + grade + yr_built + 
          waterfront + zipcode + long + yr_renovated)
vif(lm(formula = log.price ~ date + bedrooms + bathrooms + sqft_living15 + 
         sqft_lot + floors + view + condition + lat + grade + yr_built + 
         waterfront + zipcode + long + yr_renovated))
summary(a)
plot(a)

anova(a, BIC.both)

#AIC both

AIC.both <- MASS::stepAIC(interModel, direction = "both",
                          scope = list(lower = baseModel, upper = fullModel), k = 2)

summary(AIC.both)
vif(AIC.both)
par(mfrow=c(2,2))
plot(AIC.both)


a <- lm(formula = log.price ~ date + bedrooms + bathrooms + sqft_living + 
          sqft_lot + floors + view + condition + sqft_above + lat + 
          grade + yr_built + sqft_living15 + waterfront + zipcode + 
          long + yr_renovated + sqft_lot15)
vif(a)
b <- lm(formula = log.price ~ date + bedrooms + bathrooms + 
          sqft_lot + floors + view + condition + sqft_above + lat + 
          grade + yr_built + sqft_living15 + waterfront + zipcode + 
          long + yr_renovated + sqft_lot15)
vif(b)
summary(a)
plot(a)

anova(b, a)





fullModel <- lm(log.price~., data = housePrice.prepro)
summary(fullModel)


housePrice.prepro[["sqft_basement"]] <- sqft_basement**0.01
colnames(housePrice.prepro)[colnames(housePrice.prepro) == "sqft_basement"] <- "pow.sqft_basement"

hist(housePrice.prepro$pow.sqft_basement)

pairs(log.price~pow.sqft_basement)

fullModel <- lm(log.price~., data = housePrice.prepro)
summary(fullModel)

par(mfrow=c(2,2))
plot(fullModel)


library(car)
vif(fullModel)

baseModel = lm(log.price~1)
BIC.Fwd <- step(baseModel,scope=list(upper=fullModel,lower=baseModel),direction="forward", k=log(nrow(housePrice)))


summary(BIC.Fwd)


par(mfrow=c(2,2))
plot(BIC.Fwd)

vif(BIC.Fwd)

BIC.FwdH0 <- lm(log.price ~ grade + lat + yr_built + view + bathrooms + 
                        sqft_living15 + condition + waterfront + floors + date + 
                        zipcode + pow.sqft_basement + yr_renovated + 
                        long + sqft_lot)

BIC.FwdH00 <- lm(log.price ~ grade + lat + yr_built + view + bathrooms + 
                        sqft_living15 + condition + waterfront + floors + date + 
                        zipcode + yr_renovated + 
                        long + sqft_lot)
anova(BIC.FwdH0,BIC.Fwd)
anova(BIC.FwdH00,BIC.Fwd)


summary(BIC.FwdH0)
vif(BIC.FwdH0)
summary(BIC.FwdH00)
vif(BIC.FwdH00)


a <- lm(formula = log.price ~ grade + lat + log(sqft_living) + yr_built + 
           view + bathrooms + sqft_living15 + condition + waterfront + 
           floors + date + zipcode + pow.sqft_basement + log(sqft_above) + 
           yr_renovated + long + sqft_lot + bedrooms)

summary(a)
vif(a)

b <- lm(formula = log.price ~ grade + lat + log(sqft_living) + yr_built + 
                view + bathrooms + sqft_living15 + condition + waterfront + 
                floors + date + zipcode + pow.sqft_basement + log(sqft_above) + 
                yr_renovated + long + sqft_lot + bedrooms)

summary(b)
vif(b)

c










housePrice.prepro[["sqft_living"]] <- log(sqft_living)
colnames(housePrice.prepro)[colnames(housePrice.prepro) == "sqft_living"] <- "log.sqft_living"

housePrice.prepro[["sqft_above"]] <- log(sqft_above)
colnames(housePrice.prepro)[colnames(housePrice.prepro) == "sqft_above"] <- "log.sqft_above"

housePrice.prepro[["sqft_living15"]] <- log(sqft_living15)
colnames(housePrice.prepro)[colnames(housePrice.prepro) == "sqft_living15"] <- "log.log.sqft_living15"

housePrice.prepro[["sqft_lot15"]] <- log(sqft_lot15)
colnames(housePrice.prepro)[colnames(housePrice.prepro) == "sqft_lot15"] <- "log.sqft_lot15"

sbm <- sqft_basement
for (i in 1:dim(housePrice)[1]) {
        if(sbm[i] == 0)
                sbm[i] = 10^-10
}



attach(housePrice.prepro)
baseModel = lm(log.price~1)
BIC.Fwd <- step(baseModel,scope=list(upper=fullModel,lower=baseModel),direction="forward", k=log(nrow(housePrice)))
summary(BIC.Fwd)
par(mfrow=c(2,2))
plot(BIC.Fwd)

hist(resid(BIC.Fwd))


BIC.Bwd <- step(fullModel,direction="backward", k=log(nrow(housePrice)))
summary(BIC.Bwd)
par(mfrow=c(2,2))
plot(BIC.Bwd)
hist(resid(BIC.Bwd))

detach(housePrice.prepro)
###########################################
###########################################

housePrice.prepro1 <- subset( housePrice, select = -c(id, date ) )

attach(housePrice.prepro1)
housePrice.prepro1[["price"]] <- log(price)
colnames(housePrice.prepro1)[colnames(housePrice.prepro1) == "price"] <- "log.price"

housePrice.prepro1[["sqft_living"]] <- log(sqft_living)
colnames(housePrice.prepro1)[colnames(housePrice.prepro1) == "sqft_living"] <- "log.sqft_living"

housePrice.prepro1[["sqft_above"]] <- log(sqft_above)
colnames(housePrice.prepro1)[colnames(housePrice.prepro1) == "sqft_above"] <- "log.sqft_above"

housePrice.prepro1[["sqft_living15"]] <- log(sqft_living15)
colnames(housePrice.prepro1)[colnames(housePrice.prepro1) == "sqft_living15"] <- "log.log.sqft_living15"

housePrice.prepro1[["sqft_lot15"]] <- log(sqft_lot15)
colnames(housePrice.prepro1)[colnames(housePrice.prepro1) == "sqft_lot15"] <- "log.sqft_lot15"

sbm <- sqft_basement
for (i in 1:dim(housePrice)[1]) {
        if(sbm[i] == 0)
                sbm[i] = 10^-10
}

housePrice.prepro1[["sqft_basement"]] <- sqft_basement**0.01
colnames(housePrice.prepro1)[colnames(housePrice.prepro1) == "sqft_basement"] <- "pow.sqft_basement"

hist(housePrice.prepro1$pow.sqft_basement)



fullModel1 <- lm(log.price~., data = housePrice.prepro1)
summary(fullModel1)

par(mfrow=c(2,2))
plot(fullModel1)

attach(housePrice.prepro1)
baseModel1 = lm(log.price~1)
BIC.Fwd1 <- step(baseModel1,scope=list(upper=fullModel1,lower=baseModel1),direction="forward", k=log(nrow(housePrice)))
summary(BIC.Fwd1)
par(mfrow=c(2,2))
plot(BIC.Fwd1)

hist(resid(BIC.Fwd1))


BIC.Bwd1 <- step(fullModel1,direction="backward", k=log(nrow(housePrice)))
summary(BIC.Bwd1)
par(mfrow=c(2,2))
plot(BIC.Bwd1)
hist(resid(BIC.Bwd1))

detach(housePrice.prepro1)
###########################################
###########################################






drophousePrice <- subset( housePrice, select = -c(id, date ) )

attach(drophousePrice)
drophousePrice[["price"]] <- log(price)
colnames(drophousePrice)[colnames(drophousePrice) == "price"] <- "log.price"

attach(drophousePrice)
fullModel2 <- lm(log.price~., data = drophousePrice)
summary(fullModel2)

par(mfrow=c(2,2))
plot(fullModel2)

baseModel2 = lm(log.price~1)


BIC.Fwd2 <- step(baseModel,scope=list(upper=fullModel2,lower=baseModel2),direction="forward", k=log(nrow(housePrice)))
summary(BIC.Fwd2)
par(mfrow=c(2,2))
plot(BIC.Fwd2)

hist(resid(BIC.Fwd2))


BIC.Bwd2 <- step(fullModel2,direction="backward", k=log(nrow(housePrice)))
summary(BIC.Bwd2)
par(mfrow=c(2,2))
plot(BIC.Bwd2)
hist(resid(BIC.Bwd2))

detach(drophousePrice)








fullModelLog = lm(log(price)~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront
               +view+condition+grade+sqft_above+I(sqft_basement^0.01)+yr_built
               +yr_renovated+zipcode+lat+long+sqft_living15+log(sqft_lot15), data = housePrice)

fullModelLog = lm(log(price)~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront
                  +view+condition+grade+sqft_above+(sqft_basement**0.01)+yr_built
                  +yr_renovated+zipcode+lat+long+sqft_living15+log(sqft_lot15), data = housePrice)

plot(fullModelLog)
baseModel = lm(log(price)~1)

step(fullModel,scope=list(upper=fullModel,lower=~1),direction="backward")
#lm(formula = log(price) ~ grade + lat + sqft_living + yr_built + 
#     view + bathrooms + sqft_living15 + condition + waterfront + 
#     floors + zipcode + long + sqft_lot + yr_renovated + bedrooms + 
#     sqft_lot15 + sqft_above)
step(baseModel,scope=list(upper=fullModel,lower=~1),direction="forward", k=log(nrow(housePrice)))
#lm(formula = log(price) ~ grade + lat + sqft_living + yr_built + 
#view + bathrooms + sqft_living15 + condition + waterfront + 
#  floors + zipcode + long + sqft_lot + yr_renovated + bedrooms)


#AIC
AICModel <- lm(formula = price ~ sqft_living + lat + view + grade + yr_built + 
                 waterfront + bedrooms + bathrooms + zipcode + long + condition + 
                 sqft_above + sqft_living15 + yr_renovated + sqft_lot15 + 
                 sqft_lot + floors)
AICModelLog <- lm(formula = log(price) ~ grade + lat + sqft_living + yr_built + 
     view + bathrooms + sqft_living15 + condition + waterfront + 
     floors + zipcode + long + sqft_lot + yr_renovated + bedrooms + 
     sqft_lot15 + sqft_above)

BICModelLog <- lm(formula = log(price) ~ grade + lat + sqft_living + yr_built + 
     view + bathrooms + sqft_living15 + condition + waterfront + 
     floors + zipcode + long + sqft_lot + yr_renovated + bedrooms)
par(mfrow=c(2,2))
plot(AICModelLog)
plot(BICModelLog)

plot(fullModelLog)
plot(fullModel)
summary(AICModelLog)
summary(BICModelLog)

dropDepenModel <- lm(formula = price ~ sqft_living + lat + view + grade + yr_built + 
                 waterfront + bedrooms + bathrooms + zipcode + long + condition + 
                 sqft_above + sqft_living15 + yr_renovated + sqft_lot15 + 
                 sqft_lot + floors)

summary(AICModel)
summary(fullModel)


step(baseModel,scope=list(upper=fullModel,lower=~1),direction="forward", k = log(dim(housePrice)[1]))
#BICModel <- #AIC
AICModel <- lm(formula = price ~ sqft_living + lat + view + grade + yr_built + 
                 waterfront + bedrooms + bathrooms + zipcode + long + condition + 
                 sqft_above + sqft_living15 + yr_renovated + sqft_lot15 + 
                 sqft_lot + floors)









model.0 <- lm(price~1)

add1(model.0, ~ sqft_living + lat + view + grade, test="F", k = 2)












install.packages("olsrr")
library(olsrr)

baseModel = lm(log(price)~.)
model <- lm(log(price) ~ ., data = housePrice)

k <- olsrr::ols_step_backward_aic(model)

k
