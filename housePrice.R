#Tieu luan
#Data2

setwd('E:/z_Study/CH/HP1/Thongke/Tieu-luan/data')

housePrice <- read.csv('data2.csv', header = TRUE)
attach(housePrice)
dim(housePrice)
install.packages("leaps")
library(leaps)

drophousePrice <- subset( housePrice, select = -c(id, date ))
dim(drophousePrice)
leaps::leaps( x=drophousePrice[,2:19], y=drophousePrice[,1], names=names(drophousePrice)[2:19], method="Cp")


leaps::regsubsets( x=housePrice[,4:21], y=housePrice[,3], names=names(housePrice)[4:21], method="Cp")



plm::detect.lindep(drophousePrice)


plot(housePrice[3],housePrice[4]) 
logprice <-log(price)
logsqft_basement <-log(sqft_basement)
logprice <-log(price)
logprice <-log(price)

pairs(logprice~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront)
pairs(logprice~bedrooms)
pairs(logprice~log(sqft_living)+log(sqft_lot)+log(sqft_above)+log(sqft_basement))
pairs(logprice~log(sqft_lot))
pairs(logprice~log(sqft_above))
pairs(logprice~log(sqft_basement))


pairs(logprice~sqft_living)
pairs(logprice~sqft_lot)
pairs(logprice~sqft_above)
pairs(logprice~sqft_basement + log(sqft_basement) + logsqft_basement)



logprice[which(is.nan(logprice))] = NA
logprice[which(logprice==Inf)] = NA

fullModel = lm(logprice~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront
               +view+condition+grade+sqft_above+sqft_basement+yr_built
               +yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15)

fullModelLog <- lm(log(price)~log(bedrooms)+log(bathrooms)+log(sqft_living)+log(sqft_lot)+floors+waterfront
               +view+condition+grade+sqft_above+sqft_basement+yr_built
               +yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15)

fullM_droplindep = lm(price~bedrooms+bathrooms+sqft_lot+floors+waterfront
               +view+condition+grade+sqft_above+yr_built
               +yr_renovated+lat+long+sqft_living15+sqft_lot15)


baseModel = lm(logprice~1)

step(baseModel,scope=list(upper=fullModel,lower=~1),direction="forward")
step(baseModel,scope=list(upper=fullModel,lower=~1),direction="forward")
#AIC
AICModel <- lm(formula = price ~ sqft_living + lat + view + grade + yr_built + 
     waterfront + bedrooms + bathrooms + zipcode + long + condition + 
     sqft_above + sqft_living15 + yr_renovated + sqft_lot15 + 
     sqft_lot + floors)
summary(AICModel)
summary(fullModel)
summary(fullM_droplindep)
summary(fullModelLog)

AIClog <- lm(formula = logprice ~ grade + lat + sqft_living + yr_built + 
               view + bathrooms + sqft_living15 + condition + waterfront + 
               floors + zipcode + long + sqft_lot + yr_renovated + bedrooms + 
               sqft_lot15 + sqft_above)
summary(AIClog)

AICModel_drop_floors <- lm(formula = price ~ sqft_living + lat + view + grade + yr_built + 
                 waterfront + bedrooms + bathrooms + zipcode + long + condition + 
                 sqft_above + sqft_living15 + yr_renovated + sqft_lot15 + 
                 sqft_lot)
summary(AICModel_drop_floors)
