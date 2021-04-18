#Tieu luan
#Data2

#Nhap du lieu
setwd('K:/0-Caohoc/HP1/Thongke/Tieu-luan/data')
housePrice <- read.csv('data2.csv', header = TRUE)

#Tong quan
summary(housePrice)
dim(housePrice)
housePrice[1:5,]

#######################
#Tien xu ly du lieu
housePrice.prepro <- subset( housePrice, select = -id)

#Chuyen thoi gian tu ky tu qua so
housePrice.prepro[,'date'] <- as.Date(housePrice.prepro[,'date'], format = "%m/%d/%Y")
housePrice.prepro[,'date'] <- as.numeric(housePrice.prepro[,'date'])
housePrice.prepro[1:5,'date']

#pairs(price~., data = housePrice.prepro)

#Log transfom cho bien price
par(mfrow=c(1,2))
hist(housePrice.prepro$price)
hist(log(housePrice.prepro$price))

housePrice.prepro[["price"]] <- log(price)
colnames(housePrice.prepro)[colnames(housePrice.prepro) == "price"] <- "log.price"

#Tuong quan giua cac bien
par(mfrow=c(1,1))
round(cor(housePrice.prepro), 2)
corrplot::corrplot(cor(housePrice.prepro), addCoef.col = "grey", number.cex=0.7)

#######################
#Xay dung mo hinh
attach(housePrice.prepro)
fullModel <- lm(log.price~., data = housePrice.prepro)

summary(fullModel)

#Danh gia mo hinh full
par(mfrow=c(1,1))
plot(fullModel$fitted.values,log.price,xlab="Fitted Values")
abline(lsfit(fullModel$fitted.values,log.price), col = "red")

par(mfrow=c(2,2))
plot(fullModel)


car::vif(lm(log.price ~ date + bedrooms + bathrooms + sqft_living + sqft_lot + 
            floors + waterfront + view + condition + grade + sqft_above + 
            yr_built + yr_renovated + zipcode + lat + long + 
            sqft_living15 + sqft_lot15))


#######################
#Model selection: Both BIC
baseModel <- lm(log.price~1)
fullModel <- lm(log.price~., data = housePrice.prepro)
interModel <-lm(log.price ~ bedrooms + bathrooms + sqft_living + sqft_lot +
                        grade  + condition + sqft_above + sqft_living15)

BIC.both <- MASS::stepAIC(interModel, direction = "both",
                          scope = list(lower = baseModel, upper = fullModel),
                          k = log(nrow(housePrice)))

summary(BIC.both)

#Kiem tra mo hinh da chon duoc
par(mfrow=c(1,1))
plot(BIC.both$fitted.values,log.price,xlab="Fitted Values")
abline(lsfit(BIC.both$fitted.values,log.price), col = "red")

par(mfrow=c(2,2))
plot(BIC.both)

par(mfrow=c(1,1))
hist(BIC.both$residuals)
car::vif(BIC.both)


BIC.both$coefficients
