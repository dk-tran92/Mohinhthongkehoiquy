setwd('E:/z_Study/CH/HP1/Thongke/Tieu-luan/data')

dat <- read.csv('data2.csv', header = TRUE)
dat[,'date']


dat[,'date'] <- as.Date(dat[,'date'], format = "%m/%d/%Y")
dat[,'date'] <- as.numeric(dat[,'date'])
dat[,'date'] <- dat[,'date'] - min(dat[,'date'])
dat[,'sqft_basement'] <- dat[,'sqft_basement'] ^ 0.01


summary(dat)
dropDat <- subset( dat, select = -c(id,sqft_basement))




fullM <- lm(log(price) ~ . , data = dropDat)
summary(fullM)


selectedMod <- step(lmMod)
summary(selectedMod)


plot(Temperature,StanRes1,ylab="Standardized Residuals")