#Nhap data
dataPath <- ('C:/Users/SEHC/Desktop')
indicators <- read.table(file.path(dataPath,'indicators.txt'), header = TRUE)


#Xay dung mo hinh tuyen tinh
Model_ind <- lm(PriceChange ~ LoanPaymentsOverdue, data = indicators)
summary(Model_ind)

#Do thi mo hinh
par(mfrow=c(1,1))
plot(indicators$LoanPaymentsOverdue, indicators$PriceChange, 
     xlab = 'Loan Payments Overdue', ylab = 'Price Change')
abline(Model_ind,lwd=3,col="red")

######Cau a
#Khoang tin cay 95% cho Beta1
confint(Model_ind, level = 0.95)[2,]
#     2.5 %     97.5 % 
#-4.1634543 -0.3335853 

######Cau b
#khoang tin cay 95% cua trung binh E(Y|X=4)
predict(Model_ind, data.frame(LoanPaymentsOverdue = 4),
        interval = 'confidence', level = 0.95) 
#        fit       lwr       upr
#  -4.479585 -6.648849 -2.310322
