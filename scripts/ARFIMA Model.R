############ ARFIMA MODEL
#estimasi parameter d fraksional
fdGPH(diff.1.ihsg)
dgph<-diffseries(diff.1.ihsg, 0.218162)

#identifikasi model
par(mfrow=c(2,1))
acf(ts(dgph),main="Plot ACF", 30) #MA(q)
pacf(ts(dgph),main="Plot PACF", 30) #AR(p)

#Estimasi Parameter Model ARFIMA
##Tanpa Intercept
#1. ARFIMA (2,d,0)
modARF1<-arfima::arfima(z=diff.1.ihsg, order=c(2,0,0), dmean = FALSE, 
                        fixed=list(phi=c(NA,NA), frac = 0.218162))
modARF1
summary(modARF1) #signifikan

#2. ARFIMA (0,d,2)
modARF2<-arfima::arfima(z=diff.1.ihsg, order=c(0,0,2), dmean = FALSE, 
                        fixed=list(theta=c(NA,NA), frac = 0.218162))
modARF2
summary(modARF2) #signifikan

##Dengan Intercept
#3. ARFIMA (2,d,0)
modARF3<-arfima::arfima(z=diff.1.ihsg, order=c(2,0,0), dmean = TRUE, 
                        fixed=list(phi=c(NA,NA), frac = 0.218162))
modARF3
summary(modARF1) #signifikan

#4. ARFIMA (0,d,2)
modARF4<-arfima::arfima(z=diff.1.ihsg, order=c(0,0,2), dmean = TRUE, 
                        fixed=list(theta=c(NA,NA), frac = 0.218162))
modARF4
summary(modARF4) #signifikan

#Uji Diagnostik Model ARFIMA(p,d,q)dengan dgph=0.218162
#a. 1. ARFIMA (2,d,0)
# Jika resid(modARF1) adalah list dan ingin mengambil elemen pertamanya
# serta mengonversinya menjadi tipe numerik
resARF1 <- modARF1$modes[[1]][["residuals"]]
#Uji autokorelasi Ljung-Box (p-value>alpha = model layak)
Box.test(resARF1,lag = 1, "Ljung-Box") #model tdk layak
#Uji Normalitas Shapiro-Wilk (p-value>alpha = normal)
norm.data<-shapiro.test(resARF1)
norm.data #tdk normal
lillie.test(resARF1)

write.csv(resARF1, "resmod1.csv")

#b. 2. ARFIMA (0,d,2)
resARF2 <- modARF2$modes[[1]][["residuals"]]
#Uji autokorelasi Ljung-Box (p-value>alpha = model layak)
Box.test(resARF2,lag = 1, "Ljung-Box") #model tdk layak
#Uji Normalitas Shapiro-Wilk (p-value>alpha = normal)
norm.data<-shapiro.test(resARF2)
norm.data #tdk normal
lillie.test(resARF2)

#Pemilihan model terbaik
#Nilai AIC (dGPH)
NilaiAIC<-cbind(AIC(modARF1), AIC(modARF2))
NilaiAIC
min(NilaiAIC)#mod terbaik

#diagnostik sisaan model terbaik
resARF1 <- as.numeric(modARF3$modes[[1]][["residuals"]])
par(mfrow=c(2,2))
qqnorm(resARF1)
qqline(resARF1, col="blue", lwd=2)
plot(c(1:length(resARF1)),resARF1)
acf(ts(resARF1^2))
pacf(ts(resARF1^2))
