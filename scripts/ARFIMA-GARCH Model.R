#Menentukan model volatilitas jika efek ARCH secara statistik signifikan
#Pendugaan model ARCH/GARCH dengan function garch secara simultan
# Mengambil nilai parameter d dari model ARFIMA
################ GARCH(1,1)
garchSpec11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE))
garchFit11 <- ugarchfit(spec=garchSpec11, data=diff.1.ihsg)
coef(garchFit11)
garchFit11
View(garchFit11)
res11<-garchFit11@fit$residuals
res.ts11<-ts(res11)
plot.ts(res.ts11^2)
plot.ts(resARF1^2)

################ GARCH(1,2)
garchSpec12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit12 <- ugarchfit(spec=garchSpec12, data=diff.1.ihsg)
coef(garchFit12)
garchFit12
res12<-garchFit12@fit$residuals
res.ts12<-ts(res12)
plot.ts(res.ts12)

################ GARCH(1,3)
garchSpec13 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit13 <- ugarchfit(spec=garchSpec13, data=diff.1.ihsg)
coef(garchFit13)
garchFit13
res13<-garchFit13@fit$residuals
res.ts13<-ts(res13)
plot.ts(res.ts13)

################ GARCH(2,1)
garchSpec21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit21 <- ugarchfit(spec=garchSpec21, data=diff.1.ihsg)
coef(garchFit21)
garchFit21
res21<-garchFit21@fit$residuals
res.ts21<-ts(res21)
plot.ts(res.ts21)

################ GARCH(2,2)
garchSpec22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit22 <- ugarchfit(spec=garchSpec22, data=diff.1.ihsg)
coef(garchFit22)
garchFit22
res22<-garchFit22@fit$residuals
res.ts22<-ts(res22)
plot.ts(res.ts22)

################ GARCH(2,3)
garchSpec23 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit23 <- ugarchfit(spec=garchSpec23, data=diff.1.ihsg)
coef(garchFit23)
garchFit23
res23<-garchFit23@fit$residuals
res.ts23<-ts(res23)
plot.ts(res.ts23)

################ GARCH(3,1)
garchSpec31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,1)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit31 <- ugarchfit(spec=garchSpec31, data=diff.1.ihsg)
coef(garchFit31)
garchFit31
res31<-garchFit31@fit$residuals
res.ts31<-ts(res31)
plot.ts(res.ts31)

################ GARCH(3,2)
garchSpec32 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit32 <- ugarchfit(spec=garchSpec32, data=diff.1.ihsg)
coef(garchFit32)
garchFit32
res32<-garchFit32@fit$residuals
res.ts32<-ts(res32)
plot.ts(res.ts32)

################ GARCH(3,3)
garchSpec33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)),
                          mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                          distribution.model = "norm",
                          fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit33 <- ugarchfit(spec=garchSpec33, data=diff.1.ihsg)
coef(garchFit33)
garchFit33
res33<-garchFit33@fit$residuals
res.ts33<-ts(res33)
plot.ts(res.ts33)

View(garchfit11)


############################### TAHAP 6 ########################################
#UJI DIAGNOSTIK MODEL ARFIMA-GARCH TERBAIK ARFIMA-GARCH(1,1)
# Hitung residual dari model GARCH
resid11<- residuals(garchFit11)
# Hitung varians terperam
conditional_variance <- sigma(garchFit11)^2
# Standarisasi residual
std_res11<- resid11/sqrt(conditional_variance)
# Hitung kuadrat residual standar
sqr_stdresid <- std_res11^2

#Uji Normalitas(p-value>alpha = normal)
# Melakukan uji normalitas menggunakan Lilliefors test
lillie.test(sqr_stdresid)

# Uji Ljung-Box pada residual model untuk memeriksa heteroskedastisitas
#Uji autokorelasi Ljung-Box (p-value>alpha = model layak)
Box.test(sqr_stdresid,lag = 5, "Ljung-Box") #model layak
Box.test(sqr_stdresid,lag = 10, "Ljung-Box") #model layak
Box.test(sqr_stdresid,lag = 20, "Ljung-Box") #model layak

# Uji ARCH pada residual untuk memeriksa efek heteroskedastisitas conditional
ALM_5<-ArchTest(sqr_stdresid, lags = 5)
ALM_10<-ArchTest(sqr_stdresid, lags = 10)
ALM_20<-ArchTest(sqr_stdresid, lags = 20)
print(ALM_5, digits = 10)
print(ALM_10, digits = 10)
print(ALM_20, digits = 20)
garchFit11
View(garchFit11)

#Density Plot Residual Model ARFIMA-GARCH
plot(density(res11), main = "Density Plot Residual Model ARFIMA-GARCH")
curve(dnorm(x, mean = mean(res11), sd = sd(res11)), 
      add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Residual", "Normal Distribution"), 
       col = c("black", "red"), lty = 1, lwd = c(1, 2))
