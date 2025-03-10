#PERAMALAN MODEL ARFIMA TERBAIK
library(forecast)
library(arfima)
#PENENTUAN NILAI MAPE MASING-MASING MODEL ARFIMA
# mod1,mod2
#1. ARFIMA (2,d,0)
modARF1new<-arfima::arfima(ihsgts, order=c(2,0,0), dmean = FALSE, 
                           fixed=list(phi=c(NA,NA), frac = 0.218162))
modARF1new
summary(modARF1new)
resARF1ram <- modARF1new$modes[[1]][["residuals"]]
resARF1ram

tes1<-predict(modARF1new, n.ahead = length(testing))
tes1
# Memperoleh nilai peramalan
forecast_values1 <- tes1[[1]][["Forecast"]]
# Menampilkan nilai peramalan
print("Nilai Peramalan:")
print(forecast_values1)
# Membandingkan data aktual dan peramalan
comparison_df1 <- data.frame(Actual = testing, Forecast = forecast_values1)
# Menampilkan data aktual dan peramalan
print("Data Aktual dan Peramalan:")
print(comparison_df1)
# Menghitung nilai MAPE (Mean Absolute Percentage Error)
MAPE1<- mean(abs((testing - forecast_values1)/testing)) * 100
MAPE1
# Menampilkan nilai MAPE
print(paste("Nilai MAPE Model 1:", MAPE1))

write.csv(forecast_values1, "ramARFIMA.csv")
write.csv(resARF1ram, "resARF1ram.csv")

#2. ARFIMA (0,d,2)
modARF2new<-arfima::arfima(ihsgts, order=c(0,0,2), dmean = FALSE, 
                           fixed=list(theta=c(NA,NA), frac = 0.218162))
modARF2new
summary(modARF2new)

tes2<-predict(modARF2new, n.ahead = length(testing))
tes2
# Memperoleh nilai peramalan
forecast_values2 <- tes2[[1]][["Forecast"]]
# Menampilkan nilai peramalan
print("Nilai Peramalan:")
print(forecast_values2)
# Membandingkan data aktual dan peramalan
comparison_df2<- data.frame(Actual = testing, Forecast = forecast_values2)
# Menampilkan data aktual dan peramalan
print("Data Aktual dan Peramalan:")
print(comparison_df2)
# Menghitung nilai MAPE (Mean Absolute Percentage Error)
MAPE2<- mean(abs((testing - forecast_values2)/testing)) * 100
MAPE2
# Menampilkan nilai MAPE
print(paste("Nilai MAPE Model 2:", MAPE2))

#Nilai MAPE (d GPH)
NilaiMAPE<-cbind(MAPE1,MAPE2)
NilaiMAPE
min(NilaiMAPE)

###############Membuat data frame untuk plot
plot_dataA<- data.frame(
  Date = seq(as.Date("2023-01-02"), by = "days", length.out = 239),
  Actual = testing,
  Forecast = forecast_values1)

# Membuat time series
plot_data_tsA<- ts(
  plot_dataA[,c("Actual", "Forecast")],
  frequency = 239,
  start = c(2023,1,2))

# Plot time series dengan ggplot2
library(ggplot2)
plotA<-autoplot(plot_data_tsA) +
  labs(title = "Perbandingan Data Aktual dan Forecast Model ARFIMA",
       y = "Nilai",
       x = "Tanggal") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  theme_minimal()
plotA

############################### TAHAP 5 ########################################
#Analisis ACF dan PACF pada residual
par(mfrow=c(2,1))
acf(resARF1^2, lag.max=20, main="ACF Kuadrat Residual Model 1")
pacf(resARF1^2, lag.max=20, main="PACF Kuadrat Residual Model 1")

#Pengujian lag signifikan efek ARCH
lags <- 1:50
arch_tests <- vector("list", length = length(lags))
for (i in seq_along(lags)) {
  arch_tests[[i]] <- ArchTest(resARF1, lags = lags[i], demean = TRUE)
}

# Menampilkan hasil uji ARCH untuk setiap lag
for (i in seq_along(lags)) {
  cat("Lag:", lags[i], "\n")
  print(arch_tests[[i]])
  cat("\n")
}
lmtest_mod1 <- ArchTest(resARF1, lags = 50)
lmtest_mod1
