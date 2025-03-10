#PERAMALAN DATA TESTING MODEL ARFIMA-GARCH
garchSpec11new <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                             distribution.model = "norm",
                             fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit11new <- ugarchfit(spec=garchSpec11new, data=ihsgts)
coef(garchFit11new)
garchFit11new
AGnew<-cbind(garchFit11new@fit[["residuals"]],garchFit11new@fit[["var"]],
             garchFit11new@fit[["sigma"]])
AGnew
write.csv(AGnew, "AGnew.csv")

forc = ugarchforecast(garchFit11new,data = ihsgts, n.ahead = 239) 
forc
rAG<-cbind(forc@forecast[["seriesFor"]],forc@forecast[["sigmaFor"]])
rAG
write.csv(rAG, "rAG.csv")

forcAG<- read.csv("C:/Users/A/OneDrive/Documents/SKRIPSI/ramAG.csv", header=TRUE)
str(forcAG)
head(forcAG)
actual_testing<-forcAG$Actual.Data.Testing
ramalan_AG<-forcAG$Forecast_AG
ramalan_AG
###############Membuat data frame untuk plot
plot_dataAG<- data.frame(
  Date = seq(as.Date("2023-01-02"), by = "days", length.out = 239),
  Actual = testing,
  Forecast = ramalan_AG)

# Membuat time series
plot_data_tsAG<-ts(
  plot_dataAG[,c("Actual", "Forecast")],
  frequency = 239,
  start = c(2023,1,2))

# Plot time series dengan ggplot2
library(ggplot2)
plotAG<-autoplot(plot_data_tsAG) +
  labs(title = "Perbandingan Data Aktual dan Forecast Model ARFIMA-GARCH",
       y = "Nilai",
       x = "Tanggal") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  theme_minimal()
plotAG


############################### TAHAP 8 ########################################
#Menghitung MAPE model ARFIMA-GARCH
#MAPE ramalan rata2
# Memperoleh nilai peramalan
forecast_valuesAG <- ramalan_AG
# Menampilkan nilai peramalan
print("Nilai Peramalan:")
print(forecast_valuesAG)
# Membandingkan data aktual dan peramalan
comparison_AG <- data.frame(Actual = testing, Forecast = forecast_valuesAG)
# Menampilkan data aktual dan peramalan
print("Data Aktual dan Peramalan:")
print(comparison_AG)
# Menghitung nilai MAPE (Mean Absolute Percentage Error)
MAPEAG<- mean(abs((testing- forecast_valuesAG) / testing)) * 100
MAPEAG
# Menampilkan nilai MAPE
print(paste("Nilai MAPE Ramalan Mean:", MAPEAG))

#MAPE ramalan varian
FCV_AG<- read.csv("C:/Users/A/OneDrive/Documents/SKRIPSI/FCV_2024.csv", header=TRUE)
str(FCV_AG)
head(FCV_AG)
actual_var<-FCV_AG$var
FCV<-FCV_AG$F_var

# Membandingkan data aktual dan peramalan
comparison_CV <- data.frame(Actual = actual_var, Forecast = FCV)
# Menampilkan data aktual dan peramalan
print("Data Aktual dan Peramalan:")
print(comparison_CV)
# Menghitung nilai MAPE (Mean Absolute Percentage Error)
MAPECV<- mean(abs((actual_var^1/2- FCV^1/2) / actual_var^1/2)) * 100
MAPECV
# Menampilkan nilai MAPE
print(paste("Nilai MAPE Ramalan Varian:", MAPECV))
