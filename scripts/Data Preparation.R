#Library
library(TSA)
library(tseries)
library(timeSeries)
library(FinTS)
library(forecast)
library(lmtest)
library(readr)
library(nortest)
library(MASS)
library(pracma)
library(fracdiff)
library(arfima)
library(stats)
library(rugarch)
library(fGarch)

############################### TAHAP 1 ########################################
#Data Preparation
#Import data
dataskripsi<- read.csv("C:/Users/A/OneDrive/Documents/SKRIPSI/dataihsg2018-2023.csv", header=TRUE)
str(dataskripsi)
head(dataskripsi)
datatr<-dataskripsi[1:1220,2]
str(datatr)
datates<-dataskripsi[1221:1459,2]
str(datates)
testing<-ts(datates, frequency = 239,start = c(2023,1,2))
testing
ihsgts<-ts(datatr,frequency = 244, start = c(2018,1,2))
ihsgts
data_keseluruhan<-dataskripsi[,2]
alldata<-ts(data_keseluruhan, frequency = 243, start = c(2018,1,2))
alldata
#Menampilkan plot time series data training
plot.ts(ihsgts, main= "INDEKS HARGA SAHAM GABUNGAN JANUARI 2018 - DESEMBER 2022", col= "blue", 
        ylab ="Indeks", xlab= "Hari ke-", type="o", pch=20)

#Statistika Deskriptif
summary(ihsgts)
#Identifikasi ACF
acf(ts(ihsgts),main="Plot ACF IHSG", lag.max = 30, plot = TRUE)
pacf(ts(ihsgts),main="Plot PACF IHSG", lag.max = 30, plot = TRUE)
#Identifikasi Plot Periodogram
periodogram(ihsgts, main="Plot Periodogram IHSG")
#Identifikasi long memory
hurstexp(ihsgts)

############################### TAHAP 2 ########################################
#Stasioneritas Rata-Rata (Uji Augmented Dickey-Fuller (ADF))
library(uroot)
library(tseries)
acf(ts(ihsgts), lag.max = 30, plot = TRUE)
adf.test(ihsgts, alternative = "stationary",k=1) #belum stasioner thd rata2

#Differencing data
diff.1.ihsg<-diff(ihsgts, difference = 1)
plot(diff.1.ihsg, type = "l",
     main="Differencing 1 Data Training IHSG",
     xlab= "Tahun",
     ylab= "Indeks")
diff.1.ihsg
#uji stasioner data differencing
adf.test(diff.1.ihsg, alternative=c("stationary"),k=1) #sudah stasioner thd rata2

#####Data sudah stasioner
#plot acf pacf
plot(acf(ts(diff.1.ihsg)), main="Plot ACF", lag.max = 30)
plot(pacf(ts(diff.1.ihsg)), main="Plot PACF", lag.max = 30)
