#PERAMALAN DATA IHSG 3 BULAN KEDEPAN (DATA HARIAN 2024)
garchSpec11fix <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
                             distribution.model = "norm",
                             fixed.pars = list(m = 0, arfima = TRUE, arfima.d = 0.218162))
garchFit11fix <- ugarchfit(spec=garchSpec11fix, data=alldata)
coef(garchFit11fix)
garchFit11fix

AG24<-cbind(garchFit11fix@fit[["residuals"]],garchFit11fix@fit[["var"]],
            garchFit11fix@fit[["sigma"]])
AG24
write.csv(AG24, "AG24.csv")

forc_2024 = ugarchforecast(garchFit11fix,data = alldata, n.ahead = 90) 
forc_2024
FAG<-cbind(forc_2024@forecast[["seriesFor"]],forc_2024@forecast[["sigmaFor"]])
FAG
write.csv(FAG, "FAG.csv")