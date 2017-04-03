
Temp <- Copy_of_Data4loggers
par(mfrow=c(2,2))
plot(Temp$Temp11011629 [6:1460] ,type='l',col='black',lwd=3, xlab = 'Time since deployment (hours)', ylab = 'Temperature (°C)', xlim = c(-50, 1550),
     ylim = c(24,36))
abline(v=500, col="red", lty=2)
abline(v=500)
plot(Temp$Temp11023110 [60:1460] ,type='l',col='black',lwd=3, xlab = 'Time since deployment (hours)', ylab = 'Temperature (°C)', xlim = c(-50, 1550),
     ylim = c(24,36))
abline(v=500)
abline(v=500)
plot(Temp$Temp11023111 [48:1460] ,type='l',col='black',lwd=3, xlab = 'Time since deployment (hours)', ylab = 'Temperature (°C)', xlim = c(-50, 1550),
     ylim = c(24,36))
abline(v=500)
abline(v=500)
plot(Temp$Temp11023091 [10:1460] ,type='l',col='black',lwd=3, xlab = 'Time since deployment (hours)', ylab = 'Temperature (°C)', xlim = c(-50, 1550),
     ylim = c(24,36))
abline(v=500)
abline(v=500)


