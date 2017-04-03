########plotting nest/ turtle abundance

data <- nest_abundance
x <- nest_abundance$`Turtle date`
y1 <- nest_abundance$`nb of nests`
y2 <- nest_abundance$`total turtles /night`
xday <- nest_abundance$day_since_start

summary(data)
head(data)
str(data)

plot(xday,y2,type='h',col='navy',lwd=3, xlab = 'Date', ylab = 'Number')
par(new=T)
lines(xday, y1,type='h',col='red',lwd=3)
legend("topright",c("nb of nests","nb of turtles"))
       col=c("red","navy", lwd=3)


##########fitting a gam##############

library(mgcv)
Model_1 <- gam(y1~s(xday,bs="cr", k=3),family=poisson(link=log),data=data)
Model_1
summary(Model_1)
plot(Model_1,residuals=TRUE)


### Predicting - in progress

Value.for.predict <- data.frame(x=x, xday=xday, y=y1)
predvals <- predict(Model_1, newdata=pred)

plot(x,y1,type='h',col='navy',lwd=3, xlab = 'Date', ylab = 'Number')
par(new=T)
lines(predvals+ attr(predvals, "constant"),type='l',col='red',lwd=3)


#### FROM ADRIAN
#Plotting the object EARLY
Value.for.predict <- data.frame(ToD=1:24, Season="Early")  ### This is where you specify over which values you want to predict your model, note you need the same predictors
##that your model has AND they must be the same column headings!
PredGAMMEarly <- predict.gam(GAMMAR2$gam,Value.for.predict, se.fit=TRUE, type="response")  ### This does the prediction
lower.band <- PredGAMMEarly$fit - PredGAMMEarly$se.fit  ### These two lines provide the SEE
upper.band <- PredGAMMEarly$fit + PredGAMMEarly$se.fit
GAMMrespEarly <- data.frame(Value.for.predict, PredGAMMEarly, lower.band, upper.band) #put it all together ready to be plotted with original data


write.csv(GAMMrespEarly, file="FinalLateGAMMlogit", row.names=FALSE)
