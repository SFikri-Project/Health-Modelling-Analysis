

ols = lm(Y~X1+X3+X5, data = data)
summary(ols)
shapiro.test(resid(ols))

bptest(ols)

library(spgwr)
h = gwr.sel(data$Y~data$X1+data$X3+data$X5,
            coords = cbind(data$Long,data$Lat),
            data = data, adapt = FALSE, gweight = gwr.Gauss)

gwr2 = gwr(data$Y~data$X1+data$X3+data$X5,
           coords = cbind(data$Long,data$Lat),bandwidth = h,
           data = data, hatmatrix = TRUE, gweight = gwr.Gauss)

gwr2
names(gwr2)
names(gwr2$SDF)

bo = gwr2$SDF$"(Intercept)"
b1 = gwr2$SDF$"data$X1"
b2 = gwr2$SDF$"data$X2"
b3 = gwr2$SDF$"data$X3"
b4 = gwr2$SDF$"data$X4"
b5 = gwr2$SDF$"data$X5"


step(ols,direction = "both")


parametergwrl = cbind(bo,b1,b2,b3,b4,b5)
parametergwrl = as.data.frame(parametergwrl)
colnames(parametergwrl)=c("b0","b1","b2","b3","b4","b5")

BFC02.gwr.test(gwr2)

LMZ.F1GWR.test(gwr2)

LMZ.F3GWR.test(gwr2)




multikol1=as.data.frame(vif(ols))
multikol1
bptest((ols))




ln = lm(Y~X1+X2+X3+X4+X5, data = df)
summary(ln)       

multikl=as.data.frame(vif(ln))
multikl

library(spdep)
library(lmtest)
library(plm)
library(car)
library(GWmodel)

bptest(ln)


step(ln, direction = "both")

ln = lm(Y~X1+X2+X5, data = df)



ln = lm(Y~X2+X3+X4+X5, data = df)
bptest(ln)


lin = lm(Y~X1+X2+X3+X4+X5+X6+X7, data = df1)
bptest(lin)

h = gwr.sel(df$Y~df$X1+df$X2+df$X3+df$X4+df$X5,
            coords = cbind(df$Long,data$Lat),
            data = df, adapt = FALSE, gweight = gwr.Gauss)





t_X1=gwr2$SDF$`data$X1`/gwr2$SDF$`data$X1_se`
t_X2=gwr2$SDF$`data$X2`/gwr2$SDF$`data$X2_se`
t_X3=gwr2$SDF$`data$X3`/gwr2$SDF$`data$X3_se`
t_X4=gwr2$SDF$`data$X4`/gwr2$SDF$`data$X4_se`
t_X5=gwr2$SDF$`data$X5`/gwr2$SDF$`data$X5_se`

alpha=0.05

n=nrow(data)
ttabel=qt(1-(alpha/2),df=n-1)
ttest=cbind(t_X1,t_X2,t_X3,t_X4,t_X5,ttabel)
colnames(ttest)=c("X1","X2","X3","X4","X5","t-tabel")
ttest
View(parameter_gwr)
gwr2$SDF$