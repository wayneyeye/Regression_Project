rm(list=ls())
library(ggplot2);library(GGally);library(dplyr);library(grid);library(gridExtra);library(car);library(xtable)
data(mtcars);



mtcars2<-mutate(mtcars,mpg,disp,wt,hp,Cylinder=as.factor(cyl),AutoTransmission=as.factor(am))
mtcars2<-select(mtcars2,mpg,disp,wt,hp,Cylinder,AutoTransmission)

(g1<-ggplot(data=mtcars2,aes(x=AutoTransmission,y=mpg,col=AutoTransmission))+geom_violin(aes(fill=AutoTransmission),size=2)
+scale_color_brewer(palette="Set1")
+ggtitle("Exploratory Plot: mpg ~ AutoTransmission")
+labs(x="Transmission Type",y="Miles/(US)Gallon"))



g2<-ggplot(data=mtcars2,aes(x=disp,y=mpg))+geom_point(size=4,alpha=0.7,aes(col=AutoTransmission))+geom_smooth(method="lm")+ggtitle("Exploratory Plot: mpg ~ displacement")+labs(x="Displacement (cu.in.)",y="Miles/(US)Gallon")
g3<-ggplot(data=mtcars2,aes(x=hp,y=mpg))+geom_point(size=4,alpha=0.7,aes(col=AutoTransmission))+geom_smooth(method="lm")+ggtitle("Exploratory Plot: mpg ~ horsepower")+labs(x="Horsepower",y="Miles/(US)Gallon")
g4<-ggplot(data=mtcars2,aes(x=wt,y=mpg))+geom_point(size=4,alpha=0.7,aes(col=AutoTransmission))+geom_smooth(method="lm")+ggtitle("Exploratory Plot: mpg ~ wt")+labs(x="Car weight (1000 lbs)",y="Miles/(US)Gallon")
g5<-ggplot(data=mtcars2,aes(x=Cylinder,y=mpg))+geom_point(size=4,alpha=0.7,aes(col=AutoTransmission))+ggtitle("Exploratory Plot: mpg ~ wt")+labs(x="Cylinders",y="Miles/(US)Gallon")
grid.arrange(g2,g3,g4,g5,ncol=2)


fit_1<-lm(data=mtcars2,mpg~AutoTransmission*wt+Cylinder)
fit_2<-lm(data=mtcars2,mpg~AutoTransmission*wt+Cylinder+hp+disp)
fit_all<-lm(data=mtcars2,mpg~AutoTransmission*wt+disp+hp+Cylinder)
par(mfrow=c(2,2))
for (i in 1:4){
        plot(fit_1,which=i)
}


summary(fit_1)

anova(fit_1,fit_2,fit_all)

vif(fit_all)

ggpairs(mtcars)

mtcars2<-mutate(mtcars2,Trn_Cyl=interaction(AutoTransmission,Cylinder))
g6<-ggplot(data=mtcars2,aes(x=wt,y=mpg))+geom_point(size=4,alpha=0.7,aes(col=Cylinder))+geom_smooth(method="lm",aes(group=AutoTransmission),size=1.5)
g6
