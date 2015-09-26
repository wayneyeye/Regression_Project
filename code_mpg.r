library(ggplot2);library(GGally);library(dplyr);library(grid);library(gridExtra)
data(mtcars);

ggpairs(mtcars)

mtcars2<-mutate(mtcars,mpg,disp,wt,hp,Cylinder=as.factor(cyl),AutoTransmission=as.factor(am))
mtcars2<-select(mtcars2,mpg,disp,wt,hp,Cylinder,AutoTransmission)

(g1<-ggplot(data=mtcars2,aes(x=AutoTransmission,y=mpg,col=AutoTransmission))+geom_violin(aes(fill=AutoTransmission),size=2)
+scale_color_brewer(palette="Set1")
+ggtitle("Exploratory Plot: mpg ~ AutoTransmission")
+labs(x="Transmission Type",y="Miles/(US)Gallon"))



g2<-ggplot(data=mtcars2,aes(x=disp,y=mpg,col=AutoTransmission))
(g2+geom_point(size=4,alpha=0.7)
+scale_color_brewer(palette="Set1")
+ggtitle("Exploratory Plot: mpg ~ displacement")
+labs(x="Displacement (cu.in.)",y="Miles/(US)Gallon")
)


fit_all<-lm(data=mtcars2,mpg~hp+AutoTransmission)
par(mfrow=c(2,2))
for (i in 1:4){
        plot(fit_all,which=i)
}

