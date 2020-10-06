library(haven) 
library(ggplot2)
library(Hmisc)
library(outliers)
library(lattice)
library(car)
library(stargazer) 
library(pastecs)
library(psych)
library(nlme)
library(emmeans)
############################################################################################
#                                       1
#############################################################################################
dataa <- read_sav("H.1.Mat_Tim.sav")

View(dataa)    
head(dataa, n = 10)
tail(dataa, n = 10)
is.data.frame(dataa) 
str(dataa)
dim(dataa)                    
names(dataa) 
#kanei tis sthles numeric
apply(dataa,MARGIN=2,"class")
data1<-dataa  
nm1<-as.numeric(dataa$Material)   
f1<-factor(nm1, labels=c("1","2","3"))
typeof(f1)
attributes(f1)
unclass(f1)
data1$Material<-f1

#save(data1, dataa, file="newDataA") # # 
#eleghos gia to an to data 1 einai data.frame
class(data1)  # 
#prokuptei oti den einai katharo data.frame
#opote pame kai to kanoume emeis data.frame kai to apothekeuoume se kainouria metavlhth 
#etsi gia na xehwrizoume oti to kaname data.frame
data2<-as.data.frame(data1)
class(data2)   #  data frame
#kai me thn entolh class pleon vlepoume oti to data2 einai katharo data.frame
summary(data2) #  data frame
sd(data2$Time)

##############################################################################################
#                                           2
###############################################################################################

#thekogramata pou sou thinoun ton arithmo ths grammhs twn paratypwn shmeiwn
with(data2, Boxplot(Time ~ Material, 
                    notch=F,plot=T,
                    varwidth=TRUE,
                    outline=T,show.names=T,horizontal=F,las = 1, xaxs="i",
                    cex.axis=0.7, cex.lab=1.5,
                    pch = 1,
                    main="Time",
                    xlab="Material", ylab="Time",
                    id=list(labels=rownames(data2))))

#############################################################################################
#                                   3
############################################################################################           

shapirodataA1 <- list()
i <- 1
while (i <= 3) {
  k <- i
  x <- data2$Time[unclass(data2$Material)==k]
  shapirodataA1[[k]] <- shapiro.test(x)
  i<-i+1
}

# pame na paroume ta apotelesmata twn prohgoumenwn test pou kaname shapirodataA1
i <- 1
while (i <= 3) {
  k <- i
  print(
    levels(data2$Material)[k]);print(shapirodataA1[[k]])
  i<-i+1
}

qqPlot(Time ~ Material, data=data2, distribution="norm", 
       layout=c(2,4), envelope=FALSE)

#############################################################################################
#                                       4           
#########################################################################################          

bartlett.test(Time ~Material, data=data2)

leveneTest(Time ~ Material, data=data2, center=mean)

############################################################################################           
#                                     5
##########################################################################################           

# Box-Cox power transformation me skopo na veltiwsoume thn prosarmogh sthn kanonikh katanomh
bc1 <- powerTransform(Time~Material, 
                      family="bcPower", data2) 
summary(bc1)
lnTime<-log(data2$Time)

data3<- transform (data2, lnTime = log(Time)) 
View(data3)

bartlett.test(lnTime ~ Material, data=data3)
leveneTest(lnTime ~ Material, data=data3, center=mean)

i <- 1
shapirodataA2 <- list()
while (i <= 3) {
  k <- i
  x <- data3$lnTime[unclass(data3$Material)==k] 
  shapirodataA2[[k]] <- shapiro.test(x)
  i<-i+1
}

i <- 1
while (i <= 3) {
  k <- i
  print(
    levels(data3$Material)[k]);print(shapirodataA2[[k]])
  i<-i+1
}
#parathroume oti pleon exoume me xrhsh tou metasxhmatismou toso prosarmogh sthn kanonikh katanomh
#enw exoume kai omoiogeneia stis diaspores 
#auto sumvainei giati to shatiro test edeixe p-value megaluterh tou 0.05 se kathe stathmh
#kai antistoixa to levene test p-value megaluterh tou 0.05
#epomenws exoume th dunatothta na proxwrhsoume sto parakatw erwthma

############################################################################################           
#                                     6
##########################################################################################           

q <- aov(lnTime ~ Material,data3)          
summary(q)           
#h ermhneia pou prokuptei einai oti o paragontas Material einai statistika shmantikos
#exaitias tou oti  Pr(>F)= 0.00113

############################################################################################           
#                                     7
##########################################################################################           

paiw=emmeans(q,pairwise~Material)          
paiw           
#epomenws mono h 1-2 dinei statistika shmantiko apotelesma
#kai sumpairenoume oti kalutero uliko einai ti 1 "low"
# Material emmean    SE df lower.CL upper.CL
#1          1.52 0.138 42     1.24     1.80
#2          1.73 0.138 42     1.46     2.01
#3          2.27 0.138 42     2.00     2.55
#
#loge tou oti exei emmean mikrotero

############################################################################################           
#                                     8
##########################################################################################           

lmod <- lm(lnTime ~ Material, data3)
names(lmod)  
summary(lmod)
par(mfrow=c(1,2))
qqnorm(residuals(lmod),ylab="Residuals",main="")
qqline(residuals(lmod))
hist(residuals(lmod),xlab="Residuals",main="")
par(mfrow=c(1,1))
shapiro.test(residuals(lmod))
#epomensws prosarmogh twn upoloipwn sthn kanonikh katanomh
plot(lmod, which = 2)
plot(lmod)
par(mfrow = c(1,3))
plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)
#parathroume kai h deuterh prupothesh ikanopoieitai giati h ektash pou katalamvanoun ta shmeia panw kai katw apo thn kokkinh grammh 
#einai idia se olo to mhkos ths
plot(lmod, which = 1)  
plot(lmod, which = 3)
par(mfrow = c(1,1))
#
#