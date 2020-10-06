library(haven) 
library(pastecs)
library(ggplot2)
library(lattice)
library(outliers)
library(car)
library(emmeans)

dataa <- read_sav("ColorStudyCR.sav")
View(dataa)     
head(dataa, n = 10)
tail(dataa, n = 10)
is.data.frame(dataa) 
str(dataa)
dim(dataa)                    
names(dataa) 

apply(dataa,MARGIN=2,"class")
data1<-dataa 

nm1<-as.numeric(dataa$Material)   
f1<-factor(nm1, labels=c("Brx","Prt","Ktn","emx"))
typeof(f1)
attributes(f1)
unclass(f1)
data1$Material<-f1

nm2<-as.numeric(dataa$Solution)
f2<-factor(nm2, labels=c("Tea","Coffee","Wine","Aging"))
typeof(f2)
attributes(f2)
unclass(f2)
data1$Solution<-f2

Combinations2<-c()
l=c(1:240)
l[1]=1
i <- 2
while (i <= 15) {
  k <- i
  l[k]=1
  i<-i+1
}
l

i=1
while (i <= 15) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 16
while (i <= 30) {
  k <- i
  l[k]=2
  i<-i+1
}
i=16
while (i <= 30) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 31
while (i <= 45) {
  k <- i
  l[k]=3
  i<-i+1
}
i=31
while (i <= 45) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 46
while (i <= 60) {
  k <- i
  l[k]=4
  i<-i+1
}
i=46
while (i <= 60) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 61
while (i <= 75) {
  k <- i
  l[k]=5
  i<-i+1
}
i=61
while (i <= 75) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 76
while (i <= 90) {
  k <- i
  l[k]=6
  i<-i+1
}
i=76
while (i <= 90) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 91
while (i <= 105) {
  k <- i
  l[k]=7
  i<-i+1
}
i=91
while (i <= 105) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 106
while (i <= 120) {
  k <- i
  l[k]=8
  i<-i+1
}
i=106
while (i <= 120) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 121
while (i <= 135) {
  k <- i
  l[k]=9
  i<-i+1
}
i=121
while (i <= 135) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 136
while (i <= 150) {
  k <- i
  l[k]=10
  i<-i+1
}
i=136
while (i <= 150) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 151
while (i <= 165) {
  k <- i
  l[k]=11
  i<-i+1
}
i=151
while (i <= 165) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 166
while (i <= 180) {
  k <- i
  l[k]=12
  i<-i+1
}
i=166
while (i <= 180) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 181
while (i <= 195) {
  k <- i
  l[k]=13
  i<-i+1
}
i=181
while (i <= 195) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 196
while (i <= 210) {
  k <- i
  l[k]=14
  i<-i+1
}
i=196
while (i <= 210) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}

i <- 211
while (i <= 225) {
  k <- i
  l[k]=15
  i<-i+1
}
i=211
while (i <= 225) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}
i <- 226
while (i <= 240) {
  k <- i
  l[k]=16
  i<-i+1
}
i=226
while (i <= 240) {
  k <- i
  Combinations2[i]=l[i]
  i<-i+1
}
Combinations2
data5=cbind(data1, Combinations2)

nm4<-as.numeric(data5$Combinations2)
f4<-factor(nm4, labels=c("Brx Tea" , "Brx Coffee" , 
                         "Brx Wine" , "Brx Aging" , 
                         "Prt Tea" , "Prt Coffee" , 
                         "Prt Wine" , "Prt Aging" , 
                         "Ktn Tea" , "Ktn Coffee" , 
                         "Ktn Wine" , "Ktn Aging" ,
                         "Emx Tea" , "Emx Coffee" ,
                         "Emx Wine" , "Emx Aging"))

typeof(f4)
attributes(f4)
unclass(f4)
data5$Combinations2<-f4

class(data5)

data6<-as.data.frame(data5)
class(data6)

#names(data2)
shapirodataA1 <- list()

i=1
while (i <= 16) {
  k <- i
  x <- data6$CR[unclass(data6$Combinations2)==k]
  shapirodataA1[[k]] <- shapiro.test(x)
  i<-i+1
}

# pame na paroume ta apotelesmata twn prohgoumenwn test pou kaname shapirodataA1
i <- 1
while (i <= 16) {
  k <- i
  print(
    levels(data6$Combinations2)[k]);print(shapirodataA1[[k]])
  i<-i+1
  
}

leveneTest(CR ~ Material*Solution, data=data6, center=median)
#den uparei omoiogeneia stis dyaspores
#opote pame na efarmosoume to metasxhmatismo spread level plot
#exoun dokimastei kata seira ta exhs

#afou eginan ta shapiro test gia thn kathe stathmh parathrhthke kanonikothta
#me xrhsh tou levene test parathrhthhke oti den exome omoiogeneia stis diaspores
#efarmosthke to spread level plot gia na thoume ton proteinomeno metasxhmatismo
#efarmosthke o proteinomenos metasxhmatismos
#parathrhthhke oti h omoiogeneia stis diaspores veltiwthhke alla 
#oxi ston epithhmyto vathmo
#pragmatopoihthhhke efarmogh twn cochran kai doxon test gia apovolh shmeiwn 
#me paratyph symperifora
#me skopo na veltiwthei h omoiogeneia twn diasporwn
#den epiteuxthhke kati tetoio
#epomenws tha afairethoun orismoena shmeia exarxhs apo to dataset (apwleia<10%) 
#kai tha
#elegthei h kanonikothta metaxy twn stathmwn kai h omoiogeneia twn diasporwn
# h diadikasia poy perigrafike molis twra den fainetai mesa ston kwdika
#tha thewrhsoume stathmh shmantikothtas a=0.04

data6=data6[-c(169,197,180,238,227,205,204,232,202,236,239,219,
               194,210,206,189,235,226,225,218,192,240,45),]

spreadLevelPlot(CR ~ Combinations2 ,data=data6)

l2CR<-(data6$CR)^(-1.45)

data6<- transform (data6, l2CR = (CR)^(-1.45))

bartlett.test(l2CR ~ Combinations2 , data=data6)

leveneTest(l2CR ~ Material*Solution, data=data6, center=median)
#epomenws plhrountai oi proupotheseis gia to thn omoiogeneia twnn diasporwn

i <- 1
shapirodataA2 <- list()
while (i <= 16) {
  k <- i
  x <- data6$l2CR[unclass(data6$Combinations2)==k] # 
  shapirodataA2[[k]] <- shapiro.test(x)
  i<-i+1
}
#apotelesmata shapirodataA2
i <- 1
while (i <= 16) {
  k <- i
  print(
    levels(data6$Combinations2)[k]);print(shapirodataA2[[k]])
  i<-i+1
}
#kai exoume kanonikothta se kathe stathmh 

anoA<-lm(l2CR ~ Solution*Material, data = data6)
summary(anoA)
anova(anoA)
#sumfwna me ta apotelesmata pou prokuptoun apo ton pinaka ths analushs diasporas
#ws statistka shmantikoi paragontes prokuptei na einai oi 
#Material kai Solution:Material

#analush diasporas me tous statistika shmantikous paragontes

anoA3<-lm(l2CR ~  Material+Solution:Material, data = data6)
summary(anoA3)
anova(anoA3)

anoA2<-lm(l2CR ~  Material+Solution, data = data6)
summary(anoA2)
anova(anoA2)

#sugkrish duo montelwn
anova(anoA3,anoA2)
#epomenws ta duo montela diaferoun metaxu tous ,ara tha krathsoume 
#to anoA3(to pio poluploko)
# sygkriname me thn synartish anova kai ta montela anoA kai anoA3 
# h R den ebgale apotelesma gia to Pr(>F) opote proxwrisame ston ypologismo tou me 
# to xeri. Kratisame ws dedomeno tous vathmous eleytherias pou mas edwse ws 
# apotelesma h entolh anova(anoA,anoA3) pou itan 201 kai gia ta dyo montela
# opote to F = 1 giati 201 poly megalo
# opote to Pr(>F) isoutai me 0.5
#ara den yparxei statistika symantikh diafora metaksi twn montelwn anoA kai anoA3
# opote kratame to aploustero pou einai to anoA3

#######################################################################
#xrhsh ths sunarthshs aov kai update me skopo na katalhxoume pali 
#sto kalutero montelo

model<-aov(l2CR ~ Solution*Material,data=data6)
summary(model)
summary.lm(model)

plot(model)
names(model)  
res.m <- model$residuals

nm5<-as.numeric(data6$Solution)
f5<-factor(nm5, labels=c("Tea","Coffee","Wine","Aging"))

nm6<-as.numeric(data6$Material)
f6<-factor(nm6, labels=c("Brx","Prt","Ktn","emx"))

model2<-update(model , ~ . - Solution) 
summary(model2)     
summary.lm(model2)
#idia apotelesmata
##################################################################################
#                         3
##################################################################################
#pinakes analushs diasporas gia ta montela pou dokimasthkan alla 
#de vrethhkan shmantika
#kai apotelesmata gia tis sugriseis pou odhghsan sto teliko montelo
model<-aov(l2CR ~ Solution*Material,data=data6)
summary(model)
summary.lm(model)

anoA3<-lm(l2CR ~  Material+Solution:Material, data = data6)
summary(anoA3)
anova(anoA3)

anoA2<-lm(l2CR ~  Material+Solution, data = data6)
summary(anoA3)
anova(anoA3)

#sugkrish duo montelwn
anova(anoA,anoA2,anoA3)

################################################################################
#                          4
################################################################################
# Pairwise Comparisons
#kata zeugoi sugriseis
require(emmeans)
pairw2 <- emmeans(model2, pairwise ~  Solution + Material )
pairw2             

#elegxos twn proupothesewn pou prepei na ikanopoiei to teliko montelo
#to teliko einai to anoA3
shapiro.test(anoA3$res) 
#vlepoume oti exoume prosarmogh sthn kanonikh katanomh

qqnorm(anoA3$res)
qqPlot(anoA3$res,  distribution="norm", 
       layout=c(2,4), envelope=FALSE)
#elegxos statheris diasporas ton ypoloipwn
plot(anoA3$fitted.values, anoA3$residuals, xlab="fitted values", ylab="residuals")
abline(lm(anoA3$residuals ~ anoA3$fitted.values), col="red")

#ta upoloipa exoun ish diaspora giati katalamvanoun iso xwro panw kai katw
#apo thn kokkinh orizontia grammh kai auto sumvainei se olo to mhkos ths

#epomenws plhrountai oi proupotheseis pou prepei na threi to teliko montelo

################################################################################
#                          2
################################################################################

stat.desc(anoA3)

summary(anoA3)

round(stat.desc(data6$l2CR, basic=TRUE, desc=TRUE, norm=FALSE, p=0.96), 3)

with(data6, Boxplot(l2CR ~ Material+Material:Solution, 
                    notch=F,plot=T,
                    varwidth=TRUE,
                    col="lightgreen",
                    outline=T,show.names=T,horizontal=F,las = 1, xaxs="i",
                    cex.axis=0.7, cex.lab=1.5,
                    pch = 1,
                    main="l2CR",
                    xlab="Combinations2",ylab="l2CR",
                    id=list(labels=rownames(data6))))

histogram(~ l2CR | Combinations2,data=data6,
          type="percent", breaks="FD")

#epilexthhke to montelo ths analushs diasporas wste na ginei h statistikh analush
#sunoliaka ta vhmata pou akolouthhthhkan einai ta exhs
#afou eginan ta shapiro test gia thn kathe stathmh parathrhthke kanonikothta
#me xrhsh tou levene test parathrhthhke oti den exome omoiogeneia stis diaspores
#efarmosthke to spread level plot gia na thoume ton proteinomeno metasxhmatismo
#efarmosthke o proteinomenos metasxhmatismos
#parathrhthhke oti h omoiogeneia stis diaspores veltiwthhke alla oxi ston 
#epithhmyto vathmo
#pragmatopoihthhhke efarmogh twn cochran kai dixon test gia apovolh shmeiwn 
#me paratyph symperifora me skopo na veltiwthei h omoiogeneia twn diasporwn
#(mias kai h kanonikothta uphrxe ston epithumhto vathmo)
#den epiteuxthhke kati tetoio
#epomenws afairethkan orismoena shmeia exarxhs apo to dataset (apwleia<10%) kai 
#elegthke h kanonikothta metaxy twn stathmwn kai h omoiogeneia twn diasporwn
#me stathmh shmantikothtas 0.04 

#epomenws sunolika statistika shmantikoi vrethhkan oi paragontes 
#Material kai Solutio:Material
#exoun xrhsimopoihthei oi vivliothhkes car,ggplot2,haven ktl (fainontai stin arxh)
#
#