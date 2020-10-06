library(haven)
library(nlme)
library(lme4)
library(Hmisc)
library(lattice)  
library(grid)
library(RLRsim)
library(car)
library(emmeans)
ryth <-as.data.frame( read.table("Def5.txt", h = T))

# energopoihsh tvn metavlitwn tou arxeiou
attach(ryth)

# h ilikia kai oi xronies xrisimopoiountai xrisimopoiountai ws statheroi paragontes
# alla kai ws synexeis tyxaies metablites 
AGE.f <- factor(AGE)
year.n.f <- factor(year.n)

# to symplirwmeno data frame
ryth.updated <- data.frame(ryth, AGE.f, year.n.f)
head(ryth.updated, n = 6)

summary(year.n.f)
summary(year.n.f)/536
summary(AGE.f)
summary(AGE.f)/536
table(AGE.f, year.n.f)
table(AGE.f, year.n.f)/536

# perigrafikh statistikh
g <- function(x)c(N=length(x),MIN=min(x,na.rm=TRUE),MAX=max(x,na.rm=TRUE),
                  MEDIAN=median(x,na.rm=TRUE), MEAN=mean(x,na.rm=TRUE),
                  SD=sd(x,na.rm=TRUE))
sum(is.na(DV))  # katametrisi xamenwn timwn
s1 <- summarize(DV,by=llist(year.n.f),g) ;s1 
class(s1)
s1.1 <- within(s1, {
  MEAN <- round(MEAN, 1) 
  SD <- round(SD, 1)})
s1.1            # me ena dekadiko psifio 
View(s1.1)  

# h synartish ayth montelopiei ta dedomena gia kathe ktirio
ryth.g1 <- groupedData(DV ~ year.n | id, 
                       outer = ~ AGE.f, data = ryth.updated)

# oi diaxronikes kampyles gia kathe ktirio
plot(ryth.g1, display = "id", outer = TRUE, aspect = 2, key = F,   
     xlab = "year.n", ylab = "DV", 
     main = "Individual Data by AGE Group") 
#sto parapanw diagramma blepoume to ruthmo vuthishs/anupswsis gia kathe arxiko hlikiako group kata thn 8ateh
##parathroume oti sta perissotera group pou prokuptoun apo thn arxikh hlikia tou kthriou, 
#uparxei diafora sto ruthmo vuthiseis twn kthruwn 

# h synartish ayth montelopiei ta dedomena gia kathe ilikia ktiriou
ryth.g2 <- groupedData(DV ~ year.n | AGE, 
                       outer = ~ AGE.f, data = ryth.updated)

# oi diaxronikes kampyles gia kathe ilikia ktiriou
plot(ryth.g2, display = "AGE", outer = TRUE, aspect = 2, key = F,
     xlab = "AGE", ylab = "DV", 
     main = "Individual Data by AGE Group") 


# ginetai proetoimasia gia th grammiki palindromisi ths DV ws synartisi 
#  ths ilikias, ilikias^2, omadas year.n ,  
# allilepidrasi ilikias kai year.n 
# kai allilepidrasi ilikias^2 me year.n. 
# epipleon symperilamvanontai 3 tyxaies epidraseis
# o tyxaios statheros oros (diafora metaksi twn ktiriwn), 
# h tyxaia epidrash tis xronias kai  
# h tyxaia epidrash tou tetragwnou tis xronias
# (random coefficient model). 
year.n.2 <- year.n - 1
year.n.2sq <- year.n.2*year.n.2
AGE2 <- AGE
AGE2[AGE == 166] <- 25
AGE2[AGE == 169] <- 24
AGE2[AGE == 175] <- 23
AGE2[AGE == 176] <- 22
AGE2[AGE == 179] <- 21
AGE2[AGE == 184] <- 20
AGE2[AGE == 189] <- 19
AGE2[AGE == 199] <- 18
AGE2[AGE == 208] <- 17
AGE2[AGE == 209] <- 16
AGE2[AGE == 212] <- 15
AGE2[AGE == 215] <- 14
AGE2[AGE == 217] <- 13
AGE2[AGE == 219] <- 12
AGE2[AGE == 224] <- 11
AGE2[AGE == 229] <- 10
AGE2[AGE == 234] <- 9
AGE2[AGE == 239] <- 8
AGE2[AGE == 259] <- 7
AGE2[AGE == 269] <- 6
AGE2[AGE == 279] <- 5
AGE2[AGE == 284] <- 4
AGE2[AGE == 339] <- 3
AGE2[AGE == 359] <- 2
AGE2[AGE == 470] <- 1
AGE2[AGE == 734] <- 0
AGE2.f <- factor(AGE2)

ryth.updated <- subset(data.frame(ryth, AGE2.f, year.n.2), !is.na(DV))

# eidiki periptwsi omadopoihmenou data frame
ryth.grouped <- groupedData(DV ~ year.n.2 | id, data=ryth.updated, 
                            order.groups = F)

# to plhres montelo 
#model6.1.fit <- lme(DV ~ year.n.2 + I(year.n.2^2) + AGE2.f +
#                      year.n.2:AGE2.f + I(year.n.2^2):AGE2.f, 
#                    random = ~ year.n.2 + I(year.n.2^2), method="REML", 
#                    data = ryth.grouped)
#summary(model6.1.fit)  den einai efikto na ektimithoun oi parametroi tou

# diwxnoume apo to montelo to stathero oro pou afora thn tyxaia epidrash 
# tou kathe ktiriou "random = ~ age.2 + I(age.2^2) - 1"
model6.2.fit <- lme(DV ~ year.n.2 + I(year.n.2^2) + AGE2.f +
                      year.n.2:AGE2.f + I(year.n.2^2):AGE2.f, 
                    random = ~ year.n.2 + I(year.n.2^2) - 1, method="REML", 
                    data = ryth.grouped)
summary(model6.2.fit)

# sto epomeno montelo paratirwntas tous simantikous syntelestes, diwxnoume
#  thn tyxaia epidrash tou tetragwnou tou xronou (I(year.n.2^2), 
# poly mikros sygkritika me tous ypoloipous).
model6.2a.fit <- update(model6.2.fit, random = ~ year.n.2 - 1)

# sygkrinoume ta dyo montela (likelihood ratio)
# gia na krathsoume to kalytero
anova(model6.2.fit, model6.2a.fit)

# kratame to aploustero montelo (to model6.2a.fit) kai xrisimopoioume 
#th diadikasia ML ektimisis
# gia na vrethoun oi statistika symantikoi paragontes
model6.2.ml.fit <- update(model6.2a.fit, method = "ML")
summary(model6.2.ml.fit)
# oloklirwnoume th diadikasia eyreshs kalyterou montelou, trexontas to 
# veltisto sto opoio katalhksame me th methodo "REML"
model6.3.ml.fit <- update(model6.2.ml.fit, 
                          fixed = ~ year.n.2 + AGE2.f + 
                            year.n.2:AGE2.f)

# sigkrinoume ta dyo telika montela
anova(model6.2.ml.fit, model6.3.ml.fit)

# oloklirwnoume me to teliko montelo
model6.3.fit <- update(model6.2a.fit, 
                       fixed = ~ year.n.2 +  AGE2.f + 
                         year.n.2:AGE2.f)
summary(model6.3.fit)

intervals(model6.3.fit)

getVarCov(model6.3.ml.fit, type="marginal")
getVarCov(model6.3.ml.fit, type="conditional")
getVarCov(model6.3.fit)

# ektimish gia kathe omada xrisimopoiontas tous syntelestes tou montelou
curve( -0.01*x + 0.06, 0, 11, xlab = "AGE minus 2", 
       ylab = "Marginal Predicted VSAE", lty = 25, ylim=c(0,100), lwd = 2)

curve( 0.01*x + -0.01,  0, 11, add=T, lty = 25, lwd = 2)
curve(    0*x +  0.03,  0, 11, add=T, lty = 24, lwd = 2)
curve( 0.02*x +  0.02,  0, 11, add=T, lty = 23, lwd = 2)
curve(    0*x + -0.06,  0, 11, add=T, lty = 22, lwd = 2)
curve( 0.01*x + -0.04,  0, 11, add=T, lty = 21, lwd = 2)
curve(    0*x + -0.02,  0, 11, add=T, lty = 20, lwd = 2)
curve( 0.02*x + -0.02,  0, 11, add=T, lty = 19, lwd = 2)
curve( 0.01*x +  0.01,  0, 11, add=T, lty = 18, lwd = 2)
curve( 0.01*x + -0.01,  0, 11, add=T, lty = 17, lwd = 2)
curve( 0.01*x +     0,  0, 11, add=T, lty = 16, lwd = 2)
curve( 0.04*x + -0.02,  0, 11, add=T, lty = 15, lwd = 2)
curve(    0*x +  0.04,  0, 11, add=T, lty = 14, lwd = 2)
curve(    0*x +  0.01,  0, 11, add=T, lty = 13, lwd = 2)
curve( 0.03*x + -0.04,  0, 11, add=T, lty = 12, lwd = 2)
curve( 0.03*x + -0.03,  0, 11, add=T, lty = 11, lwd = 2)
curve( 0.01*x +  0.03,  0, 11, add=T, lty = 10, lwd = 2)
curve( 0.01*x + -0.03,  0, 11, add=T, lty = 9 , lwd = 2)
curve( 0.02*x + -0.08,  0, 11, add=T, lty = 8 , lwd = 2)
curve( 0.01*x +  0.08,  0, 11, add=T, lty = 7 , lwd = 2)
curve( 0.01*x + -0.01,  0, 11, add=T, lty = 6 , lwd = 2)
curve( 0.04*x +   0.1,  0, 11, add=T, lty = 5 , lwd = 2)
curve( 0.04*x +     0,  0, 11, add=T, lty = 4 , lwd = 2)
curve( 0.01*x + -0.05,  0, 11, add=T, lty = 3 , lwd = 2)
curve( 0.05*x +  0.04,  0, 11, add=T, lty = 2 , lwd = 2)
curve( 0.01*x +     0,  0, 11, add=T, lty = 1 , lwd = 2)

legend(locator(1),c("AGE = 1" , "AGE = 2" , "AGE = 3" , "AGE = 4" , "AGE = 5" , "AGE = 6" ,
                    "AGE = 7" , "AGE = 8" , "AGE = 9" , "AGE = 10", "AGE = 11", "AGE = 12",
                    "AGE = 13", "AGE = 14", "AGE = 15", "AGE = 16", "AGE = 17", "AGE = 18",
                    "AGE = 19", "AGE = 20", "AGE = 21", "AGE = 22", "AGE = 23", "AGE = 24",
                    "AGE = 25"), lty = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25),
       lwd = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2))

# ektimomenh kampylh gia kathe ktirio
# h synartisi "augPred"  voitha sthn anaparastash ths perithorias ektimwmenhs 
# timis (timh omadas) kai ths atomikhs.
plot(augPred(model6.3.fit, level = 0:1), layout=c(10,7,1),
     xlab="AGE minus 2", ylab="Predicted DV",
     key = list(lines = list(lty = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25), 
                             col = c(28,10), 
                             lwd = c(1,1)), 
                text = list(c(" mean profile",
                              "id-specific profile")), columns = 6))

# diagrama diasporas ypoloipwn
plot(model6.3.fit, resid(., type="p") ~ fitted(.) | id, 
     layout=c(10,7), aspect=2, abline=0)

# eyresh twn ktiriwn pou deixnoun megala ypolloipa (se s.s.=0.05)
plot(model6.3.fit, resid(., type="p") ~ fitted(.) | factor(AGE), 
     id = 0.05, layout=c(10,3), aspect=2, abline=0)

# ypoloipa omadwn
plot(model6.3.fit, resid(.) ~ year.n.2, abline=0)

# optikos elegxos prosarmoghs sthn kanonikh katanomh
qqnorm(model6.3.fit, ~resid(.) | id, layout=c(10,7),
       aspect = 2, id = 0.05) 
#
#
model6.3.fit$residuals
ks.test(model6.3.fit$residuals,mean(model6.3.fit$residuals),sd(model6.3.fit$residuals))
plot(model6.3.fit, resid(., type="p") ~ fitted(.) | factor(AGE), 
     layout=c(4,6), aspect=2, abline=0)
