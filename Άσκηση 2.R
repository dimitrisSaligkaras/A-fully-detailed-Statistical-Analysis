library(haven)     # φορτώνει τη βιβλιοθήκη
library(ggplot2)

regression<-read.table("cars1920.txt",header=T)
attach(regression)
names(regression)
View(regression)      # εντολές για να δούμε το πλαίσιο δεδομένων
head(regression, n = 11)      
tail(regression, n = 11) # στην πρωτη στηλη το 0 ειναι το καθαρο σφαλμα 
#ολες οι τιμες που παιρνει το 0 ειναι η τυχαια μεταβλητη
# save(regression, file="example 1-reg") # εντολή αποθήκευσης

is.data.frame(regression)  # εντολές διερεύνησης του πλαισίου δεδομένων
str(regression) #ειναι tbl data frame
dim(regression)    #δειχνει γραμμες και στηλες                
names(regression) 
apply(regression,MARGIN=2,"class") #τι κλαση ειναι καθε στηλη

# Μετατροπή ποσοτικής μεταβλητής σε παράγοντα (Ordered factor)
regression$speed   # οι τιμές είναι σε άυξουσα διάταξη
fspeed <- as.ordered(regression$speed)
levels(fspeed) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
# Προσθήκη της νέας μεταβλητής στο πλαίδιο δεδομένων
regression1 <- cbind.data.frame(regression,fspeed) #το καινουριο data frame
View(regression1)

#------------------------------------------------------------------------
# Ερώτημα 1:  Γραφικός έλεγχος συσχέτισης με την ggplot2
#                
#------------------------------------------------------------------------

#  η συνάρτηση "ggplot" - "συντακτικό" γραφικής παράστασης 
# διάγραμμα διασποράς και ευθεία γραμμικής παλινδρόμησης
ggplot(data = regression1) +
  geom_point(mapping = aes(x = speed, y = dist), shape=21, size = 3) +
  labs(x  = "Ταχύτητα",  y  = "Απόσταση")  +
  geom_smooth(mapping = aes(x = speed, y = dist), 
              method  = "lm",  se = FALSE)+
  labs(title = "Μελέτη 1", subtitle = NULL)

#Παρατηρώ οτι οι δύο μεταβλητές σχετίζονται μεταξύ τους καθώς φαίνεται οτι όσο
#μεγαλώνει η ταχύτητα ανάλογα μεγαλώνει και η απόσταση. Βέβαια για ταχύτητες απο 
#13 εως 18 έχουμε πολύ μεγάλο ευρος απόστασης αλλά θα το δούμε παρακάτω.

# ggsave("regression1.pdf") # αποθήκευση σε μορφή pdf

#------------------------------------------------------------------------
# Ερώτημα 2:    Γραμμική παλινδρόμηση με τη συνάρτηση lm
#                
#------------------------------------------------------------------------

names(regression1)

# η συνάρτηση της απλής γραμμικής παλινδρόμησης
lmod <- lm(dist ~ speed, regression1)

names(lmod)   # αποτελέσματα της γραμμικής παλινδρόμσης ονομαστικά
summary(lmod)  
#βασικά συγκεντρωτικά αποτελέσματα τα υπολοιπα ειναι απο -29.069.. εως 43.201..
#το 50% (πρωτο - τριτο τεταρτημοριο) θεωρειται καλος δεικτης?
#ο μεσος ορος δεν δινει καλη εικονα αν δεν ειναι συμμετρικη η κατανομη?
#τα τεταρτημορια δεν εξαρτονται απο το μ.ο.?
#coefficients:για καθε χιλιοστο που μειωνεταιταχύτητα, η αποσταση αυξανεται κατα 3.93
#το t value βγαινει αν διαιρεσω το estimate std με το error
#η πιθανοτητα να ισχυει η Η0 ειναι 1.49e-12

#------------------------------------------------------------------------
#Ερώτημα 3-4     Διαγνωστικοί έλεγχοι (προυποθέσεων)
#                
#------------------------------------------------------------------------

# 1. Έλεγχος προσαρμογής των υπολοίπων στην κανονική κατανομή
par(mfrow=c(1,3))
qqnorm(residuals(lmod),ylab="Residuals",main="")
qqline(residuals(lmod))
hist(residuals(lmod),xlab="Residuals",main="")
par(mfrow=c(1,1))
shapiro.test(residuals(lmod)) #δινει αποτελεσμα για σχετικα μικρο πληθος παρατηρησεων
#δεν εχουμε προσαρμογη στην κανονικη κατανομη
plot(lmod, which = 2)         #υποψια για ενδεχομενα παρατυπα σημεια 
plot(lmod)


# 2 Σταθερή διασπορά (ομοιογένεια της διασποράς των υπολοίπων)
par(mfrow = c(1,2))
plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)

plot(lmod, which = 1)  # ή συνάρτηση που παράγεται αυτόματα
plot(lmod, which = 3)
par(mfrow = c(1,1))


# 3 Έλεγχος αυτοσυσχέτισης 1ης τάξης των σφαλμάτων (Durbin-Watson test)
n <- length(residuals(lmod))
plot(tail(residuals(lmod),n-1) ~ head(residuals(lmod),n-1), xlab=
       expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(0.75))

require(lmtest)
dwtest(dist ~ speed, data=regression1) 
#οριακα δεν εχουμε συσχετιση 1ης ταξης των υπολοιπων


# 4 Εύρεση ασυνήθιστων παρατηρήσεων (Mahalanobis)
# Σημεία με απόσταση Mahalanobis > 2p/n πρέπει να ερευνώνται περεταίρω
lmod <- lm(dist ~ speed, regression1)
hatv <- hatvalues(lmod)  # Mahalanobis
head(hatv)
sum(hatv)       # ισχύει ότι άθροισμα = p = 2 (πλήθος παραμέτρων)
mah <- 2*2/50   
plot(hatv,ylab="Leverages")
abline(h = mah)
hatv[hatv > mah]
# πιθανα σημεια: 1,2,50
# αφου τρεξω cook & inf.measures

# 5. Cook's distance 
cook <- signif(cooks.distance(lmod), 3)
cook
# Με χρήση αυτόματης συνάρτησης από τη διαδικασία της παλινδρόμησης
plot(lmod, which = 4)
# πιθανά σημεία 23,39,49

# Χρησιμοποιώντας αυτοματη συνάρτηση της R
# που περιέχει και άλλα μέτρα διερεύνησης παράτυπων
# σημείων που επηρεάζουν τους συντελεστές της γ.π.
imI <- influence.measures(lmod)
summary(imI)
#το lmod εχει ολες τις παρατηρησεις
# πιθανά παράτηπα σημεία 1,2,23,49,50

#------------------------------------------------------------------------
#Ερώτημα 6-9  Επίδραση επαναλήψεων των μετρήσεων (καθαρό σφάλμα)
#                
#------------------------------------------------------------------------

lmod.f <- lm(dist ~ fspeed, regression1) #το fdistance το εχω κανει παραγοντα
summary(lmod.f) #intercept : ο γενικος μεσος ορος 40.725
anova(lmod, lmod.f) 
#συγκρινει τα δυο μοντελα το αρχικο και αυτο με τον παραγοντα αποσταση
#παραγει την F κατανομη 
#Η0 τα δυο μοντελα ειναι ιδια
#το ενα λαμβανει υποψην το σφαλμα που προκυπτει ενω το αλλο οχι ?

#Παρατηρούμε ότι το καθαρό σφάλμα (διασπορά που εξηγείται από τις επαναλήψεις των
#μετρήσεων στις ίδιες τιμές της x) δεν είναι στατιστικά σημαντικό 
#F(17, 31)= 1.2369, p =0.2948  , επομένως το μοντέλο είναι εντάξει.
hist(speed,xlab="Speed",main="")
hist(as.numeric(fspeed),xlab="fSpeed",main="")

#Ερώτημα 7
# pure error (διασπορά που εξηγείται από τις επαναλήψεις)
sqrt(11353.5/48) #236.531
sqrt(6764.8/31) # 218.219
#άρα καλύτερο είναι το δέυτερο μοντέλο, με τις επαναλαμβανόμενες τιμές
#γιατί έχει μικρότερη διασπορά

# x2
lmod2 <- lm(dist ~ speed +I(speed^2), regression1)
summary(lmod2) #βλεπω σταθερο ορο το Χ και το Χ^2 
summary(lmod)

anova(lmod2, lmod.f)  

sqrt(10824.7/48) #225.514
sqrt(6764.8/31) # 218.219
#’ρα και πάλι το δεύτερο μοντέλο είναι καλύτερο

#Ερώτημα 9: Έλεγχος προυποθέσεων και εισαγγωγή του νέου μοντέλου

#προυπόθεση 1
shapiro.test(lmod.f$residuals)
# ’ρα σύμφωνα με το shapiro test W=0.977 p=0.456 τα υπόλοιπα ακολουθούν 
# κανονική κατανομή

#Προυπόθεση 2
plot(lmod.f$fitted.values,lmod.f$residuals,xlab = "Fitted.values",ylab = "Residuals")
abline(lm(lmod.f$residuals ~ lmod.f$fitted.values),col="red") 

par(mfrow=c(2,2))
plot(lmod.f, which = 1)
plot(lmod.f, which = 2)
plot(lmod.f, which = 3)
plot(lmod.f, which = 4)
par(mfrow=c(1,1))


inf.f <- influence.measures(lmod.f)
attributes(lmod.f)

summary(inf.f)  
class(inf.f) #
str(inf.f) # ειναι λιστα απο 3 στοιχεια

#Η εξισωση του νέου μοντέλου είναι 
lmod.f <- lm(dist ~ fspeed, regression1)

# το εύρος δείγματος που καλύπτουμε μετά την αφαίρεση των σημείων είναι μικρότερο από το εύρος που καλύπτουμε 
#πριν την αφαίρεσή τους. Τα Adjusted R-squared των μοντέλων που χρησιμοποιήθηκαν μετά την αφαίρεση είναι μικρότερα 
#σε σχέση με τα αρχικά οπότε το καλύτερο μοντέλο είναι αυτό που αναφέρθηκε παραπάνω
regression1 <- regression1[ -c(1,2,49,50),]
lmod <- lm(dist ~ speed, regression1)
summary(lmod)
shapiro.test(residuals(lmod)) 

lmod.f <- lm(dist ~ fspeed, regression1)
summary(lmod.f)
shapiro.test(residuals(lmod.f))
#
#