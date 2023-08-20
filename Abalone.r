#-------------------------------------------------------------------------------
## Understanding the dataset
summary(Abalone)
str(Abalone)
#-------------------------------------------------------------------------------
## Outliers replacement 

Outliers_replacement <- function(variable_to_check, Q1, Q3) {
  Vmax = Q3+1.5*(Q3-Q1)
  Vmin = Q1-1.5*(Q3-Q1)
  outliers_inf<-which(Abalone$variable_to_check<Vmin)
  outliers_sup<-which(Abalone$variable_to_check>Vmax)
  for (variable in outliers_inf) { 
    Abalone$variable_to_check[variable] <- NA 
  }
  for (variable in outliers_sup) { 
    Abalone$variable_to_check[variable] <- NA 
  }
}

  #Diameter
names(Abalone)
boxplot(Abalone$Diameter,main = "Diameter")
summary(Abalone$Diameter)
Q1 = 0.3500
Q3 = 0.4800
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Diameter<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Diameter[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Diameter>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Diameter[variable] = NA 
}
boxplot(Abalone$Diameter,main = "Diameter")


  #Height
names(Abalone)
boxplot(Abalone$Height,main = "Height")
summary(Abalone$Height)
Q1 = 0.1150
Q3 = 0.1650
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Height<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Height[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Height>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Height[variable] = NA 
}
boxplot(Abalone$Height,main = "Height")


  #Whole_Weight
names(Abalone)
boxplot(Abalone$Whole_Weight,main = "Whole_Weight")
summary(Abalone$Whole_Weight)
Q1 = 0.4415
Q3 = 1.1530
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Whole_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Whole_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Whole_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Whole_Weight[variable] = NA 
}
boxplot(Abalone$Whole_Weight,main = "Whole_Weight")


  #Shucked_Weight
names(Abalone)
boxplot(Abalone$Shucked_Weight,main = "Shucked_Weight")
summary(Abalone$Shucked_Weight)
Q1 = 0.1860
Q3 = 0.5020
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Shucked_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Shucked_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Shucked_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Shucked_Weight[variable] = NA 
}
boxplot(Abalone$Shucked_Weight,main = "Shucked_Weight")


  #Viscera_Weight
names(Abalone)
boxplot(Abalone$Viscera_Weight,main = "Viscera_Weight")
summary(Abalone$Viscera_Weight)
Q1 = 0.0935
Q3 = 0.2530
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Viscera_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Viscera_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Viscera_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Viscera_Weight[variable] = NA 
}
boxplot(Abalone$Viscera_Weight,main = "Viscera_Weight")


  #SHell_Weight
names(Abalone)
boxplot(Abalone$SHell_Weight,main = "SHell_Weight")
summary(Abalone$SHell_Weight)
Q1 = 0.1300
Q3 = 0.3290
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$SHell_Weight<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$SHell_Weight[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$SHell_Weight>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$SHell_Weight[variable] = NA 
}
boxplot(Abalone$SHell_Weight,main = "SHell_Weight")


  #Rings
names(Abalone)
boxplot(Abalone$Rings,main="Rings")
summary(Abalone$Rings)
Q1 = 8.0000
Q3 = 11.000
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(Abalone$Rings<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  Abalone$Rings[variable] = NA 
}
valeurs_aberrantes_sup<-which(Abalone$Rings>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  Abalone$Rings[variable] = NA 
}
boxplot(Abalone$Rings,main="Rings")
  #Sex
Abalone["Sex"][Abalone["Sex"] == ''] <- NA

## Missing values
  
   sum(is.na(Abalone)) 
   T = (sum(is.na(Abalone))/prod(dim(Abalone)))
   T

  
  dataset=na.omit(Abalone)
  View(dataset)
  sum(is.na(dataset))
  
#-------------------------------------------------------------------------------
## Univariate analysis  
  
attach(dataset)

# Normality test
shapiro.test(Height)
shapiro.test(Length)
shapiro.test(Diameter)
shapiro.test(Whole_Weight)
shapiro.test(Viscera_Weight)
shapiro.test(SHell_Weight)
shapiro.test(Shucked_Weight)
shapiro.test(Rings)

# Modality of the qualitative variable
table(Sex)
install.packages("ggplot2")
library(ggplot2)
ggplot(data=dataset,aes(x=Sex,fill=Sex))+geom_bar()
#-------------------------------------------------------------------------------
## Bivariate analysis 

# Quantitative variable - Quantitative variable 

#Diameter - Height
#1 Graphic representation
plot(Diameter,Height,main="Correlation Diameter-Height")
#2 Correlation test
cor.test(Diameter,Height,method="spearman")  

#Diameter - Length
#1 Graphic representation
plot(Diameter,Length,main="correlation Diameter-Length")
#2 Correlation test
cor.test(Length,Diameter,method="spearman")  

#Diameter - Whole_Weight
#1 Graphic representation
plot(Diameter,Whole_Weight,main="correlation Height-Whole_Weight")
#2 Correlation test
cor.test(Diameter,Whole_Weight,method="spearman")  

#Diameter-Shucked_Weight
#1 Graphic representation
plot(Diameter,Shucked_Weight,main="correlation Diameter-Shucked_Weight")
#2 Correlation test
cor.test(Diameter,Shucked_Weight,method="spearman")  

#Diameter-Viscera_Weight
#1 Graphic representation
plot(Diameter,Viscera_Weight,main="correlation Diameter-Viscera_Weight")
#2 Correlation test
cor.test(Diameter,Viscera_Weight,method="spearman")  

#Diameter-SHell_Weight
#1 Graphic representation
plot(Diameter,SHell_Weight,main="correlation Diameter-SHell_Weight")
#2 Correlation test
cor.test(Diameter,SHell_Weight,method="spearman")  

#Diameter-Rings
#1 Graphic representation
plot(Diameter,Rings,main="correlation Diameter-Rings")
#2 Correlation test
cor.test(Diameter,Rings,method="spearman")  

#-------------------------------------------------------------------------------
# Quantitative variable - Qualitative variable

boxplot(Length~Sex)
wilcox.test(Length~Sex)

boxplot(Height~Sex)
wilcox.test(Height~Sex)

boxplot(Diameter~Sex)
wilcox.test(Diameter~Sex)

boxplot(Whole_Weight~Sex)
wilcox.test(Whole_Weight~Sex)

boxplot(Shucked_Weight~Sex)
wilcox.test(Shucked_Weight~Sex)

boxplot(Viscera_Weight~Sex)
wilcox.test(Viscera_Weight~Sex)

boxplot(SHell_Weight~Sex)
wilcox.test(SHell_Weight~Sex)

boxplot(Rings~Sex)
wilcox.test(Rings~Sex)

#-------------------------------------------------------------------------------
## Linear regression

RM= lm(Diameter ~ Length + Height + Whole_Weight + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings
        , data=dataset)
RM
summary(RM)



RM1= lm(Diameter ~ Length + Height + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings
       , data=dataset)
RM1
summary(RM1)


RM2=lm(Diameter ~  Height + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings
       , data=dataset)
RM2
summary(RM2)

AIC(RM) 
AIC(RM1) 

R= residuals(RM1)
plot(R)

#-------------------------------------------------------------------------------
## PCA

New_Data <- subset(dataset, select = -c(Whole_Weight) )
summary(New_Data)
AbalonePC <- New_Data[,2:8]
head(AbalonePC)

# Princomp package
pca <- princomp(AbalonePC, cor=TRUE )
pca
# Output information
names(pca)
summary(pca)

# eigenvalues/eigenvectors
eigenvectors <- pca$loadings 
eigenvalues<- pca$sdev *pca$sdev
eigenvalues

# scree plots
# Elbow method
screeplot(pca,type="l",main="Screeplot for the Abalone data")
abline(1,0 ,col='red',lty = 2)

components <- cbind(Diameter = AbalonePC[, "Diameter"], pca$x[, 1]) 
comp <- as.data.frame(components)


RM3 <- lm(Diameter ~ Height + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings,  data = comp)
RM3  
summary(RM3)

RM4 <- lm(Diameter ~ Height + Shucked_Weight  + SHell_Weight + Rings,  data = comp)
RM4
summary(RM4)
AIC(RM3)
AIC(RM4)

#-------------------------------------------------------------------------------
## Generalized linear regression

Model1 <- glm(Diameter ~ Length + Height + Whole_Weight + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings, family = Gamma(link = "inverse"))
summary(Model1)

Model2 <- glm(Diameter ~ Length + Height + Whole_Weight + Shucked_Weight + Viscera_Weight + SHell_Weight + Rings, family = Gamma(link = "log"))
summary(Model2)
AIC(Model1)
AIC(Model2)

