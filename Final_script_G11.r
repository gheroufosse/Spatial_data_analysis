


# Script final Projet Analyse de données spatiales - LBRTI2101(A) -------

# Import des données & Différents Packages -----


warning("Dont forget to change the working directory ! ")

setwd(dir = "/Users/gauthierheroufosse/Documents/Studies/MA 1/Q1/DataSciences - Partim A/Projet/")


rm(list=ls())


library(data.table)
library(ggplot2)
theme_set(theme_classic())
library(gridExtra)
library("PerformanceAnalytics")
library(car)
library(rgl)
library(gstat)
library(rgdal)
library(AID)
library(dplyr)
library(latticeExtra)
library(ggthemes)
library(tidyr)


Donnees <- fread("groupe11.csv")



# Analyse exploratoire des données -----


## Localisation de nos données -----

# On commence par renommer les colonnes de localisation
Donnees <- Donnees %>% 
  rename(
    x = X,
    y = Y
  )


# Retrait de la double localisation

Donnees <- Donnees  %>% 
  group_by(x,y) %>%
  summarise(Ni = mean(na.omit(Ni)), Zn = mean(na.omit(Zn)), Cr = mean(na.omit(Cr)))

Donnees <- data.table(Donnees)


##  Grillage de la province du Hainaut

gridsize = 1000 # Taille du maillage [m]
margin = 5000 # Ajout d'une marge pour être sûr d'englober toutes les données

x <- seq(floor(min(Donnees$x)-margin), # from minimum longitude
         ceiling(max(Donnees$x+margin)), # to maximum longitude
         by=gridsize)
y <- seq(floor(min(Donnees$y)-margin), # from minimum latitude
         ceiling(max(Donnees$y)+margin), # to maximum latitude
         by=gridsize)
hainaut.grid <- as.data.table(expand.grid(x=x, y=y)) 

## Create a spatial version of the grid
ETMdataSpatial <- copy(hainaut.grid)
coordinates(ETMdataSpatial) <- ~x+y
proj4string(ETMdataSpatial) <- CRS("+init=epsg:31370") # Specify coordinate system (Lambert belge 1972)

## Load provinces shapefile and specify it's coordinate system

provinces <- readOGR("/Users/gauthierheroufosse/Documents/Studies/MA 1/Q1/DataSciences - Partim A/Projet/province/Hainaut.shp", use_iconv = TRUE, encoding = "UTF-8")
provinces <- spTransform(provinces, CRS("+proj=longlat +datum=WGS84"))
plot(provinces) # Display provinces
provinces$NAME_2 # Display all province names

## Transform grid coordinates system to match provinces
ETMdataSpatial <- spTransform(ETMdataSpatial, CRS("+proj=longlat +datum=WGS84"))

## Get index of points in the province of Hainaut
prov.id <- over(provinces[provinces$NAME_2 == "Hainaut", ],
                ETMdataSpatial, 
                returnList = TRUE)

## Select rows in your original grid that are located in Hainaut
prov.grid = hainaut.grid[prov.id[[1]],]

# Spatial Ni repartition in Hainaut province
ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_point(data=prov.grid, aes(x=x, y=y), color='burlywood1', shape = 3) + 
  geom_point(data=Donnees, aes(x=x, y=y, color=Ni),size=2) + 
  labs(fill="", color="Ni [mg/kg]") +
  scale_color_gradientn(name="Nickel concentration [mg/kg of soil]", 
                        colors=c('lightblue1','skyblue2', 'blue4', 'navyblue'), na.value = "black") +
  xlab("Longitude [Lambert 1972]") + ylab("Latitude [Lambert 1972]") + 
  theme(plot.title=element_text(hjust=0.5)) +
  ggtitle("Nickel concentration repartition in Hainaut Province")+
  theme(axis.title = element_text())

# Spatial Zn repartition in Hainaut province
ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_point(data=prov.grid, aes(x=x, y=y), color='burlywood1', shape = 3) + 
  geom_point(data=Donnees,aes(x=x, y=y, color=Zn),size=2) + 
  labs(fill="", color="Zn [mg/kg]") +
  scale_color_gradientn(name="Zinc concentration [mg/kg of soil]", 
                        colors=c('mistyrose','lightcoral', 'indianred', 'indianred4'),na.value = "black") +
  xlab("Longitude [Lambert 1972]") + ylab("Latitude [Lambert 1972]") + 
  theme(plot.title=element_text(hjust=0.5)) +
  ggtitle("Zinc concentration repartition in Hainaut province")+
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())

# Spatial Cr repartition in Hainaut province
ggplot() + geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) + 
  geom_point(data=prov.grid, aes(x=x, y=y), color='burlywood1', shape = 3) + 
  geom_point(data=Donnees,aes(x=x, y=y, color=Cr),size=2) + 
  labs(fill="", color="Cr [mg/kg]") +
  scale_color_gradientn(name="Chrome concentration [mg/kg of soil]", 
                        colors=c('lightgreen','darkseagreen', 'green4', 'darkseagreen4'), na.value = "black") + 
  xlab("Longitude [Lambert 1972]") + ylab("Latitude [Lambert 1972]") + 
  theme(plot.title=element_text(hjust=0.5)) +
  ggtitle("Chrome concentration repartition in Hainaut province")+
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())

## Statistiques de base ----

summary(Donnees) 



# Visualisation de la distribution de nos variables
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], # Light gray background
     col = "#ebebeb")

grid(nx = NULL, ny = NULL, col = "white", lty = 10, # Add white grid
     lwd = par("lwd"), equilogs = TRUE)

par(new = TRUE) # Boxplot

boxplot(Donnees$Ni,Donnees$Zn,Donnees$Cr,
        names = c("Nickel","Zinc", "Chrome"),
        main = "Boxplots of the different elements",
        col = rgb(0, 0, 1, alpha = 0.4),
        ylab="Concentration in the soil [mg/kg of soil]",
        las=1,
        border = "black",  # Boxplot border color
        outpch =23,      # Outliers symbol
        outbg = "red",   # Outliers color
        whiskcol = "blue", # Whisker color
        whisklty = 1      # Whisker line type
)


# Relève de la présence de NaN parmi nos données
sum(is.na(Donnees$Ni))
sum(is.na(Donnees$Zn))
sum(is.na(Donnees$Cr))


# Correlation entre variables

chart.Correlation(Donnees[,.(Ni,Cr,Zn)])



# Nettoyage des données ----

## Distribution des concentrations et corrections ---- 

### Ni ----

ggplot(Donnees, aes(x = Ni)) + # if you put 'aes' here, all geom_ functions will use Ni as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram"),
                 bins = 15, # number of bins
                 color = "dodgerblue4") + 
  geom_density(col = "dodgerblue1", # Bleue curve
               aes(fill = "Fitted probability density function"), 
               alpha = 0.5) +  
  xlab("Nickel concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle("Density histogram of Nickel concentration") +
  theme(axis.title = element_text())+
  stat_function(fun=dnorm, # display a normal distribution -> red curve
                args=list(mean = mean(na.omit(Donnees$Ni)), sd = sd(na.omit(Donnees$Ni))), # with mean and sd of Ni
                aes(color="Normal probability density function"),
                size= 1.5) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="dodgerblue4",
                                   "Fitted probability density function"=alpha("dodgerblue1",.2))) +
  scale_color_manual("", values = ("Normal probability density function" = "red"))

ggplot(Donnees, aes(x = Ni)) + 
  stat_ecdf(geom = "step", aes(color = "Estimated cumulative density function"), size = 1.5) +
  xlab("Nickel concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle(expression(atop("Cumulative density function of the Nickel concentration", 
                          atop(italic("Estimated vs normal"), "")))) +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun = pnorm, # display a normal cumulative distribution
                args = list(mean = mean(na.omit(Donnees$Ni)), sd = sd(na.omit(Donnees$Ni))), 
                aes(color = 'Normal cumulative density function'),
                size= 1.5) +
  scale_color_manual("", values = c("Estimated cumulative density function" = "black",
                                    "Normal cumulative density function" = "red")) 

car::qqPlot(na.omit(Donnees$Ni)) #Vieux qq plot

#Nouveau qqplot

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], # Light gray background
     col = "#ebebeb")

grid(nx = NULL, ny = NULL, col = "white", lty = 10, # Add white grid
     lwd = par("lwd"), equilogs = TRUE)

par(new = TRUE)

qqnorm(na.omit(Donnees$Ni), pch = 1, frame = TRUE, grid = TRUE, main="Nickel quantile-quantile diagram", 
       ylab ="Nickel concentration quantiles") #Nouveau qqplot
qqline(na.omit(Donnees$Ni), col = "red", lwd = 2)

# Correction via une Box cox transformation

Cdbx.Ni <- boxcoxnc(na.omit(Donnees$Ni), verbose = FALSE) # Find best alpha 

Donnees[!is.na(Ni), Cdbx.Ni := Cdbx.Ni$tf.data] # Create column with transformed data
alpha <- Cdbx.Ni$lambda.hat


### Zn ----

ggplot(Donnees, aes(x = Zn)) + # if you put 'aes' here, all geom_ functions will use Zn as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram"),
                 bins = 15, # number of bins
                 color = "dodgerblue4") + 
  geom_density(col = "dodgerblue1", # Bleue curve
               aes(fill = "Fitted probability density function"), 
               alpha = 0.5) +  
  xlab("Zinc concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle("Density histogram of Zinc concentration") +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun=dnorm, # display a normal distribution -> red curve
                args=list(mean = mean(na.omit(Donnees$Zn)), sd = sd(na.omit(Donnees$Zn))), # with mean and sd of Zn
                aes(color="Normal probability density function"),
                size= 1.5) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="dodgerblue4",
                                   "Fitted probability density function"=alpha("dodgerblue1",.2))) +
  scale_color_manual("", values = ("Normal probability density function" = "red"))

ggplot(Donnees, aes(x = Zn)) + 
  stat_ecdf(geom = "step", aes(color = "Estimated cumulative density function"), size = 1.5) +  
  xlab("Zinc concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle(expression(atop("Cumulative density function of the Zinc concentration", 
                          atop(italic("Estimated vs normal"), "")))) +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun = pnorm, # display a normal cumulative distribution
                args = list(mean = mean(na.omit(Donnees$Zn)), sd = sd(na.omit(Donnees$Zn))), 
                aes(color = 'Normal cumulative density function'),
                size= 1.5) +
  scale_color_manual("", values = c("Estimated cumulative density function" = "black",
                                    "Normal cumulative density function" = "red")) 

car::qqPlot(na.omit(Donnees$Zn))#Vieux qqplot

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], # Light gray background
     col = "#ebebeb")

grid(nx = NULL, ny = NULL, col = "white", lty = 10, # Add white grid
     lwd = par("lwd"), equilogs = TRUE)

par(new = TRUE)

qqnorm(na.omit(Donnees$Zn), pch = 1, frame = TRUE, grid = TRUE, main="Zinc quantile-quantile diagram", 
       ylab ="Zinc concentration quantiles") #Nouveau qqplot
qqline(na.omit(Donnees$Zn), col = "red", lwd = 2)

# Correction via une Box cox transformation

Cdbx.Zn <- boxcoxnc(na.omit(Donnees$Zn), verbose = FALSE) # Find best alpha 

Donnees[!is.na(Zn),Cdbx.Zn := Cdbx.Zn$tf.data] # Create column with transformed data
alpha <- Cdbx.Zn$lambda.hat

### Cr ----

ggplot(Donnees, aes(x = Cr)) + # if you put 'aes' here, all geom_ functions will use Cr as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram"),
                 bins = 15, # number of bins
                 color = "dodgerblue4") + 
  geom_density(col = "dodgerblue1", # Bleue curve
               aes(fill = "Fitted probability density function"), 
               alpha = 0.5) + 
  xlab("Chrome concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle("Density histogram of Chrome concentration") +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun=dnorm, # display a normal distribution -> red curve
                args=list(mean = mean(na.omit(Donnees$Cr)), sd = sd(na.omit(Donnees$Cr))), # with mean and sd of rain
                aes(color="Normal probability density function"),
                size= 1.5) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="dodgerblue4",
                                   "Fitted probability density function"=alpha("dodgerblue1",.2))) +
  scale_color_manual("", values = ("Normal probability density function" = "red"))

ggplot(Donnees, aes(x = Cr)) + 
  stat_ecdf(geom = "step", aes(color = "Estimated cumulative density function"), size = 1.5) +  
  xlab("Chrome concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle(expression(atop("Cumulative density function of the Chrome concentration", 
                          atop(italic("Estimated vs normal"), "")))) +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun = pnorm, # display a normal cumulative distribution
                args = list(mean = mean(na.omit(Donnees$Cr)), sd = sd(na.omit(Donnees$Cr))), 
                aes(color = 'Normal cumulative density function'),
                size= 1.5) +
  scale_color_manual("", values = c("Estimated cumulative density function" = "black",
                                    "Normal cumulative density function" = "red")) 

car::qqPlot(na.omit(Donnees$Cr))#Vieux qqplot

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], # Light gray background
     col = "#ebebeb")

grid(nx = NULL, ny = NULL, col = "white", lty = 10, # Add white grid
     lwd = par("lwd"), equilogs = TRUE)

par(new = TRUE)

qqnorm(na.omit(Donnees$Cr), pch = 1, frame = TRUE, grid = TRUE, main="Chrome quantile-quantile diagram", 
       ylab ="Chrome concentration quantiles") #Nouveau qqplot
qqline(na.omit(Donnees$Cr), col = "red", lwd = 2)

# Correction via une Box cox transformation

Cdbx.Cr <- boxcoxnc(na.omit(Donnees$Cr), verbose = FALSE) # Find best alpha 

Donnees[!is.na(Cr),Cdbx.Cr := Cdbx.Cr$tf.data] # Create column with transformed data
alpha <- Cdbx.Cr$lambda.hat




## Retrait des valeurs aberrantes ----
### Ni ----

Q1n <- quantile(na.omit(Donnees$Ni), .25)
Q3n <- quantile(na.omit(Donnees$Ni), .75)
IQRn <- IQR(na.omit(Donnees$Ni))

clean.donnees.Ni <- subset(Donnees, Donnees$Ni > (Q1n - 1.5*IQRn) & Donnees$Ni < (Q3n + 1.5*IQRn))

### Zn ----
Q1z <- quantile(na.omit(Donnees$Zn), .25)
Q3z <- quantile(na.omit(Donnees$Zn), .75)
IQRz <- IQR(na.omit(Donnees$Zn))

clean.donnees.Zn <- subset(Donnees, Donnees$Zn > (Q1z - 1.5*IQRz) & Donnees$Zn < (Q3z + 1.5*IQRz))


### Cr ----
Q1c <- quantile(na.omit(Donnees$Cr), .25)
Q3c <- quantile(na.omit(Donnees$Cr), .75)
IQRc <- IQR(na.omit(Donnees$Cr))

clean.donnees.Cr <- subset(Donnees, Donnees$Cr > (Q1c - 1.5*IQRc) & Donnees$Cr < (Q3c + 1.5*IQRc))





## Visualisation des nouvelles distributions ----


rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],# Light gray background
     col = "#ebebeb")
grid(nx = NULL, ny = NULL, col = "white", lty = 10,# Add white grid
     lwd = par("lwd"), equilogs = TRUE)
par(new = TRUE)

boxplot(clean.donnees.Ni$Ni,clean.donnees.Zn$Zn, clean.donnees.Cr$Cr,
        names = c("Nickel", "Zinc", "Chrome"),
        main = "New boxplots of the different elements",
        col = rgb(0, 0, 1, alpha = 0.4),
        ylab="Concentration in the soil [mg/kg of soil]",
        las=1,
        border = "black",  # Boxplot border color
        outpch =23,      # Outliers symbol
        outbg = "red",   # Outliers color
        whiskcol = "blue", # Whisker color
        whisklty = 1      # Whisker line type
        )
### Ni ----

ggplot(clean.donnees.Ni, aes(x = Ni)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram"),
                 bins = 15, # number of bins
                 color = "dodgerblue4") + 
  geom_density(col = "dodgerblue1", # Bleue curve
               aes(fill = "Fitted probability density function"), 
               alpha = 0.5) +  
  xlab("Nickel concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle("Density histogram of Nickel concentration") +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun=dnorm, # display a normal distribution -> red curve
                args=list(mean = mean(clean.donnees.Ni$Ni), sd = sd(clean.donnees.Ni$Ni)), # with mean and sd of Ni
                aes(color="Normal probability density function"),
                size= 1.5) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="dodgerblue4",
                                   "Fitted probability density function"=alpha("dodgerblue1",.2))) +
  scale_color_manual("", values = ("Normal probability density function" = "red"))


ggplot(clean.donnees.Ni, aes(x = Ni)) + 
  stat_ecdf(geom = "step", aes(color = "Estimated cumulative density function"), size = 1.5) +  
  xlab("Nickel concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle(expression(atop("Cumulative density function of the Nickel concentration", 
                          atop(italic("Estimated vs normal"), "")))) +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun = pnorm, # display a normal cumulative distribution
                args = list(mean = mean(clean.donnees.Ni$Ni), sd = sd(clean.donnees.Ni$Ni)), 
                aes(color = 'Normal cumulative density function'),
                size= 1.5) +
  scale_color_manual("", values = c("Estimated cumulative density function" = "black",
                                    "Normal cumulative density function" = "red"))  


car::qqPlot(clean.donnees.Ni$Ni) #Vieux qqplot 

#Nouveau QQplot

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], # Light gray background
     col = "#ebebeb")

grid(nx = NULL, ny = NULL, col = "white", lty = 10, # Add white grid
     lwd = par("lwd"), equilogs = TRUE)

par(new = TRUE)


qqnorm(clean.donnees.Ni$Ni, pch = 1, frame = TRUE, grid = TRUE, main="Nickel quantile-quantile diagram", 
       ylab ="Nickel concentration quantiles") #Nouveau qqplot
qqline(clean.donnees.Ni$Ni, col = "red", lwd = 2)

### Zn ----

ggplot(clean.donnees.Zn, aes(x = Zn)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram"),
                 bins = 15, # number of bins
                 color = "dodgerblue4") + 
  geom_density(col = "dodgerblue1", # Bleue curve
               aes(fill = "Fitted probability density function"), 
               alpha = 0.5) +  
  xlab("Zinc concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle("Density histogram of Zinc concentration") +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun=dnorm, # display a normal distribution -> red curve
                args=list(mean = mean(clean.donnees.Zn$Zn), sd = sd(clean.donnees.Zn$Zn)), # with mean and sd of Zn
                aes(color="Normal probability density function"),
                size= 1.5) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="dodgerblue4",
                                   "Fitted probability density function"=alpha("dodgerblue1",.2))) +
  scale_color_manual("", values = ("Normal probability density function" = "red"))

ggplot(clean.donnees.Zn, aes(x = Zn)) + 
  stat_ecdf(geom = "step", aes(color = "Estimated cumulative density function"), size = 1.5) +  
  xlab("Zinc concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle(expression(atop("Cumulative density function of the Zinc concentration", 
                          atop(italic("Estimated vs normal"), "")))) +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun = pnorm, # display a normal cumulative distribution
                args = list(mean = mean(clean.donnees.Zn$Zn), sd = sd(clean.donnees.Zn$Zn)), 
                aes(color = 'Normal cumulative density function'),
                size= 1.5) +
  scale_color_manual("", values = c("Estimated cumulative density function" = "black",
                                    "Normal cumulative density function" = "red")) 

car::qqPlot(clean.donnees.Zn$Zn)#Vieux qqplot

#Nouveau qqplot

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], # Light gray background
     col = "#ebebeb")

grid(nx = NULL, ny = NULL, col = "white", lty = 10, # Add white grid
     lwd = par("lwd"), equilogs = TRUE)

par(new = TRUE)

qqnorm(clean.donnees.Zn$Zn, pch = 1, frame = TRUE, grid = TRUE, main="Zinc quantile-quantile diagram", 
       ylab ="Zinc concentration quantiles") #Nouveau qqplot
qqline(clean.donnees.Zn$Zn, col = "red", lwd = 2)

### Cr ----

ggplot(clean.donnees.Cr, aes(x = Cr)) + # if you put 'aes' here, all geom_ functions will use rain as x
  geom_histogram(aes(y = ..density..,  # display density function (and not count)
                     fill = "Density histogram"),
                 bins = 15, # number of bins
                 color = "dodgerblue4") + 
  geom_density(col = "dodgerblue1", # Bleue curve
               aes(fill = "Fitted probability density function"), 
               alpha = 0.5) + 
  xlab("Chrome concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle("Density histogram of Chrome concentration") +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun=dnorm, # display a normal distribution -> red curve
                args=list(mean = mean(clean.donnees.Cr$Cr), sd = sd(clean.donnees.Cr$Cr)), # with mean and sd of rain
                aes(color="Normal probability density function"),
                size= 1.5) + # width of the line
  scale_fill_manual("", values = c("Density histogram"="dodgerblue4",
                                   "Fitted probability density function"=alpha("dodgerblue1",.2))) +
  scale_color_manual("", values = ("Normal probability density function" = "red"))

ggplot(clean.donnees.Cr, aes(x = Cr)) + 
  stat_ecdf(geom = "step", aes(color = "Estimated cumulative density function"), size = 1.5) + 
  xlab("Chrome concentration [mg/Kg]") + 
  ylab("Probability") + 
  ggtitle(expression(atop("Cumulative density function of the Chrome concentration", 
                          atop(italic("Estimated vs normal"), "")))) +
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())+
  stat_function(fun = pnorm, # display a normal cumulative distribution
                args = list(mean(clean.donnees.Cr$Cr), sd = sd(clean.donnees.Cr$Cr)), 
                aes(color = 'Normal cumulative density function'),
                size= 1.5) +
  scale_color_manual("", values = c("Estimated cumulative density function" = "black",
                                    "Normal cumulative density function" = "red")) 

car::qqPlot(clean.donnees.Cr$Cr)#Vieux qqplot

#Nouveau qqplot

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], # Light gray background
     col = "#ebebeb")

grid(nx = NULL, ny = NULL, col = "white", lty = 10, # Add white grid
     lwd = par("lwd"), equilogs = TRUE)

par(new = TRUE)

qqnorm(clean.donnees.Cr$Cr, pch = 1, frame = TRUE, grid = TRUE, main="Chrome quantile-quantile diagram", 
       ylab ="Chrome concentration quantiles") #Nouveau qqplot
qqline(clean.donnees.Cr$Cr, col = "red", lwd = 2)


# Modélisation spatiale ----

## Fonction espérance ----



## Variogramme ----

### Variogramme initial après correction des données ----
#### Ni ----

# First we create a gstat object
Ni.gstat <- gstat(id ="Ni", formula = Ni~1, data = clean.donnees.Ni, locations = ~x+y)

# Then we calculate the semi-variogram values
Ni.vario <- variogram(Ni.gstat, cutoff = 18000, width = 1200)
Ni.vg.fit <- fit.variogram(Ni.vario, model=vgm(c('Exp','Sph','Gau')))
head(Ni.vario)


plot(Ni.vario,  model=Ni.vg.fit, main = "Variogram of Nickel", pch=16,col='black',ylim=c(0,40), 
     xlab="Distance", ylab="Semivariance", lty = 1, lwd = 3)

trellis.focus("panel",1,1)
llines(x=c(0,max(Ni.vario$dist)), y=c(var(clean.donnees.Ni$Ni),var(clean.donnees.Ni$Ni)), col="red", lwd=1, lty=2)
trellis.unfocus()

#### Zn ----

# First we create a gstat object
Zn.gstat <- gstat(id ="Zn", formula = Zn~1, data = clean.donnees.Zn, locations = ~x+y)

# Then we calculate the semi-variogrma values
Zn.vario <- variogram(Zn.gstat, cutoff = 20000, width = 1800)
Zn.vg.fit <- fit.variogram(Zn.vario, model=vgm(c('Exp','Sph','Gau')))
head(Zn.vario)


plot(Zn.vario,  model=Zn.vg.fit, main = "Variogram of Zinc", pch=16,col='black', ylim=c(0,200), 
     xlab="Distance", ylab="Semivariance", lty = 1, lwd = 3)

trellis.focus("panel",1,1)
llines(x=c(0,max(Zn.vario$dist)), y=c(var(clean.donnees.Zn$Zn),var(clean.donnees.Zn$Zn)), col="red", lwd=1, lty=2)
trellis.unfocus()

#### Cr ----

# First we create a gstat object
Cr.gstat <- gstat(id ="Cr", formula = Cr~1, data = clean.donnees.Cr, locations = ~x+y)

# Then we calculate the semi-variogram values
Cr.vario <- variogram(Cr.gstat, cutoff = 12000, width = 600)
Cr.vg.fit <- fit.variogram(Cr.vario, model=vgm(c('Exp','Sph','Gau')))
head(Cr.vario)


plot(Cr.vario,  model=Cr.vg.fit, main = "Variogram of Chrome", pch=16,col='black', ylim=c(0,60), 
     xlab="Distance", ylab="Semivariance", lty = 1, lwd = 3)

trellis.focus("panel",1,1)
llines(x=c(0,max(Cr.vario$dist)), y=c(var(clean.donnees.Cr$Cr),var(clean.donnees.Cr$Cr)), col="red", lwd=1, lty=2)
trellis.unfocus()

### Variogramme des résidus après retrait de l'influence de la moyenne ---- 
#### Ni ----

# Visualisation des données
Ni.lm <- lm(Ni ~x+y, data=clean.donnees.Ni)

scatter3d(x=clean.donnees.Ni$x, z=clean.donnees.Ni$y, y=clean.donnees.Ni$Ni, xlab="Longitude", zlab ="Latitude", 
          ylab='Ni [mg/kg]')
rglwidget(width=600, height=600, reuse=FALSE)


# Retrait de l'influence de la moyenne
Ni.derive <- predict(Ni.lm, clean.donnees.Ni)
clean.donnees.Ni[, Ni.res:=Ni-Ni.derive]

# Visualisation des résidus
Ni_res.lm <- lm(Ni.res ~x+y, data=clean.donnees.Ni)
scatter3d(x=clean.donnees.Ni$x, z=clean.donnees.Ni$y, y=clean.donnees.Ni$Ni.res, 
          xlab="Longitude", zlab ="Latitude", ylab='Ni residuals [mg/Kg]')
rglwidget(width=600, height=600, reuse=FALSE)


#  Variogramme des résidus
Ni.res.gstat <- gstat(formula = Ni.res~1, data = clean.donnees.Ni, locations = ~x+y)

# Then we calculate the semi-variogrma values
Ni.res.vario <- variogram(Ni.res.gstat, cutoff = 18000, width = 900)
head(Ni.res.vario)

clean.fit.ni <- fit.variogram(Ni.res.vario, model=vgm(c(model='Exp','Sph','Gau'), nugget = 15), fit.method = 6)

plot(Ni.res.vario, model = clean.fit.ni, main = "Variogram of Nickel residuals", pch=16,col='black', 
     xlab="Distance", ylab="Semivariance", lty = 1, lwd = 3)

trellis.focus("panel",1,1)
llines(x=c(0,max(Ni.res.vario$dist)), y=c(var(clean.donnees.Ni$Ni.res),var(clean.donnees.Ni$Ni.res)), 
       col="red", lwd=1, lty=2)
trellis.unfocus()

# Visualisaion des paramètres optimisés du modèle
clean.fit.ni


#### Zn ----

# Visualisation des données
Zn.lm <- lm(Zn ~x+y, data=clean.donnees.Zn)
scatter3d(x=clean.donnees.Zn$x, z=clean.donnees.Zn$y, y=clean.donnees.Zn$Zn, xlab="Longitude", 
          zlab ="Latitude", ylab='Zn [mg/kg]')
rglwidget(width=600, height=600, reuse=FALSE)

# Retrait de l'influence de la moyenne
Zn.derive <- predict(Zn.lm, clean.donnees.Zn)
clean.donnees.Zn[, Zn.res:=Zn-Zn.derive]

# Visualisation des résidus
Zn_res.lm <- lm(Zn.res ~x+y, data=clean.donnees.Zn)
scatter3d(x=clean.donnees.Zn$x, z=clean.donnees.Zn$y, y=clean.donnees.Zn$Zn.res, 
          xlab="Longitude", zlab ="Latitude", ylab='Zn residuals [mg/Kg]')
rglwidget(width=600, height=600, reuse=FALSE)


#  Variogramme des résidus
Zn.res.gstat <- gstat(formula = Zn.res~1, data = clean.donnees.Zn, locations = ~x+y)

# Then we calculate the semi-variogrma values
Zn.res.vario <- variogram(Zn.res.gstat, cutoff = 15000, width = 1500)
head(Zn.res.vario)

clean.fit.zn <- fit.variogram(Zn.res.vario, model=vgm(c('Exp','Sph','Gau'), nugget = 85), fit.method = 6)

plot(Zn.res.vario, model = clean.fit.zn, main = "Variogram of Zinc residuals", pch=16,col='black', 
     xlab="Distance", ylab="Semivariance", lty = 1, lwd = 3)

trellis.focus("panel",1,1)
llines(x=c(0,max(Zn.res.vario$dist)), y=c(var(clean.donnees.Zn$Zn.res),var(clean.donnees.Zn$Zn.res)), 
       col="red", lwd=1, lty=2)
trellis.unfocus()

# Visualisaion des paramètres optimisés du modèle
clean.fit.zn 

#### Cr ----

# Visualisation des données
Cr.lm <- lm(Cr ~x+y, data=clean.donnees.Cr)
scatter3d(x=clean.donnees.Cr$x, z=clean.donnees.Cr$y, y=clean.donnees.Cr$Cr, xlab="Longitude", zlab ="Latitude", 
          ylab='Cr [mg/kg]')
rglwidget(width=600, height=600, reuse=FALSE)

# Retrait de l'influence de la moyenne
Cr.derive <- predict(Cr.lm, clean.donnees.Cr)
clean.donnees.Cr[, Cr.res:=Cr-Cr.derive]

# Visualisation des résidus
Cr_res.lm <- lm(Cr.res ~x+y, data=clean.donnees.Cr)
scatter3d(x=clean.donnees.Cr$x, z=clean.donnees.Cr$y, y=clean.donnees.Cr$Cr.res, xlab="Longitude", zlab ="Latitude",
          ylab='Cr residuals [mg/Kg]')
rglwidget(width=600, height=600, reuse=FALSE)


#  Variogramme des résidus
Cr.res.gstat <- gstat(formula = Cr.res~1, data = clean.donnees.Cr, locations = ~x+y)

# Then we calculate the semi-variogrma values
Cr.res.vario <- variogram(Cr.res.gstat, cutoff = 20000, width = 1200)
head(Cr.res.vario)

clean.fit.cr<- fit.variogram(Cr.res.vario, model=vgm(c('Exp','Sph','Gau'), nugget = 29), fit.method = 6)

plot(Cr.res.vario, model = clean.fit.cr, main = "Variogram of Chrome residuals", pch=16,col='black', xlab="Distance",
     ylab="Semivariance", lty = 1, lwd = 3)

trellis.focus("panel",1,1)
llines(x=c(0,max(Cr.res.vario$dist)), y=c(var(clean.donnees.Cr$Cr.res),var(clean.donnees.Cr$Cr.res)), 
       col="red", lwd=1, lty=2)
trellis.unfocus()

# Visualisaion des paramètres optimisés du modèle
clean.fit.cr



# Prédiction spatiale sur l'ensemble du territoire étudié ---- 

## Méthode déterministe : IDW ---- 

# Création d'une fonction de prédiction pour éviter les copiers-collers
plot.predictions <- function(prediction,varname,database, plot.title){
  ggplot() + 
    geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
    geom_tile(data=prediction, 
              aes(x = x, y = y, fill = varname)) +
    
    geom_point(data=database, 
               aes(x=x, y=y, color="Measurement points"),
               shape=18,
               size=1) +
    scale_color_manual("", values="black") +
    scale_fill_gradientn(name="Zn predictions [mg/m³]", colors=c('royalblue','green3', 'yellow', 'red')) +
    theme(legend.key = element_rect(fill = "green3", 
                                    color = NA)) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(plot.title)+
    #theme_fivethirtyeight()  +
    theme(axis.title = element_text())
}



### Ni ----

powers <- seq(0.5, 3.5, 0.25)
pmse_Ni <- data.table(power = powers, mse = rep(0,length(powers)) )# Power mean squared error
for (p in powers){
  pse <- rep(0, nrow(clean.donnees.Ni)) # Power squared errors
  for (i in 1:nrow(clean.donnees.Ni)){
    point.idw <- idw(formula = Ni~1,
                     data = clean.donnees.Ni[-i,],  
                     locations = ~x+y,
                     newdata = clean.donnees.Ni[i,],
                     idp = p,
                     nmax=20,
                     debug.level = 0) # to avoid getting many output messages
    pse[i] <- (point.idw$var1.pred - clean.donnees.Ni$Ni[i])^2
  }
  pmse_Ni[power==p,"mse"] = mean(pse)
}

plot(pmse_Ni)


Ni.idw <- idw(formula = Ni~1,
              data = clean.donnees.Ni,  
              locations = ~x+y,
              newdata = prov.grid,
              idp = 1.25,
              nmax=20)

setnames(Ni.idw, "var1.pred", "Ni.pred")

# We plot the predictions using IDW for Nickel 

plot.predictions(Ni.idw, Ni.idw$Ni.pred, clean.donnees.Ni, 
                 "Prédiction des valeurs de Nickel par distance inverse - Hainaut, Belgique")

### Zn ----

# Optimisation of the theta parameter
powers <- seq(0.5, 3.5, 0.25)
pmse_Zn <- data.table(power = powers, mse = rep(0,length(powers)) )# Power mean squared error
for (p in powers){
  pse <- rep(0, nrow(clean.donnees.Zn)) # Power squared errors
  for (i in 1:nrow(clean.donnees.Zn)){
    point.idw <- idw(formula = Zn~1,
                     data = clean.donnees.Zn[-i,],  
                     locations = ~x+y,
                     newdata = clean.donnees.Zn[i,],
                     idp = p,
                     nmax=20,
                     debug.level = 0) # to avoid getting many output messages
    pse[i] <- (point.idw$var1.pred - clean.donnees.Zn$Zn[i])^2
  }
  pmse_Zn[power==p,"mse"] = mean(pse)
}

plot(pmse_Zn)


Zn.idw <- idw(formula = Zn~1,
              data = clean.donnees.Zn,  
              locations = ~x+y,
              newdata = prov.grid,
              idp = 1.25,
              nmax=20)

setnames(Zn.idw, "var1.pred", "Zn.pred")

# We create a function to plot results for  

plot.predictions(Zn.idw, Zn.idw$Zn.pred, clean.donnees.Zn, 
                 "Prédiction des valeurs de Zinc par distance inverse - Hainaut, Belgique")

### Cr ----

powers <- seq(0.5, 3.5, 0.25)
pmse_Cr <- data.table(power = powers, mse = rep(0,length(powers)) )# Power mean squared error
for (p in powers){
  pse <- rep(0, nrow(clean.donnees.Cr)) # Power squared errors
  for (i in 1:nrow(clean.donnees.Cr)){
    point.idw <- idw(formula = Cr~1,
                     data = clean.donnees.Cr[-i,],  
                     locations = ~x+y,
                     newdata = clean.donnees.Cr[i,],
                     idp = p,
                     nmax=20,
                     debug.level = 0) # to avoid getting many output messages
    pse[i] <- (point.idw$var1.pred - clean.donnees.Cr$Cr[i])^2
  }
  pmse_Cr[power==p,"mse"] = mean(pse)
}

plot(pmse_Cr)




Cr.idw <- idw(formula = Cr~1,
              data = clean.donnees.Cr,  
              locations = ~x+y,
              newdata = prov.grid,
              idp = 1.25,
              nmax=20)

setnames(Cr.idw, "var1.pred", "Cr.pred")

# We create a function to plot results for  

plot.predictions(Cr.idw, Cr.idw$Cr.pred, clean.donnees.Cr, 
                 "Prédiction des valeurs de Chrome par distance inverse - Hainaut, Belgique ")


## Kriegage ---- 

# In order to realize the krigage prediction :
# we created this new file. Combination of the duplicate loc data 
# to get the mean



### Ni ----


Ni.krig <- krige(formula = Ni~1,
                 data = clean.donnees.Ni,  
                 locations = ~x+y,
                 newdata = prov.grid, 
                 model = clean.fit.ni,
                 nmax=20)



setnames(Ni.krig, c("var1.pred", "var1.var"), c("Ni.predkrig", "Ni.varkrig"))

plot.predictions(Ni.krig, Ni.krig$Ni.predkrig, clean.donnees.Ni, 
                 "Prédiction de la concentration en Nickel par krigeage - Hainaut, Belgique")

### Zn ---- 

Zn.krig <- krige(formula = Zn~1,
                 data = clean.donnees.Zn,  
                 locations = ~x+y,
                 newdata = prov.grid, 
                 model = clean.fit.zn,
                 nmax=20)



setnames(Zn.krig, c("var1.pred", "var1.var"), c("Zn.predkrig", "Zn.varkrig"))

plot.predictions(Zn.krig, Zn.krig$Zn.predkrig, clean.donnees.Zn, 
                 "Prédiction de la concentration en Zinc avec un krigeage - Hainaut, Belgique")



### Cr ----

Cr.krig <- krige(formula = Cr~1,
                 data = clean.donnees.Cr,  
                 locations = ~x+y,
                 newdata = prov.grid, 
                 model = clean.fit.cr,
                 nmax=20)



setnames(Cr.krig, c("var1.pred", "var1.var"), c("Cr.predkrig", "Cr.varkrig"))

plot.predictions(Cr.krig, Cr.krig$Cr.predkrig, clean.donnees.Cr, 
                 "Prédiction de la concentration en Chrome avec un krigeage - Hainaut, Belgique")




## Co-krigage ----

# As the higher correlation is between the Cr and Ni value, we will keep those columns
# We keep a dataset after removing all NA values for Cr and Ni

donnees.cokrig <- Donnees %>% 
  drop_na(Cr) %>%
  drop_na(Ni)


# We need to create a gstat object with both data
g <- gstat(id="Ni", formula=Ni~1, data=donnees.cokrig, locations=~x+y)
g <- gstat(g, id="Cr", formula=Cr~1, data=donnees.cokrig, locations=~x+y)

v.cross <- variogram(g, cutoff=14000, width=1000)

plot(v.cross, main="Variograms and Cross-variogram of Nickel and Chrome", ylab="Semivariance", xlab="Distance")

LMC <- fit.lmc(v.cross, g, model=Ni.vg.fit, correct.diagonal=1.0000001) 
LMC


g <- copy(LMC)
g <- gstat(g, id="Cr", form=Cr~1, data=clean.donnees.Cr, locations=~x+y, model=LMC$model$Cr)

Ni.cok <- predict(g, prov.grid)

plot.predictions(Ni.cok, Ni.cok$Ni.pred, donnees.cokrig, 
                 "Predicted Ni Concentration using Cokriging algorithm with Cr in Hainaut province")

# Display points from both variables


ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_tile(data=Ni.cok, 
            aes(x=x, y=y, fill=Ni.pred)) +
  geom_point(data=donnees.cokrig, 
             aes(x=x, y=y, shape="Chrome data", size="Chrome data"),
             color="black") +
  geom_point(data=donnees.cokrig, 
             aes(x=x, y=y, shape="Nickel data", size="Nickel data"),
             color="black") +
  scale_shape_manual("", values=c(1,3)) +
  scale_size_manual("", values=c(1,1)) +
  scale_fill_gradientn(name="Ni prediction [(mg/m?)]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key=element_rect(fill="green3", 
                                color=NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Nickel cokriging prediction displaying both Nickel and Chrome points")+
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())
  



## Comparaison between Krigage and Co-krigage ----

# First we define the max and min values of both prediction variances to build 
# the same scale on both figures to be able to compare them
Ni_minvar = min(c(Ni.cok$Ni.varcok, Ni.krig$Ni.varkrig))
Ni_maxvar = max(c(Ni.cok$Ni.varcok, Ni.krig$Ni.varkrig))


# Krigage Variance for Ni prediction


ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_tile(data=Ni.krig, 
            aes(x = x, y = y, fill = Ni.varkrig)) +
  geom_point(data=donnees.cokrig, 
             aes(x=x, y=y, color="Measurement points"),
             shape=18,
             size=1) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Ni prediction variance [(mg/m³)²]",
                       colors=c('royalblue','green3', 'yellow', 'red'), limits=c(Ni_minvar,Ni_maxvar))+
  theme(legend.key = element_rect(fill = 'green3', 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Kriging prediction variance of Ni concentration in Hainaut province")+
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())


# Co-krigage Variance for Ni prediction

ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_tile(data=Ni.cok, 
            aes(x=x, y=y, fill=cov.Ni.Cr)) +
  geom_point(data=clean.donnees.Cr, 
             aes(x=x, y=y, shape="Chrome data", size="Chrome data"),
             color="black") +
  geom_point(data=donnees.cokrig, 
             aes(x=x, y=y, shape="Nickel data", size="Nickel data"),
             color="black") +
  scale_shape_manual("", values=c(1,3)) +
  scale_size_manual("", values=c(1,1)) +
  scale_fill_gradientn(name="Ni prediction variance [(mg/m³)²]", colors=c('royalblue','green3', 'yellow', 'red'))+
  theme(legend.key=element_rect(fill="green3", 
                                color=NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Cokriging prediction variance of Nickel using Chrome")+
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())


# First we define the max and min values of both prediction variances to build 
# the same scale on both figures to be able to compare them
vardif <- prov.grid
vardif$vardif <- Ni.cok$Ni.var - Ni.krig$Ni.varkrig

# Variance difference between kriging and co-kriging prediction
ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_tile(data=vardif, 
            aes(x=x, y=y, fill=vardif)) +
  geom_point(data=clean.donnees.Cr, 
             aes(x=x, y=y, shape="Chrome data", size="Chrome data"),
             color="black") +
  geom_point(data=clean.donnees.Ni, 
             aes(x=x, y=y, shape="Nickel data", size="Nickel data"),
             color="black") +
  
  labs(fill="Ni prediction variance difference") +
  scale_fill_gradientn( colors=c('red', 'yellow','green3', 'royalblue'))+
  scale_shape_manual("", values=c(1,3)) +
  scale_size_manual("", values=c(1,1)) +
  theme(legend.key=element_rect(fill='green3', color=NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Difference between kriging variance and cokriging variance of Nickel in Hainaut province")+
  theme_fivethirtyeight()  +
  theme(axis.title = element_text())


# Thématique : Carte de risques de hautes concentrations ---- 

## Ni ----


Ni.condsim <- krige(formula = Ni~1, 
                    data = clean.donnees.Ni, 
                    loc=~x+y, 
                    newdata = prov.grid,
                    model = clean.fit.ni, 
                    nsim = 1000, 
                    nmax = 20)

ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_tile(data=Ni.condsim, 
            aes(x = x, y = y, fill = sim2)) + # center title
  geom_point(data=clean.donnees.Ni, 
             aes(x=x, y=y, color="Measurement points"),
             shape=18,
             size=1) +
  scale_color_manual("", values="black") +
  scale_fill_gradientn(name="Ni simulation [mg/l]", colors=c('royalblue','green3', 'yellow', 'red')) +
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Conditional simulation") +
  theme(axis.title = element_text())

# Find the points where the concentration limit is probably exceeded

Ni.condsim <- as.data.table(Ni.condsim)
ishighN <- Ni.condsim[,-c(1,2)] > 20
riskN <- data.table(x=Ni.condsim$x, y = Ni.condsim$y, Cam = rowSums(ishighN)/1000)

ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_point(data = riskN, aes(x=x,y=y)) + 
  geom_point(data = riskN[Cam>.8,], aes(x=x,y=y), 
             shape = 1, 
             color = 'red',
             size = 3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  ggtitle("High risk areas") +
  theme(axis.title = element_text())


ggplot() + 
  geom_point(data=hainaut.grid, aes(x=x, y=y), color='grey97', shape = 3) +
  geom_tile(data = riskN, aes(x=x,y=y, fill=Cam)) +
  scale_fill_gradientn(name="P(Ni > 20 mg/l)", colors=c('royalblue','green3', 'yellow', 'red')) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.key = element_rect(fill = "green3", 
                                  color = NA)) +
  ggtitle("High calcium concentration") +
  theme(axis.title = element_text())

# Histogram of location 

riskmapN <- colSums(ishighN)/nrow(Ni.condsim)*100
ggplot(mapping = aes(riskmapN)) + 
  geom_histogram(color = 'black', bins = 30) + xlab("[%]") +
  ggtitle("Percentage of the surface with Ni > 20mg/l")

# END
