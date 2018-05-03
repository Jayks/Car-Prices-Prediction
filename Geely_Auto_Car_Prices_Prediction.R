##***************************************************************************##
##                                                                           ##
## Project        : Geely Auto - Predicting car prices                       ##
## Objective      : Understand the factors affecting the pricing of cars in  ## 
##                  the American market and create a model that predicts the ##
##                  price of cars with the available independent variables   ## 
## Date           : 29-Apr-2018                                              ##
## Author         : Jayakumar Sekar                                          ##
##                                                                           ##
##***************************************************************************##

# Check and Import required libraries
options(warn = -1)
libs = c("tidyverse", "caret", "MASS","corrplot", "cowplot", 
         "car")
install.lib <- libs[!libs %in% installed.packages()]
for (pkg in install.lib) 
  install.packages(pkg, dependencies = T)
loadlib     <- lapply(libs, library, character.only = T) # load them
remove(list = ls())
options(warn = 0)

# library(tidyverse)
# library(caret)
# library(MASS)
# library(corrplot)
# library(cowplot)
# library(car)

# Import input file
car_eda <- car_raw <-  read_csv("CarPrice_Assignment.csv")

# Looking at the data
dim(car_raw)                                    # 205 obs. of  26 variables
str(car_raw)
glimpse(car_raw)
summary(car_raw)

# Checking for duplicates
length(unique(car_raw$car_ID)) != dim(car_raw)[1]
# 205 IDs matches with 205 total observations. So no duplicates

##***************************************************************************##
#                                                                             #
#                             Data Understanding                              #
#                                                                             #
##***************************************************************************##

#---------------------- Independent/Predictor variables -----------------------
# Symboling   -  Assigned insurance risk rating
  # A value of +3 indicates that the auto is risky, # -3 that it is probably 
  # pretty safe.
# carCompany  -  Name of car company  - Contains company and Car model
# fueltype    -  Car fuel type i.e gas or diesel 
# aspiration  -  Aspiration used in a car
  #  http://www.oneshift.com/features/8284/naturally-aspirated-vs-turbocharged-vs-supercharged-vs-twincharged-engines
# doornumber  -  Number of doors in a car 
# carbody     - Body of car
  # https://www.cartrade.com/blog/2013/auto-guides/different-car-body-types-494.html
# drivewheel  - type of drive wheel
  # https://en.wikipedia.org/wiki/Automobile_layout
# enginelocation - Location of car engine
# wheelbase  - Wheelbase of car 
  # https://www.carwow.co.uk/guides/glossary/what-is-a-car-wheelbase-0282
# carlength - Length of car 
# carwidth  - Width of car 
# carheight - height of car
# curbweight - The weight of a car without occupants or baggage
# enginetype - Type of engine
  # https://www.samarins.com/glossary/dohc.html
  # https://carbiketech.com/ohc-sohc-dohc/
# cylindernumber - Number of cylinders placed in the car
# enginesize - Size of engine
# fuelsystem 
  # bbl - BBL means the amount of holes that air enters the engine 
    # the technical name is BARRELS! When you take off the air filter there 
    # will be either 1 hole or 2 holes side by side or 4 holes 
    # 2 in the front and 2 in the rear. Hence the values 1bbl, 2bbl, 4bbl
    # idi - Indirect injection. Fuel is not directly injected into the 
    # combustion chamber
  # port injection
    # spfi - Sequential Port fuel injection
    # mpfi - Multipoint/port Fuel Injection

# boreratio & Stroke ratio
  # https://en.wikipedia.org/wiki/Stroke_ratio
# compressionratio  - Compression ratio is simply the volume of the cylinder
  # and the volume of the combustion chamber of the cylinder head when the 
  # piston is at Bottom Dead Center (BDC) and the volume of the cylinder head 
  # combustion chamber when the piston is at Top Dead Center (TDC)
  # http://blog.jpcycles.com/engine-compression-ratios-what-they-are-how-they-work/
# horsepower - 33,000 foot-pounds in one minute
  # A horse exerting 1 horsepower can raise 330 pounds of coal 100 feet 
  # in a minute, or 33 pounds of coal 1,000 feet in one minute, or 1,000 
  # pounds 33 feet in one minute
  #  https://auto.howstuffworks.com/horsepower.htm
# peakrpm - The power band of an internal combustion engine or electric motor 
  # is the range of operating speeds under which the engine or motor is able to
  # operate most efficiently. ... Diesel engines in cars and small trucks may 
  # develop maximum torque below 2,000 RPM with the power peak below 5,000 RPM.
  # https://en.wikipedia.org/wiki/Power_band
# citympg - Mileage in city 
# highwaympg - Mileage on highway 

#--------------------------- Dependant/Target variable ------------------------
# Price  - Price of car 

##***************************************************************************##
#                                                                             #
#                             Data Cleaning                                   #
#                                                                             #
##***************************************************************************##

# Are there are NA values? 
anyNA(car_eda)
# False - No NA Values

# Remove Car ID variable
car_eda <-  car_eda[,-1]

# Categorical Variables 
catvarnames <- names(Filter(is.character, car_eda))
# symboling, CarName, fueltype, aspiration, doornumber, carbody, 
# drivewheel, enginelocation, enginetype, fuelsystem, cylindernumber
# doornumber

# Lets look at the distribution of the categorical variables 
sapply(car_eda[catvarnames], table)

# Issues identified: 
# 1. Separate out company from the CarName as per the business need
# 2. Typo in enginetype, fuelsystem and Car name. 

# Separate CarName into company and model
# Keep only the Car Company column and drop the model
car_eda %>% separate(CarName, into = c("company"), 
                  sep = " ", extra = "drop" ) -> car_eda
# Check the data
table(car_eda$company)
# Correct the typos in the company names
car_eda$company <-  gsub("maxda", "mazda", car_eda$company)
car_eda$company <-  gsub("porcshce", "porsche", car_eda$company)
car_eda$company <-  gsub("toyouta", "toyota", car_eda$company)
car_eda$company <-  gsub("vw|vokswagen", "volkswagen", car_eda$company)
# Check the data again
table(car_eda$company)

# Following columns to be corrected for typos
# enginetype - We dont have dohcv type engine. Looks like a typo
car_eda$enginetype[which(car_eda$enginetype == "dohcv")] <- "dohc"
# fuelsystem - both mfi & mpfi are one and the same. 
car_eda$fuelsystem[which(car_eda$fuelsystem == "mfi")] <- "mpfi"

# doornumber, cylindernumber are in words. This shouldn't be an issue since
# we can treat them as categorical and use them in model by converting them to 
# dummy variables

# Convert Symboling to Character variable
car_eda$symboling <-  as.character(car_eda$symboling)

# Update Catgeorical variables vector 
catvarnames <- names(Filter(is.character, car_eda))

# Let's check the data again
sapply(car_eda[catvarnames], table)
# Looks good 

# Converting to Factors
car_eda <- car_eda %>% mutate_if(is.character,as.factor)

str(car_eda)
# Looks good  

##***************************************************************************##
#                                                                             #
#                         Derive New variables                                #
#                                                                             #
##***************************************************************************##

# Wheelbase to length ratio - tells us what proportion of the car's length 
# falls between the front and back wheels. In general, a car with a longer 
# wheelbase compared to its overall length will make better use of the 
# cabin space and feel more stable on the road.

car_eda$WBtoLratio  <-  car_eda$wheelbase/car_eda$carlength

# The car_eda data is now ready for exploratory data analysis

##***************************************************************************##
#                                                                             #
#                                EDA                                          #
#                                                                             #
##***************************************************************************##

#####   Common Functions  ##### 

# Setting the theme of plots
plot_theme <- theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14,face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12))

# Continuous Univariate plots 
ContUnivar <- function(yfeature, ylabel) {
  ggplot(car_eda, aes(x = "", y = yfeature)) +
    geom_boxplot(fill = "#F8766D", outlier.colour = "red", outlier.shape = 1) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs( y = ylabel, title = paste(ylabel, "Distribution")) +
    plot_theme
}

# Bivariate plots 
ContCatBivar <- function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(car_eda, aes(x = xfeature, y = yfeature, fill = xfeature)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = F) + 
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    plot_theme
}

ContContBivar <- function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(car_eda, aes(x = xfeature, y = yfeature)) +
    geom_point() +
    geom_smooth() +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    plot_theme
}


##***************************************************************************##
#                                                                             #
#                         Univariate Analysis                                 #
#                        (Continuos Variables)                                #
#                                                                             #
##***************************************************************************##
summary(car_eda$price)

p1 <- ContUnivar(car_eda$price, "Car Price" )
p2 <- ContUnivar(car_eda$wheelbase, "Wheel Base" )
p3 <- ContUnivar(car_eda$carlength, "Car Length" )
p4 <- ContUnivar(car_eda$carheight, "Car height" )
p5 <- ContUnivar(car_eda$curbweight, "Curb Weight")
p6 <- ContUnivar(car_eda$enginesize, "Engine Size")
p7 <- ContUnivar(car_eda$boreratio, "Bore Ratio")
p8 <- ContUnivar(car_eda$stroke, "Stroke")
p9 <- ContUnivar(car_eda$compressionratio, "Compression Ratio")
p10 <- ContUnivar(car_eda$horsepower, "Horse Power")
p11 <- ContUnivar(car_eda$peakrpm, "Peak RPM")
p12 <- ContUnivar(car_eda$citympg, "city mpg")
p13 <- ContUnivar(car_eda$highwaympg, "Highway mpg")
p14 <- ContUnivar(car_eda$WBtoLratio, "WB To L ratio")

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9 ,p10, p11, p12, p13, p14)

# Handling outliers since it might impact the model as metrics 
# such as R squared depend on the mean. 
car_eda[which(car_eda$wheelbase > 114),]$wheelbase <-  114
car_eda[which(car_eda$enginesize > 200),]$enginesize <-  200
car_eda[which(car_eda$stroke > 3.8),]$stroke <-  3.8
car_eda[which(car_eda$stroke < 2.7),]$stroke <-  2.7
car_eda[which(car_eda$compressionratio > 10),]$compressionratio <-  10
car_eda[which(car_eda$compressionratio < 7.5),]$compressionratio <-  7.5
car_eda[which(car_eda$horsepower > 180),]$horsepower <-  180
car_eda[which(car_eda$peakrpm > 6000),]$peakrpm <-  6000
car_eda[which(car_eda$citympg > 45),]$citympg <-  45
car_eda[which(car_eda$highwaympg > 45),]$highwaympg <-  45
car_eda[which(car_eda$WBtoLratio > 0.610),]$WBtoLratio <-  0.610

p1  <- ContUnivar(car_eda$price, "Car Price" )
p2  <- ContUnivar(car_eda$wheelbase, "Wheel Base" )
p3  <- ContUnivar(car_eda$carlength, "Car Length" )
p4  <- ContUnivar(car_eda$carheight, "Car height" )
p5  <- ContUnivar(car_eda$curbweight, "Curb Weight")
p6  <- ContUnivar(car_eda$enginesize, "Engine Size")
p7  <- ContUnivar(car_eda$boreratio, "Bore Ratio")
p8  <- ContUnivar(car_eda$stroke, "Stroke")
p9  <- ContUnivar(car_eda$compressionratio, "Compression Ratio")
p10 <- ContUnivar(car_eda$horsepower, "Horse Power")
p11 <- ContUnivar(car_eda$peakrpm, "Peak RPM")
p12 <- ContUnivar(car_eda$citympg, "city mpg")
p13 <- ContUnivar(car_eda$highwaympg, "Highway mpg")
p14 <- ContUnivar(car_eda$WBtoLratio, "WB To L ratio")

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9 ,p10, p11, p12, p13, p14)


##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Categorical Variables)                              #
#                                                                             #
##***************************************************************************##

# How does the categorical variables impact price? 

# Annova test
car_aov <-  aov(car_eda$price ~ car_eda$symboling + car_eda$company + 
                car_eda$fueltype + car_eda$aspiration + car_eda$doornumber + 
                car_eda$carbody + car_eda$drivewheel +car_eda$enginelocation +  
                car_eda$enginetype + car_eda$cylindernumber + 
                car_eda$fuelsystem)

summary(car_aov)

# The following variables have higher significance i.e P value < 0.05

# car_eda$symboling        5 1.768e+09 353527182  58.516  < 2e-16 ***
# car_eda$company         21 9.028e+09 429927266  71.162  < 2e-16 ***
# car_eda$aspiration       1 9.384e+07  93839247  15.532 0.000122 ***
# car_eda$carbody          4 6.320e+07  15800098   2.615 0.037379 *  
# car_eda$drivewheel       2 3.556e+08 177785078  29.427 1.47e-11 ***
# car_eda$enginelocation   1 3.466e+07  34658382   5.737 0.017807 *  
# car_eda$enginetype       4 3.548e+08  88697630  14.681 3.43e-10 ***
# car_eda$cylindernumber   4 2.972e+08  74311348  12.300 1.04e-08 ***
# car_eda$fuelsystem       5 7.394e+07  14787562   2.448 0.036298 * 
  
# Lets looks at these using plots

p1 <- ContCatBivar(car_eda$carbody, car_eda$price, 
                  "Carbody", "Price")
p2 <- ContCatBivar(car_eda$fueltype, car_eda$price,
                   "fueltype",  "Price")
p3 <- ContCatBivar(car_eda$doornumber, car_eda$price,
                   "Doors", "Price")
p4 <- ContCatBivar(car_eda$drivewheel, car_eda$price, 
                   "Drivewheel", "Price")
p5 <- ContCatBivar(car_eda$symboling, car_eda$price,
                   "Symboling", "Price")
p6 <- ContCatBivar(car_eda$enginetype, car_eda$price,
                   "enginetype", "Price")

plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2)


p1 <- ContCatBivar(car_eda$cylindernumber, car_eda$price,
                   "Cylinders", "Price")
p2 <- ContCatBivar(car_eda$fuelsystem, car_eda$price,
                   "Fuel system", "Price")
p3 <- ContCatBivar(car_eda$aspiration, car_eda$price,
                   "Aspiration", "Price")
p4 <- ContCatBivar(car_eda$enginelocation, car_eda$price,
                   "Engine Location",  "Price")

plot_grid(p1, p2, p3, p4, ncol = 2)

# High prices for : Rear engineloaction 

ContCatBivar(car_eda$company, car_eda$price, 
                   "company", "Price")
# High prices for companies: 
  # BMW, Buick, Jaguar, Porshce

##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Continuous Variables)                               #
#                                                                             #
##***************************************************************************##

# Price vs <rest of continuous variables>
p1 <- ContContBivar(car_eda$enginesize, car_eda$price, 
                   "Engine Size", "Price")
p2 <- ContContBivar(car_eda$horsepower, car_eda$price, 
                    "Horse Power", "Price")
p3 <- ContContBivar(car_eda$carwidth, car_eda$price, 
                    "Car Width", "Price")
p4 <- ContContBivar(car_eda$carlength, car_eda$price, 
                    "Car Length", "Price")
p5 <- ContContBivar(car_eda$carheight, car_eda$price, 
                     "Car Height", "Price")
p6 <- ContContBivar(car_eda$curbweight, car_eda$price, 
                    "CurbWeight", "Price")
p7 <- ContContBivar(car_eda$wheelbase, car_eda$price, 
                    "Wheelbase", "Price")
p8 <- ContContBivar(car_eda$boreratio, car_eda$price, 
                    "Bore ratio", "Price")
p9 <- ContContBivar(car_eda$citympg, car_eda$price, 
                    "City-mpg", "Price")
p10 <- ContContBivar(car_eda$highwaympg, car_eda$price, 
                    "Highway-mpg", "Price")
p11 <- ContContBivar(car_eda$WBtoLratio, car_eda$price, 
                     "WBtoLRatio", "Price")
p12 <- ContContBivar(car_eda$peakrpm, car_eda$price, 
                     "peakrpm", "Price")
p13 <- ContContBivar(car_eda$compressionratio, car_eda$price, 
                     "compressionratio", "Price")
p14 <- ContContBivar(car_eda$stroke, car_eda$price, 
                     "stroke", "Price")

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)


contvarnames <- names(Filter(is.numeric, car_eda))

# Correlation plot

corrplot.mixed(cor(car_eda[contvarnames]), upper = "ellipse", 
               tl.cex = 0.55, tl.pos = 'd')
# Covariance of variables
cov(car_eda[contvarnames])

##### Correlation with Price
  # Positive correlation 
    # horsepower  
    # enginesize 
    # CurbWeight
    # CarWidth
    # Carlength
    # Wheelbase
    # boreratio
  # Negative correlation 
    # citympg
    # highwaympg
    # WBtoLRatio

# Lets looks at the  Multicollinearity between variables
  # pairs(car_eda[contvarnames])
# We can use either pairs plot or correlation plot to conclude 
# on multicollineraity 

##### Multicollinearity between variables 
  #                +ve collineraity                 -ve collinearity
  # Carlength      Wheelbase, Carwidth,             Citympg, Highwaympg
  #                Curbweight, Enginesize 
  #                horsepower
  # Carwidth       Carlength, Curbweight            Citympg, Highwaympg
  #                Enginesize, horsepower
  # Curbweight     Carlength, Carwidth              Citympg, Highwaympg
  #                Wheelbase , enginesize
  #                horsepower
  # Enginesize     Curbweight, Carlength            Citympg, Highwaympg
  #                Carwidth
  # Horsepower     Curbweight, Carlength            Citympg, Highwaympg
  #                Carwidth
  # Citympg        Highwaympg                       All the above five 
  #                                                 variables 
  # Highwaympg     Citympg                          All the above five 
  #                                                 variables 
  # Wheel base     Carlength , Curbweight


##***************************************************************************##
#                                                                             #
#                           Model Building                                    #
#                                                                             #
##***************************************************************************##

# Dummy Variables                             

car_dummy <- dummyVars(" ~ .", data = car_eda, fullRank = T)
car_model <- data.frame(predict(car_dummy, newdata = car_eda))

# The data is now ready for Model building 

# Settng seed so that we could reproduce same results when run mutiple times
set.seed(2000) 
# Split training and test datasets
train_ind = sample(1:nrow(car_model), 0.7*nrow(car_model))
car_train = car_model[train_ind,]
car_test = car_model[-train_ind,]

# For quicker variable reduction lets use step AIC which reduces 
# variables based on Akaike information criterion
# http://www.statisticshowto.com/akaikes-information-criterion/

m1 = lm(price~., data = car_train)
summary(m1)
(stepAIC(m1, direction = "both"))

m2 <- lm(formula = price ~ symboling..2 + symboling.0 + symboling.1 + 
          symboling.2 + symboling.3 + company.audi + company.bmw + 
          company.buick + company.dodge + company.honda + company.jaguar + 
          company.mazda + company.mitsubishi + company.nissan + 
          company.plymouth + company.porsche + company.volvo + 
          carbody.hardtop + carbody.hatchback + carbody.sedan + 
          carbody.wagon + drivewheel.rwd + enginelocation.rear + 
          carwidth + curbweight + cylindernumber.five + cylindernumber.four + 
          cylindernumber.six + enginesize + fuelsystem.4bbl + 
          fuelsystem.mpfi + fuelsystem.spdi + boreratio + compressionratio + 
          horsepower, data = car_train)

# Let us look at the summary of the model
summary(m2)

## Let us check for multicollinearity 
vif(m2)

# Remove variables with high VIF (>2 generally) and 
# which are insignificant (p>0.05), one by one

# Remove symboling.0 - Pvalue : 0.207737  VIF : 5.533866
m3 <- lm(formula = price ~ symboling..2 +  symboling.1 + symboling.2 + 
          symboling.3 + company.audi + company.bmw + company.buick + 
          company.dodge + company.honda + company.jaguar + company.mazda +
          company.mitsubishi + company.nissan + company.plymouth + 
          company.porsche + company.volvo + carbody.hardtop + 
          carbody.hatchback + carbody.sedan + carbody.wagon + drivewheel.rwd +
          enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
          cylindernumber.four + cylindernumber.six + enginesize + 
          fuelsystem.4bbl + fuelsystem.mpfi + fuelsystem.spdi + boreratio + 
          compressionratio + horsepower, data = car_train)
summary(m3)
vif(m3)

# Remove symboling.2 - Pvalue : 0.186669  VIF : 2.258011 
m4 <- lm(formula = price ~ symboling..2 +  symboling.1 + symboling.3 + 
          company.audi + company.bmw + company.buick + company.dodge + 
          company.honda + company.jaguar + company.mazda + 
          company.mitsubishi + company.nissan + company.plymouth + 
          company.porsche + company.volvo + carbody.hardtop + 
          carbody.hatchback + carbody.sedan + carbody.wagon + 
          drivewheel.rwd + enginelocation.rear + carwidth + curbweight + 
          cylindernumber.five + cylindernumber.four + cylindernumber.six + 
          enginesize + fuelsystem.4bbl + fuelsystem.mpfi + fuelsystem.spdi + 
          boreratio + compressionratio + horsepower, data = car_train)
summary(m4)
vif(m4)

# Remove symboling.3 - Pvalue : 123582  VIF : 2.375856 
m5 <- lm(formula = price ~ symboling..2 +  symboling.1 + company.audi + 
          company.bmw + company.buick + company.dodge + company.honda + 
          company.jaguar + company.mazda + company.mitsubishi + 
          company.nissan + company.plymouth + company.porsche + 
          company.volvo + carbody.hardtop + carbody.hatchback + 
          carbody.sedan + carbody.wagon + drivewheel.rwd + 
          enginelocation.rear + carwidth + curbweight + cylindernumber.five +
          cylindernumber.four + cylindernumber.six + enginesize + 
          fuelsystem.4bbl + fuelsystem.mpfi + fuelsystem.spdi + boreratio + 
          compressionratio + horsepower, data = car_train)
summary(m5)
vif(m5)

# Remove symboling.1 - Pvalue : 0.218399. VIF : 1.903867
# Remove this variable eventhough VIF is < 2  since it has less significance
# We will follow similar strategy for rest of the variables
m6 <- lm(formula = price ~ symboling..2 + company.audi + company.bmw + 
          company.buick + company.dodge + company.honda + company.jaguar + 
          company.mazda + company.mitsubishi + company.nissan + 
          company.plymouth + company.porsche + company.volvo + 
          carbody.hardtop + carbody.hatchback + carbody.sedan + 
          carbody.wagon + drivewheel.rwd + enginelocation.rear + 
          carwidth + curbweight + cylindernumber.five + cylindernumber.four + 
          cylindernumber.six + enginesize + fuelsystem.4bbl + 
          fuelsystem.mpfi + fuelsystem.spdi + boreratio + compressionratio + 
          horsepower, data = car_train)
summary(m6)
vif(m6)

# Remove company.nissan - Pvalue : 0.441864 VIF : 1.481298
m7 <- lm(formula = price ~ symboling..2 + company.audi + company.bmw + 
          company.buick + company.dodge + company.honda + company.jaguar + 
          company.mazda + company.mitsubishi + company.plymouth + 
          company.porsche + company.volvo + carbody.hardtop + 
          carbody.hatchback + carbody.sedan + carbody.wagon + 
          drivewheel.rwd + enginelocation.rear + carwidth + curbweight + 
          cylindernumber.five + cylindernumber.four + 
          cylindernumber.six + enginesize + fuelsystem.4bbl + 
          fuelsystem.mpfi + fuelsystem.spdi + boreratio + compressionratio + 
          horsepower, data = car_train)
summary(m7)
vif(m7)

# Remove company.plymouth - Pvalue : 0.29232 VIF : 1.116948
m8 <- lm(formula = price ~ symboling..2 + company.audi + company.bmw + 
          company.buick + company.dodge + company.honda + company.jaguar + 
          company.mazda + company.mitsubishi +
          company.porsche + company.volvo + carbody.hardtop + 
          carbody.hatchback + carbody.sedan + carbody.wagon + 
          drivewheel.rwd + enginelocation.rear + carwidth + curbweight + 
          cylindernumber.five + cylindernumber.four + 
          cylindernumber.six + enginesize + fuelsystem.4bbl + 
          fuelsystem.mpfi + fuelsystem.spdi + boreratio + compressionratio + 
          horsepower, data = car_train)
summary(m8)
vif(m8)

# Remove fuelsystem.spdi - Pvalue : 0.066937 VIF : 2.617433
m9 <- lm(formula = price ~ symboling..2 + company.audi + company.bmw + 
           company.buick + company.dodge + company.honda + company.jaguar + 
           company.mazda + company.mitsubishi + company.porsche + 
           company.volvo + carbody.hardtop + carbody.hatchback + 
           carbody.sedan + carbody.wagon + drivewheel.rwd +
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + cylindernumber.six + enginesize + 
           fuelsystem.4bbl + fuelsystem.mpfi + boreratio + compressionratio + 
           horsepower, data = car_train)
summary(m9)
vif(m9)

# Remove company.honda - Pvalue : 0.12661 VIF : 1.336671
m10 <- lm(formula = price ~ symboling..2 + company.audi + company.bmw + 
           company.buick + company.dodge + company.jaguar + company.mazda +
           company.mitsubishi + company.porsche + company.volvo + 
           carbody.hardtop + carbody.hatchback + carbody.sedan + 
           carbody.wagon + drivewheel.rwd + enginelocation.rear + carwidth + 
           curbweight + cylindernumber.five + cylindernumber.four + 
           cylindernumber.six + enginesize + fuelsystem.4bbl + 
           fuelsystem.mpfi + boreratio + compressionratio + horsepower, 
           data = car_train)
summary(m10)
vif(m10)

# Remove company.dodge - Pvalue : 0.13972 VIF : 1.225234
m11 <- lm(formula = price ~ symboling..2 + company.audi + company.bmw + 
           company.buick +  company.jaguar + company.mazda + 
           company.mitsubishi + company.porsche + company.volvo + 
           carbody.hardtop + carbody.hatchback + carbody.sedan + 
           carbody.wagon + drivewheel.rwd + enginelocation.rear + carwidth +
           curbweight + cylindernumber.five + cylindernumber.four + 
           cylindernumber.six + enginesize + fuelsystem.4bbl + 
           fuelsystem.mpfi + boreratio + compressionratio + horsepower, 
           data = car_train)
summary(m11)
vif(m11)

# Remove symboling..2 - Pvalue : 0.091644 VIF : 1.306244
m12 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.mazda + company.mitsubishi +
           company.porsche + company.volvo + carbody.hardtop + 
           carbody.hatchback + carbody.sedan + carbody.wagon + drivewheel.rwd +
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + cylindernumber.six + enginesize + 
           fuelsystem.4bbl + fuelsystem.mpfi + boreratio + compressionratio + 
           horsepower, data = car_train)
summary(m12)
vif(m12)

# Remove boreratio - Pvalue : 0.122188 VIF : 3.455096
m13 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.mazda + company.mitsubishi +
           company.porsche + company.volvo + carbody.hardtop + 
           carbody.hatchback + carbody.sedan + carbody.wagon + drivewheel.rwd +
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + cylindernumber.six + enginesize + 
           fuelsystem.4bbl + fuelsystem.mpfi + compressionratio + horsepower, 
           data = car_train)
summary(m13)
vif(m13)

# Remove compressionratio - Pvalue : 0.198766 VIF : 1.971725
m14 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.mazda + company.mitsubishi +
           company.porsche + company.volvo + carbody.hardtop + 
           carbody.hatchback + carbody.sedan + carbody.wagon + drivewheel.rwd +
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + cylindernumber.six + enginesize + 
           fuelsystem.4bbl + fuelsystem.mpfi + horsepower, data = car_train)
summary(m14)
vif(m14)

# Remove fuelsystem.mpfi - Pvalue : 0.068040 VIF : 3.201412
m15 <- lm(formula = price ~ company.audi + company.bmw + company.buick + 
           company.jaguar + company.mazda + company.mitsubishi +
           company.porsche + company.volvo + carbody.hardtop + 
           carbody.hatchback + carbody.sedan + carbody.wagon + drivewheel.rwd +
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + cylindernumber.six + enginesize + 
           fuelsystem.4bbl + horsepower, data = car_train)
summary(m15)
vif(m15)

# Remove company.mazda - Pvalue : 0.144948 VIF : 1.491882
m16 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.mitsubishi + company.porsche + 
           company.volvo + carbody.hardtop + carbody.hatchback + 
           carbody.sedan + carbody.wagon + drivewheel.rwd + 
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + cylindernumber.six + enginesize + 
           fuelsystem.4bbl + horsepower, data = car_train)
summary(m16)
vif(m16)

# Remove cylindernumber.six - Pvalue : 0.019898 VIF : 8.016080
m17 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.mitsubishi + company.porsche + 
           company.volvo + carbody.hardtop + carbody.hatchback + 
           carbody.sedan + carbody.wagon + drivewheel.rwd + 
           enginelocation.rear + carwidth + curbweight + cylindernumber.five +
           cylindernumber.four + enginesize + fuelsystem.4bbl + horsepower, 
           data = car_train)
summary(m17)
vif(m17)

# Remove company.mitsubishi - Pvalue : 0.016761 VIF : 1.160546
m18 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo + 
           carbody.hardtop + carbody.hatchback + carbody.sedan + 
           carbody.wagon + drivewheel.rwd + enginelocation.rear + 
           carwidth + curbweight + cylindernumber.five + cylindernumber.four + 
           enginesize + fuelsystem.4bbl + horsepower, data = car_train)
summary(m18)
vif(m18)

# Remove drivewheel.rwd - Pvalue : 0.029077 VIF : 3.413830
m19 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo + 
           carbody.hardtop + carbody.hatchback + carbody.sedan + 
           carbody.wagon + enginelocation.rear + carwidth + curbweight + 
           cylindernumber.five + cylindernumber.four + enginesize + 
           fuelsystem.4bbl + horsepower, data = car_train)
summary(m19)
vif(m19)

# Remove fuelsystem.4bbl - Pvalue : 0.005363 VIF : 1.763442
m20 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo + carbody.hardtop +
           carbody.hatchback + carbody.sedan + carbody.wagon + 
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + enginesize + horsepower, data = car_train)
summary(m20)
vif(m20)

# Remove enginesize - Pvalue : 0.007423 VIF : 6.879208
m21 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo + carbody.hardtop +
           carbody.hatchback + carbody.sedan + carbody.wagon + 
           enginelocation.rear + carwidth + curbweight + cylindernumber.five + 
           cylindernumber.four + horsepower, data = car_train)
summary(m21)
vif(m21)

# Remove curbweight - Pvalue : 0.010690 VIF : 12.181051
m22 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo + carbody.hardtop +
           carbody.hatchback + carbody.sedan + carbody.wagon + 
           enginelocation.rear + carwidth +  cylindernumber.five + 
           cylindernumber.four + horsepower, data = car_train)
summary(m22)
vif(m22)

# Remove carbody.sedan - Pvalue : 4.62e-07  VIF : 11.680116
# Removing this variable eventhough very low P value since VIF is very high 
# and is highly collinear with other varaibales. 
m23 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo +  
           carbody.hardtop + carbody.hatchback + carbody.wagon + 
           enginelocation.rear + carwidth +  cylindernumber.five + 
           cylindernumber.four + horsepower, data = car_train)
summary(m23)
vif(m23)

# Remove carbody.hardtop - Pvalue : 0.554280 VIF : 1.282394
m24 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo +  
           carbody.hatchback + carbody.wagon + enginelocation.rear + 
           carwidth +  cylindernumber.five + cylindernumber.four + 
           horsepower, data = car_train)
summary(m24)
vif(m24)

# Remove carbody.wagon - Pvalue : 0.203255 VIF : 1.127713
m25 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo +  
           carbody.hatchback + enginelocation.rear + carwidth +  
           cylindernumber.five + cylindernumber.four + horsepower, 
           data = car_train)
summary(m25)
vif(m25)

# Remove carbody.hatchback - Pvalue : 0.048336 VIF : 1.227726
m26 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo +  
           enginelocation.rear + carwidth + cylindernumber.five + 
           cylindernumber.four + horsepower, data = car_train)
summary(m26)
vif(m26)

# Remove cylindernumber.four - Pvalue : 0.010571 VIF : 2.614653
m27 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo +  
           enginelocation.rear + carwidth + cylindernumber.five + 
           horsepower, data = car_train)
summary(m27)
vif(m27)

# Remove cylindernumber.five - Pvalue : 0.009296 VIF : 2.529317
m28 <- lm(formula = price ~ company.audi + company.bmw + company.buick +  
           company.jaguar + company.porsche + company.volvo +  
           enginelocation.rear + carwidth + horsepower, data = car_train)
summary(m28)
vif(m28)

# Remove company.audi - Pvalue : 0.019097 VIF : 1.178744
m29 <- lm(formula = price ~ company.bmw + company.buick + company.jaguar +
           company.porsche + company.volvo + enginelocation.rear + carwidth +
           horsepower, data = car_train)
summary(m29)
vif(m29)

# Remove company.volvo - Pvalue : 0.001768 VIF : 1.176292
m30 <- lm(formula = price ~ company.bmw + company.buick + company.jaguar +
           company.porsche + enginelocation.rear + carwidth +
           horsepower, data = car_train)
summary(m30)
vif(m30)

# Remove company.porsche - Pvalue : 0.00494 VIF : 2.185554
m31 <- lm(formula = price ~ company.bmw + company.buick + company.jaguar +
           enginelocation.rear + carwidth + horsepower , data = car_train)
summary(m31)
vif(m31)

(rsquared <-  summary(m31)$r.squared)
(rsquared_adjusted <-  summary(m31)$adj.r.squared)

# Predict prices on test dataset using model m31
car_test$predicted_price <- predict(m31,car_test[,-67])

# R squared between actual & predicted prices 
(rsquared_predicted <- cor(car_test$price,car_test$predicted_price)^2)

# Plot - Actual vs Predicted Prices 
ggplot(car_test, aes(x = seq_along(predicted_price))) + 
  geom_line(aes(y = price), col = 'blue') + 
  geom_line(aes(y = predicted_price), col = 'red') + 
  geom_text(aes(x = 60, y = 8000), label = "Predicted Price", color = 'red') +
  geom_text(aes(x = 58, y = 14500), label = "Actual Price", color = 'blue') +
  labs(x = " ", y = "Price", title = "Actual vs Predicted Price") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Error calculation (Actual prices - predicted prices)
car_test$error <-  car_test$price - car_test$predicted_price

# Plot errors
ggplot(car_test, aes(price, error)) + geom_point() + plot_theme
# The errors (the differences between the actual values and 
# the values predicted by the model) are randomly distributed. 
# What this essentially confirms is that there are no variables that could have
# helped explain the model better.

##***************************************************************************##
#                                                                             #
#                             Summary                                         #
#                                                                             #
##***************************************************************************##

# No. of model iterations = 31. The final model is m31
# Final coefficients : 
  # Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)         -87124.905   8688.251 -10.028  < 2e-16 ***
  #   company.bmw           9281.917    908.098  10.221  < 2e-16 ***
  #   company.buick        11820.702   1180.869  10.010  < 2e-16 ***
  #   company.jaguar       10598.190   1692.828   6.261 4.68e-09 ***
  #   enginelocation.rear  18135.781   1780.099  10.188  < 2e-16 ***
  #   carwidth              1380.497    140.679   9.813  < 2e-16 ***
  #   horsepower              77.971      8.552   9.118 8.97e-16 ***
# All the final variables selected for the model:  
  # Are Significant i.e P value almost = 0 and negligible 
  # Have VIF < 3 indicating no major multicollinearity 
# Multiple R-squared for train data set:  92.32% (~92%)
# Adjusted R-squared for train data set - 91.98% (~92%)
  # The R-squared and adjusted R-squared are pretty close indicating 
  # that all of the predictors are significant variables. 
# R-squared for test data set  - 89.98% (~90%)
  # The model explains 90% of variation of price around its mean 
  # in the test dataset
# Randomness in errors
# Final variables selected for the model are 
  # Company : BWM, Buick, Jaquar
  # Engine location : Rear
  # Carwidth 
  # Horsepower 
# All the above variables were identified as top variables that impacts
  # prices during our EDA analysis. This indicates that the model iterations
  # has rightly drilled down and identified the correct predictors for 
  # the car price in the final model. 

#************************************End of file*******************************