# Library -----------------------------------------------------------------


#BAM Data charts
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(lubridate)
library(openair) 
library(weathermetrics)


# Reading Data ------------------------------------------------------------


#Reading BAM Data from CSV  : for excel files use read.xl
bam_data <-read.csv("Your Data Path Here")
#C:/Users/Air Quality/Google Drive/ITEP R/R/GP BAM Data/DT122901.CSV


# Cleaning Data -----------------------------------------------------------


#cleaning column names
bam_data <- clean_names(bam_data, case = c("snake"))

#removing columns that are not needed
bam_data <- select(bam_data, -c(e, u, m, i, l, r, n ,f, p, d, c, t, 
                                qtot_m3, no_v, ws_mps, at_c, ft_c))

#converting Celceus to Farienheidt (dont mind the spelling too tired to really check)
bam_data <- mutate(bam_data, at_c_1 = celsius.to.fahrenheit(at_c_1, round = 1))

#Changing column names for graphs and removing excess columns
bam_data <- bam_data %>%
  mutate(Concentration = conc_ug_m3,
         Temperture = at_c_1,
         RelativeHumidity = rh)

#removing excess columns
bam_data <- select(bam_data,-c(conc_ug_m3, rh, at_c_1))

#inputting date columns
bam_data <- mutate(bam_data, date_time = mdy_hm(time))
bam_data <- mutate(bam_data, date = date(date_time))

#you will need filters on all plots due to maitnence and negative numbers. 
#If the number is around 985 then it is maintenance, 
#if it is negative then the filter tape is usually wacky


# Plot with Temp Color Dots -----------------------------------------------

#plot with color changing temperture dots
filter(bam_data, Concentration < 500, Concentration >= 0) %>%
  ggplot(aes(x = date,
             y = Concentration,
             color = Temperture)) +
  scale_colour_gradientn(colours = c( "blue", "blue","blue","cyan","yellow","red","red"),
                         breaks = c(-60,-40,-20,0,20,40,60,80, 100), 
                         limits=c(-60,100))+
  geom_point() 
  #+
  #geom_smooth(color = "red")

# Concentration Color Dots (AQI Colors) -----------------------------------


#plot with color changing concentration dots
filter(bam_data, Concentration < 500, Concentration >= 0) %>%
  ggplot(aes(x = date,
             y = Temperture,
             color = Concentration)) +
  scale_colour_gradientn(colours = c( "darkgreen", "yellow", "orange", "red", "purple", "maroon"),
                         breaks = c(0,50,100,150,200,300, 500), 
                         limits=c(0,500))+
  geom_point() 
#+
#geom_smooth(color = "red")


# Relative Humidity Color Dots --------------------------------------------


#plot with color changing relative humidity with date on x axis and temp on y axis
filter(bam_data, Concentration < 500, Concentration >= 0) %>%
  ggplot(aes(x = date,
             y = Temperture,
             color = RelativeHumidity)) +
  scale_colour_gradientn(colours = c( "white","cyan","blue", "blue3","darkblue"),
                         breaks = c(10,20,30,40,50,60,70,80,90,100), 
                         limits=c(0,100))+
  
  geom_point() 
  #+
  #geom_smooth(color = "brown")

# Concentration Color Dots (AQI Colors) RH on y axis and Date on x --------

#plot with color changing concentration with date on x axis and RH on y axis
filter(bam_data, Concentration < 500, Concentration >= 0) %>%
  ggplot(aes(x = date,
             y = RelativeHumidity,
             color = Concentration)) +
  scale_colour_gradientn(colours = c("darkgreen", "yellow", "orange", "red", "purple", "maroon"),
                         breaks = c(0,50,100,150,200,300,500), 
                         limits=c(0,500))+
  
  geom_point() 
#+
#geom_smooth(color = "brown")

# Relative Humidity x axis temp, y axis conc ------------------------------


#plot with color changing relative humidity
filter(bam_data, Concentration < 500, Concentration >= 0) %>%
  ggplot(aes(x = Temperture,
             y = Concentration,
             color = RelativeHumidity)) +
  scale_colour_gradientn(colours = c( "white","cyan","blue", "blue3","darkblue"),
                         breaks = c(10,20,30,40,50,60,70,80,90,100), 
                         limits=c(0,100))+
  
  geom_point() 
#+
#geom_smooth(color = "brown")


# Calendar with AQI Colors ------------------------------------------------



#plot with Calendar with color changing days
filter(bam_data, Concentration < 500, Concentration >= 0) %>%
 calendarPlot(bam_data, 
             pollutant    = "Concentration", 
             statistic    = "mean",
             cols         = c("darkgreen", "yellow", "orange", "red", "purple", "maroon"),
             limits       = c(0,500),
             month        = 1:6,
             year         = 2019, 
             annotate     = "date",
             digits       = 0, 
             key.footer   = "Concentration Levels", 
             data.thresh = 75,
             par.settings = list(fontsize       = list(text = 14), 
                                 layout.heights = list(top.padding = 1)),
             main = "PM 2.5 Concentrations")