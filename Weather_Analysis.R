#LOE HUI LIN
#TP060359 
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#READING WEATHER DATASET
weather<-read.csv(file = "C:\\Users\\Loe Hui Lin\\Desktop\\PDFA\\Assignment\\weather.csv", header=TRUE, sep=",")
View(weather)



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DATA CLEANING (HANDLING MISSING DATA)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#CHECK FOR MISSING VALUES (NA)
summary(weather)

#Sunshine NA: 3
#WindGustSpeed NA: 2
#WindSpeed9am NA: 7

length(which(is.na(weather$WindGustDir)))
length(which(is.na(weather$WindDir9am)))
length(which(is.na(weather$WindDir3pm)))

#WindGustDir NA: 3
#WindDir9am NA: 31
#WindDir3pm NA: 1



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#FILLING IN MISSING VALUES WITH MEAN VALUE(NA)
#filling in missing Sunshine values
Sunshine_Fill <- weather$Sunshine
Sunshine_Fill[is.na(Sunshine_Fill)] <- round(mean(Sunshine_Fill,na.rm=TRUE), digits = 1)
#to show NA values are replaced in Sunshine
Sunshine_Fill2 <- table(is.na(Sunshine_Fill))
Sunshine_Fill2 

#filling in missing WindGustSpeed values
WindGustSpeed_Fill <- weather$WindGustSpeed
WindGustSpeed_Fill[is.na(WindGustSpeed_Fill)] <- round(mean(WindGustSpeed_Fill,na.rm=TRUE), digits = 1)
#to show NA values are replaced in WindGustSpeed
WindGustSpeed_Fill2 <- table(is.na(WindGustSpeed_Fill))
WindGustSpeed_Fill2 

#filling in missing WindSpeed9am values
WindSpeed9am_Fill <- weather$WindSpeed9am
WindSpeed9am_Fill[is.na(WindSpeed9am_Fill)] <- round(mean(WindSpeed9am_Fill,na.rm=TRUE), digits = 1)
#to show NA values are replaced in WindSpeed9am
WindSpeed9am_Fill2 <- table(is.na(WindSpeed9am_Fill))
WindSpeed9am_Fill2



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#FILLING IN MISSING VALUES WITH MODE VALUE(NA)
#filling in missing  WindGustDir values
WindGustDir_Fill <- weather$WindGustDir
WindGustDir_Fill[is.na(WindGustDir_Fill)] <- names(table(WindGustDir_Fill))[table(WindGustDir_Fill) == max(table(WindGustDir_Fill))]
#to show NA values are replaced in WindGustDir
table(WindGustDir_Fill)

#filling in missing WindDir9am values
WindDir9am_Fill <- weather$WindDir9am
WindDir9am_Fill[is.na(WindDir9am_Fill)] <- names(table(WindDir9am_Fill))[table(WindDir9am_Fill) == max(table(WindDir9am_Fill))]
#to show NA values are replaced in WindDir9am
table(WindDir9am_Fill)

#dropping missing value in WindDir3pm might create future problems
#due to 1 NA value + 2 mode values
#WindDir3pm_Fill <- na.omit(weather$WindDir3pm)

#renaming columns after data cleaning
SS <- Sunshine_Fill
WGS <- WindGustSpeed_Fill
WS9 <- WindSpeed9am_Fill
WGD <- WindGustDir_Fill
WD9 <- WindDir9am_Fill

MinT <- weather$MinTemp
MaxT <- weather$MaxTemp
RF <- weather$Rainfall
E <- weather$Evaporation
WS3 <-weather$WindSpeed3pm
WD3 <- weather$WindDir3pm
H9 <- weather$Humidity9am
H3 <- weather$Humidity3pm
P9 <-weather$Pressure9am
P3 <- weather$Pressure3pm
C9 <- weather$Cloud9am
C3 <- weather$Cloud3pm
T9 <- weather$Temp9am
T3 <- weather$Temp3pm
RToday <- weather$RainToday
RTmr <- weather$RainTomorrow
RiskMM <- weather$RISK_MM

#creating + renaming new columns with updated values
#install.packages("dplyr")
library(dplyr)
weather = mutate(weather, SS=SS)
weather = mutate(weather, WGS=WGS)
weather = mutate(weather, WS9=WS9)
weather = mutate(weather, WGD=WGD)
weather = mutate(weather, WD9=WD9)
#WD3  not mutated 
summary(weather)

#renaming existing columns with new names
#before renaming
colnames(weather)
weather <- weather %>% rename(MinT = MinTemp, MaxT = MaxTemp, RF = Rainfall,
                              E = Evaporation, WD3 = WindDir3pm, WS3 = WindSpeed3pm, 
                              H9 = Humidity9am, H3 = Humidity3pm, P9 = Pressure9am, 
                              P3 = Pressure3pm, C9 = Cloud9am, C3 = Cloud3pm, 
                              T9 = Temp9am, T3 = Temp3pm, RToday = RainToday, 
                              RTmr = RainTomorrow, RiskMM = RISK_MM)
#after renaming 
colnames(weather)



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUESTION1: WHAT ARE THE FACTORS THAT AFFECT IMPLEMENTATION OF WIND TURBINES? 
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS1.1: Distribution of Wind Speed & Wind Direction determines location for wind turbines
#VARIABLES USED: WS9, WS3, WD3
#CONCLUSION: North direction at 9am has the highest + most of the low wind speeds
#CONCLUSION: West direction at 3pm has high consistency in average wind speed

#selecting specific columns for Question1
Q1 <- data.frame(subset (weather, select = c("WGD", "WGS", "WD9", "WD3", "WS9", "WS3", 
                                             "P9", "P3", "T3", "T9")))
View(Q1)

#binning WD3 values into 4 categories: South, East, West, North
Directions <- Q1$WD3
Wind_Directions <- ifelse(Directions %in% c("S", "SE", "SSE", "SSW", "SW"), "South",
                    ifelse(Directions %in% c("E", "ENE", "ESE"), "East",
                      ifelse(Directions %in% c("W", "WNW", "WSW"), "West",
                        "North")))
table(Wind_Directions)

#plotting histogram for distribution of wind speed in different wind directions
#wind speed distribution histogram at 9am 
library(ggplot2)
Q1_hist <- ggplot(weather, aes(x = WS9, fill = Wind_Directions, color = Wind_Directions)) +
  geom_histogram(bins = 50, binwidth = 1, position = "identity", alpha = 0.5) + 
                scale_x_continuous(name = "Wind Speed at 9am (mph)", 
                                   breaks = seq(0, 41, 5), limits = c(0, 41)) + 
                scale_y_continuous(name = "Count") + 
                ggtitle("Distribution of Wind Speed at 9am Across Wind Directions")
Q1_hist

#distribution of histograms for wind speed at 3pm
Q1_hist2 <- ggplot(Q1, aes(x = WS3, fill = Wind_Directions, color = Wind_Directions)) +
  geom_histogram(bins = 50, binwidth = 1, position = "identity", alpha = 0.5) + 
                scale_x_continuous(name = "Wind Speed at 3pm (mph)", 
                                   breaks = seq(0, 52, 5), limits = c(0, 52)) + 
                scale_y_continuous(name = "Count") + 
                ggtitle("Distribution of Wind Speed at 3pm Across Wind Directions")
Q1_hist2



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS1.2: Change in Wind Direction throughout a day determines location choice for wind turbines
#VARIABLES USED: WD9, WD3
#CONCLUSION: South East is most windy at 9am (21%)
#CONCLUSION: North West & West North West are most windy at 3pm (17%)

#Generating WD9am piechart
WD9am <- c()
WD9am[1] = nrow(Q1[WD9 == "N",])
WD9am[2] = nrow(Q1[WD9 == "S",])
WD9am[3] = nrow(Q1[WD9 == "E",])
WD9am[4] = nrow(Q1[WD9 == "W",])
WD9am[5] = nrow(Q1[WD9 == "SE",])
WD9am[6] = nrow(Q1[WD9 == "SW",])
WD9am[7] = nrow(Q1[WD9 == "NE",])
WD9am[8] = nrow(Q1[WD9 == "NW",])
WD9am[9] = nrow(Q1[WD9 == "ENE",])
WD9am[10] = nrow(Q1[WD9 == "ESE",])
WD9am[11] = nrow(Q1[WD9 == "NNE",])
WD9am[12] = nrow(Q1[WD9 == "NNW",])
WD9am[13] = nrow(Q1[WD9 == "SSE",])
WD9am[14] = nrow(Q1[WD9 == "SSW",])
WD9am[15] = nrow(Q1[WD9 == "WNW",])
WD9am[16] = nrow(Q1[WD9 == "WSW",])

WindName <- c("North","South","East","West",
              "South East", "South West","North East", "North West", 
              "East North East", "Ease South East", "North North East", 
              "North North West", "South South East","South South West", 
              "West North West", "West South West")

Q1pie <- round(WD9am/sum(WD9am)*100)
Q1pie
WindName <- paste(WindName, Q1pie)
WindName <- paste(WindName,"%",sep = "")

pie(WD9am, labels = WindName, main="Pie chart of Wind Direction at 9am",
    radius = 5, border = "white", col = rainbow(length(WindName)))

#check frequency of WD3 values
#WD3 <- factor(Question1$WD3)
WD3_Table <- table(factor(Q1$WD3))
WD3_Table

#removing NA values from WD3
WD3 <- na.omit(Q1$WD3)

#Generating WD3pm piechart
WD3pm <- c()
WD3pm[1] = nrow(Q1[WD3 == "N",])
WD3pm[2] = nrow(Q1[WD3 == "S",])
WD3pm[3] = nrow(Q1[WD3 == "E",])
WD3pm[4] = nrow(Q1[WD3 == "W",])
WD3pm[5] = nrow(Q1[WD3 == "SE",])
WD3pm[6] = nrow(Q1[WD3 == "SW",])
WD3pm[7] = nrow(Q1[WD3== "NE",])
WD3pm[8] = nrow(Q1[WD3== "NW",])
WD3pm[9] = nrow(Q1[WD3 == "ENE",])
WD3pm[10] = nrow(Q1[WD3 == "ESE",])
WD3pm[11] = nrow(Q1[WD3 == "NNE",])
WD3pm[12] = nrow(Q1[WD3 == "NNW",])
WD3pm[13] = nrow(Q1[WD3 == "SSE",])
WD3pm[14] = nrow(Q1[WD3 == "SSW",])
WD3pm[15] = nrow(Q1[WD3 == "WNW",])
WD3pm[16] = nrow(Q1[WD3 == "WSW",])

Q1pie2 <- round(WD3pm/sum(WD3pm)*100)

WindName <- paste(WindName, Q1pie2)
WindName <- paste(WindName,"%",sep = "")

pie(WD3pm, labels = WindName, main="Pie chart of Wind Direction at 3pm",
    radius = 6.1, border = "white", col = rainbow(length(WindName)))



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS1.3: Temperature Affects Atmospheric Pressure
#VARIABLES USED: P3, P9, WS3, WS9
#CONCLUSION: Atmospheric pressure is directly proportional to temperature

#generating scatterplot
#AC209B = purple = Pressure at 9am
#866B86 = brown = Pressure at 3pm
cols2 <-c("9AM" = "#AC209B", "3PM" = "#866B86")

Q1_scatterplot <- ggplot(Q1) + 
  geom_point(aes(x=T9,y=P9, colour = "9AM")) +
  geom_smooth(aes(x=T9,y=P9, colour = "9AM"), method = "lm") +
  geom_point(aes(x=T3,y=P3, colour = "3PM")) +
  geom_smooth(aes(x=T3,y=P3, colour = "3PM"), method = "lm") +
  labs(x = "Temperature (degrees)", y = "Atmosheric Pressure (hpa)",
       title = "Relation between Temperature and Atmospheric Pressure") +
  scale_colour_manual(name="Time",values=cols2,
                      guide = guide_legend(override.aes=aes(fill=NA)))
Q1_scatterplot



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS1.4: Atmospheric Pressure affects Wind Speed
#VARIABLES USED: P3, P9, WS3, WS9
#CONCLUSION: Wind Speed is inversely proportional to Atmospheric Pressure

#generating scatterplot
#4D9A24 = green = Wind Speed at 9am
#287CD1 = blue = Wind Speed at 3pm
cols2 <-c("9AM" = "#4D9A24", "3PM" = "#287CD1")

Q1_scatterplot2 <- ggplot(Q1) + 
              geom_point(aes(x=P9,y=WS9, colour = "9AM")) +
              geom_smooth(aes(x=P9,y=WS9, colour = "9AM"), method = "lm") +
              geom_point(aes(x=P3,y=WS3, colour = "3PM")) +
              geom_smooth(aes(x=P3,y=WS3, colour = "3PM"), method = "lm") +
              labs(x = "Atmospheric pressure (hpa)", y = "Wind Speed (mph)",
                 title = "Relation between Atmospheric Pressure and Wind Speed") +
              scale_colour_manual(name="Time",values=cols2,
                                  guide = guide_legend(override.aes=aes(fill=NA)))
Q1_scatterplot2



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS1.5: Air density as determinant of efficiency of wind turbines
#VARIABLES USED: T3, T9, P9, P3
#CONCLUSION: Air density decreases as temperature increases (inversely proportional)
#CONCLUSION: Increase in air density also increases energy received by wind turbines

#calculating average pressure and temperature
Avg_P <- round(((P9 + P3)/2), digits = 1)
AvgP_Pascal <- Avg_P * 100

Avg_T <- round(((T3 + T9)/2), digits = 1)
AvgT_Kelvin <- Avg_T + 273.15

#function to calculate air density
AirDensity <- function (AvgT_Kelvin, AvgP_Pascal)
{
  WP = (28.9647*0.001)*AvgP_Pascal
  RT = 8.314*AvgT_Kelvin 
  result = round((WP/RT), digits = 4)
  return(result)
}

AirDen <- AirDensity(AvgT_Kelvin, AvgP_Pascal)

#renaming variables + add new columns to data frame
Air_Density <- AirDen
Average_Temperature <- AvgT_Kelvin


Q1 = mutate(Q1, Air_Density=Air_Density)
Q1 = mutate(Q1, Average_Temperature=Average_Temperature)

#generate scatterplot of air density against temperature
Q1_scatterplot3 <- ggplot(Q1, aes(x = Average_Temperature, y = Air_Density)) + 
                    geom_point() +
                    geom_smooth(method = "lm") +
                    scale_y_continuous(breaks = seq(1.1, 1.3, by = 0.05)) +
                    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
                    labs(title="Distribution of Average Air Density", 
                         x="Average Temperature (degrees)", y="Average Air Density (in kg/m^3)")
Q1_scatterplot3



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUESTION2: FACTORS THAT AFFECT LOCATION CHOICE FOR SOLAR PANELS
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS2.1: Distribution of Sunshine Intensity 
#VARIABLES USED: P9, P3, SS
#CONCLUSION: Lowest pressure + highest sunshine = best location for collecting solar energy
#CONCLUSION: High pressure + highest sunshine = second best location for collecting solar energy
#CONCLUSION: Unable to fulfill first point* 
#CONCLUSION: Best: Pressure between 1015 to 1025 under very high sunshine intensity category

#selecting specific columns for Question2
Q2 <- data.frame(subset (weather, select = c("SS", "RiskMM", "P9", "P3", "MinT", "MaxT", "C9", "C3")))
View(Q2)

#calculate average pressure of the day + adding new column into data frame
Avg_P <- round(((P9 + P3)/2), digits = 1)
Q2 = mutate(Q2, Avg_P=Avg_P)

#binning sunlight hours into categories + adding new column into data frame
SS_gap <- c(0, 3, 6, 9, 14) 
SS_label <- c("Low", "Medium", "High", "Very High")
SS_group <- cut(Q2$SS, breaks = SS_gap, include.lowest = TRUE, right = TRUE, labels = SS_label)
Q2_SS <- SS_group
Q2 = mutate(Q2, Q2_SS=Q2_SS)

#calculate mean value of each average pressure distribution under each sunshine category
library(plyr)
mu <- ddply(Q2, "Q2_SS", summarise, grp.mean=mean(Avg_P))
mu

#generating histogram for average pressure distribution against sunlight hours
Q2_histogram <- ggplot(Q2, aes(x = Avg_P))+
                geom_histogram(color="#FF5733", fill="#F5B468", binwidth = 1) + facet_wrap("Q2_SS") 
legend2 = c("Low Sunshine: 0 - 3", "Medium Sunshine: 3 - 6", "High Sunshine: 6 - 9", "Very High Sunshine: 9 - 14")
Q2_histogram + geom_vline(data = mu, aes(xintercept = grp.mean, color=legend2), linetype="dashed") +
                          labs(title="Distribution of Average Pressure Against Sunshine Intensity",
                          x="Average Atmospheric Pressure", y = "Count of Sunshine Intensity") +
                          guides(col = guide_legend("Mean Atmpsheric Pressure Under\nDifferent Sunshine Intensities"))



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS2.2: Temperature will affect efficiency of solar panels 
#VARIABLES USED: MaxT, MinT, SS
#CONCLUSION: Solar panels produce less power on hot temps. (lower temps. are favorable)

#generating boxplot (MaxT & SS)
Q2_boxplot <- ggplot(Q2, aes(x = Q2_SS, y = MaxT)) +
                    geom_boxplot(aes(color = factor(Q2_SS))) +
                      stat_summary(funn.y = mean, geom="point", shape=23, size=6)
Q2_boxplot + labs( x = "Sunshine Intensity", y = "Maximum Temperature",
                  title = "Relation between Sunshine Intensity and Maximum Temperature") +
                  scale_y_continuous(breaks = seq(0, 36, by = 5)) +
                  guides(col = guide_legend("Sunshine Intensities"))

#generating boxplot (MinT & SS)
Q2_boxplot2 <- ggplot(Q2, aes(x = Q2_SS, y = MinT)) +
                    geom_boxplot(aes(color = factor(Q2_SS))) +
                      stat_summary(fun.y=mean, geom="point", shape=23, size=6)
Q2_boxplot2 + labs(x = "Sunshine Intensity", y = "Minimum Temperature",
                  title = "Relation between Sunshine Intensity and Minimum Temperature") +
                  scale_y_continuous(breaks = seq(-6, 21, by = 5)) +
                  guides(col = guide_legend("Sunshine Intensities"))



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS2.3: Cloud Coverage will affect efficiency of solar panels
#VARIABLES USED: C9, C3
#CONCLUSION: higher cloud coverage = higher solar shading = lower solar panel efficiency

#calculating average cloud coverage
Avg_C <- round(((C9 + C3)/2), digits = 1)
Q2 = mutate(Q2, Avg_C=Avg_C)
View(Q2)

Q2_hist2 <- ggplot(data=Q2, aes(Avg_C)) + 
  geom_histogram(col="white", fill="#92D8E7", binwidth = 1, stat = "bin") +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+ 
  labs(title="Distribution of Average Cloud Coverage") +
  labs(x="Cloud Coverage (in oktas)", y="Count")
Q2_hist2



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUESTION3: FACTORS THAT MIGHT CAUSE NATURAL DISASTERS
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS3.1: Correlation between temp diff and evaporation = (increased wind speed at sea)
#VARIABLES USED: MinT, MaxT, E
#CONCLUSION: High temp diff will cause increase in evaporation (as indicated by linear regression)

#selecting specific columns for Question3
Q3 <- data.frame(subset (weather, select = c("RF", "RiskMM", "WGD", "WGS", "P9", "P3", "MaxT", "MinT", "E")))
View(Q3)

#calculating difference in temp + adding new column 
TempDiff <- round((MaxT - (MinT)), digits = 1)
Q3 = mutate(Q3, TempDiff = TempDiff)

#binning temperature difference
TD_gap <- c(0, 7, 14, 23) 
TD_label <- c("Low", "Medium", "High")
TD_group <- cut(Q3$TempDiff, breaks = TD_gap, include.lowest = TRUE, right = TRUE, labels = TD_label)
Q3_TD <- TD_group
Q3 = mutate(Q3, Q3_TD=Q3_TD)

#generating scatter plot
Q3_scatterplot <- ggplot(Q3, aes(x = TempDiff, y = E, color = Q3_TD)) + 
                    geom_point() +
                      geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                        guides(col = guide_legend("Difference in Temperature")) +
                          labs(x = "Difference in Temperature (degrees)", y = "Evaporation",
                              title = "Relation between Evaporation and Temperature Difference") +
                            scale_y_continuous(breaks = seq(0, 14, by = 3)) +
                            scale_x_continuous(breaks = seq(0, 25, by = 5))
Q3_scatterplot



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS3.2: Distribution of wind gust direction
#VARIABLES USED: WGD
#CONCLUSION: Most wind gusts come from North West (21%)

#generating wind gust direction pie chart
library(plotrix)
WG_D <- c()
WG_D[1] = nrow(Q3[WGD == "N",])
WG_D[2] = nrow(Q3[WGD == "S",])
WG_D[3] = nrow(Q3[WGD == "E",])
WG_D[4] = nrow(Q3[WGD == "W",])
WG_D[5] = nrow(Q3[WGD == "SE",])
WG_D[6] = nrow(Q3[WGD == "SW",])
WG_D[7] = nrow(Q3[WGD== "NE",])
WG_D[8] = nrow(Q3[WGD== "NW",])
WG_D[9] = nrow(Q3[WGD== "ENE",])
WG_D[10] = nrow(Q3[WGD== "ESE",])
WG_D[11] = nrow(Q3[WGD== "NNE",])
WG_D[12] = nrow(Q3[WGD== "NNW",])
WG_D[13] = nrow(Q3[WGD== "SSE",])
WG_D[14] = nrow(Q3[WGD== "SSW",])
WG_D[15] = nrow(Q3[WGD== "WNW",])
WG_D[16] = nrow(Q3[WGD== "WSW",])

WindNameT <- c("N","S","E","W","SE", "SW","NE", "NW",
               "ENE", "ESE", "NNW", "SSE",
               "SSW", "WNW", "WSW")

Q3pie <- round(WG_D/sum(WG_D)*100)

WindNameT <- paste(WindNameT, Q3pie)
WindNameT <- paste(WindNameT,"%",sep = "")

pie3D(WG_D, labels = WindNameT, main="Pie chart of Wind Gust Direction",
      radius = 1, border = "white", col = rainbow(length(WindNameT)))



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS3.3: Distribution of wind gust speed in North West
#VARIABLES USED: WGS
#CONCLUSION:Mean wind speed is around 45, highest wind speed can reach close to 100 (dangerous)

#selecting wind gust speeds with North West direction
filter_NW <- filter(Q3, WGD %in% c("NW"))
filter_NW <- filter_NW[, 4]
filter_NW

#put selected values into new data frame
SubQ3 <- rbind(data.frame(WindDirection = "NW", WindSpeed = filter_NW))
View(SubQ3)

#generating histogram for wind speed in north west
Q3_hist <- ggplot(data=SubQ3, aes(WindSpeed)) + 
            geom_histogram(col="white", fill="steelblue", binwidth = 3) +
            geom_vline(aes(xintercept=mean(WindSpeed)),
            color="black", linetype="dashed", size=.5) + 
            scale_x_continuous(breaks = seq(0, 100, by = 5)) +
            scale_y_continuous(breaks = seq(0, 20, by = 2))+ 
            labs(title="Histogram for Wind Gust Speed in North West Direction") +
            labs(x="Wind Gust Speed (mph)", y="Count")
Q3_hist


#MIGHT BE USEFUL - LINE CHART FOR DISTRIBUTION OF WIND SPEED
x = ggplot(data=Q3,aes(WGS))+
      geom_freqpoly(bins = 50)+
       geom_vline(aes(xintercept=mean(WGS)), color="red", size=1) +
        scale_x_continuous(breaks = seq(0, 100, by = 5)) +
        scale_y_continuous(breaks = seq(0, 70, by = 5))+ 
        labs(title="Histogram for Wind Speed in North West") +
        labs(x="Wind Speed (mph)", y="Count")
x



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUESTION4: FACTORS THAT DETERMINE AGRICULTURE SITES
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS4.1: Pattern of humidity throughout the year
#VARIABLES USED: H9, H3
#CONCLUSION: Humidity is lower at 3pm comparing to 9am
#CONCLUSION: Crops should avoid areas where humidity is high

#selecting specific columns for Question4
Q4 <- data.frame(subset (weather, select = c("H3", "H9", "T3", "T9", "RF")))
View(Q4)

#generating smooth graph
day = 1:366
YearH9 = cbind(H9,day,"9am")
YearH3 = cbind(H3,day,"3pm")
YearH = as.data.frame(rbind(YearH9,YearH3))
colnames(YearH) = c("Humidity","Day","Time")
YearH

Q4_smooth = ggplot(data=YearH, aes(x=as.numeric(Day), y=as.numeric(Humidity), group=Time)) +
              geom_smooth(aes(color=Time))+
              scale_x_continuous("Number of Days",breaks = seq(0,366,by = 100)) +
              scale_y_continuous("Humidity",breaks = seq(0,100,by = 10)) +
              labs(title="Humidity Pattern Throughtout One Year")
Q4_smooth



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS4.2: Correlation between average temperature and humidity
#VARIABLES USED: H9, H3, T3, T9
#CONCLUSION: There is correlation between temperature and humidity (higher temp = lower humidity)
#CONCLUSION: Crops should avoid areas where humidity is high

#calculating average humidity & temperature + add to Question4 data frame
Avg_H <- ((H9 + H3)/2)
View(Avg_H)

Avg_T <- ((T9 + T3)/2)
View(Avg_T)

Q4 = mutate(Q4, Avg_H=Avg_H)
Q4 = mutate(Q4, Avg_T=Avg_T)

#generating scatterplot for avg humidity and temp.
Q4_scatterplot <- ggplot(Q4, aes(x = Avg_T, y = Avg_H)) +
                geom_point(color="cornflowerblue", 
                          size = 2, 
                          alpha=.5) +
                geom_smooth(method = "lm") +
                scale_y_continuous(breaks = seq(0, 100, by = 10)) +
                scale_x_continuous(breaks = seq(0, 30, by = 5)) +
                labs(x = "Average Temperature", y = "Average Humidity",
                    title = "Relation Between Average Temperature and Average Humidity")
Q4_scatterplot



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS4.3: Rainfall in a year (Does rainfall affect crops production?)
#VARIABLES USED: RF
#CONCLUSION: Not much rainfall throughout the year (only 2 months worth of rainfall)
#CONCLUSION: Droughts(?) Possible famine/ poor crop production

#generating histogram for rainfall distribution
Q4_hist <- ggplot(data=Q4, aes(RF)) + 
  geom_histogram(col="white", fill="steelblue", binwidth = 2) +
  scale_x_continuous(breaks = seq(0, 40, by = 5)) + 
  labs(title="Distribution of Rainfall",
    subtitle="on an annual basis") +
  labs(x="Rainfall (in mm)", y="Count")
Q4_hist



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS4.3: Distribution of rainfall on rainy days
#VARIABLES USED: RF
#CONCLUSION: Dry weather, not much rain even on rainy days

#selecting rainfall with more than 1 mm
filter_RF <- filter(Q3, RF > 1)
filter_RF <- filter_RF[, 1]
filter_RF

#put selected values into new data frame
SubQ4 <- rbind(data.frame(Rain = "TRUE", RainFall = filter_RF))
View(SubQ4)

#generating histogram for rainfall distribution (that is not 0 and more than 1)
Q4_hist2 <- ggplot(data=SubQ4, aes(filter_RF)) + 
  geom_histogram(col="white", fill="steelblue", binwidth = 2) +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) + 
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  labs(title="Distribution of Rainfall", 
       subtitle="On rainy days") +
  labs(x="Rainfall (in mm)", y="Count")
Q4_hist2



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS4.4: Distribution of Evaporation
#VARIABLES USED: E
#CONCLUSION: High evaporation rate since day 1, decreases around day 150 and increase again around day 250
#CONCLUSION: Average daily evaporation rate is 4.521858mm (high) 
#CONCLUSION: Lost of soil water and will affect plant grows

#generating line graph
day = 1:366
YearEvap = as.data.frame(cbind(E,day))
YearEvap

Q4_line = ggplot(YearEvap,aes(x=day,y=E))+
            geom_line()+
            geom_hline(aes(yintercept=mean(E)), color="green", linetype="dashed", size=1) +
            labs(title="Distribution of Evaporation") +
            labs(x="Day", y="Evaporation (mm)")
Q4_line
mean(E)



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUESTION5: WEATHER PREDICTION TO DETERMINE ACTIVITIES
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS5.1: Frequency of days with change in weather (Probability of rainy or sunny days?)
#VARIABLES USED: RToday, RTmr
#CONCLUSION: Mostly sunny throughout the year as indicated by No_No relation
#CONCLUSION: Same frequency for having rainy days in 1 out of 2 days as indicated by Yes_No and No_Yes

#selecting specific columns for Question5
Q5 <- data.frame(subset (weather, select = c("RiskMM", "RToday", "RTmr", "C9", "C3")))
View(Q5)

#selecting values to reflect change in weather
Rain_Today = Q5$RToday
Rain_Tmr = Q5$RTmr
RainRelations = paste(Rain_Today,Rain_Tmr,sep="_")
table(RainRelations)

#generating bar chart to frequency of days with change in weather
barplot(table(RainRelations), col = "pink", ylim = c(0, 300),
        main = "Count of Days with Change in Weather",
        xlab = "Categories of Days with Change in Weather",
        ylab = "Count")



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS5.2: Distribution of cloud cover in oktas at specific time intervals (9am & 3pm)
#VARIABLES USED: C9, C3
#CONCLUSION: Sky is often clear, since cloud okta of 1 has highest frequency in both time intervals
#CONCLUSION: Sky is cloudier at 3pm, seems to have increase in okta values (higher chance of raining?)

#generating histogram for cloud distribution at 9am
Q5_hist <- ggplot(data=Q5, aes(C9)) + 
  geom_histogram(col="white", fill="#D16103", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Distribution of Cloud Cover in Oktas at 9am") +
  labs(x="Cloud (in Oktas)", y="Count")
Q5_hist

#generating histogram for cloud distribution at 3pm
Q5_hist2 <- ggplot(data=Q5, aes(C3)) + 
  geom_histogram(col="white", fill="#D16103", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Distribution of Cloud Cover in Oktas at 3pm") +
  labs(x="Cloud (in Oktas)", y="Count")
Q5_hist2



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS5.3: Proportion of evaporation under different sunshine intensity
#VARIABLES USED: E, SS
#CONCLUSION: Higher sunshine intensity = higher evaporation
#CONCLUSION: Low sunshine intensity but high evaporation = high precipitaion
evap_gap <- c(0,2,4,6,8,10,12,14)
evap_label <- c("0~2","2~4","4~6","6~8","8~10","10~12","12~14")
evap_group <- cut(as.numeric(E), breaks = evap_gap, include.lowest = TRUE, right = TRUE, labels = evap_label)
evap_group
combine1 = cbind(E,as.data.frame(evap_group))

SS_gap <- c(0,2,4,6,8,10,12,14)
SS_label <- c("0~2","2~4","4~6","6~8","8~10","10~12","12~14")
SS_group <- cut(as.numeric(Sunshine_Fill), breaks = SS_gap, include.lowest = TRUE, right = TRUE, labels = SS_label)
SS_group
combine2 = cbind(combine1,as.data.frame(SS_group))
combine2

Q5_hist3 = ggplot(combine2, aes(x=SS_group, color=evap_group,fill=evap_group)) +
  geom_histogram(, position="identity",stat="count")+
  labs(x = "Sunshine intensity ", y = "Frequency",
       title = "Proportion of Evporation Groups Under Different Sunshine Intensity Category")
Q5_hist3



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS5.3: Distribution of annual risk MM 
#VARIABLES USED: RiskMM
#CONCLUSION: Risk in amount of rain is very low (almost close to none, as indicated by the mean line)
#CONCLUSION: Flooding probably doesn't ovvur very often

#generating histogram for annual risk mm distribution
Q5_hist4 <- ggplot(data=Q5, aes(RiskMM)) + 
  geom_histogram(col="white", fill="#52854C", binwidth = 2) +
  geom_vline(aes(xintercept=mean(RiskMM)),
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 40, by = 10)) + 
  labs(title="Distribution of Annual Risk in Amount of Rain") +
  labs(x="Risk in Amount of Rain (in mm)", y="Count")
Q5_hist4



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS5.3: Distribution of risk MM 
#VARIABLES USED: RiskMM
#CONCLUSION: Risk in amount of rain is still relatively low on rainy days
#CONCLUSION: Maybe not much of a shower, just a slight drizzle(?)

#selecting risk mm with more than 1 mm
filter_Risk <- filter(Q5, RiskMM > 1)
filter_Risk <- filter_Risk[, 1]
filter_Risk

#put selected values into new data frame
SubQ5 <- rbind(data.frame(Risk = "TRUE", RainFall = filter_Risk))
View(SubQ5)

#generating histogram for risk mm distribution
Q5_hist5 <- ggplot(data=SubQ5, aes(filter_Risk)) + 
  geom_histogram(col="white", fill="#52854C", binwidth = 2) +
  geom_vline(aes(xintercept=mean(filter_Risk)),
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks = seq(0, 40, by = 10)) + 
  scale_y_continuous(breaks = seq(0, 20, by = 5)) + 
  labs(title="Distribution of Risk in Amount of Rain",
       subtitle="on rainy days") +
  labs(x="Risk in Amount of Rain (in mm)", y="Count")
Q5_hist5



