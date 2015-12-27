# Reproducible Research: Peer Assessment 2
27 December 2015  

## 1. Synopsis
During this analysis, the goal was to detect, if any, trends between weather 
events and public health (fatalities or injuries) moreover weather events and 
ecomonic cost (property or crop damage). The data came from the U.S. National 
Oceanic and Atmospheric Administration's (NOAA) database. In this analysis, 
it was found that there are statistically siginicant differences between total 
fatalities+injuries among different weather event according to the ANOVA and 
Tukeys HSD Comparison. Futhermore, the weather event which result in the highest
public danger were: Tornadoes, Floods, Lightning, and Heat (top 4 events).
It was also found economic cost were mostly due by different weather types. For
Property Damage-- Tornadoes and Hail resulted in the highest cost and for Crop
Damage-- Drought and Flood resulted in the highest cost.


## 2. Data Processing
*Variables Incorporated (during analysis):* <br> 
**BGN_DATE:** Date, and Time Event occured <br>
**TIME_ZONE:** Standard time Zone i.e, Eastern Standard Time (EST),
Central Standard Time (CST), Mountain Standard Time (MST), <br>
Time Zone correspond to the event's (weathers) location <br> 
**STATE:** United States Territory (72 Locations: 50 States + 22 Other Locations) <br> 
**EVTYPE:** Specific Weather Event (985 Different Weather Listed) <br> 
**FATALITIES:** Direct/indirect causes of weather-related fatalities/injuries <br> 
**INJURIES:** Direct/indirect causes of weather-related fatalities/injuries <br>
**PROPDMG:** Property Damage Cost <br>
**PROPDMGEXP:** Estimates Power cost, “K”/"k" for thousands, “M”/"m" for millions, <br> 
“B”/"b" for billions, "h" for hundred and "0" for zero (p. 12) <br>
**CROPDMG:** Crop Damage <br>
**CROPDMGEXP:** Crop Damage Expenses <br>

*Data:* (Link: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) <br> 
The raw data was read into R using the basic 'read.csv()' function into a descriptive 
object called "weather_data". The National Weather Service NOAA,
contains specific weather data going back to 1995, so this data set was cut 
off to 1995 and later, so missing values could be filled in by the NOAA data 
(http://www.nws.noaa.gov/om/hazstats.shtml)
Also it was noted in course information that more recent data have better 
records. The storm data was processed by subseting the variables listed above. 

The weather data was processed again by subsetting the the type of weather event including:  <br>
TORNADO, HAIL, RIP CURRENTS, LIGHTNING, <br>
HEAVY RAIN, FLOOD, HEAT, COLD, WIND, <br>
WINTER STORM, FOG, HURRICANE, <br>
WILDFIRE, DROUGHT, <br>

*Then data analysis included:* <br>
1\. Factorial ANOVA, to see statistical pairwise comparisons between different
weather groups. It was valid to use Factorial ANOVA becuase: <br>
"Factorial ANOVA’s are used in research when one wants to analyze differences on
a continuous dependant variable between two or more independent discrete
grouping variables."" In this analysis, Weather Event (EVTYPE) will be compared
by both Location (STATE) and COST. 
(https://www.statisticssolutions.com/factorial-anova/)

2\. Graphing, for fatalities/injuries and damage 
<br>
<br>


```r
# Step 1: Load Packages Needed for Analysis
library(dplyr) # For Data Manipulation
library(ggplot2) # For Graphing
library(lattice) # For Graphing
library(stats) # For Statistical Analysis
library(utils) # For Graphing and Data Manipulation
library(gridExtra) # For Graphings
```


```r
# Step 1: Set WD to cloned repository (located on the Computer)
setwd("~/Desktop/StormData_PeerAssessment2")

# Step 2: Unzip file and Read the data into descriptive object
weather_data <- read.csv(bzfile("repdata_data_StormData.csv.bz2")) # Data to Manipulate
storm_data <- weather_data # Raw Data

# Step 3: Look and Clean the Data,
str(weather_data, strict.width = "cut")
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 42..
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 ..
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 ..
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PR"..
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2..
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 8..
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1..
##  $ BGN_LOCATI: Factor w/ 54429 levels ""," Christiansburg",..: 1 1 1 1 1 ..
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 ..
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 ..
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1..
##  $ END_LOCATI: Factor w/ 34506 levels ""," CANTON"," TULIA",..: 1 1 1 1 1..
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 1..
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ..
##  $ WFO       : Factor w/ 542 levels ""," CI","%SD",..: 1 1 1 1 1 1 1 1 1 ..
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1..
##  $ ZONENAMES : Factor w/ 25112 levels "","                              "..
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1..
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

```r
# Step 4: Use dplyr package for easier manipulation of data frame
weather_data <- tbl_df(weather_data) # Create dplyr data frame

# Step 5: Subset Data, where, EVTYPE factor levels are those associated with 
# specifc weather events
weather_sub <- weather_data
weather_sub$EVTYPE <- as.character(weather_sub$EVTYPE) # Convert To Character
weather_sub <- weather_sub[weather_data$EVTYPE=="TORNADO" 
                            | weather_data$EVTYPE=="LIGHTNING"
                            | weather_data$EVTYPE=="HAIL" 
                            | weather_data$EVTYPE=="RIP CURRENTS"
                            | weather_data$EVTYPE=="LIGHTNING" 
                            | weather_data$EVTYPE=="HEAVY RAIN" 
                            | weather_data$EVTYPE=="FLOOD" 
                            | weather_data$EVTYPE=="HEAT"
                            | weather_data$EVTYPE=="COLD" 
                            | weather_data$EVTYPE=="WIND" 
                            | weather_data$EVTYPE=="WINTER STORM" 
                            | weather_data$EVTYPE=="FOG"
                            | weather_data$EVTYPE=="HURRICANE" 
                            | weather_data$EVTYPE=="WILDFIRE" 
                            | weather_data$EVTYPE=="DROUGHT", ]

weather_sub$EVTYPE <- as.factor(weather_sub$EVTYPE) # Convert back to Factor

# Step 6: Subset Data, where date 1995 or later
str <- c(1950:1994) # Set Object equal to unwanted dates
toMatch <- paste(str, collapse="|") # Paste in OR operator for grepl() subset
weather <- filter(weather_sub, !grepl(toMatch ,BGN_DATE)) # Subset the Date

# Step 7: Subset Columns of Interest
weather <- subset(weather, select= c("BGN_DATE", "TIME_ZONE", "STATE", "EVTYPE", 
                                      "FATALITIES", "INJURIES", "PROPDMG",
                                     "PROPDMGEXP","CROPDMG", "CROPDMGEXP" ))

# Step 8: Rename the cost values in data frame for PROPDMGEXP and CROPDMGEXP
levels(weather$PROPDMGEXP) # Look at all cost name indicators for PROPDMGEXP
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

```r
levels(weather$CROPDMGEXP) # Look at all cost name indicators for CROPDMGEXP
```

```
## [1] ""  "?" "0" "2" "B" "k" "K" "m" "M"
```

```r
Transform_COST <- function(c) {
  
    if (c %in% c('h', 'H')) # Return numeric value 2
        return(2)
    else if (c %in% c('k', 'K')) # Return numeric value 3
        return(3)
    else if (c %in% c('m', 'M')) # Return numeric value 6
        return(6)
    else if (c %in% c('b', 'B')) # Return numeric value 9
        return(9)
    else if (!is.na(as.numeric(c))) # Return a numeric value if already a numeric value
        return(as.numeric(c))
    else if (c %in% c('', '-', '?', '+')) # Return numeric value 0
        return(0)
  }

# Property Damage
Property_Damage_Expected <- sapply(weather$PROPDMGEXP, FUN=Transform_COST) 
# Use sapply to apply function(c) to data

weather$PROPERTY_DAMAGE_COST <- weather$PROPDMG * (10 ** Property_Damage_Expected)
# Add new Column called PROPERTY_DAMAGE_COST with Computed Cost

# Crop Damage
Crop_Damage_Expected <- sapply(weather$CROPDMGEXP, FUN=Transform_COST) 
# Use sapply to apply function(c) to data

weather$CROP_DAMAGE_COST <- weather$CROPDMG * (10 ** Crop_Damage_Expected)
# Add new Column called PROPERTY_DAMAGE_COST with Computed Cost

weather[1:6, 3:12] # View Edited Data
```

```
## Source: local data frame [6 x 10]
## 
##   STATE EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1    AL   HAIL          0        0       0                  0           
## 2    AL   HAIL          0        0       0                  0           
## 3    AL   HAIL          0        0       0                  0           
## 4    AL   HAIL          0        0       0                  0           
## 5    AL   HAIL          0        0       0                  0           
## 6    AL   HAIL          0        0       0                  0           
## Variables not shown: PROPERTY_DAMAGE_COST (dbl), CROP_DAMAGE_COST (dbl)
```


## 3. Results

## Question 1: 
#### Across the United States, which types of events (as indicated in the EVTYPE variable)
#### are most harmful with respect to population health? <br>


```r
# Step 1: Let's Conduct an ANOVA to see if there exist a relationship between
# Specific Weather Event (EVTYPE variable) and FATALITIES+INJURIES

Sum_by_Weather <- with(weather, tapply(FATALITIES+INJURIES , list(EVTYPE), sum, na.rm=TRUE))
print(Sum_by_Weather) # Number of FATALITIES and INJURIES by Weather Type
```

```
##         COLD      DROUGHT        FLOOD          FOG         HAIL 
##           70            4         7192          779          926 
##         HEAT   HEAVY RAIN    HURRICANE    LIGHTNING RIP CURRENTS 
##         2954          332          107         5360          501 
##      TORNADO     WILDFIRE         WIND WINTER STORM 
##        23310          986          104         1493
```

```r
weather_ANOVA <- aov(FATALITIES+INJURIES ~ EVTYPE , data = weather) # ANOVA
Weather_Comparisons <- TukeyHSD(weather_ANOVA,"EVTYPE", ordered=TRUE) # Tukeys Comparision
Weather_Comparisons_df <- data.frame(Weather_Comparisons$EVTYPE) # Tukeys Comparision as dataframe
Siginficant_diff<- Weather_Comparisons_df[Weather_Comparisons_df$p.adj<= 0.05,] # Subset P-values
# Above Subset Valid Differences, a siginificant difference in mean FATALITIES+INJURIES values occurs
# when p-value <= alpha and our alpha is 0.05 because the CI is 95%, an alpha of
# 0.05 is our error, the chance of not seeing a siginigicant difference
print(Siginficant_diff) # All significant differences
```

```
##                                diff        lwr       upr        p.adj
## TORNADO-DROUGHT           0.9562608 0.58981960 1.3227020 1.099121e-13
## FOG-DROUGHT               1.4544560 0.62685390 2.2820581 3.415791e-07
## RIP CURRENTS-DROUGHT      1.6464075 0.59153816 2.7012769 1.476931e-05
## HEAT-DROUGHT              3.9109640 3.18925406 4.6326739 0.000000e+00
## FLOOD-HAIL                0.2875829 0.17088147 0.4042843 1.679767e-13
## WILDFIRE-HAIL             0.3533467 0.02070483 0.6859886 2.517552e-02
## LIGHTNING-HAIL            0.3710618 0.22109958 0.5210239 1.609823e-13
## TORNADO-HAIL              0.9535912 0.83623306 1.0709494 0.000000e+00
## FOG-HAIL                  1.4517864 0.70050814 2.2030646 8.300233e-09
## RIP CURRENTS-HAIL         1.6437379 0.64762389 2.6398520 2.815687e-06
## HEAT-HAIL                 3.9082944 3.27555510 4.5410337 0.000000e+00
## FLOOD-HEAVY RAIN          0.2633023 0.06799642 0.4586082 5.216541e-04
## LIGHTNING-HEAVY RAIN      0.3467812 0.12995447 0.5636079 7.304459e-06
## TORNADO-HEAVY RAIN        0.9293106 0.73361162 1.1250096 0.000000e+00
## FOG-HEAVY RAIN            1.4275058 0.66007882 2.1949328 4.018794e-08
## RIP CURRENTS-HEAVY RAIN   1.6194573 0.61110803 2.6278067 6.452342e-06
## HEAT-HEAVY RAIN           3.8840138 3.23218241 4.5358452 0.000000e+00
## LIGHTNING-WINTER STORM    0.2440628 0.02593124 0.4621943 1.279948e-02
## TORNADO-WINTER STORM      0.8265922 0.62944851 1.0237359 0.000000e+00
## FOG-WINTER STORM          1.3247874 0.55699074 2.0925840 6.495086e-07
## RIP CURRENTS-WINTER STORM 1.5167389 0.50810825 2.5253696 4.049965e-05
## HEAT-WINTER STORM         3.7812954 3.12902881 4.4335620 0.000000e+00
## TORNADO-FLOOD             0.6660083 0.50915737 0.8228593 0.000000e+00
## FOG-FLOOD                 1.1642035 0.40575226 1.9226547 2.342211e-05
## RIP CURRENTS-FLOOD        1.3561550 0.35461999 2.3576901 4.758807e-04
## HEAT-FLOOD                3.6207115 2.97947185 4.2619512 0.000000e+00
## HEAT-WIND                 3.5964734 2.44994825 4.7429985 1.373346e-13
## TORNADO-WILDFIRE          0.6002445 0.25148436 0.9490046 7.083140e-07
## FOG-WILDFIRE              1.0984397 0.27851302 1.9183663 5.944366e-04
## RIP CURRENTS-WILDFIRE     1.2903912 0.24153284 2.3392496 2.953956e-03
## HEAT-WILDFIRE             3.5549477 2.84205237 4.2678430 0.000000e+00
## TORNADO-LIGHTNING         0.5825295 0.39957685 0.7654821 1.384448e-13
## FOG-LIGHTNING             1.0807246 0.31644869 1.8450006 1.832184e-04
## RIP CURRENTS-LIGHTNING    1.2726762 0.26672295 2.2786294 1.802590e-03
## HEAT-LIGHTNING            3.5372326 2.88911403 4.1853513 0.000000e+00
## HEAT-HURRICANE            3.2831710 1.80979606 4.7565460 7.221335e-12
## HEAT-TORNADO              2.9547032 2.31334368 3.5960627 0.000000e+00
## HEAT-FOG                  2.4565080 1.47569811 3.4373179 1.447731e-13
## HEAT-RIP CURRENTS         2.2645565 1.08565348 3.4434595 1.071332e-08
```

#### Above we were able to see with the ANOVA Tukeys HSD comparisions which weather events 
#### differ in mean total FATALITIES+INJURIES, we can see from the output above there exist
#### statistically siginifcant differences in mean total FATALITIES+INJURIES between many
#### different types of weather events i.e.,TORNADO-DROUGHT, FOG-DROUGHT...


```r
# Step 2: Create Panel Plot Graph showing Weather Event Vs. Amount of FATALITIES or INJURIES
weather$Total_Hurt <- weather$FATALITIES + weather$INJURIES # Sum FATALITIES and INJURIES

health_plot <- ggplot(weather, aes(EVTYPE,Total_Hurt, color=EVTYPE )) + geom_bar(stat="identity")
health_plot <- health_plot+ ggtitle("Weather Events vs. Total Health Fatalities/Injuries ")
health_plot <- health_plot + theme(axis.text.x= element_text(angle=90, size=10))
health_plot <- health_plot + scale_y_continuous(breaks = seq(0, 25000, by = 1500))
health_plot <- health_plot + labs(x = "Weather Event Type", y = "Number of Fatalities+Injuries")
print(health_plot) # Print Graph
```

![](StormData_PeerReview_files/figure-html/Plot_1-1.png) 

#### The above graph shows the most harmful weather events with respect to population health, 
#### the top 4 events are: Tornadoes, Floods, Lightning, and Heat
 <br>
 <br>
 
## Question 2: 
#### Across the United States, which types of events have the greatest economic consequences? <br>


```r
# Step 1: Plot Graph sowing Weather Event vs. Property Damage Cost
number_ticks <- function(n) {function(limits) pretty(limits, n)} # Set Amount of Y-axis Ticks
damage_cost <- ggplot(weather, aes(x=EVTYPE, y=PROPERTY_DAMAGE_COST, color=EVTYPE))
damage_cost <- damage_cost + geom_bar(stat="identity")+ scale_y_continuous(breaks=number_ticks(10))
damage_cost <- damage_cost+ ggtitle("Weather Events vs. Property Damage Cost")
damage_cost <- damage_cost+ labs(x = "Weather Event Type", y = "Property Damage Cost ($)")
damage_cost <- damage_cost+ theme(axis.text.x= element_text(angle=90, size=10))
print(damage_cost) # Print Graph
```

![](StormData_PeerReview_files/figure-html/question_2-1.png) 

```r
max_cost <- max(weather$PROPERTY_DAMAGE_COST) # Find Highest Cost
weather_max_cost <- weather[weather$PROPERTY_DAMAGE_COST==max_cost,"EVTYPE"] # Find Weather Event
print(weather_max_cost) # Event Associated with most Property Damage Cost
```

```
## Source: local data frame [1 x 1]
## 
##    EVTYPE
## 1 TORNADO
```

```r
# Step 2: Plot Graph sowing Weather Event vs. Crop Damage Cost
number_ticks <- function(n) {function(limits) pretty(limits, n)} # Set Amount of Y-axis Ticks
damage_crop_cost <- ggplot(weather, aes(x=EVTYPE, y=CROP_DAMAGE_COST, color=EVTYPE))
damage_crop_cost <- damage_crop_cost + geom_bar(stat="identity") 
damage_crop_cost <- damage_crop_cost+ scale_y_continuous(breaks=number_ticks(10))
damage_crop_cost <- damage_crop_cost + scale_y_continuous(breaks=number_ticks(10))
```

```
## Scale for 'y' is already present. Adding another scale for 'y', which will replace the existing scale.
```

```r
damage_crop_cost <- damage_crop_cost+ ggtitle("Weather Events vs. Crop Damage Cost")
damage_crop_cost <- damage_crop_cost+ labs(x = "Weather Event Type", y = "Crop Damage Cost ($)")
damage_crop_cost <- damage_crop_cost+ theme(axis.text.x= element_text(angle=90, size=10))
print(damage_crop_cost) # Print Graph
```

![](StormData_PeerReview_files/figure-html/question_2-2.png) 

```r
max_crop_cost <- max(weather$CROP_DAMAGE_COST) # Find Highest Cost
weather_max__crop_cost <- weather[weather$CROP_DAMAGE_COST==max_crop_cost,"EVTYPE"] 
# Find Weather Event
print(weather_max__crop_cost) # Event Associated with most Crop Damage Cost
```

```
## Source: local data frame [1 x 1]
## 
##    EVTYPE
## 1 DROUGHT
```

```r
# Step 3: See Cost Break Down

# Property
top_3_prop <- weather[order(weather$PROPERTY_DAMAGE_COST, decreasing=T)[1:3],]
top_3_prop <- subset(top_3_prop, select= c("EVTYPE", "PROPERTY_DAMAGE_COST"))
print(top_3_prop)
```

```
## Source: local data frame [3 x 2]
## 
##    EVTYPE PROPERTY_DAMAGE_COST
## 1 TORNADO              8.8e+11
## 2    HAIL              3.0e+11
## 3 TORNADO              1.4e+11
```

```r
# Crop
top_3_crop <- weather[order(weather$CROP_DAMAGE_COST, decreasing=T)[1:3],]
top_3_crop <- subset(top_3_crop, select= c("EVTYPE", "CROP_DAMAGE_COST"))
print(top_3_crop)
```

```
## Source: local data frame [3 x 2]
## 
##    EVTYPE CROP_DAMAGE_COST
## 1 DROUGHT       1000000000
## 2 DROUGHT        578850000
## 3 DROUGHT        515000000
```

#### We can see, from the above graphs, the greatest economic consequence for Property Damage: <br>
#### 1. Tornadoes 2. Hail and 3. Flood Cost <br>
#### for Crop Damage: <br>
#### 1. Drought 2. Flood and 3. Hail cost the most. <br>

<br>
