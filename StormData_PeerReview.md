# Reproducible Research: Peer Assessment 2
27 December 2015  

## 1. Synopsis

## 2. Data Processing

## 3. Results


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lattice)
library()
```


```r
# Step 1: Set WD to cloned repository (located on the Computer)
setwd("~/Desktop/StormData_PeerAssessment2")

weather_data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))

#https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
```

