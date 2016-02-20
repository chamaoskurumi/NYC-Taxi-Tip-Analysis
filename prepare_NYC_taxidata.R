#******************************************************
#
# Prepare NYC Yellow Tripdata 2015-06 for Tip Analysis
#
# GUIDO SCHULZ
#
#******************************************************

# ==== 0. Setup  ====

# Load required packages
library("data.table")
library("dplyr")
library("ggplot2")
library("mapview")
library("rgdal")
library("sp")

# Set working directory
MYDIR <- file.path("/home/dao/ownCloud/MeinFernbus_Hausaufgabe/DATA")
setwd(MYDIR)

# ==== 1. Download & Read Data ====

# NYC Yellow Tripdata 2015-06 
download.file("https://storage.googleapis.com/tlc-trip-data/2015/yellow_tripdata_2015-06.csv",
              "yellow_tripdata_2015-06.csv")
data <- tbl_df(fread("yellow_tripdata_2015-06.csv")) # fread is much faster here than read.csv!

# Suitable Shapefile for aggregation & spatial analysis:
# NY Community Districts (n=71)
download.file("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_15d.zip",
              "nycd_15d.zip", mode="wb")
unzip("nycd_15d.zip")
NYCD <- readOGR("nycd_15d","nycd") 
#plot(NYCD)
#View(NYCD@data)


# ==== 2. Prepare data ====

### data ###
#%%%%%%%%%%%

# First we take a look at the data
glimpse(data)

# We take a look at the data dictionary to see the doc of the variables:
# http://www.nyc.gov/html/tlc/downloads/pdf/data_dictionary_trip_records_yellow.pdf

# Check if it's true, that tips were ONLY recorded for Payment-Type=="Credit Card" (1)
data %>% 
  select(tip_amount, payment_type) %>% 
  tapply(tip_amount, payment_type, function(x){quantile(x, probs=seq(0, 1, 0.05))})
# --> thats correct.

# Filtering, subsetting and renaming of data:
data <- data %>% 
  filter(payment_type==1) %>% # Tips were ONLY recorded for Payment-Type=="Credit Card" (1)
  filter(extra==0 | extra==0.5 | extra==1) %>% # extra charge can only be 0, 0.5 or 1
  filter(improvement_surcharge==0.3) %>% # imp. surcharge is mandatory and CAN only be 0.3. Other values MUST be invalid. 
  select(-payment_type, # only one payment_type left --> drop
         -store_and_fwd_flag,  # not interesting --> drop
         -mta_tax, # ALL passengers pay mta_tax --> drop
         -dropoff_longitude, # Question is about travelling FROM not TO. dropoff location --> drop
         -dropoff_latitude
         -improvement_surcharge) %>% # only one level left --> drop
  rename(long = pickup_longitude, # renaming variables
         lat  = pickup_latitude,
         passengers = passenger_count,
         distance = trip_distance,
         pickup_time  = tpep_pickup_datetime, 
         dropoff_time = tpep_dropoff_datetime,
         fare = fare_amount,
         tip = tip_amount,
         tolls = tolls_amount,
         total = total_amount)

# We convert the date-time variables into POSIX format
Sys.setlocale("LC_TIME", "C") # set english sys 
data$pickup_time  <- as.POSIXct(data$pickup_time, tz = "EST")
data$dropoff_time <- as.POSIXct(data$dropoff_time, tz = "EST")


# We convert the variables RateCodeID & VendorID into factors
data$RateCodeID[data$RateCodeID==99] <- NA # 99 is supposed to be NA
data$RateCodeID <- factor(data$RateCodeID, labels=c("Standard rate",
                                                    "JFK",
                                                    "Newark",
                                                    "Nassau or Westchester",
                                                    "Negotiated fare",
                                                    "Group ride"))

data$VendorID   <- factor(data$VendorID, labels=c("Creative Mobile Technologies, LCC",
                                                  "VeriFone Inc."))

# Generate additional variables that help us to carry out our tip-analysis:
data <- data %>% mutate(weekday  = factor(weekdays(data$pickup_time)),
                        duration = as.numeric(difftime(dropoff_time, pickup_time, units = c("mins"))), # trip duration in decimal minutes
                        ratio_tip_total    = (tip / total)*100,    # A) possible measure for tips
                        ratio_tip_distance = tip / distance,       # B) possible measure for tips
                        ratio_tip_duration = tip / duration)       # C) possible measure for tips


### NYCD ###
#%%%%%%%%%%%

# Community Districts
NYCD@data <- NYCD@data %>% select(id = BoroCD)
NYCD <- spTransform(NYCD, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Add borough names to Community district shape file
boroughs <- c(rep("Manhatten", 13),
              rep("Bronx", 15),
              rep("Brooklyn", 20),
              rep("Queens", 19),
              rep("Staten Island", 4))
id_boroughs <- data.frame(id = sort(NYCD@data$id),
                          boroughs = boroughs)
NYCD@data <- left_join(NYCD@data, id_boroughs)

# Save data as RData file for lazy loading
save(data, NYCD, file = "data.RData")
#load("data.RData")


# ==== 3. Create Georeferenced data ====

data <- data.frame(data) # für spatial operations, data needs to be a "usual" data.frame

# Create Spatial Points File with long/lat coords:
data_pt <- SpatialPointsDataFrame(data.frame(data$long, data$lat), 
                                  subset(data, select=-c(long,lat)), 
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
rm(data) # let's empty up some space in the memory (-:

data_NYCD_pt <- SpatialPointsDataFrame(data_pt@coords,
                                       data = data.frame(data_pt@data,
                                                         over(data_pt, NYCD)),
                                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
rm(data_pt) # let's empty up some space in the memory (-:

save(data_NYCD_pt, file="data_NYCD_pt.RData")


# ==== 4. Aggregate georeferenced data on NYCD level ====

# We restart the R session to empty the memory:
.rs.restartR() 
# Dirs & Packages need to be loaded again, because of the restart:
MYDIR <- file.path("/home/dao/ownCloud/MeinFernbus_Hausaufgabe/DATA"); setwd(MYDIR)
library("data.table");library("dplyr");library("ggplot2")
library("mapview");library("rgdal");library("sp")

# Load data:
load("data_NYCD_pt.RData")

# Aggregate data and create meaningful aggregate variables:
data_NYCD_ply <- data_NYCD_pt@data %>% group_by(id) %>% 
  summarise(count    = n(),
            tip_med  = median(tip, na.rm=TRUE),
            tip_q0.1 = quantile(tip, probs=0.1),
            tip_q0.9 = quantile(tip, probs=0.9),
            tip_sum  = sum(tip),
            ratio_tip_total_med = median(round(ratio_tip_total, digits=0), na.rm=TRUE),
            ratio_tip_distance_med = median(round(ratio_tip_distance, digits=2), na.rm=TRUE),
            ratio_tip_duration_med = median(round(ratio_tip_duration, digits=2), na.rm=TRUE),
            distance_med = median(distance, na.rm=TRUE),
            duration_med = median(duration, na.rm=TRUE),
            total_med    = median(total, na.rm=TRUE))

# Associate NYCD Polygons and our data with NYCD shape file
NYCD@data <- left_join(NYCD@data, data_NYCD_ply, by="id")

# We delete all polygons for which no taxi rides were counted
glimpse(NYCD) 
NYCD[is.na(NYCD@data$count),]

# ==== 5. Save final dataset ====

# Save final data, which is ready for the analysis:
NYtaxi <- NYCD 
save(NYtaxi, file = "NYtaxi.Rdata")

