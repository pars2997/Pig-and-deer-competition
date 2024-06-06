####### Deer body condition and fawn to do ratios ##############################
### Modeling these as a function of environemnt and pig density ################
#################### Mitch Parsons #############################################
################# 10/24/2023####################################################

#### load packages ####
library(tidyverse)
library(sf)
library(terra)
library(lme4)
library(Matrix)

#### read in data ####
# cams is photo data with deer sex and body condition info
# kills is killsite datea
# stations is camera locations for extracting covariates
cams <- read.csv("../RawData/BodyCond_DoeFawn3.csv")
kills <- read.csv("../RawData/cougar_cluster_investigations.csv")
stations <- read.csv("../RawData/GridProblems_correctedseasons.csv")

# format dates
cams$dt <- paste(cams$Date, cams$Time)
cams$dt <- as.POSIXct(cams$dt, format = "%d-%b-%y %H:%M:%S", tz = "America/Los_Angeles")
cams$Date <- dmy(cams$Date)
cams$Time <- hms(cams$Time)


# filter camera stations to only needed variables and one instance per camera
stations <- stations %>% 
  select(c(Station.ID,CSY,UTM_X,UTM_Y)) %>% 
  filter(!duplicated(Station.ID))

# combine stationdata with camera detections
colnames(stations) <- c("Camera","CSY","UTM_X","UTM_Y")
cams <- cams %>% 
  left_join(stations)

#### Explore body condition data ####
hist(cams$Animal_1_Condition)
boxplot(cams$Animal_1_Condition ~ cams$Season)
boxplot(cams$Animal_1_Condition ~ cams$Year)
boxplot(cams$Animal_1_Condition ~ cams$Camera)

#### Create visits from camera data ####
# order by camera and date/time to find continuous visits
cams <- cams[with(cams, order(Camera, dt)),]

#Add covariates for whether photos are a new independent visit
# define independent as >2 minutes apart
ind_time = 120
ind = vector()
time_dif = vector()
time_dif[1] <- NA
ind[1] <- 1
visit_id = vector()
visit_id[1] <- 1

# loop through, if photo is more than "ind_time" from photos, it starts a new visit
for(i in 2:nrow(cams)){
  time_dif[i] <- abs(difftime(cams$dt[i],cams$dt[i-1],units = "secs"))
  ind[i] <- abs(difftime(cams$dt[i],cams$dt[i-1],units = "secs")) > ind_time
  visit_id[i] <- sum(ind[1:i])
}

# visit_id increases by one each time a new visit starts
cams$time_dif <- time_dif
cams$ind <- ind
cams$visit_id <- visit_id

head(cams)

#### Add sex data ####

#Add deer sex to each photo based on count information
#For photos with multiple sexes, mark sex as mixed
sex <- vector()
for(i in 1:nrow(cams)){
  if(cams$Doe_count[i] > 0 & cams$Buck_count[i] == 0){
    sex[i] <- "F"
  }
  if(cams$Doe_count[i] == 0 & cams$Buck_count[i] > 0){
    sex[i] <- "M"
  }
  if(cams$Doe_count[i] > 0 & cams$Buck_count[i] > 0){
    sex[i] <- "Mix"
  }
  if(cams$Doe_count[i] == 0 & cams$Buck_count[i] == 0){
    sex[i] <- "Fawn"
  }
}

cams$sex <- sex

# create seperate sex for each individual animal
cams$Animal_1_Sex <- sex
cams$Animal_2_Sex <- sex
cams$Animal_3_Sex <- sex
cams$Animal_4_Sex <- sex
cams$Animal_5_Sex <- sex
cams$Animal_6_Sex <- sex

cams_mixed <- cams[cams$sex == "Mix",]

# For mixed photos, use comments to determine which animal was which sex
# Comments only tell one sex for a few animals
cams_mixed <- cams_mixed %>% 
  mutate(Animal_1_Sex = case_when(
    grepl("buck",Comments,ignore.case = TRUE) & grepl("1",Comments) ~ "M",
    grepl("doe",Comments,ignore.case = TRUE) & grepl("1",Comments) ~ "F",
  )) %>% 
  mutate(Animal_2_Sex = case_when(
    grepl("buck",Comments,ignore.case = TRUE) & grepl("2",Comments) ~ "M",
    grepl("doe",Comments,ignore.case = TRUE) & grepl("2",Comments) ~ "F",
  )) %>% 
  mutate(Animal_3_Sex = case_when(
    grepl("buck",Comments,ignore.case = TRUE) & grepl("3",Comments) ~ "M",
    grepl("doe",Comments,ignore.case = TRUE) & grepl("3",Comments) ~ "F",
  )) %>% 
  mutate(Animal_4_Sex = case_when(
    grepl("buck",Comments,ignore.case = TRUE) & grepl("4",Comments) ~ "M",
    grepl("doe",Comments,ignore.case = TRUE) & grepl("4",Comments) ~ "F",
  )) %>% 
  mutate(Animal_5_Sex = case_when(
    grepl("buck",Comments,ignore.case = TRUE) & grepl("5",Comments) ~ "M",
    grepl("doe",Comments,ignore.case = TRUE) & grepl("5",Comments) ~ "F",
  )) %>% 
  mutate(Animal_6_Sex = case_when(
    grepl("buck",Comments,ignore.case = TRUE) & grepl("6",Comments) ~ "M",
    grepl("doe",Comments,ignore.case = TRUE) & grepl("6",Comments) ~ "F",
  ))

# Therefore, any animal not specified in comments is the opposite.
# This sequence fills in opposite sex for remaining animals in mixed animals
cams_mixed <- cams_mixed %>% 
  mutate(Animal_1_Sex = case_when(
    Animal_1_Sex == "F" ~ "F",
    Animal_1_Sex == "M" ~ "M",
    !is.na(Animal_1_Condition) & Animal_2_Sex == "F" ~ "M",
    !is.na(Animal_1_Condition) & Animal_3_Sex == "F" ~ "M",
    !is.na(Animal_1_Condition) & Animal_4_Sex == "F" ~ "M",
    !is.na(Animal_1_Condition) & Animal_5_Sex == "F" ~ "M",
    !is.na(Animal_1_Condition) & Animal_6_Sex == "F" ~ "M",
    !is.na(Animal_1_Condition) & Animal_2_Sex == "M" ~ "F",
    !is.na(Animal_1_Condition) & Animal_3_Sex == "M" ~ "F",
    !is.na(Animal_1_Condition) & Animal_4_Sex == "M" ~ "F",
    !is.na(Animal_1_Condition) & Animal_5_Sex == "M" ~ "F",
    !is.na(Animal_1_Condition) & Animal_6_Sex == "M" ~ "F"
  )) %>% 
  mutate(Animal_2_Sex = case_when(
    Animal_2_Sex == "F" ~ "F",
    Animal_2_Sex == "M" ~ "M",
    !is.na(Animal_2_Condition) & Animal_1_Sex == "F" ~ "M",
    !is.na(Animal_2_Condition) & Animal_3_Sex == "F" ~ "M",
    !is.na(Animal_2_Condition) & Animal_4_Sex == "F" ~ "M",
    !is.na(Animal_2_Condition) & Animal_5_Sex == "F" ~ "M",
    !is.na(Animal_2_Condition) & Animal_6_Sex == "F" ~ "M",
    !is.na(Animal_2_Condition) & Animal_1_Sex == "M" ~ "F",
    !is.na(Animal_2_Condition) & Animal_3_Sex == "M" ~ "F",
    !is.na(Animal_2_Condition) & Animal_4_Sex == "M" ~ "F",
    !is.na(Animal_2_Condition) & Animal_5_Sex == "M" ~ "F",
    !is.na(Animal_2_Condition) & Animal_6_Sex == "M" ~ "F"
  )) %>% 
  mutate(Animal_3_Sex = case_when(
    Animal_3_Sex == "F" ~ "F",
    Animal_3_Sex == "M" ~ "M",
    !is.na(Animal_3_Condition) & Animal_1_Sex == "F" ~ "M",
    !is.na(Animal_3_Condition) & Animal_2_Sex == "F" ~ "M",
    !is.na(Animal_3_Condition) & Animal_4_Sex == "F" ~ "M",
    !is.na(Animal_3_Condition) & Animal_5_Sex == "F" ~ "M",
    !is.na(Animal_3_Condition) & Animal_6_Sex == "F" ~ "M",
    !is.na(Animal_3_Condition) & Animal_1_Sex == "M" ~ "F",
    !is.na(Animal_3_Condition) & Animal_2_Sex == "M" ~ "F",
    !is.na(Animal_3_Condition) & Animal_4_Sex == "M" ~ "F",
    !is.na(Animal_3_Condition) & Animal_5_Sex == "M" ~ "F",
    !is.na(Animal_3_Condition) & Animal_6_Sex == "M" ~ "F"
  )) %>% 
  mutate(Animal_4_Sex = case_when(
    Animal_4_Sex == "F" ~ "F",
    Animal_4_Sex == "M" ~ "M",
    !is.na(Animal_4_Condition) & Animal_1_Sex == "F" ~ "M",
    !is.na(Animal_4_Condition) & Animal_2_Sex == "F" ~ "M",
    !is.na(Animal_4_Condition) & Animal_3_Sex == "F" ~ "M",
    !is.na(Animal_4_Condition) & Animal_5_Sex == "F" ~ "M",
    !is.na(Animal_4_Condition) & Animal_6_Sex == "F" ~ "M",
    !is.na(Animal_4_Condition) & Animal_1_Sex == "M" ~ "F",
    !is.na(Animal_4_Condition) & Animal_2_Sex == "M" ~ "F",
    !is.na(Animal_4_Condition) & Animal_3_Sex == "M" ~ "F",
    !is.na(Animal_4_Condition) & Animal_5_Sex == "M" ~ "F",
    !is.na(Animal_4_Condition) & Animal_6_Sex == "M" ~ "F"
  )) %>% 
  mutate(Animal_5_Sex = case_when(
    Animal_5_Sex == "F" ~ "F",
    Animal_5_Sex == "M" ~ "M",
    !is.na(Animal_5_Condition) & Animal_1_Sex == "F" ~ "M",
    !is.na(Animal_5_Condition) & Animal_2_Sex == "F" ~ "M",
    !is.na(Animal_5_Condition) & Animal_3_Sex == "F" ~ "M",
    !is.na(Animal_5_Condition) & Animal_4_Sex == "F" ~ "M",
    !is.na(Animal_5_Condition) & Animal_6_Sex == "F" ~ "M",
    !is.na(Animal_5_Condition) & Animal_1_Sex == "M" ~ "F",
    !is.na(Animal_5_Condition) & Animal_2_Sex == "M" ~ "F",
    !is.na(Animal_5_Condition) & Animal_3_Sex == "M" ~ "F",
    !is.na(Animal_5_Condition) & Animal_4_Sex == "M" ~ "F",
    !is.na(Animal_5_Condition) & Animal_6_Sex == "M" ~ "F"
  )) %>% 
  mutate(Animal_6_Sex = case_when(
    Animal_6_Sex == "F" ~ "F",
    Animal_6_Sex == "M" ~ "M",
    !is.na(Animal_6_Condition) & Animal_1_Sex == "F" ~ "M",
    !is.na(Animal_6_Condition) & Animal_2_Sex == "F" ~ "M",
    !is.na(Animal_6_Condition) & Animal_3_Sex == "F" ~ "M",
    !is.na(Animal_6_Condition) & Animal_4_Sex == "F" ~ "M",
    !is.na(Animal_6_Condition) & Animal_5_Sex == "F" ~ "M",
    !is.na(Animal_6_Condition) & Animal_1_Sex == "M" ~ "F",
    !is.na(Animal_6_Condition) & Animal_2_Sex == "M" ~ "F",
    !is.na(Animal_6_Condition) & Animal_3_Sex == "M" ~ "F",
    !is.na(Animal_6_Condition) & Animal_4_Sex == "M" ~ "F",
    !is.na(Animal_6_Condition) & Animal_5_Sex == "M" ~ "F"
  ))

# recombine single sex and mixed sex photos so all are together
cams <- cams[!cams$File %in% cams_mixed$File,]
cams <- rbind(cams,cams_mixed)

# Only leave sex information for animals that have body condition info

cams <- cams %>% 
  mutate(Animal_1_Sex = case_when(
    is.na(Animal_1_Condition) ~ NA,
    TRUE ~ Animal_1_Sex
  )) %>% 
  mutate(Animal_2_Sex = case_when(
    is.na(Animal_2_Condition) ~ NA,
    TRUE ~ Animal_2_Sex
  )) %>% 
  mutate(Animal_3_Sex = case_when(
    is.na(Animal_3_Condition) ~ NA,
    TRUE ~ Animal_3_Sex
  )) %>% 
  mutate(Animal_4_Sex = case_when(
    is.na(Animal_4_Condition) ~ NA,
    TRUE ~ Animal_4_Sex
  )) %>% 
  mutate(Animal_5_Sex = case_when(
    is.na(Animal_5_Condition) ~ NA,
    TRUE ~ Animal_5_Sex
  )) %>% 
  mutate(Animal_6_Sex = case_when(
    is.na(Animal_6_Condition) ~ NA,
    TRUE ~ Animal_6_Sex
  ))

# Create dataframe of visit information by aggregating information where visit_id is the same
# for each visit, this provides the camera station, season, year, and coordinates
# the count of does, bucks, fawns, and adults
# each animal's condition and each animal's sex
# This gives a lot of warnings because of NAs

visits <- cams %>% 
  group_by(visit_id) %>% 
  summarise(Camera = unique(Camera),
            Season = unique(Season),
            Year = unique(Year),
            UTM_X = mean(UTM_X),
            UTM_Y = mean(UTM_Y),
            doecount = max(Doe_count),
            buckcount = max(Buck_count),
            fawncount = max(Fawn_count),
            adultcount = max(Doe_count) + max(Buck_count),
            totalcount = max(Doe_count) + max(Buck_count) + max(Fawn_count),
            Condition1 = mean(Animal_1_Condition,na.rm = TRUE),
            Condition2 = mean(Animal_2_Condition,na.rm = TRUE),
            Condition3 = mean(Animal_3_Condition,na.rm = TRUE),
            Condition4 = mean(Animal_4_Condition,na.rm = TRUE),
            Condition5 = mean(Animal_5_Condition,na.rm = TRUE),
            Condition6 = mean(Animal_6_Condition,na.rm = TRUE),
            Condition_con = max(Confidence_Condition,na.rm = TRUE),
            Sex1 = max(Animal_1_Sex,na.rm = T),
            Sex2 = max(Animal_2_Sex,na.rm = T),
            Sex3 = max(Animal_3_Sex,na.rm = T),
            Sex4 = max(Animal_4_Sex,na.rm = T),
            Sex5 = max(Animal_5_Sex,na.rm = T),
            Sex6 = max(Animal_6_Sex,na.rm = T)
            )

# Because photo counts do not have max count of animals in a visit, 
# Use condition values to sum number of deer of each sex
does <- vector()
bucks <- vector()
adults <- vector()

for(i in 1:nrow(visits)){
  temp <- c(visits$Sex1[i],visits$Sex2[i],visits$Sex3[i],visits$Sex4[i],visits$Sex5[i],visits$Sex6[i])
  does[i] <- sum(temp == "F", na.rm = T)
  bucks[i] <- sum(temp == "M", na.rm = T)
  adults[i] <- sum(temp %in% c("F","M"), na.rm = T)
}

# Where there is a discrepency between photo count and number of conditions/visit
# Replace with the higher number of animals
visits$doecount[does > visits$doecount] <- does[does > visits$doecount]
visits$buckcount[bucks > visits$buckcount] <- bucks[bucks > visits$buckcount]
visits$adultcount[adults > visits$adultcount] <- adults[adults > visits$adultcount]


#### Read in landscape covariates ####
deer_S <- rast("../ProcessedData/deerpred_summer_289cell.tif")
deer_W <- rast("../ProcessedData/deerpred_winter_289cell.tif")
pig_S <- rast("../ProcessedData/pigpred_summer_289cell.tif")
pig_W <- rast("../ProcessedData/pigpred_winter_289cell.tif")
forest_dist <- rast("../ProcessedData/log_forest_dist.tiff")
grass_dist <- rast("../ProcessedData/log_grass_dist.tiff")
shrub_dist <- rast("../ProcessedData/log_shrub_dist.tiff")
riparian_dist <- rast("../ProcessedData/log_riparian_dist.tiff")
# TRI <- rast("../ProcessedData/terrain_rugged.tiff")
# TPI <- rast("../ProcessedData/topo_posi.tiff")
NDVIS21 <- rast("../ProcessedData/NDVIS21_289cell.tif")
NDVIS22 <- rast("../ProcessedData/NDVIS22_289cell.tif")
NDVIW21 <- rast("../ProcessedData/NDVIW21_289cell.tif")
NDVIW22 <- rast("../ProcessedData/NDVIW22_289cell.tif")

# TRI <- resample(TRI,deer)
# TPI <- resample(TPI,deer)

# read in study area boundary 
FHLbound <- vect("../ProcessedData/FHLBoundNoHoles.shp")
FHLRast <- rasterize(FHLbound,deer_S)

# multiple density by study area boundary to remove predictions 
# outside of the study area
FHLpigs <- pig_S*FHLRast
FHLpigw <- pig_W*FHLRast
FHLdeers <- deer_S*FHLRast
FHLdeerw <- deer_W*FHLRast

# check summary to get min/max deer values
deer_values_s <- values(FHLdeers)
deer_values_s <- deer_values_s[!is.na(deer_values_s)]
summary(deer_values_s)

deer_values_w <- values(FHLdeerw)
deer_values_w <- deer_values_w[!is.na(deer_values_w)]
summary(deer_values_w)

summary(c(deer_values_w,deer_values_s))
# check summary to get min/max pig values
pig_values_s <- values(FHLpigs)
pig_values_s <- pig_values_s[!is.na(pig_values_s)]
summary(pig_values_s)

pig_values_w <- values(FHLpigw)
pig_values_w <- pig_values_w[!is.na(pig_values_w)]
summary(pig_values_w)

summary(c(pig_values_w,pig_values_s))

# check correlation between deer and pig predictions
cor.test(deer_values_s,pig_values_s)
cor.test(deer_values_w,pig_values_w)

# make plots of deer and pig density
par(mfrow = c(1,2))
plot(terra::crop(FHLdeerw,FHLbound),main = expression("Deer Density (#/km" ^2 * ")"),
     cex.lab = 1.75, cex.axis = 1.5)
plot(terra::crop(FHLpigw,FHLbound),main = expression("Pig Density (#/km" ^2 * ")"),
     cex.lab = 1.75, cex.axis = 1.5)

# create raster stack of all variables for easy data extraction
allcovs <- c(deer_S,deer_W,pig_S,pig_W,forest_dist,grass_dist,shrub_dist,riparian_dist,
             NDVIS21,NDVIS22,NDVIW21,NDVIW22)

names(allcovs) <- c('deer_S','deer_W','pig_S','pig_W','forest_dist',
                    'grass_dist','shrub_dist','riparian_dist',
                    'NDVIS21','NDVIS22','NDVIW21','NDVIW22')

#### Doe to Fawn Ratios ####
# Calculate fawn to doe ratio for each visit
doefawn <- visits %>% 
  group_by(Camera,Season,Year) %>% 
  summarise(doe = sum(doecount),
            fawn = sum(fawncount),
            fawn_to_doe = sum(fawncount)/sum(doecount))

hist(doefawn$fawn_to_doe)
boxplot(doefawn$fawn_to_doe ~ doefawn$Season)

# Add station info for locations
doefawn <- doefawn %>% 
  left_join(stations)

# Extract landscape covariates
doefawn <- st_as_sf(doefawn, coords = c("UTM_X", "UTM_Y"), crs = 32610)
doefawncovs <- extract(allcovs,doefawn)
doefawn <- cbind(doefawn,doefawncovs)

#Create column for proper NDVI value and pig density based on season and year
doefawn <- doefawn %>% 
  mutate(NDVI = case_when(
    Season == "Summer" & Year == 2021 ~ NDVIS21,
    Season == "Summer" & Year == 2022 ~ NDVIS22,
    Season == "Winter" & Year == 2021 ~ NDVIW21,
    Season == "Winter" & Year == 2022 ~ NDVIW22
  )) %>% 
  mutate(deer = case_when(
    Season == "Summer" & Year == 2021 ~ deer_S,
    Season == "Summer" & Year == 2022 ~ deer_S,
    Season == "Winter" & Year == 2021 ~ deer_W,
    Season == "Winter" & Year == 2022 ~ deer_W
  )) %>% 
  mutate(pig = case_when(
    Season == "Summer" & Year == 2021 ~ pig_S,
    Season == "Summer" & Year == 2022 ~ pig_S,
    Season == "Winter" & Year == 2021 ~ pig_W,
    Season == "Winter" & Year == 2022 ~ pig_W
  ))

# select only the needed columns
doefawn <- doefawn %>% 
  select(Camera,Season,Year,doe,fawn,fawn_to_doe,deer,pig,forest_dist,grass_dist,
         shrub_dist,riparian_dist,NDVI)

doefawn <- st_drop_geometry(doefawn)

# pigcenter <- 3.571821
# pigscale <- 3.288569

# deercenter <- 7.848369
# deerscale <- 3.197528
# scale covariates
# doefawn[,7:13] <- scale(doefawn[,7:13])

# remove cameras that never detected deer and calculate total of doe + fawn
doefawn <- doefawn %>% 
  filter(!is.na(fawn_to_doe)) %>% 
  mutate(total = doe + fawn)

# model doe to fawn ratios as a function of pig density
# season, NDVI, grass dist, riparian dist, and year
mod1 <- glm(cbind(fawn,doe) ~ pig*Season + NDVI + grass_dist + 
              riparian_dist + as.factor(Year),
            family = binomial(link = "logit"), data = doefawn)

summary(mod1)
newdat <- data.frame(pig = rep(seq(0,15,length = 100),2),
                     Season = rep(c("Summer","Winter"),each = 100),
                     NDVI = rep(mean(doefawn$NDVI),200),
                     grass_dist = rep(mean(doefawn$grass_dist),200),
                     riparian_dist = rep(mean(doefawn$riparian_dist),200),
                     Year = rep("2021",200))

newdat$pred <- predict(mod1,newdat = newdat,se.fit = T,type = "response")$fit
newdat$pred.se <- predict(mod1,newdat = newdat,se.fit = T,type = "response")$se.fit

doefawn$propfawn <- doefawn$fawn/(doefawn$doe + doefawn$fawn)

par(mfrow = c(1,2))
par(mar = c(5,5,2,2))
# par(bg = "#F8F5EE")
plot(doefawn$propfawn[doefawn$Season == "Summer"] ~ 
       doefawn$pig[doefawn$Season == "Summer"],col = "darkgreen", pch = 16,
     xlab = expression("Pig density (#/km" ^2 *")"),ylab = "Proportion of fawns",cex.lab = 2,
     cex.axis = 1.5,ylim = c(0,1))

# labeledden <- c(0,3,6,9,12,15)
# labelsat <- (labeledden-pigcenter)/pigscale

# axis(side = 1, at = labelsat, labels = c("0","3","6","9","12","15"),cex.axis = 1.5)

lines(newdat$pig[1:100],newdat$pred[1:100],col = "darkgreen", lwd = 3)
lines(newdat$pig[1:100],newdat$pred[1:100] + 1.96*newdat$pred.se[1:100],
      col = "darkgreen", lwd = 3, lty = "dashed")
lines(newdat$pig[1:100],newdat$pred[1:100] - 1.96*newdat$pred.se[1:100],
      col = "darkgreen", lwd = 3, lty = "dashed")
text(x = 0.2, y = 0.95, label = "A", cex = 1.5)

plot(doefawn$propfawn[doefawn$Season == "Winter"] ~ 
       doefawn$pig[doefawn$Season == "Winter"],col = "skyblue", pch = 16,
     xlab = expression("Pig density (#/km" ^2 *")"),ylab = "Proportion of fawns",cex.lab = 2,
     cex.axis = 1.5,ylim = c(0,1))
# axis(side = 1, at = labelsat, labels = c("0","3","6","9","12","15"),cex.axis = 1.5)

lines(newdat$pig[101:200],newdat$pred[101:200],col = "skyblue", lwd = 3)
lines(newdat$pig[101:200],newdat$pred[101:200] + 1.96*newdat$pred.se[101:200],
      col = "skyblue", lwd = 3, lty = "dashed")
lines(newdat$pig[101:200],newdat$pred[101:200] - 1.96*newdat$pred.se[101:200],
      col = "skyblue", lwd = 3, lty = "dashed")
text(x = 0.2, y = 0.95, label = "B", cex = 1.5)



# indicates no relationship between pig density and doe to fawn ratios

#### Condition Data ####

#### Camera Data Body Condition ####
# Make  data long form, with one row for each individual
condition_visits <- visits %>% 
  pivot_longer(cols = Condition1:Condition6) %>% 
  filter(!is.na(value))

# Add correct sex to each individual
condition_visits <- condition_visits %>% 
  mutate(sex = case_when(
    grepl("1",condition_visits$name) ~ Sex1,
    grepl("2",condition_visits$name) ~ Sex2,
    grepl("3",condition_visits$name) ~ Sex3,
    grepl("4",condition_visits$name) ~ Sex4,
    grepl("5",condition_visits$name) ~ Sex5,
    grepl("6",condition_visits$name) ~ Sex6,
  ))

# Select only the needed columns
condition_visits <- condition_visits %>% 
  select(visit_id,Camera,Season,Year,UTM_X,UTM_Y,value,sex,Condition_con)

# Make an SF object for extracting landscape covariates
condition_visits <- st_as_sf(condition_visits, coords = c("UTM_X", "UTM_Y"), crs = 32610)

# Extract covaraites 
camcovs <- extract(allcovs,condition_visits)
condition_visits <- cbind(condition_visits,camcovs)

#Create column for proper NDVI, pig, and deer values based on season and year
condition_visits <- condition_visits %>% 
  mutate(NDVI = case_when(
    Season == "Summer" & Year == 2021 ~ NDVIS21,
    Season == "Summer" & Year == 2022 ~ NDVIS22,
    Season == "Winter" & Year == 2021 ~ NDVIW21,
    Season == "Winter" & Year == 2022 ~ NDVIW22
  )) %>% 
  mutate(deer = case_when(
    Season == "Summer" & Year == 2021 ~ deer_S,
    Season == "Summer" & Year == 2022 ~ deer_S,
    Season == "Winter" & Year == 2021 ~ deer_W,
    Season == "Winter" & Year == 2022 ~ deer_W
  )) %>% 
  mutate(pig = case_when(
    Season == "Summer" & Year == 2021 ~ pig_S,
    Season == "Summer" & Year == 2022 ~ pig_S,
    Season == "Winter" & Year == 2021 ~ pig_W,
    Season == "Winter" & Year == 2022 ~ pig_W
  ))

# select only the needed columns
condition_visits <- condition_visits %>% 
  select(visit_id,Camera,Season,Year,value,sex,Condition_con,deer,pig,forest_dist,
         grass_dist,shrub_dist,riparian_dist,NDVI)

# drop geometry and scale covariates
condition_visits <- st_drop_geometry(condition_visits)

pigcenter <- 4.205838
pigscale <- 3.687636
deercenter <- 8.399013
deerscale <- 2.711867

# condition_visits[,8:14] <- scale(condition_visits[,8:14])

condition_visits$station <- with(condition_visits,paste0(Camera,"_",Season,"_",Year))


# filter to adult females with condition scores and a confidence >0
condition_visits <- condition_visits[condition_visits$sex == "F",]
condition_visits <- condition_visits[!is.na(condition_visits$visit_id),]
condition_visits <- condition_visits[condition_visits$Condition_con != 0,]

# summary stats
mean(condition_visits$value)
sd(condition_visits$value)/sqrt(nrow(condition_visits))
table(condition_visits$Condition_con)

# model adult female body condition as a function of pig density, season, NDVI
# grass dist, riparian dist, and year, with a random effect for station
# to accound for repeated visits to the same camera

mod1 <- lmer(value ~ pig*Season + NDVI + grass_dist + riparian_dist +
               as.factor(Year) + (1|station),
           data = condition_visits,
           weights = Condition_con)

summary(mod1)

newdat2 <- newdat[,1:6]
newdat2$Year[101:200] <- "2022"
newdat2$station <- "C04_Summer_2022"

# newdat2$pred <- predict(mod1,newdata = newdat2,type = "response",re.form = NA)

newdat2$sspred.rc<-predict(mod1, newdata = newdat2)
boot.pred <- bootMer(mod1,
                     FUN = function(x){predict(x, newdata = newdat2)},
                     nsim = 500)
predboot.CI <- apply(boot.pred$t, 2, quantile, c(0.025,0.975))
newdat2$lowCI <- predboot.CI[1,]
newdat2$upCI <- predboot.CI[2,]

ggplot(newdat2, aes(x = pig, y = sspred.rc))+
  # geom_point(size = 2)+
  geom_line(aes(pig, sspred.rc),lwd = 1.3, col = "red")+
  geom_line(aes(pig,lowCI),lwd = 1.3, col = "red",lty = 2)+
  geom_line(aes(pig,upCI),lwd = 1.3, col = "red",lty = 2)+
  facet_wrap(~Season)+
  ylim(0,4)+
  geom_point(data = condition_visits, aes(x = pig, y = value))


# mm <- model.matrix(~pig*Season + NDVI + grass_dist + 
#                      riparian_dist + as.factor(Year), newdat2)
# newdat2$pred <- mm%*%fixef(mod1)
# pvar1 <- diag(mm %*% tcrossprod(vcov(mod1),mm))
# newdat2$se.low <- newdat2$pred-2*sqrt(pvar1)
# newdat2$se.high <- newdat2$pred+2*sqrt(pvar1)

par(mfrow = c(1,2))
par(mar = c(5,5,2,2))
plot(condition_visits$value[condition_visits$Season == "Summer"] ~ 
       condition_visits$pig[condition_visits$Season == "Summer"],col = "darkgreen", pch = 16,
     xlab = expression("Pig density (#/km" ^2 *")"),ylab = "Deer body condition score",
     cex.lab = 2,cex.axis = 1.5,ylim = c(0,5))

# labeledden <- c(0,3,6,9,12,15)
# labelsat <- (labeledden-pigcenter)/pigscale

# axis(side = 1, at = labelsat, labels = c("0","3","6","9","12","15"),cex.axis = 1.5)

lines(newdat2$pig[1:100],newdat2$sspred.rc[1:100],col = "darkgreen", lwd = 3)
lines(newdat2$pig[1:100],newdat2$lowCI[1:100],col = "darkgreen", lwd = 2,lty = "dashed")
lines(newdat2$pig[1:100],newdat2$upCI[1:100],col = "darkgreen", lwd = 2,lty = "dashed")

text(x = 0.2, y = 4.9, label = "A", cex = 1.5)


plot(condition_visits$value[condition_visits$Season == "Winter"] ~ 
       condition_visits$pig[condition_visits$Season == "Winter"],col = "skyblue", pch = 16,
     xlab = expression("Pig density (#/km" ^2 *")"),ylab = "Deer body condition score",
     cex.lab = 2,cex.axis = 1.5,ylim = c(0,5))
# axis(side = 1, at = labelsat, labels = c("0","3","6","9","12","15"),cex.axis = 1.5)

lines(newdat2$pig[101:200],newdat2$sspred.rc[101:200],col = "skyblue", lwd = 3)
lines(newdat2$pig[101:200],newdat2$lowCI[101:200],col = "skyblue", lwd = 2,lty = "dashed")
lines(newdat2$pig[101:200],newdat2$upCI[101:200],col = "skyblue", lwd = 2,lty = "dashed")
text(x = 0.2, y = 4.9, label = "B", cex = 1.5)







#### Kill Data ####
# This section processes the kill site data. It is commented out because 
# it requires sensitive GPS data. The processed file can be read in 
# at the end of this section

# # check if body condition from bone marrow tells the same story
# 
# # filter kills to only deer and required covariates
# kills <- kills %>% 
#   select(c(cluster_id,cougar_id,type,form_date,centroid_lat,centroid_lon,
#            inv_date,carcass_found,carcass_lat,carcass_lon,species,sex,age,marrow,
#            confidence_cougar_kill)) %>% 
#   filter(species == "deer")
# 
# kills <- kills[grepl("red",kills$marrow) | grepl("pink",kills$marrow) | grepl("white",kills$marrow),]
# 
# # break marrow into color and texture components
# 
# kills$red <- grepl("red",kills$marrow)
# kills$pink <- grepl("pink",kills$marrow)
# kills$white <- grepl("white",kills$marrow)
# 
# kills$red[kills$pink == "TRUE"] <- FALSE
# 
# kills$gelat <- grepl("gelat",kills$marrow)
# kills$soft <- grepl("soft",kills$marrow)
# kills$hard <- grepl("hard",kills$marrow)
# kills$waxy <- grepl("waxy",kills$marrow)
# 
# kills <- kills %>% 
#   mutate(marrow_color = case_when(
#     red == TRUE ~ 0,
#     pink == TRUE ~ 1,
#     white == TRUE ~ 2
#   )) %>% 
#   mutate(marrow_texture = case_when(
#     gelat == TRUE ~ 0,
#     soft == TRUE ~ 1,
#     hard == TRUE ~ 2,
#     waxy == TRUE ~ 2
#   )) %>% 
#   mutate(body_cond = marrow_color + marrow_texture)
# 
# 
# # format dates
# kills$form_date <- mdy(kills$form_date)
# kills$inv_date <- mdy(kills$inv_date)
# 
# # fix errors in coordinates
# kills$centroid_lon <- kills$centroid_lon * -1
# 
# # # filter to kills 
# # kills <- kills[kills$form_date < mdy("08/15/2023"),]
# # kills <- kills[1:123,]
# 
# # Create SF object for extracting covariates
# kills <- st_as_sf(kills, coords = c("centroid_lon","centroid_lat"),crs = 4326)
# kills <- st_transform(kills, 32610)
# 
# # extract raster covariates
# killcovs <- extract(allcovs,kills)
# kills <- cbind(kills,killcovs)
# 
# # Add season and year to each kill for collecting proper NDVI data
# kills$month <- month(kills$form_date)
# kills$Year <- year(kills$form_date)
# 
# kills <- kills %>% 
#   mutate(Season = case_when(
#     kills$month %in% c(4,5,6,7,8) ~ "Summer",
#     kills$month %in% c(10,11,12,1,2) ~ "Winter"
#   ))
# 
# kills$Year[kills$Year == 2023] <- 2022
# 
# #Create column for proper NDVI, deer, and pig values
# kills <- kills %>% 
#   mutate(NDVI = case_when(
#     Season == "Summer" & Year == 2021 ~ NDVIS21,
#     Season == "Summer" & Year == 2022 ~ NDVIS22,
#     Season == "Winter" & Year == 2021 ~ NDVIW21,
#     Season == "Winter" & Year == 2022 ~ NDVIW22
#   ))%>% 
#   mutate(deer = case_when(
#     Season == "Summer" & Year == 2021 ~ deer_S,
#     Season == "Summer" & Year == 2022 ~ deer_S,
#     Season == "Winter" & Year == 2021 ~ deer_W,
#     Season == "Winter" & Year == 2022 ~ deer_W
#   )) %>% 
#   mutate(pig = case_when(
#     Season == "Summer" & Year == 2021 ~ pig_S,
#     Season == "Summer" & Year == 2022 ~ pig_S,
#     Season == "Winter" & Year == 2021 ~ pig_W,
#     Season == "Winter" & Year == 2022 ~ pig_W
#   ))
# 
# # Select need variables, drop geometry, and scale variables
# kills <- kills %>% 
#   select(cluster_id,cougar_id,Season,Year,form_date,inv_date,carcass_found,
#          species,sex,age,marrow_color,marrow_texture,
#          body_cond,deer,pig,forest_dist,grass_dist,shrub_dist,riparian_dist,
#          NDVI)
# 
# kills <- st_drop_geometry(kills)
# # scale covariates
# # kills[,16:22] <- scale(kills[,16:22])
# 
# head(kills)
# write.csv(kills,file = "../ProcessedData/Killsnolocs.csv")


#### Read in location-censored data ####
kills <- read.csv("../ProcessedData/killsnolocs.csv")

# Run models ####
adult_kills <- kills[kills$age %in% c("adult","fawn","yearling"),]
# remove second iteration of one shared kills that had two entries
adult_kills <- adult_kills[!duplicated(adult_kills$deer),]
# adult_kills$sex[adult_kills$sex == "UNK"] <- "F"

# model marrow data from all kills as a function of pig density, season, NDVI
# grass dist, riparian dist, sex, and year
mod1 <- glm(body_cond ~ pig*Season + NDVI + grass_dist + 
             riparian_dist + sex + as.factor(Year),
            family = poisson(link = "log"), data = kills) 
summary(mod1)

# repeat modeling with ony adult kills
adult_mod1 <- glm(body_cond ~ pig*Season + NDVI + grass_dist + 
             riparian_dist + as.factor(Year) + sex,
             family = poisson(link = "log"), data = adult_kills) 
summary(adult_mod1)

# Both support that there is no relationship between pig density and deer
# body condition
