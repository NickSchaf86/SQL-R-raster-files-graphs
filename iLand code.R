setwd("C:/Users/nicks/OneDrive/Documents/R/iLand job interview/Input data")
 
library(dplyr)
library(dbplyr)
library(RSQLite) 


##Import SQLite db
con <- dbConnect(RSQLite::SQLite(), dbname = "Nomgm.sqlite")

con4 <- dbConnect(RSQLite::SQLite(), dbname = "Nomgm_plus4C.sqlite")

##list all tables
tables <- dbListTables(con)
tables4 <- dbListTables(con4)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]
lDataFrames <- vector("list", length=length(tables))

tables4 <- tables4[tables4 != "sqlite_sequence"]
lDataFrames <- vector("list", length=length(tables4))

## create separate tables and turn them into data frames
barkbeetle <- tbl(con, "barkbeetle")
barkbeetle %>% head(200) %>% collect() %>% View()
barkbeetle_0C <- as.data.frame(barkbeetle)

barkbeetle4C <- tbl(con4, "barkbeetle")
barkbeetle4C %>% head(200) %>% collect() %>% View()
barkbeetle_4C <- as.data.frame(barkbeetle4C)

landscape <- tbl(con, "landscape")
landscape %>% head(50) %>% collect() %>% View()
landscape_0C <- as.data.frame(landscape)

landscape4C <- tbl(con4, "landscape")
landscape4C %>% head(50) %>% collect() %>% View()
landscape_4C <- as.data.frame(landscape4C)

landscape_removed <- tbl(con, "landscape_removed")
landscape_removed %>% head(50) %>% collect() %>% View()
landscape_removed_0C <- as.data.frame(landscape_removed)

landscape_removed4C <- tbl(con4, "landscape_removed")
landscape_removed4C %>% head(50) %>% collect() %>% View()
landscape_removed_4C <- as.data.frame(landscape_removed4C)

wind <- tbl(con, "wind")
wind %>% head(50) %>% collect() %>% View()
wind_0C <- as.data.frame(wind)

wind4C <- tbl(con4, "wind")
wind4C %>% head(50) %>% collect() %>% View()
wind_4C <- as.data.frame(wind4C)


###two windstorms were simulated, one at 10 yr and one at 50 yr. Bark beetle outbreaks were activated by a high "stormActivation_ha" and infested area grew from year x+1.


##plot data
library(ggplot2)

##difference in windstorms plotted ####prefer to express in _ha

wind_0C["BA_Ha"] <- wind_0C$killedBasalArea/wind_0C$area_ha
wind_0C["trees_Ha"] <- wind_0C$killedTrees/wind_0C$area_ha
wind_0C["vol_Ha"] <- wind_0C$killedVolume/wind_0C$area_ha
wind_0C

wind_4C["BA_Ha"] <- wind_4C$killedBasalArea/wind_4C$area_ha
wind_4C["trees_Ha"] <- wind_4C$killedTrees/wind_4C$area_ha
wind_4C["vol_Ha"] <- wind_4C$killedVolume/wind_4C$area_ha
wind_4C

### combine the two dfs, add column with values wind_0C and wind_4c

wind_both <- rbind(wind_0C, wind_4C)
wind_both %>% head(50) %>% collect() %>% View()

wind_lab <- c("+0C","+0C","+4C","+4C")

wind_both$model <-wind_lab

##plots

ggplot(data = wind_both, aes(fill=model, y=trees_Ha, x=year)) + 
  geom_bar(position="dodge", stat="identity", width = 5) +
  labs(title = "Wind trees") +
  xlim(0,100)

ggplot(data = wind_both, aes(fill=model, y=BA_Ha, x=year)) + 
  geom_bar(position="dodge", stat="identity", width = 5) +
  labs(title = "Wind basal area") +
  xlim(0,100)

ggplot(data = wind_both, aes(fill=model, y=vol_Ha, x=year)) + 
  geom_bar(position="dodge", stat="identity", width = 5) +
  labs(title = "Wind volume") +
  xlim(0,100)


###Now for the bark beetle data
##plot(barkbeetle$year; trees, area_ha and Basal Area)

BB_trees = ggplot() + 
  geom_line(data = barkbeetle_0C, aes(x = year, y = killedTrees)) +
  geom_line(data = barkbeetle_4C, aes(x = year, y = killedTrees), color = "red") +
  labs(y="Trees", title="Number of trees killed by bark beetles") 
plot(BB_trees)

BB_BA = ggplot() +
  geom_line(data = barkbeetle_0C, aes(x = year, y = killedBasalArea)) +
  geom_line(data = barkbeetle_4C, aes(x = year, y = killedBasalArea), color = "red") +
  labs(y="Basal Area", title="Tree basal area killed by bark beetles") 
plot(BB_BA)

BB_area = ggplot() + 
  geom_line(data = barkbeetle_0C, aes(x = year, y = killedArea_ha)) +
  geom_line(data = barkbeetle_4C, aes(x = year, y = killedArea_ha), color = "red") +
  labs(y="Area in Ha", title="Area killed by bark beetles") 
plot(BB_area)

###Look at the number of trees and BA per hectare of forest, killed by BB

barkbeetle_0C["Trees_area"] <- barkbeetle_0C$killedTrees/barkbeetle_0C$killedArea_ha
barkbeetle_4C["Trees_area"] <- barkbeetle_4C$killedTrees/barkbeetle_4C$killedArea_ha

barkbeetle_0C["BA_area"] <- barkbeetle_0C$killedBasalArea/barkbeetle_0C$killedArea_ha
barkbeetle_4C["BA_area"] <- barkbeetle_4C$killedBasalArea/barkbeetle_4C$killedArea_ha

BB_Tree_ha = ggplot() +
  geom_line(data = barkbeetle_0C, aes(x = year, y = Trees_area)) +
  geom_line(data = barkbeetle_4C, aes(x = year, y = Trees_area), color = "red") +
  labs(y="Basal Area_ha", title="Number of trees per hectare killed by bark beetles") 
plot(BB_Tree_ha)

BB_BA_ha = ggplot() +
  geom_line(data = barkbeetle_0C, aes(x = year, y = BA_area)) +
  geom_line(data = barkbeetle_4C, aes(x = year, y = BA_area), color = "red") +
  labs(y="Basal Area_ha", title="Tree basal area per hectare killed by bark beetles") 
plot(BB_BA_ha)

### Looking at the tree composition through time
###Plot number of trees under both climate scenarios     

ggplot(data = landscape_0C, aes(x = year, y = count_ha)) + geom_line() +
  facet_wrap(~species) +
  labs(title = "Tree count_ha +0C") +
  ylim(0,800)

ggplot(data = landscape_4C, aes(x = year, y = count_ha)) + geom_line() +
  facet_wrap(~species) +
  labs(title = "Tree count_ha +4C") 

ggplot(data = landscape_0C, aes(x = year, y = basal_area_m2)) + geom_line() +
  facet_wrap(~species) +
  labs(title = "Tree BA_ha +0C")

ggplot(data = landscape_4C, aes(x = year, y = basal_area_m2)) + geom_line() +
  facet_wrap(~species) +
  labs(title = "Tree BA_ha +4C") 

###Plot forest structure at t=0, 20, 40, 60, 80, 100 for both scenarios 
### Make a stacked bar chart of forest composition (# trees) 

ggplot(landscape_0C, aes(fill=species, y=count_ha, x=year)) +
    geom_bar(position="stack", stat="identity") +
  labs(title = "Tree composition +0C") +
  scale_x_continuous(limits = c(80, 100)) 

ggplot(landscape_4C, aes(fill=species, y=count_ha, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Tree composition +4C") +
  scale_x_continuous(limits = c(80, 100)) 

ggplot(landscape_4C, aes(fill=species, y=basal_area_m2, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Tree BA-ha +4C") +
  scale_x_continuous(limits = c(80, 100)) 
  
##Final graph of the total amount of removed tree volume for Picea abies  

piab_landscape_removed_0C <- filter(landscape_removed_0C, species == "piab")
piab_landscape_removed_4C <- filter(landscape_removed_4C, species == "piab")

piab_landscape_removed %>% head(200) %>% collect() %>% View()

PiAb_num = ggplot() + 
  geom_line(data = piab_landscape_removed_0C, aes(x = year, y = count, color = reason)) +
  labs(title = "Picea abies count +0C")
plot(PiAb_num)

PiAb_4C_num = ggplot() + 
  geom_line(data = piab_landscape_removed_4C, aes(x = year, y = count, color = reason)) +
  labs(title = "Picea abies count +4C")
plot(PiAb_4C_num)

PiAb_vol = ggplot() + 
  geom_line(data = piab_landscape_removed_0C, aes(x = year, y = basal_area_m2, color = reason)) +
  labs(title = "Picea abies BA_m2 +0C")
plot(PiAb_vol)

PiAb_4C_vol = ggplot() + 
  geom_line(data = piab_landscape_removed_4C, aes(x = year, y = basal_area_m2, color = reason)) +
  labs(title = "Picea abies BA_m2 +4C")
plot(PiAb_4C_vol)




       
