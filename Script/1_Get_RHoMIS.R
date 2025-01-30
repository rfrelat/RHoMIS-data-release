# Transform RHoMIS dataset in farmhousehold format
# Last update: 03/12/2024

# Make sure to use the last update of the rhomis and farmhousehold package
# devtools::install_github("l-gorman/rhomis-R-package")
library(rhomis)
# devtools::install_github("rfrelat/farmhousehold")
library(farmhousehold)

library(dplyr) #for fast row binding
library(terra) # for spatial information
# Load parameters
source("Script/Param_RHOMIS.R")

# 1. Load data ----------------------------------

# Load processed dataset
tab <- read.csv("Data/full_survey_data.csv")
dim(tab) #54873  1599

# Load indicator dataset
ind <- read.csv("Data/indicator_data.csv")
dim(ind) #54873    46

tab <- cbind(tab, ind[,!names(ind)%in%names(tab)])
tab$hhid <- tab$id_unique
# table(duplicated(tab$hhid)) # no duplicates

# 2. Create crop table --------------------------
# get the number of columns with crop
loopcrop <- gsub("^crop_name_", "",
                 names(tab)[grep("^crop_name_", names(tab))])

# information to keep
colC <- c("crop_name_", "crop_harvest_kg_per_year_", "crop_consumed_kg_per_year_",
          "crop_sold_kg_per_year_", "crop_income_per_year_",
          "crop_land_area_", "crop_intercrop_", "crop_use_", "crop_residue_use_")

# add gender information columns
genC <- c(gsub("1", "", names(tab)[grep("_crop_consumed_kg_per_year_1", names(tab))]),
          gsub("1", "", names(tab)[grep("_crop_income_per_year_1", names(tab))]))

colC <- c(colC, genC)

# to be added later :
#  crop feed livestock: crop_feed_lstk_prop
#  fertilizer: fertiliser_crops, fertiliser_type, fertiliser_units, fertiliser_amount

# new column name
labC <- c("name", "harvest_kg", "consumed_kg",
          "sold_kg", "income_lcu",
          "prop_land_area", "intercrop", "use_list", "crop_residue_use", 
          gsub("_$", "", genC))

# create a list with for each loopcrop
tdfC = lapply(loopcrop, function(x) {
  y <- tab[,paste0(colC, x)]
  names(y) <- labC
  return(y)}
)

# for speed, bind_rows appear to be faster
newdf =  dplyr::bind_rows(tdfC)

# create the new crop table
crop <- data.frame(
  "hhid"= rep(tab$hhid,
              length(loopcrop)),
  newdf
)

# check and remove rows with no information or no harvest
crop <- crop[!is.na(crop$name)& !is.na(crop$harvest_kg),]
crop <- crop[crop$harvest_kg>0,]

# add gender control variables
# female control over consumption
col_consAll <- names(crop)[grep("_crop_consumed_kg_per_year", names(crop))]
consAll <- rowSums(crop[,col_consAll], na.rm = TRUE)

col_consFem <- col_consAll[grep("female", col_consAll)]
consFem <- rowSums(crop[,col_consFem], na.rm = TRUE)

crop$control_consumed_female <- ifelse(consAll<=0, NA, consFem/consAll*100)

# female control over income
col_sellAll <- names(crop)[grep("_crop_income_per_year", names(crop))]
sellAll <- rowSums(crop[,col_sellAll], na.rm = TRUE)

col_sellFem <- col_sellAll[grep("female", col_sellAll)]
sellFem <- rowSums(crop[,col_sellFem], na.rm = TRUE)

crop$control_income_female <- ifelse(sellAll<=0, NA, sellFem/sellAll*100)

#remove the gender temporary variables
crop <- crop[,-match(c(col_consAll, col_sellAll), names(crop))]

# Clean proportion area and add area in ha
crop$prop_land_area[crop$prop_land_area=="no_answer"] <- NA
convLA <- crop$prop_land_area%in%rhomis::proportion_conversions$survey_value
matLA <- match(crop$prop_land_area[convLA], rhomis::proportion_conversions$survey_value)
crop$prop_land_area[convLA] <- rhomis::proportion_conversions$conversion[matLA]
crop$prop_land_area <- NAto0(as.numeric(crop$prop_land_area))
tot_prop_area <- tapply(crop$prop_land_area, crop$hhid, sum, na.rm=TRUE)
tot_prop_area_matchid <- tot_prop_area[match(crop$hhid, names(tot_prop_area))]

# for the household without proportion per crop, assume equal to proportion of harvest
tot_harvest <- tapply(crop$harvest_kg, crop$hhid, sum, na.rm=TRUE)
crop$prop_harvest <- crop$harvest_kg/tot_harvest[match(crop$hhid, names(tot_harvest))]

crop$prop_land_area <- ifelse(tot_prop_area_matchid>0,
                              crop$prop_land_area/tot_prop_area_matchid,
                              crop$prop_harvest)

tot_area <- tab$land_cultivated_ha[match(crop$hhid, tab$hhid)]

crop$land_area_ha <- tot_area*crop$prop_land_area

# consistency among quantities
# if yield to high, use minimum among both
# else use maximum among both
maxkg <- ifelse(crop$harvest_kg>crop$consumed_kg+crop$sold_kg,
                crop$harvest_kg,
                crop$consumed_kg+crop$sold_kg)
minkg <- ifelse(crop$harvest_kg>crop$consumed_kg+crop$sold_kg,
                crop$consumed_kg+crop$sold_kg,
                crop$harvest_kg)

crop$harvest_kg <- ifelse(maxkg/crop$land_area_ha>20000,
                          minkg, maxkg)
psold <- crop$sold_kg/(crop$consumed_kg+crop$sold_kg)
# compute relative quantities to harvest_kg
crop$sold_kg <- ifelse(crop$consumed_kg+crop$sold_kg>crop$harvest_kg,
                       psold*crop$harvest_kg,
                       crop$sold_kg)
crop$consumed_kg <- ifelse(crop$consumed_kg+crop$sold_kg>crop$harvest_kg,
                           (1-psold)*crop$harvest_kg,
                           crop$consumed_kg)

crop <- crop[order(crop$hhid, crop$name),]
# dim(crop) #105271, 14

# add the listed crops
listcrop <- paste(tab$crops_all, tab$fruits_which, tab$vegetables_which, sep=" ")
# remove "indigenous" or "fruits"
listcrop <- gsub("indigenous", "", listcrop)
listcrop <- gsub("fruits", "", listcrop)

newcrop <- c()
for (i in seq_along(listcrop)){
  hi <- tab$hhid[i]
  # get simplified names
  si <- simplifynames(listcrop[[i]], names(conv_energy), warn=FALSE)
  # remove the crop that are already reported
  si <- si[!si%in%crop$name[crop$hhid%in%hi]]
  if (length(si)>0){
    ni <- data.frame(
      "hhid"=hi,
      "name"=si,
      "harvest_kg"=NA,
      "consumed_kg"=NA,
      "sold_kg"=NA,
      "income_lcu"=NA,
      "prop_land_area"=NA,
      "intercrop"=NA,
      "use_list"=NA,     
      "crop_residue_use"=NA,
      "control_consumed_female"=NA,
      "control_income_female"=NA,
      "prop_harvest"=NA,
      "land_area_ha"=NA 
    ) 
    newcrop <- rbind(newcrop, ni)
  }
}
rmcrop <- c("n_a","na", "no_answer", "none", "no_fruit", "no_veg", "")
newcrop <- newcrop[!newcrop$name%in%rmcrop,]
dim(newcrop) #217036 14

# simplifynames(newcrop$name, names(conv_energy), warn=TRUE)

crop <- rbind(crop, newcrop)
crop <- crop[order(crop$hhid, crop$name),]

#few little corrections:
crop$name <- gsub("^bean$", "beans", crop$name)
crop$name <- gsub("^soya$", "soya_bean", crop$name)
crop$name <- gsub("^ground_nut$", "ground_nuts", crop$name)

# 3. Create livestock table----------------------
looplstkcat <- gsub("^livestock_heads_", "",
                    names(tab)[grep("^livestock_heads_", names(tab))])
# remove other categories
rm <- c("other", "other_lstk", "other2_lstk",
        "other3_lstk")
looplstkcat <- looplstkcat[!looplstkcat%in%rm]

tdfLC <-  lapply(looplstkcat, function(x) {
  y <- data.frame(
    "hhid"= tab$hhid,
    "name"= x,
    "n"=tab[,paste0("livestock_heads_", x)])
  return(y)}
)

lstkA <- bind_rows(tdfLC)
lstkA <- lstkA[!is.na(lstkA$n),]

lstk <- lstkA[lstkA$n>0,]
lstk <- lstk[order(lstk$hhid, lstk$name),]

# 4. Create livestock production table ----------
# get the number of columns per livestock products
looplstk <- gsub("^meat_kg_per_year_", "",
                 names(tab)[grep("^meat_kg_per_year_", names(tab))])

lstk_meat <- data.frame(
  "hhid"=rep(tab$hhid, length(looplstk)),
  "name"= unlist(tab[,paste0("livestock_name_", looplstk)]),
  "prod"="meat",
  "harvest_kg"= NAto0(unlist(tab[,paste0("meat_kg_per_year_", looplstk)])),
  "consumed_kg"= NAto0(unlist(tab[,paste0("meat_consumed_kg_per_year_", looplstk)])),
  "sold_kg"= NAto0(unlist(tab[,paste0("meat_sold_kg_per_year_", looplstk)])),
  "income_lcu"= NAto0(unlist(tab[,paste0("meat_sold_income_", looplstk)])),
  "use_list"= unlist(tab[,paste0("meat_use_", looplstk)]),
  "male_adult_consumed"=NAto0(unlist(tab[,paste0("male_adult_meat_consumed_kg_per_year_", looplstk)])),
  "male_youth_consumed"=NAto0(unlist(tab[,paste0("male_youth_meat_consumed_kg_per_year_", looplstk)])),
  "female_adult_consumed"=NAto0(unlist(tab[,paste0("female_adult_meat_consumed_kg_per_year_", looplstk)])), 
  "female_youth_consumed"=NAto0(unlist(tab[,paste0("female_youth_meat_consumed_kg_per_year_", looplstk)])),
  "male_adult_income"=NAto0(unlist(tab[,paste0("male_adult_meat_sold_income_", looplstk)])),
  "male_youth_income"=NAto0(unlist(tab[,paste0("male_youth_meat_sold_income_", looplstk)])),
  "female_adult_income"=NAto0(unlist(tab[,paste0("female_adult_meat_sold_income_", looplstk)])),
  "female_youth_income"=NAto0(unlist(tab[,paste0("female_youth_meat_sold_income_", looplstk)]))
)

lstk_milk <- data.frame(
  "hhid"=rep(tab$hhid, length(looplstk)),
  "name"= unlist(tab[,paste0("livestock_name_", looplstk)]),
  "prod"="milk",
  "harvest_kg"= NAto0(unlist(tab[,paste0("milk_collected_litres_per_year_", looplstk)])),
  "consumed_kg"= NAto0(unlist(tab[,paste0("milk_consumed_litres_per_year_", looplstk)])),
  "sold_kg"= NAto0(unlist(tab[,paste0("milk_sold_litres_per_year_", looplstk)])),
  "income_lcu"= NAto0(unlist(tab[,paste0("milk_sold_income_per_year_", looplstk)])),
  "use_list"= unlist(tab[,paste0("milk_use_", looplstk)]),
  "male_adult_consumed"=NAto0(unlist(tab[,paste0("male_adult_milk_consumed_litres_per_year_", looplstk)])),
  "male_youth_consumed"=NAto0(unlist(tab[,paste0("male_youth_milk_consumed_litres_per_year_", looplstk)])),
  "female_adult_consumed"=NAto0(unlist(tab[,paste0("female_adult_milk_consumed_litres_per_year_", looplstk)])), 
  "female_youth_consumed"=NAto0(unlist(tab[,paste0("female_youth_milk_consumed_litres_per_year_", looplstk)])),
  "male_adult_income"=NAto0(unlist(tab[,paste0("male_adult_milk_sold_income_per_year_", looplstk)])),
  "male_youth_income"=NAto0(unlist(tab[,paste0("male_youth_milk_sold_income_per_year_", looplstk)])),
  "female_adult_income"=NAto0(unlist(tab[,paste0("female_adult_milk_sold_income_per_year_", looplstk)])),
  "female_youth_income"=NAto0(unlist(tab[,paste0("female_youth_milk_sold_income_per_year_", looplstk)]))
)

lstk_eggs <- data.frame(
  "hhid"=rep(tab$hhid, length(looplstk)),
  "name"= unlist(tab[,paste0("livestock_name_", looplstk)]),
  "prod"="eggs",
  "harvest_kg"= NAto0(unlist(tab[,paste0("eggs_collected_kg_per_year_", looplstk)])),
  "consumed_kg"= NAto0(unlist(tab[,paste0("eggs_consumed_kg_per_year_", looplstk)])),
  "sold_kg"= NAto0(unlist(tab[,paste0("eggs_sold_kg_per_year_", looplstk)])),
  "income_lcu"= NAto0(unlist(tab[,paste0("eggs_income_per_year_", looplstk)])),
  "use_list"= unlist(tab[,paste0("eggs_use_", looplstk)]),
  "male_adult_consumed"=NAto0(unlist(tab[,paste0("male_adult_eggs_consumed_kg_per_year_", looplstk)])),
  "male_youth_consumed"=NAto0(unlist(tab[,paste0("male_youth_eggs_consumed_kg_per_year_", looplstk)])),
  "female_adult_consumed"=NAto0(unlist(tab[,paste0("female_adult_eggs_consumed_kg_per_year_", looplstk)])), 
  "female_youth_consumed"=NAto0(unlist(tab[,paste0("female_youth_eggs_consumed_kg_per_year_", looplstk)])),
  "male_adult_income"=NAto0(unlist(tab[,paste0("male_adult_eggs_income_per_year_", looplstk)])),
  "male_youth_income"=NAto0(unlist(tab[,paste0("male_youth_eggs_income_per_year_", looplstk)])),
  "female_adult_income"=NAto0(unlist(tab[,paste0("female_adult_eggs_income_per_year_", looplstk)])),
  "female_youth_income"=NAto0(unlist(tab[,paste0("female_youth_eggs_income_per_year_", looplstk)]))
)


lstk_honey <- data.frame(
  "hhid"=rep(tab$hhid, length(looplstk)),
  "name"= unlist(tab[,paste0("livestock_name_", looplstk)]),
  "prod"="honey",
  "harvest_kg"= NAto0(unlist(tab[,paste0("bees_honey_kg_per_year_", looplstk)])),
  "consumed_kg"= NAto0(unlist(tab[,paste0("bees_honey_consumed_kg_per_year_", looplstk)])),
  "sold_kg"= NAto0(unlist(tab[,paste0("bees_honey_sold_kg_per_year_", looplstk)])),
  "income_lcu"= NAto0(unlist(tab[,paste0("bees_honey_sold_income_", looplstk)])),
  "use_list"= unlist(tab[,paste0("bees_honey_use_", looplstk)]),
  "male_adult_consumed"=NAto0(unlist(tab[,paste0("male_adult_bees_honey_consumed_kg_per_year_", looplstk)])),
  "male_youth_consumed"=NAto0(unlist(tab[,paste0("male_youth_bees_honey_consumed_kg_per_year_", looplstk)])),
  "female_adult_consumed"=NAto0(unlist(tab[,paste0("female_adult_bees_honey_consumed_kg_per_year_", looplstk)])), 
  "female_youth_consumed"=NAto0(unlist(tab[,paste0("female_youth_bees_honey_consumed_kg_per_year_", looplstk)])),
  "male_adult_income"=NAto0(unlist(tab[,paste0("male_adult_bees_honey_sold_income_", looplstk)])),
  "male_youth_income"=NAto0(unlist(tab[,paste0("male_youth_bees_honey_sold_income_", looplstk)])),
  "female_adult_income"=NAto0(unlist(tab[,paste0("female_adult_bees_honey_sold_income_", looplstk)])),
  "female_youth_income"=NAto0(unlist(tab[,paste0("female_youth_bees_honey_sold_income_", looplstk)]))
)


kgperanimal <-  conv_tlu[unlist(tab[,paste0("livestock_name_", looplstk)])]*250

lstk_whole <- data.frame(
  "hhid"=rep(tab$hhid, length(looplstk)),
  "name"= unlist(tab[,paste0("livestock_name_", looplstk)]),
  "prod"="whole",
  "harvest_kg"= NAto0(unlist(tab[,paste0("livestock_sold_", looplstk)]))*kgperanimal,
  "consumed_kg"= 0,
  "sold_kg"= NAto0(unlist(tab[,paste0("livestock_sold_", looplstk)]))*kgperanimal,
  "income_lcu"= NAto0(unlist(tab[,paste0("livestock_sale_income_", looplstk)])),
  "use_list"= "sell",
  "male_adult_consumed"=0,
  "male_youth_consumed"=0,
  "female_adult_consumed"=0, 
  "female_youth_consumed"=0,
  "male_adult_income"=NAto0(unlist(tab[,paste0("male_adult_livestock_sale_income_", looplstk)])),
  "male_youth_income"=NAto0(unlist(tab[,paste0("male_youth_livestock_sale_income_", looplstk)])),
  "female_adult_income"=NAto0(unlist(tab[,paste0("female_adult_livestock_sale_income_", looplstk)])),
  "female_youth_income"=NAto0(unlist(tab[,paste0("female_youth_livestock_sale_income_", looplstk)]))
)

lstk_meat <- lstk_meat[lstk_meat$harvest_kg>0,]
lstk_eggs <- lstk_eggs[lstk_eggs$harvest_kg>0,]
lstk_milk<- lstk_milk[lstk_milk$harvest_kg>0,]
lstk_honey <- lstk_honey[lstk_honey$harvest_kg>0,]
lstk_whole <- lstk_whole[lstk_whole$harvest_kg>0,]
lstk_prod <- rbind(lstk_meat, lstk_eggs, lstk_milk,lstk_honey, lstk_whole)

# calculate gender control percentages
# female control over consumption
col_consAll <- names(lstk_prod)[grep("_consumed", names(lstk_prod))]
consAll <- rowSums(lstk_prod[,col_consAll], na.rm = TRUE)

col_consFem <- col_consAll[grep("female", col_consAll)]
consFem <- rowSums(lstk_prod[,col_consFem], na.rm = TRUE)

lstk_prod$control_consumed_female <- ifelse(consAll<=0, NA, consFem/consAll*100)

# female control over income
col_sellAll <- names(lstk_prod)[grep("_income", names(lstk_prod))]
sellAll <- rowSums(lstk_prod[,col_sellAll], na.rm = TRUE)

col_sellFem <- col_sellAll[grep("female", col_sellAll)]
sellFem <- rowSums(lstk_prod[,col_sellFem], na.rm = TRUE)

lstk_prod$control_income_female <- ifelse(sellAll<=0, NA, sellFem/sellAll*100)

#remove the gender temporary variables
lstk_prod <- lstk_prod[,-match(c(col_consAll, col_sellAll), names(lstk_prod))]

lstk_prod <- lstk_prod[order(lstk_prod$hhid, lstk_prod$name),]


# 5. Create household table ---------------------

# food and diet
listmonth <- strsplit(tab$foodshortagetime_months_which, " ")
listmonth <- sapply(listmonth, uniqueNA)
foodshortage_months<-sapply(listmonth, paste, collapse = " ")
foodshortage_count<-sapply(listmonth, length)


ifun <- function(x) {
  if (sum(is.na(x))==length(x)){
    return(NA)
  } else {
    return(max(unlist(convtime[unlist(x)]), na.rm = TRUE))
  }
}

dietdiv_bad_season <- matrix(NA, nrow = nrow(tab), ncol = length(ten_groups))
dietdiv_last_month <- matrix(NA, nrow = nrow(tab), ncol = length(ten_groups))
farmdietdiv_bad_season <- matrix(NA, nrow = nrow(tab), ncol = length(ten_groups))
farmdietdiv_last_month <- matrix(NA, nrow = nrow(tab), ncol = length(ten_groups))

for (i in seq_along(ten_groups)){
  if (length(ten_groups[[i]])>1){
    temp_BS <- apply(tab[,paste(ten_groups[[i]], "bad_season", sep="_")],1, ifun)
    temp_LM <- apply(tab[,paste(ten_groups[[i]], "last_month", sep="_")],1, ifun)
  } else {
    temp_BS <- unlist(ifelse(is.na(tab[,paste(ten_groups[[i]], "bad_season", sep="_")]), NA,
                             convtime[tab[,paste(ten_groups[[i]], "bad_season", sep="_")]]))
    temp_LM <- unlist(ifelse(is.na(tab[,paste(ten_groups[[i]], "last_month", sep="_")]), NA,
                             convtime[tab[,paste(ten_groups[[i]], "last_month", sep="_")]]))

  }
  temp_FBS <- temp_BS%in%c(1) & grepl("on-farm", tab[,paste(ten_groups[[i]], "source_bad", sep="_")])
  temp_FLM <- temp_LM%in%c(1) & grepl("on-farm", tab[,paste(ten_groups[[i]], "source_last_month", sep="_")])
  
  dietdiv_bad_season[,i]  <- temp_BS
  dietdiv_last_month[,i]  <- temp_LM
  farmdietdiv_bad_season[,i]  <- as.numeric(temp_FBS)
  farmdietdiv_last_month[,i]  <- as.numeric(temp_FLM)
}
colnames(dietdiv_bad_season) <- names(ten_groups)
colnames(dietdiv_last_month) <- names(ten_groups)

#merge hdds: bad season if available, else last month
hdds_bad_season <- rowSums(dietdiv_bad_season, na.rm=TRUE)
hdds_last_month <- rowSums(dietdiv_last_month, na.rm=TRUE)
dietdiv <- dietdiv_bad_season
farmdietdiv <- farmdietdiv_bad_season
nobad <- rowSums(dietdiv, na.rm=TRUE)==0
dietdiv[nobad,] <- dietdiv_last_month[nobad,]
farmdietdiv[nobad,] <- farmdietdiv_last_month[nobad,]
hdds_score <- rowSums(dietdiv, na.rm=TRUE)
farm_hdds_score<- rowSums(farmdietdiv, na.rm=TRUE)

hdds_score[hdds_score==0] <- NA
hdds_bad_season[hdds_bad_season==0] <- NA
hdds_last_month[hdds_last_month==0] <- NA
farm_hdds_score[is.na(hdds_score)] <- NA

colnames(farmdietdiv) <- gsub("^HDDS_", "FARMHDDS_", names(ten_groups))

#FIES (Food Security Experience)
fies <- tab[,paste0("fies_", 1:8)]
fies <- apply(fies=="y",2,as.numeric)
colnames(fies) <- fies_lab
# table(rowSums(fies),ind$fies_score)

colH <- c("hhid", "id_form", "year", "country",
          "region", "sublocation")
hhinfo <- data.frame(
  tab[,colH],
  "gps_lat" = tab$gps_lat_rounded,
  "gps_lon" = tab$gps_lon_rounded,
  "currency_conversion_lcu_to_ppp"=tab$currency_conversion_lcu_to_ppp,
  "hh_size_mae"=NAto0(tab$hh_size_mae),
  "hh_size_members"=NAto0(tab$hh_size_members),
  "household_type"=tab$household_type,
  "land_owned_ha"=tab$land_owned_ha,
  "hfias_status" = tab$hfias_status,
  foodshortage_months,
  foodshortage_count,
  dietdiv,
  "hdds_score" = hdds_score,
  "hdds_bad_season"=hdds_bad_season,
  "hdds_last_month"=hdds_last_month,
  farmdietdiv,
  "farmhdds_score" = farm_hdds_score,
  fies,
  "fies_score" = ind$fies_score,
  "off_farm_lcu"=tab$off_farm_income_lcu_per_year,
  "off_farm_div"=tab$offfarm_incomes_count
)

# add other indicators from rhomis package
addind <- names(ind)[!names(ind) %in% names(hhinfo)][-(1:7)]
hhinfo <- cbind(hhinfo, ind[,addind])


# 6. Add GIS data -------------------------------

# Remove outliers
out1 <- hhinfo$id_form=="bf_gld_2018" & hhinfo$gps_lat==13.8 &hhinfo$gps_lon==8.9
out2 <- hhinfo$id_form=="bf_pra_2021" & hhinfo$gps_lat==20.9 &hhinfo$gps_lon==45.3
out3 <- hhinfo$id_form=="et_srl_2019" & hhinfo$gps_lat==14.3 &hhinfo$gps_lon==4
out4 <- hhinfo$id_form=="ma_crd_2019" & hhinfo$gps_lat==51.4 &hhinfo$gps_lon==-2.4

hhinfo$gps_lat[out1|out2|out3|out4] <- NA
hhinfo$gps_lon[out1|out2|out3|out4] <- NA


p <- vect(cbind(hhinfo$gps_lon, hhinfo$gps_lat))

# Farming systems from Dixon
fs <- rast("Data/GIS/cell5m_Agroecology_FarmingSystems_FS_2012_TX.tif")
activeCat(fs) <- 2 #to get the full name of AGROECOLOG
fsp <- terra::extract(fs, p)
#replace "" by NA
fsp$AGROECOLOG[fsp$AGROECOLOG==""] <- NA
table(fsp$AGROECOLOG, useNA="ifany")
hhinfo$farming_system <- as.character(fsp$AGROECOLOG)

# Population
# pop <- rast("Data/GIS/gpw_v4_population_density_rev11_2020_30s.tif")
# popp <- terra::extract(pop, p)$gpw_v4_population_density_rev11_2020_30_sec
# hhinfo$population_2020 <- popp
hhinfo$population_2020 <- NA

# Travel time to cities
# ttc <- rast("Data/GIS/travel_time_to_cities_u6.tif")
# ttcp <- terra::extract(ttc, p)$travel_time_to_cities_1
# hhinfo$travel_time_cities <- ttcp
hhinfo$travel_time_cities <- NA

#koeppen
# kg <- rast("Data/GIS/Beck_KG_V1_present_0p0083.tif")
# kgp <- terra::extract(kg, p)$Beck_KG_V1_present_0p0083
# remove household on sea
# kgp[kgp==0] <- NA
# plot(kg, col=c("black", "red"), breaks=c(-0.5,0.5,30.5))
# hhinfo$koeppen <- conv_kg[kgp]
hhinfo$koeppen <- NA

# harmonize country name
#table(hhinfo$country,substr(hhinfo$id_form, 1,2))
hhinfo$country[hhinfo$country%in% "burkina"] <- "burkina_faso"
hhinfo$country[hhinfo$country%in% "bi"] <- "burundi"
hhinfo$country[hhinfo$country%in% "country_name"] <- "mali"
# colSums(table(hhinfo$country,substr(hhinfo$id_form, 1,2))>0)

hhinfo$large_region <- large[hhinfo$country]
# table(hhinfo$large_region, useNA="ifany")


# 7. Post processing ---------------------

# add summary of farm production

# check the name of the crop and livestock and find best matching one
crop$name <- bestname(crop$name, names(conv_energy), warn = TRUE)
lstk$name <- bestname(lstk$name, names(conv_tlu), warn = TRUE)
lstk_prod$name <- bestname(lstk_prod$name, names(conv_tlu), warn = TRUE)
lstk_prod$prod <- bestname(lstk_prod$prod, names(conv_energy), warn = TRUE)

# calculate crop and livestock summary characteristics
hhinfo <- calc_farm_prod(crop, lstk, lstk_prod, hhinfo, conv_tlu, conv_energy)

# Detect and remove abnormally large values
outliers <- hhinfo$hh_size_members>100 | hhinfo$hh_size_members==0
# table(hhinfo$id_form[outliers])
# many from vn_nt2_2019, zm_fa2_2019, ph_usm_2022
# Replace outliers by NA
hhinfo$hh_size_members[outliers] <- NA
hhinfo$hh_size_mae[outliers] <- NA

outliers <- NAto0(hhinfo$land_cultivated_ha)>500
# table(hhinfo$id_form[outliers])
hhinfo$land_cultivated_ha[outliers] <- NA

outliers <- NAto0(hhinfo$livestock_tlu)>250
# table(hhinfo$id_form[outliers]) #tz_glv_2017 
hhinfo$livestock_tlu[outliers] <- NA

# Remove dataset with lots of NA
colI <- c("land_cultivated_ha", "livestock_tlu", "hh_size_members")
complete <- complete.cases(hhinfo[,colI])
# table(complete) #4017 households with incomplete information
ninc <- table(hhinfo$id_form, complete)
# print(sort(ninc[,1]/rowSums(ninc), decreasing = TRUE) [1:20])
rm <- row.names(ninc)[ninc[,1]/rowSums(ninc)>0.75]
# remove ph_usm_2022, vn_nt2_2019, and zm_fa2_2019
rmhh <- hhinfo$id_form %in% rm

#select rural household with crop or livestock
selhh <- hhinfo$hhid%in%crop$hhid | hhinfo$hhid%in%lstk$hhid | hhinfo$hhid%in%lstk_prod$hhid

# table(selhh, !rmhh)
hhinfo <- hhinfo[selhh & !rmhh,]
crop <- crop[crop$hhid %in%hhinfo$hhid,]
lstk <- lstk[lstk$hhid %in%hhinfo$hhid,]
lstk_prod <- lstk_prod[lstk_prod$hhid %in%hhinfo$hhid,]


# 8. Save dataset ---------------------
hhdb_rhomis <- farmhousehold(
  "crop"=crop, 
  "lstk"=lstk, 
  "lstk_prod"=lstk_prod, 
  "hhinfo"=hhinfo,
  "conv_tlu"=conv_tlu, 
  "conv_energy"=conv_energy
)

saveRDS(hhdb_rhomis,
        file="Data/HHDB_RHOMIS_02122024.rds", compress="xz")
