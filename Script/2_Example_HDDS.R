# Script to perform the analysis presented as example 1
# The household-level drivers of dietary diversity according to farming system
# Last update: 02/12/2024

# Load needed packages and functions
library(farmhousehold)
library(maps)
library(rcartocolor)
library(terra)
library(randomForest)

abc_label <- function(text, cex=NULL, xplus=0, ...) {
  
  ds <- dev.size("in")
  # xy coordinates of device corners in user coordinates
  x <- grconvertX(c(0, ds[1]), from="in", to="user")
  y <- grconvertY(c(0, ds[2]), from="in", to="user")
  
  # fragment of the device we use to plot
  # account for the fragment of the device that 
  # the figure is using
  fig <- par("fig")
  dx <- (x[2] - x[1])
  #dy <- (y[2] - y[1])
  x <- x[1] + dx * fig[1:2]
  #y <- y[1] + dy * fig[3:4]
  #check and extend to graph when necessary
  u <- par("usr")
  y <- u[3:4]
  
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
  
  x1 <- x[1] + sw
  y1 <- y[2] - sh
  
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
  
  text(x1+xplus, y1, text, cex=cex, ...)
  return(invisible(c(x,x1, y, y1)))
}

# Load RHoMIS dataset
rhomis <- readRDS("Data/HHDB_RHOMIS_02122024.rds")

# Load crop group information
groupcrop <- read.csv("Data/crop_param.csv")

# Load farming system map
fs <- rast("Data/GIS/cell5m_Agroecology_FarmingSystems_FS_2012_TX.tif")
activeCat(fs) <- 2 #to get the full name of AGROECOLOG

# resolution of the figure
ppi <- 300



## Household selection ---------------------------

# Select household in SSA
ssa <- c("burkina_faso", "burundi", "cote d'ivoire", "drc", 
         "ethiopia", "ghana", "kenya", "malawi", "mali", 
         "niger", "nigeria", "rwanda", "senegal", "sierra leone",
         "south africa", "tanzania", "uganda", "zambia")

db <- select_farmhousehold(rhomis,rhomis$hhinfo$hhid[rhomis$hhinfo$country %in% ssa])

# Rename and order farming system

# Merge Pastoral with Agro pastoral
db$hhinfo$farming_system <- gsub("12. Pastoral", "11. Agro-pastoral", db$hhinfo$farming_system)
# Merge artisanal fishing with root and tuber
db$hhinfo$farming_system <- gsub("14. Artisanal fishing", "7. Root and tuber crop", db$hhinfo$farming_system)
# Merge artisanal fishing with agro-pastoral
db$hhinfo$farming_system <- gsub("1. Irrigated", "11. Agro-pastoral", db$hhinfo$farming_system)
# remove the number of the farming system
db$hhinfo$farming_system <- gsub("^ ", "", substr(db$hhinfo$farming_system, 4, nchar(db$hhinfo$farming_system)))
# simplify the humid tree crop name
db$hhinfo$farming_system <- gsub("Humid lowland tree crop", "Tree crop", db$hhinfo$farming_system)
# order the farming systems
db$hhinfo$farming_system <- factor(db$hhinfo$farming_system,
                                levels=c("Agro-pastoral",
                                         "Cereal-root crop mixed",
                                         "Root and tuber crop",
                                         "Tree crop",
                                         "Highland mixed",
                                         "Maize mixed",
                                         "Highland perennial",
                                         "Forest based"),
                                ordered = TRUE)

# Removing outliers
# Land cultivated less than 100 ha
cond1 <- NAto0(db$hhinfo$land_cultivated_ha)<100
# Household size >0 
cond2 <- NAto0(db$hhinfo$hh_size_members)>0
# Complete GPS coordinates
cond3 <- !is.na(db$hhinfo$gps_lat) & !is.na(db$hhinfo$gps_lon)
# With information on HDDS 
cond4 <- !(is.na(db$hhinfo$hdds_score)|db$hhinfo$hdds_score==0)

# Removing projects with incomplete information
# more than 70% of complete data
nsel <- tapply(cond1&cond2&cond3&cond4, db$hhinfo$id_form, sum)
pcond1 <- nsel/table(db$hhinfo$id_form)>0.70
# some income off farm recorded
off <- tapply(NAto0(db$hhinfo$off_farm_lcu), db$hhinfo$id_form, sum)
pcond2 <- off>0
# some crop consumed recorded
ccons <- tapply(NAto0(db$hhinfo$crop_consumed_kcal), db$hhinfo$id_form, sum)
pcond3 <- ccons>0
# some crop income recorded
csold <- tapply(NAto0(db$hhinfo$crop_income_lcu), db$hhinfo$id_form, sum)
pcond4 <- csold>0

#list of project discarded
# names(pcond1)[!(pcond1&pcond2&pcond3&pcond4)] 
keepP <- names(pcond1)[(pcond1&pcond2&pcond3&pcond4)]
cond5 <- db$hhinfo$id_form%in%keepP

# removing households with unknown farming system
cond6 <- !is.na(db$hhinfo$farming_system)

db <- select_farmhousehold(db, db$hhinfo$hhid[cond1&cond2&cond3&cond4&cond5&cond6])

# nrow(db$hhinfo) #34653 households
# table(db$hhinfo$country)
# table(db$hhinfo$farming_system, useNA="ifany")


## Figure 9: Map of households and farming system ------------------------
# create the vector of household GPS coordinates
p <- vect(cbind(jitter(db$hhinfo$gps_lon, amount = 0.05), 
                jitter(db$hhinfo$gps_lat, amount = 0.05)))
dixon_pal <- c(
    "#c94026", #perennial mixed
    "#3c63ae", #fish
    "#faf8c4", #arid
    "#fce164", #pastoral
    "#e9c4dd", #agro pastoral
    "#a89bca", #maize
    "#fcac1b", #cereal root
    "#cf962b", #root tuber
    "#7861aa", #highland mixed
    "#93af3c", #highland perennial
    "#5c8a46", #forest based
    "#b8d433", #tree crop
    "#82d5f4", #irrigated
    "#ffffff" #null
)
fs_pal <- function(){
  return(dixon_pal[c(14,3,4,13,5,7,2,8,12,9,6,10, 11, 1)])
}

fs_lab <- c("Perennial mixed",
            "Forest based",
            "Highland perennial",
            "Maize mixed",
            "Highland mixed",
            "Tree crop",
            "Root and tuber crop",
            "Cereal-root crop mixed", 
            "Agro-pastoral",
            "Pastoral",
            "Arid")


png("Figures/Fig9_map_household.png", width=5.8*ppi, height = 5*ppi, res=ppi)
plot(fs, ylim=c(-37,27.8), xlim=c(-20,60),
     box=FALSE, legend=FALSE,
     xlab="Longitude (°E)", ylab="Latitude (°N)",
     col=fs_pal())
map("world", col="grey50",
    fill=FALSE, add=TRUE)
points(p, cex=0.5)
legend(-20, 0, legend = rev(fs_lab),cex = 0.7,
       fill = dixon_pal[c(3:5,7:8,12, 9,6, 10, 11, 1)], bty="n")
dev.off()


# Food group cultivated -------------------------
# replace empty character by NA
groupcrop$group[groupcrop$group%in%""] <- NA
# group as ordered factor
groupcrop$group <- as.factor(groupcrop$group)
groupcrop$group <- factor(groupcrop$group, 
                          levels = levels(groupcrop$group)[c(1,6:13,2:5)], ordered = TRUE)

# get the food group produced per household
db$crop$group <- groupcrop$group[match(db$crop$name, groupcrop$crop)]
foodgroup <- tapply(db$crop$name, list(db$crop$hhid, db$crop$group), lunique)
prodgroup <- foodgroup[match(db$hhinfo$hhid, row.names(foodgroup)),]
prodgroup[is.na(prodgroup)] <- 0

# get the livestock product
prodgroup[,4] <- as.numeric(db$hhinfo$hhid%in%db$lstk_prod$hhid[db$lstk_prod$prod%in%"milk"])
prodgroup[,5] <- as.numeric(db$hhinfo$hhid%in%db$lstk_prod$hhid[db$lstk_prod$prod%in%"meat"])
prodgroup[,6] <- as.numeric(db$hhinfo$hhid%in%db$lstk_prod$hhid[db$lstk_prod$prod%in%"eggs"])

row.names(prodgroup) <- db$hhinfo$hhid
colnames(prodgroup) <- gsub("^ ", "", substr(gsub("_", " ", colnames(prodgroup)), 4, nchar(colnames(prodgroup))))

# get the food groups cultivated per household
farmdiv <- apply(prodgroup>0,2,as.numeric)
# keep only the 10 first food group
farmdiv <- farmdiv[,1:10]
# rename pulses by legumes
colnames(farmdiv) <- gsub("Pulses", "Legumes", colnames(farmdiv))

#get the column starting with "HDDS"
HDDS_col <- names(db$hhinfo)[grep("^HDDS_G", names(db$hhinfo))]
dietdiv <- db$hhinfo[,HDDS_col]


## Figure 2 : Diet and farm diversity -----------

# colors of the food group
palFG <- carto_pal(10, "Safe")

# diet diversity per farming system (2A)
nf <- table(db$hhinfo$farming_system)
ncat <- rowsum(dietdiv, db$hhinfo$farming_system, na.rm = TRUE)

# farm diversity per farming system (2B)
ncat2 <- rowsum(farmdiv, db$hhinfo$farming_system, na.rm = TRUE)

apply(t(ncat/as.numeric(nf)), 1, sd)
png("Figures/Fig2_scoresH.png", width=6*ppi, height = 4*ppi, res=ppi)
par(mfrow=c(1,2),mar=c(4,0.5,1,1), oma=c(0,9,0,7), cex=0.7)

barx <- barplot(t(ncat/as.numeric(nf)), col=palFG, las=1, horiz=TRUE,
                xlab="mean HDDS")
abc_label("A", cex=1.4, xplus = 4)
barx <- barplot(t(ncat2/as.numeric(nf)), col=palFG, las=1, horiz=TRUE,
                xlab="mean food groups produced on-farm", names=rep("", 8), xpd=NA)
abc_label("B", cex=1.4, xplus = 0.1)
legend(4,5, legend = rev(colnames(farmdiv)), 
       fill = rev(palFG), xpd=NA, bty="n")
dev.off()


## Table 2: percentage of food consumed from farm ---------

farm_hdds_bad <- db$hhinfo[,grep("^FARMHDDS_G", names(db$hhinfo))]
ncatfarm <- rowsum(farm_hdds_bad, db$hhinfo$farming_system, na.rm = TRUE)
# calculate the percentage of household food group that come from farm
perc_farm <- round(ncatfarm/ncat*100)
colnames(perc_farm) <- gsub("^FARMHDDS ", "", 
                            gsub("_", " ", colnames(perc_farm)))

write.csv(perc_farm, file = "Figures/Table2_DietFarm_percentage.csv")




# Random forest predicting diet diversity -----------------

#selection of variables
sel <- c("hh_size_mae","land_cultivated_ha", "livestock_tlu",
         "crop_yield_kg_per_ha", "lstk_yield_kg_per_tlu", 
         "farm_div", "income_per_person_per_day_usd", "off_farm_perc", "farm_sold_perc_kg", 
         "farm_income_div", "hdds_score")

df <- db$hhinfo[,sel]
#remove NAs (temporal, might be better to remove households)
df <- as.data.frame(apply(df,2, NAto0))
# rename columns
newnames <- c("family size","land size", "herd size",
              "crop yield", "lstk yield", 
              "farm diversity", "income", "off farm", "market", 
              "income diversity")

# Check the skewness
skewV <- apply(df, 2, moments::skewness, na.rm=TRUE)
# Transform the highly skewed variables
root <- cut(skewV, breaks = c(min(skewV), 2, 10, 30, max(skewV)),
            include.lowest = TRUE, labels = c("1", "0.5", "0.25", "0.125"))

root <- as.numeric(as.character(root))
for (i in 1:ncol(df)){
  df[,i] <- df[,i]**(root[i])
  if (root[i] <1){
    print(paste0(names(df)[i], " is ", 
                 root[i], "square root transformed"))
  }
}

impMSE_fs <- c()
impINP_fs <- c()
mse <- c()
rsq_fs <- c()
for (i in sort(unique(db$hhinfo$farming_system))){
  # select data for darming system i
  dfi <- df[db$hhinfo$farming_system==i,]
  
  # compute random forest, predicting hdds score
  rfi <- randomForest(hdds_score~., data=dfi, importance=TRUE)
  
  # keep track of MSE and R2 : model performance
  mse <- c(mse, rfi$mse[length(rfi$mse)])
  rsq_fs <- c(rsq_fs, rfi$rsq[length(rfi$rsq)]*100)
  
  # keep nude purity and decrease in MSE as variable importance 
  impMSE_fs <- cbind(impMSE_fs, rfi$importance[,1])
  impINP_fs <- cbind(impINP_fs, rfi$importance[,2])
}


## Figure 3 -------------------------------------
maxi <- apply(impMSE_fs,2,max)
pimp <- t(t(impMSE_fs)/maxi)
meanimp <- apply(pimp,1,mean)
ord <- order(meanimp, decreasing = FALSE)
yseq <- seq(-0.4, 0.4, length.out=ncol(pimp))

# rename variable for graphs
newlab <- c("family size","land size", "herd size",
            "crop yield", "livestock yield", 
            "farm diversity", "income", "off farm", "market", 
            "income diversity")

# match farming system colors as in map Fig 9
palFS <- c("#5c8a46", "#93af3c",
           "#a89bca", "#7861aa", "#b8d433",
           "#cf962b", "#fcac1b", "#e9c4dd")

pchFS <- rep(c(15:18),2)

png("Figures/Fig3_randomforest_mse.png", width=7*ppi, height = 6*ppi, res=ppi)
par(mar=c(4,8,1,1),yaxs="i", cex=1.2)
plot(pimp[ord,1], 1:nrow(pimp)+yseq[1], xlim=c(0,1), 
     ylim=c(0.5, nrow(pimp)+0.5), xlab="Variable importance (MSE)",
     yaxt="n", ylab="", pch=16, col=palFS[1])
rect(-0.1, 1:nrow(pimp)-0.5, 1.1, 1:nrow(pimp)+0.5, border = NA,
     col=c("white", "grey95"))
abline(h=c(1:nrow(pimp)), lty=2)
box()
for (j in 1:ncol(pimp)){
  points(ifelse(pimp[ord,j]<0, 0, pimp[ord,j]), 1:nrow(pimp)+yseq[j], 
         pch=pchFS[j], col=palFS[j], xpd=NA, cex=rep(c(rep(1.2,3),1.4),2))
}
axis(2, at=1:nrow(pimp), labels = newlab[ord], las=2)
legend("bottomright", legend = rev(sort(unique(db$hhinfo$farming_system))), pch=rev(pchFS),
       col = rev(palFS), xpd=NA, cex=0.8, pt.cex=rep(c(rep(1.2,3),1.4),2),
       title="Farming system")
dev.off()
