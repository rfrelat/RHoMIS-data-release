# Script to perform the analysis presented as example 2
# Gender control over agricultural production and income
# Last update: 02/12/2024


# Load libraries, functions and data ------------
library(ggplot2)
library(ggpubr)
library(lme4)
library(rhomis)
library(farmhousehold)

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
nan0 <- function(x) {
  x[is.nan(x)] <- 0
  return(x)
}
nan100 <- function(x) {
  x[is.nan(x)] <- 100
  return(x)
}  


# Load RHoMIS dataset
rhomis <- readRDS("Data/HHDB_RHOMIS_02122024.rds")


# Select households ----------------------

# Select household in West or East Africa or Asia
keep <- c("Eastern Africa", "South+East Asia", "Western Africa")
db <- select_farmhousehold(rhomis,rhomis$hhinfo$hhid[rhomis$hhinfo$large_region %in% keep])


# Classify household by gender control
# sum the proportion of female adult and youth
prop_female <- rowSums(db$hhinfo[,c("proportion_of_value_controlled_female_adult", 
                     "proportion_of_value_controlled_female_youth")], na.rm = TRUE)
# check that the sum of proportions is equal to 1
prop_all <- rowSums(db$hhinfo[,grep("proportion_of_value_controlled_", names(db$hhinfo))], na.rm = TRUE)

# get the proportion of values controlled by female
db$hhinfo$prop_female <- ifelse(prop_all<0.99 | prop_all>1.01, NA, prop_female)
# classify the households 
db$hhinfo$gender_control <- cut(db$hhinfo$prop_female, breaks = c(0,0.49,0.51, 1), 
                          labels = c("male", "parity", "female"),
                          include.lowest = TRUE)
table(db$hhinfo$gender_control, useNA="ifany")


# calculate the total income in usd
db$hhinfo$total_income_usd_per_year <- db$hhinfo$total_income_lcu_per_year/db$hhinfo$currency_conversion_lcu_to_ppp
# Set NA to households that earn less than one dollar per year or more than 500.000
outlier_income <- db$hhinfo$total_income_usd_per_year<1| db$hhinfo$total_income_usd_per_year>500000
db$hhinfo$total_income_usd_per_year[outlier_income] <- NA
# log transform the total income
db$hhinfo$log_total_income_usd_per_year <- log(db$hhinfo$total_income_usd_per_year)

# remove households that are not in couple
# table(db$hhinfo$household_type)
keeptype <- c("couple", "together", "married_monogamous", "couple_polygamous", "married_polygamous", "polygamous")
db$hhinfo$household_type[!db$hhinfo$household_type%in%keeptype] <- NA

# remove households with incomplete information
complete_col <- c("gender_control", "log_total_income_usd_per_year", "household_type")
keephh <- complete.cases(db$hhinfo[,complete_col])

dat_ind <- db$hhinfo[keephh,]
db_ind <- select_farmhousehold(db,dat_ind$hhid)


# Figure 4 : crop gender control ----------------

# define crop of interest
intcrop <- c("beans", "tomato", "onion", "field_pea", "cassava",
             "sweet_potato", "sorghum", "cowpea", "soya_bean",
             "rice", "sesame", "groundnut", "millet", "maize")

#select the crop information
crop <- db_ind$crop[db_ind$crop$name %in% intcrop,]
# add the region
crop$large_region <- dat_ind$large_region[match(crop$hhid, dat_ind$hhid)]

# calculate the average female control on consumption per crop and region
crop_consume_gender <- tapply(crop$control_consumed_female, list(crop$name, crop$large_region), mean, na.rm=TRUE)
crop_income_gender <- tapply(crop$control_income_female, list(crop$name, crop$large_region), mean, na.rm=TRUE)
#for checking robustness: number of values
n_crop_consume_gender <- tapply(!is.na(crop$control_income_female), list(crop$name, crop$large_region), sum, na.rm=TRUE)
n_crop_income_gender <- tapply(!is.na(crop$control_income_female), list(crop$name, crop$large_region), sum, na.rm=TRUE)

# remove values if only from one household
crop_consume_gender[n_crop_consume_gender<=1] <- NaN
crop_income_gender[n_crop_income_gender<=1] <- NaN


#reorder the crop
ordC <- match(rev(intcrop), row.names(crop_consume_gender))
crop_consume_gender <- crop_consume_gender[ordC,]
crop_income_gender <- crop_income_gender[ordC,]
n_crop_consume_gender <- n_crop_consume_gender[ordC,]
n_crop_income_gender <- n_crop_income_gender[ordC,]

png("Figures/Figure4_Gender_crop.png", width=2000, height = 2200, res = 300)

par(mar=c(4,6,1,1), oma=c(0,2,1,0),mfrow=c(3,2))
barplot(sapply(crop_consume_gender[,1], nan0), horiz=TRUE, xaxt="n",
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", row.names(crop_consume_gender)), las=1)
barplot(sapply(crop_consume_gender[,1], nan100)-100, horiz=TRUE, 
        add=TRUE, xaxt="n", col="#D55E00", names.arg=NA)
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("Crop consumption", side = 3, line = 0.5)
mtext("East Africa", side = 2, line = 6)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("A", cex=1.4, xplus = 20)

barplot(sapply(crop_income_gender[,1], nan0), horiz=TRUE, xaxt="n",
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", row.names(crop_income_gender)), las=1)
barplot(sapply(crop_income_gender[,1], nan0)-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n", names.arg=NA)
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("Crop income", side = 3, line = 0.5)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("B", cex=1.4, xplus = 15)

barplot(sapply(crop_consume_gender[,3], nan0), horiz=TRUE, xaxt="n", 
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", row.names(crop_consume_gender)), las=1)
barplot(sapply(crop_consume_gender[,3], nan100)-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n", names.arg=NA)
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("West Africa", side = 2, line = 6)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("C", cex=1.4, xplus = 20)

barplot(sapply(crop_income_gender[,3], nan0), horiz=TRUE, xaxt="n", 
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", row.names(crop_income_gender)), las=1)
barplot(sapply(crop_income_gender[,3], nan100)-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n", names.arg=NA)
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("D", cex=1.4, xplus = 15)

barplot(sapply(crop_consume_gender[,2], nan0), horiz=TRUE, xaxt="n", 
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", row.names(crop_consume_gender)), las=1)
barplot(sapply(crop_consume_gender[,2], nan100)-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n", names.arg=NA)
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("South + SE Asia", side = 2, line = 6)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("E", cex=1.4, xplus = 20)


barplot(sapply(crop_income_gender[,2], nan0), horiz=TRUE, xaxt="n", 
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", row.names(crop_income_gender)), las=1)
barplot(sapply(crop_income_gender[,2], nan100)-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n", names.arg=NA)
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("F", cex=1.4, xplus = 15)

dev.off()


# Figure 5 livestock gender control -----------------------
#select the crop information
lstk_prod <- db_ind$lstk_prod
# add the region
lstk_prod$large_region <- as.factor(dat_ind$large_region[match(lstk_prod$hhid, dat_ind$hhid)])

# make selection per livestock product
seleggs <- lstk_prod$prod=="eggs"
selmilk <- lstk_prod$prod=="milk"
selchmeat <- lstk_prod$prod=="meat" & lstk_prod$name=="chicken"
selpimeat <- lstk_prod$prod=="meat" & lstk_prod$name=="pigs"
selshmeat <- lstk_prod$prod=="meat" & lstk_prod$name=="sheep"
selgomeat <- lstk_prod$prod=="meat" & lstk_prod$name=="goats"
selcameat <- lstk_prod$prod=="meat" & lstk_prod$name=="cattle"
selchwhole <- lstk_prod$prod=="whole" & lstk_prod$name=="chicken"
selpiwhole <- lstk_prod$prod=="whole" & lstk_prod$name=="pigs"
selshwhole <- lstk_prod$prod=="whole" & lstk_prod$name=="sheep"
selgowhole <- lstk_prod$prod=="whole" & lstk_prod$name=="goats"
selcawhole <- lstk_prod$prod=="whole" & lstk_prod$name=="cattle"


#calculate average per product and per region
consume_gender <- data.frame(
  "eggs"=tapply(lstk_prod$control_consumed_female[seleggs], lstk_prod$large_region[seleggs], mean, na.rm=TRUE),
  "milk"=tapply(lstk_prod$control_consumed_female[selmilk], lstk_prod$large_region[selmilk], mean, na.rm=TRUE),
  "chicken_meat"=tapply(lstk_prod$control_consumed_female[selchmeat], lstk_prod$large_region[selchmeat], mean, na.rm=TRUE),
  "pig_meat"=tapply(lstk_prod$control_consumed_female[selpimeat], lstk_prod$large_region[selpimeat], mean, na.rm=TRUE),
  "sheep_meat"=tapply(lstk_prod$control_consumed_female[selshmeat], lstk_prod$large_region[selshmeat], mean, na.rm=TRUE),
  "goat_meat"=tapply(lstk_prod$control_consumed_female[selgomeat], lstk_prod$large_region[selgomeat], mean, na.rm=TRUE),
  "cattle_meat"=tapply(lstk_prod$control_consumed_female[selcameat], lstk_prod$large_region[selcameat], mean, na.rm=TRUE)
)

income_gender <- data.frame(
  "goat_meat"=tapply(lstk_prod$control_income_female[selgomeat], lstk_prod$large_region[selgomeat], mean, na.rm=TRUE),
  "cattle_meat"=tapply(lstk_prod$control_income_female[selcameat], lstk_prod$large_region[selcameat], mean, na.rm=TRUE),
  "eggs"=tapply(lstk_prod$control_income_female[seleggs], lstk_prod$large_region[seleggs], mean, na.rm=TRUE),
  "milk"=tapply(lstk_prod$control_income_female[selmilk], lstk_prod$large_region[selmilk], mean, na.rm=TRUE),
  "live_chicken"=tapply(lstk_prod$control_income_female[selchwhole], lstk_prod$large_region[selchwhole], mean, na.rm=TRUE),
  "live_pig"=tapply(lstk_prod$control_income_female[selpiwhole], lstk_prod$large_region[selpiwhole], mean, na.rm=TRUE),
  "live_sheep"=tapply(lstk_prod$control_income_female[selshwhole], lstk_prod$large_region[selshwhole], mean, na.rm=TRUE),
  "live_goat"=tapply(lstk_prod$control_income_female[selgowhole], lstk_prod$large_region[selgowhole], mean, na.rm=TRUE),
  "live_cattle"=tapply(lstk_prod$control_income_female[selcawhole], lstk_prod$large_region[selcawhole], mean, na.rm=TRUE)
)

#check the number of information
n_consume <- data.frame(
  "eggs"=tapply(!is.na(lstk_prod$control_consumed_female[seleggs]), lstk_prod$large_region[seleggs], sum, na.rm=TRUE),
  "milk"=tapply(!is.na(lstk_prod$control_consumed_female[selmilk]), lstk_prod$large_region[selmilk], sum, na.rm=TRUE),
  "chicken_meat"=tapply(!is.na(lstk_prod$control_consumed_female[selchmeat]), lstk_prod$large_region[selchmeat], sum, na.rm=TRUE),
  "pig_meat"=tapply(!is.na(lstk_prod$control_consumed_female[selpimeat]), lstk_prod$large_region[selpimeat], sum, na.rm=TRUE),
  "sheep_meat"=tapply(!is.na(lstk_prod$control_consumed_female[selshmeat]), lstk_prod$large_region[selshmeat], sum, na.rm=TRUE),
  "goat_meat"=tapply(!is.na(lstk_prod$control_consumed_female[selgomeat]), lstk_prod$large_region[selgomeat], sum, na.rm=TRUE),
  "cattle_meat"=tapply(!is.na(lstk_prod$control_consumed_female[selcameat]), lstk_prod$large_region[selcameat], sum, na.rm=TRUE)
)

n_income <- data.frame(
  "goat_meat"=tapply(!is.na(lstk_prod$control_income_female[selgomeat]), lstk_prod$large_region[selgomeat], sum, na.rm=TRUE),
  "cattle_meat"=tapply(!is.na(lstk_prod$control_income_female[selcameat]), lstk_prod$large_region[selcameat], sum, na.rm=TRUE),
  "eggs"=tapply(!is.na(lstk_prod$control_income_female[seleggs]), lstk_prod$large_region[seleggs], sum, na.rm=TRUE),
  "milk"=tapply(!is.na(lstk_prod$control_income_female[selmilk]), lstk_prod$large_region[selmilk], sum, na.rm=TRUE),
  "live_chicken"=tapply(!is.na(lstk_prod$control_income_female[selchwhole]), lstk_prod$large_region[selchwhole], sum, na.rm=TRUE),
  "live_pig"=tapply(!is.na(lstk_prod$control_income_female[selpiwhole]), lstk_prod$large_region[selpiwhole], sum, na.rm=TRUE),
  "live_sheep"=tapply(!is.na(lstk_prod$control_income_female[selshwhole]), lstk_prod$large_region[selshwhole], sum, na.rm=TRUE),
  "live_goat"=tapply(!is.na(lstk_prod$control_income_female[selgowhole]), lstk_prod$large_region[selgowhole], sum, na.rm=TRUE),
  "live_cattle"=tapply(!is.na(lstk_prod$control_income_female[selcawhole]), lstk_prod$large_region[selcawhole], sum, na.rm=TRUE)
)


png("Figures/Figure5_Gender_livestock.png", width=2000, height = 2000, res = 300)

par(mar=c(4,6,1,1), oma=c(0,2,1,0),mfrow=c(3,2))
barplot(as.numeric(consume_gender[1,]), horiz=TRUE, xaxt="n",
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", names(consume_gender)), las=1)
barplot(as.numeric(consume_gender[1,])-100, horiz=TRUE, add=TRUE, 
        col="#D55E00", , xaxt="n")
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("Livestock consumption", side = 3, line = 0.5)
mtext("East Africa", side = 2, line = 6)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("A", cex=1.4, xplus = 20)

barplot(as.numeric(income_gender[1,]), horiz=TRUE, xaxt="n",
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", names(income_gender)), las=1)
barplot(as.numeric(income_gender[1,])-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n")
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("Livestock income", side = 3, line = 0.5)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("B", cex=1.4, xplus = 15)

barplot(as.numeric(consume_gender[3,]), horiz=TRUE, xaxt="n",
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", names(consume_gender)), las=1)
barplot(as.numeric(consume_gender[3,])-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n")
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("West Africa", side = 2, line = 6)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("C", cex=1.4, xplus = 20)

barplot(as.numeric(income_gender[3,]), horiz=TRUE, xaxt="n", 
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", names(income_gender)), las=1)
barplot(as.numeric(income_gender[3,])-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n")
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("D", cex=1.4, xplus = 15)

#because there are NaN, better use sapply and replace them by 0 or 100
barplot(sapply(consume_gender[2,], nan0), horiz=TRUE, xaxt="n", 
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", names(consume_gender)), las=1)
barplot(sapply(consume_gender[2,], nan100)-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n", names=NA)
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
mtext("South + SE Asia", side = 2, line = 6)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("E", cex=1.4, xplus = 20)

barplot(as.numeric(income_gender[2,]), horiz=TRUE, xaxt="n", 
        xlim=c(-100, 100), col="#0072B2", xlab="Gender control (%)",
        names.arg=gsub("_", " ", names(income_gender)), las=1)
barplot(as.numeric(income_gender[2,])-100, horiz=TRUE, 
        add=TRUE, col="#D55E00", xaxt="n")
abline(v=c(-50,50), lty=3, col="grey", lwd=0.7)
axis(side = 1, at=seq(-100, 100, 50), labels = abs(seq(-100, 100, 50)))
mtext("Male", side = 1, line = 2, adj=0.22, cex = 0.7)
mtext("Female", side = 1, line = 2, adj=0.8, cex=0.7)
abc_label("F", cex=1.4, xplus = 15)

dev.off()




# Figure 6: gender control and food security --------------

# A. Gender and FIES score
# remove households without information on FIES
dat_fies <- dat_ind[!is.na(dat_ind$fies_score),] #only 8607 households

# mod00<-lmer(fies_score ~log_total_income_usd_per_year + gender_control+(1|country), dat_fies)
# anova(mod00)
# summary(mod00)

p1 <- ggplot(dat_fies, aes(x=log_total_income_usd_per_year, y=fies_score, colour = gender_control, linetype = gender_control)) + 
  geom_smooth(method=lm)+   # Add linear regression line 
  theme_light()+ xlab(label = "Log transformed total income (USD.Yr-1)")+
  ylab(label = "FIES score (0-8)")+ labs(colour = "Gender control category")+
  scale_colour_manual(values=c("#0072B2","green4", "#D55E00"),
                      name="Gender control category", 
                      breaks=c("female", "parity", "male"), 
                      labels = c("Women-controlled", "Parity", "Men-controlled")) +
  scale_linetype_discrete(name="Gender control category", 
                          breaks=c("female", "parity", "male"), 
                          labels = c("Women-controlled", "Parity", "Men-controlled"))


# B. Gender and HDDS score
# remove households without information on HDDS
dat_hdds <- dat_ind[!is.na(dat_ind$hdds_score),]

# mod00<-lmer(hdds_score ~log_total_income_usd_per_year + gender_control+(1|country), dat_hdds)
# anova(mod00)
# summary(mod00)

p2 <- ggplot(dat_hdds, aes(x=log_total_income_usd_per_year, y=hdds_score, colour = gender_control, linetype = gender_control)) + 
  geom_smooth(method=lm, fullrange = TRUE, alpha = .15)+   # Add linear regression line 
  theme_light()+ xlab(label = "Log transformed total income (USD.Yr-1)")+
  ylab(label = "HH dietary diversity (0-10)")+ labs(colour = "Gender control category")+
  scale_colour_manual(values=c("#0072B2","green4", "#D55E00"),
                      name="Gender control category", 
                      breaks=c("female", "parity", "male"), 
                      labels = c("Women-controlled", "Parity", "Men-controlled")) +
  scale_linetype_discrete(name="Gender control category", 
                          breaks=c("female", "parity", "male"), 
                          labels = c("Women-controlled", "Parity", "Men-controlled"))

# final output
pp <- ggarrange(p1, p2,  
          labels = c("A", "B"),
          ncol = 2, nrow = 1, 
          common.legend = TRUE, legend = "right")
ggsave("Figures/Figure6_Food_Gender.png", width=2000, height=800, units="px", bg="white", dpi=200)


