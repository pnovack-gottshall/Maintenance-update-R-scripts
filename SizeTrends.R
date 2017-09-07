## DOCUMENT BODY-SIZE TRENDS


## PREPARE TIME SCALE -------------------------------------------------------------
rm(list=ls())
library(geoscale)
library(paleobioDB)
strat_names <- pbdb_intervals(limit="all", vocab="pbdb")
head(strat_names)
## "Level-4" "subperiods" (eons are level 1, eras=level 2, periods=3,
## subperiods=4, epochs=5)
ages <- strat_names[which(strat_names$level==4), ]
## Add in Ediacaran, too:
edia <- strat_names[which(strat_names$interval_name=="Ediacaran"), ]
ages <- rbind(ages, edia)
ages[ ,1:5]

# Get base, top, and midpoint ages for PBDB subperiods
mids <- apply(ages[ ,4:5], 1, mean)
base <- ages$early_age
top <- ages$late_age

# IMPORT DATA -------------------------------------------------------------
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
data <- read.delim(file="PostSizes.tab", sep="\t", header=TRUE)
# data <- read.delim(file="Tiering.tab", sep="\t", header=TRUE)
# data <- read.delim(file="PostSizes_withPBDB.tab", sep="\t", header=TRUE)

# Calculate biovolume:
# BVOL = 0.5439 * (DVLength * TransverseLength * APLength / 1000) ^ 0.896
data$BodyVolume <- 0.5439 * (data$DVLength * data$TransverseLength * data$APLength 
  / 1000) ^ 0.896

str(data)
head(data, 2)

hist(log10(data$BodyVolume), col="darkgray", border="white", n=20)

## MAKE TRENDS -------------------------------------------------------------

## CHOOSE TAXONOMIC GROUP TO SAMPLE
# Choose from following (or update accordingly):
sort(table(data$Class))
sort(table(data$Order))

# Use first if want to analyze ALL data, or second if want to select a
# particular group
group <- data
# group.name <- "Cephalopoda"; group <- data[which(data$Class == group.name),]
group[1:10, 2:7]  # Confirm you've chosen the correct data

# Create a size-by-age matrix
sizes <- matrix(NA, nrow=nrow(group), ncol=length(mids))
for(t in 1:length(mids)) {
  wh.gr <- which((group$early_age >= base[t] & group$late_age <= top[t]) | 
      (group$early_age <= base[t] & group$late_age >= top[t]))
  sizes[wh.gr,t] <- group$BodyVolume[wh.gr]
}

# Plot size trend, with individual genus sizes and mean trend
lim <- range(log10(sizes), na.rm=TRUE)
# lim <- c(-0.4, 0.6) # Set manually to focus on mean trend
geoscalePlot(mids, rep(lim[1], length(mids)), units=c("Epoch", "Period"),
             tick.scale="Period", boxes="Epoch", cex.age=0.65, cex.ts=0.75,
             cex.pt=1, age.lim=c(540, 0), data.lim=lim, ts.col=TRUE,
             label="log10 body volume (cm3)", vers="ICS2015", type="n")
mtext(text=group.name, side=3, cex=1.25)
for(t in 1:nrow(group)) {
 polygon(c(group$early_age[t], group$late_age[t]), 
   rep(log10(group$BodyVolume[t]), 2), border="gray50")
 }

# mean trend:
means <- rep(NA, length(mids))
means <- apply(log10(sizes), 2, mean, na.rm=TRUE)
lines(mids, means, lwd=3)

# 5th and 95th percentiles:
perc.5 <- rep(NA, length(mids))
perc.5 <- apply(log10(sizes), 2, quantile, probs=0.05, na.rm=TRUE)
perc.95 <- rep(NA, length(mids))
perc.95 <- apply(log10(sizes), 2, quantile, probs=0.95, na.rm=TRUE)
lines(mids, perc.5, lty=2, lwd=2)
lines(mids, perc.95, lty=2, lwd=2)



## Tiering trends (NOTE: MAKE SURE TO RE-DOWNLOAD SIZES ONLY FOR SUSPENSION-FEEDERS!)

# Create separate infaunal and epifaunal trends
# Create a tier-by-age matrix
tiers <- matrix(NA, nrow=nrow(group), ncol=length(mids))
for(t in 1:length(mids)) {
  wh.gr <- which((group$early_age >= base[t] & group$late_age <= top[t]) | 
      (group$early_age <= base[t] & group$late_age >= top[t]))
  tiers[wh.gr,t] <- group$AbsStratDist[wh.gr]
}

# Plot size trend, with individual genus tiers and mean trend
lim <- range(tiers, na.rm=TRUE)
geoscalePlot(mids, rep(lim[1], length(mids)), units=c("Epoch", "Period"), 
  tick.scale="Period", boxes="Epoch", cex.age=0.65, cex.ts=0.75,
  cex.pt=1, age.lim=c(540, 0), data.lim=lim, ts.col=TRUE,
  label="tiering height (mm)", vers="ICS2015", type="n")
mtext(text=group.name, side=3, cex=1.25)
for(t in 1:nrow(group)) {
  polygon(c(group$early_age[t], group$late_age[t]), 
    rep(group$AbsStratDist[t], 2), border="gray50")
}
abline(h=0, lwd=2)

pro <- c(.5, .75, .99)

epif <- data.frame(mids=mids, median=NA, q75=NA, top=NA)
epif[,2:4] <- t(apply(tiers[which(group$AbsStratDist > 0), ], 2, quantile, 
  probs=pro, na.rm=TRUE))
# lines(mids, epif$median, lwd=2, col="gray15")
# lines(mids, epif$q75, lwd=2, col="gray15")
lines(mids, epif$top, lwd=2, col="gray15")

inf <- data.frame(mids=mids, median=NA, q75=NA, top=NA)
inf[,2:4] <- t(apply(tiers[which(group$AbsStratDist < 0), ], 2, quantile, 
  probs=(1 - pro), na.rm=TRUE))
# lines(mids, inf$median, lwd=2, col="gray15")
# lines(mids, inf$q75, lwd=2, col="gray15")
lines(mids, inf$top, lwd=2, col="gray15")



# Just plot trend
t.lim <- range(c(epif$top, inf$top), na.rm=TRUE)
geoscalePlot(mids, epif$top, units=c("Epoch", "Period"), tick.scale="Period", boxes="Epoch", 
  cex.age=0.65, cex.ts=0.75, cex.pt=1, age.lim=c(540, 0), data.lim=t.lim, ts.col=TRUE, 
  label="tiering height (mm)", vers="ICS2015", type="n", lwd=3)
# lines(mids, epif$median, lwd=2, col="gray25")
# lines(mids, epif$q75, lwd=2, col="gray25")
lines(mids, epif$top, lwd=2, col="gray25")
# lines(mids, inf$median, lwd=2, col="gray25")
# lines(mids, inf$q75, lwd=2, col="gray25")
lines(mids, inf$top, lwd=2, col="gray25")
abline(h=0, lwd=2)



