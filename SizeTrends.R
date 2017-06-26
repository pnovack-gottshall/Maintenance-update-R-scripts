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

# Calculate biovolume:
# BVOL = 0.5439 * (DVLength * TransverseLength * APLength / 1000) ^ 0.896
data$BodyVolume <- 0.5439 * (data$DVLength * data$TransverseLength * data$APLength 
  / 1000) ^ 0.896

str(data)
head(data, 2)

hist(log10(data$BodyVolume), col="darkgray", border="white")

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
geoscalePlot(mids, rep(lim[1], length(mids)), units=c("Epoch", "Period"),
             tick.scale="Period", boxes="Epoch", cex.age=0.65, cex.ts=0.75,
             cex.pt=1, age.lim=c(540, 0), data.lim=lim, ts.col=TRUE,
             label="log10 body volume (cm3)", vers="ICS2015", type="n")
mtext(text=group.name, side=3, cex=1.25)
for(t in 1:nrow(group)) {
 polygon(c(group$early_age[t], group$late_age[t]), 
   rep(log10(group$BodyVolume[t]), 2), border="gray50")
 }

means <- rep(NA, length(mids))
means <- apply(log10(sizes), 2, mean, na.rm=TRUE)
lines(mids, means, lwd=3)
