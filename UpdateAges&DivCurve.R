## Update age ranges from Paleobiology Database and create diversity curves.
## Check for missing ID numbers. Check that extant/extincts are correct (but
## note false flag if a subgenus is extinct within a still-extant genus).


## IMPORT AND PROCESS FILES ############################################

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

## Download data directly from PaleobioDB and save to working directory (will be
## > 25 MB)

## Easier if paste link into URL and save manually
# pbdb <- read.csv("www.paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&interval=Phanerozoic&show=app&show=acconly&vocab=pbdb")
# If want forams too, use base_name=Metazoa,Retaria
pbdb.all <- read.csv("pbdb_data.csv")
head(pbdb.all)

# Extract only genera (to ensure only searching for genus ranges sometimes a
# genus, like Nuculoidea, has same name as a higher taxon)
pbdb <- pbdb.all[which(pbdb.all$taxon_rank == "genus" | 
                         pbdb.all$taxon_rank == "subgenus"), ]
nrow(pbdb)

## Export occurrences as .csv file named "occs.csv" from "LifeHabits.fmp12" (in
## any sort order) with following columns: IDNumber, Phylum, Class, Order,
## Superfamily, Family, Genus, Subgenus, Species, early_period, early_age,
## late_period, late_age). Make sure to manually enter the header names!
## (easiest is to save as Excel to retain column names and then re-save as
## .csv.)
occs <- read.csv("occs.csv", header = TRUE)
table(occs$Phylum)
cl.tbl <- table(occs$Class)
sort(cl.tbl, decreasing = FALSE)
or.tbl <- table(occs$Order)
sort(or.tbl, decreasing = FALSE)

# Confirm that early_period and late_period are factors
is.factor(occs$early_period)
is.factor(occs$late_period)
# If not, run following:
# occs$early_period <- as.character(occs$early_period)
# occs$late_period <- as.character(occs$late_period)
head(occs)

## Any duplicated ID numbers?
which(table(occs$IDNumber) > 1)

## Any missing ID numbers?
num <- seq(nrow(occs))
all(num %in% sort(occs$IDNumber)) # TRUE if nothing missing
which(num %in% sort(occs$IDNumber) == FALSE)

# Any duplicated genus entries?
if(length(table(table(occs$Genus))) > 1L) {
  print(which(table(occs$Genus) > 1L))
  stop("The above genus entries are entered twice. Delete the outdated entry/entries?")
}
# Ignore the 72 extant brachiopod genera with multiple species in the database
# (mostly extant species, some fossil species), cases where multiple subgenera
# are included in same genus, where there are ecologically quite different
# species in same genus, and homonyms.




## PREP TIME SCALE #####################################################

## Get updated PBDB intervals and ages
library(paleobioDB)
strat_names <- pbdb_intervals(limit = "all", vocab = "pbdb")
head(strat_names)
## "Level-4" "subperiods" (eons are level 1, eras=level 2, periods=3,
## subperiods=4, epochs=5)
l4s <- strat_names[which(strat_names$level == 4),]
## Add in Ediacaran, too:
edia <- strat_names[which(strat_names$interval_name == "Ediacaran"),]
l4s <- rbind(l4s, edia)
l4s[, 1:5]

## Any disallowed intervals? (Allow blank ("") and Recent, although not official
## subperiods)
ints <- unique(c(levels(occs$early_period), levels(occs$late_period)))
ints[which(!ints %in% l4s$interval_name)]

## Manually replace the bad interval (one at a time, within FMP file [and
## occ.csv], manually changing 'bad' value and re-running until all replaced).
## But keep the listed interval (mya), if listed, which is higher resolution
## than summary subperiod

# bad <- "Botomian"; (wh.bad <- strat_names[which(strat_names$interval_name==bad),])

## With this one: 

# l4s[c(length(which(l4s$late_age <= wh.bad$late_age)), length(which(l4s$late_age < wh.bad$early_age))), 3:5]



## EXTRACT AGE AND INTERVAL RANGES #####################################

# If crashes, most likely because the genus is mis-spelled, or not in PBDB AND 
# there are no ranges available to use. Manually set as Recent (if extant and no
# fossil record, according to WoRMS) or enter manually (and start at next "i" in
# loop).

index <- seq(0, 10000, by=100)
# In next line, make sure Genus is added before Subgenus. Algorithm will first
# update the ranges for all subgenera in a genus to the range for the type
# subgenus (if subgenus shares name same as genus), then will override for
# individual subgenera later
Gen <- sort(unique(unlist(list(occs$Genus, occs$Subgenus), recursive = TRUE)))
for (i in 1:length(Gen)) {
  # for(i in 72:length(Gen)) {
  gen.pbdb <- max.ma <- min.ma <- Early <- Late <- NA
  override <- FALSE
  gen <- as.character(Gen[i])
  if(gen == "") next   # Combining genera and subgenera sometimes adds a blank
  if(i %in% index) cat("genus ", i, ":", gen, "\n")
  # PBDB treats the accepted names of subgenera as "Genus (Subgenus)"
  if (gen %in% occs$Subgenus) {
    wh.occs.G <- which(occs$Subgenus == gen)
    gen <- paste(occs$Genus[wh.occs.G[1]], " (",
                 occs$Subgenus[wh.occs.G[1]], ")", sep = "")
  } else wh.occs.G <- which(occs$Genus==gen)
  len.g <- length(wh.occs.G)
  wh.pbdb.G <- which(pbdb$taxon_name == gen)
  len.pbdb.G <- length(wh.pbdb.G)
  
  # Manual override: Taxa not in PBDB but manually given dates or ages (the code
  # here re-confirms the ages and interval names are correct if more recently
  # added to PBDB)
  if (len.pbdb.G == 0L) override <- TRUE
  if (override) {
    # If only range is provided, give correct interval name :
    if (occs$early_period[wh.occs.G] == "" &
        occs$late_period[wh.occs.G] == "" &
        !is.na(occs$early_age[wh.occs.G]) &
        !is.na(occs$late_age[wh.occs.G])) {
      occs$early_period[wh.occs.G] <-
        rep(as.character(l4s$interval_name[length(which(l4s$late_age < occs$early_age[wh.occs.G]))]), len.g)
      occs$late_period[wh.occs.G] <-
        rep(as.character(l4s$interval_name[length(which(l4s$late_age <= occs$late_age[wh.occs.G]))]), len.g)
    }
    # If only interval names are given, give correct range (be aware range could be overextended):
    if (occs$early_period[wh.occs.G] != "" &
        occs$late_period[wh.occs.G] != "" &
        is.na(occs$early_age[wh.occs.G]) &
        is.na(occs$late_age[wh.occs.G])) {
      if (occs$early_period[wh.occs.G] == "Recent") {
        occs$early_age[wh.occs.G] <- rep(0, len.g)
      } else {
        occs$early_age[wh.occs.G] <-
          rep(l4s$early_age[which(l4s$interval_name == as.character(occs$early_period[wh.occs.G]))], len.g)
      }
      if (occs$late_period[wh.occs.G] == "Recent") { 
        occs$late_age[wh.occs.G] <- rep(0, len.g)
      } else {
        occs$late_age[wh.occs.G] <-
          rep(l4s$late_age[which(l4s$interval_name == as.character(occs$late_period[wh.occs.G]))], len.g)
      }
    }
    # If no stratigraphic information is available:
    if (occs$early_period[wh.occs.G] == "" &
        occs$late_period[wh.occs.G] == "" &
        is.na(occs$early_age[wh.occs.G]) &
        is.na(occs$late_age[wh.occs.G])) {
      cat(paste("  - check ", gen, " (", occs$Class[wh.occs.G], 
        "): no age known in PBDB; check Treatise/Sepkoski\n", sep=""))
    }
  }
  
  if(override) next

  # In case of homonyms, reconfirm "by hand" (using PBDB) so that do not assign
  # range to wrong taxon. (The PBDB uses the same 'accepted_name' for synonyms,
  # making it challenging in the raw data to distinguish homonyms from synonyms,
  # but synonym are assigned the same strat ranges whereas homonyms will not.)
  if (len.pbdb.G > 1L &
      (length(unique(pbdb$firstapp_max_ma[wh.pbdb.G])) > 1L |
      length(unique(pbdb$lastapp_min_ma[wh.pbdb.G])) > 1L))
      cat(paste("         * Manually confirm range for possible homonym ", gen, " (", occs$Class[wh.occs.G], ")\n", sep=""))
  
  # Continue with those taxa with PBDB ranges:
  gen.pbdb <- pbdb[wh.pbdb.G, ]
  max.ma <- max(gen.pbdb$firstapp_max_ma)
  min.ma <- min(gen.pbdb$lastapp_min_ma)
  
  # Flag any discrepancies in extinct / extant tags
  if (any(occs$late_period[wh.occs.G] == "Recent") &
      any(gen.pbdb$is_extant == "extinct"))
    cat("+ Confirm extinct/extant status for", gen, "\n")
  if (any(occs$late_period[wh.occs.G] != "Recent") &
      any(gen.pbdb$is_extant == "extant"))
    cat("+ Confirm extinct/extant status for", gen, "\n")
  
  # Assign to "level-4" ages
  Early <-
    as.character(l4s$interval_name[length(which(l4s$late_age < max.ma))])
  Late <-
    as.character(l4s$interval_name[length(which(l4s$late_age <= min.ma))])
  occs$early_age[wh.occs.G] <- rep(max.ma, len.g)
  occs$early_period[wh.occs.G] <- rep(Early, len.g)
  # Implement pull-of-the-Recent
  if (any(occs$late_period[wh.occs.G] == "Recent") |
      any(gen.pbdb$is_extant == "extant")) {
    Late <- "Recent"
    min.ma <- 0
  }
  occs$late_age[wh.occs.G] <- rep(min.ma, len.g)
  occs$late_period[wh.occs.G] <- rep(Late, len.g)
}
# write.csv(occs, file="PBDBDates.csv", row.names=FALSE)

# Before importing ranges, manually delete 'NA's in strat ranges

# Use next command if want to use a direct export from FileMakerPro. (Make
# sure the file details are the same as above.)

# occs <- read.csv("PBDBDates.csv", header = TRUE)
# occs <- read.delim("PostSizes_withPBDB.tab", sep = "\t", header = TRUE)
head(occs)



## CONSTRUCT DIVERSITY CURVES ##########################################
## (using 'Total Diversity' of Foote 2000)
## X-FL, number of taxa w/ 1st and last appearance in interval (singletons),
## X-bL, number of taxa crossing bottom boundary & last appear in interval,
## X-Ft, number of taxa crossing top boundary and first appear in interval,
## X-bt, number of taxa crossing both bottom and top interval boundaries,
##
## Total D, total number of taxa in interval (X-FL + X-bL + X-Ft + X-bt)

# Get midpoint age for PBDB subperiods
mids <- apply(l4s[ ,4:5], 1, mean)
divs <- data.frame(interval = l4s$interval_name, base = l4s$early_age,
                   top = l4s$late_age, midpt = mids, div = NA)

# Genus-level diversity curve (using database occurrences above)
for(t in 1:nrow(divs)) {
  FL <- length(which(occs$early_age > divs$base[t] &
                       occs$late_age < divs$top[t]))
  bL <- length(which(occs$early_age > divs$base[t] &
                       occs$late_age < divs$base[t] &
                       occs$late_age >= divs$top[t]))
  Ft <- length(which(occs$late_age < divs$top[t] &
                       occs$early_age <= divs$base[t] &
                       occs$early_age > divs$top[t]))
  bt <- length(which(occs$early_age <= divs$base[t] &
                       occs$late_age >= divs$top[t]))
  divs$div[t] <- FL + bL + Ft + bt
}
head(divs)
summary(divs$div)

# Plot diversity curve
library(geoscale)
geoscalePlot(divs$midpt, divs$div, units = c("Epoch", "Period"), 
             tick.scale = "Period", boxes = "Epoch", cex.age = 0.65,
             cex.ts = 0.75, cex.pt = 1, age.lim = c(540, 0), data.lim = NULL,
             ts.col = TRUE, label = "Genus diversity", vers = "ICS2015", 
             type = "l", lwd = 3)


## Recalculate ranges for other taxonomic levels

## Extract age and interval ranges for higher taxa
pbdb <- pbdb.all
index <- seq(0, 10000, by = 100)
ranks <- c("Family", "Superfamily", "Order", "Class", "Phylum")
for (r in 1:length(ranks)) {
  t.rank <- ranks[r]
  wh.col <- which(colnames(occs) == t.rank)
  t.occs <- occs[0 , c(2:wh.col, 9:12)]
  taxa <- sort(unique(occs[[wh.col]]))
  for (i in 1:length(taxa)) {
    taxon.pbdb <- max.ma <- min.ma <- Early <- Late <- NA
    taxon <- as.character(taxa[i])
    if (i %in% index) cat("taxon ", i, ":", taxon, "\n")
    if (taxon == "UNCERTAIN") next
    if (taxon == "Trace fossil") next
    wh.occs.taxon <- which(occs[[wh.col]] == taxon)
    t.occs[i, 1:(wh.col - 1)] <- occs[wh.occs.taxon[1], 2:wh.col]
    # Use current ranges before updating with PBDB:
    wh.max <- which.max(occs$early_age[wh.occs.taxon])
    wh.min <- which.min(occs$late_age[wh.occs.taxon])
    occs.max.ma <- occs$early_age[wh.occs.taxon][wh.max]
    occs.min.ma <- occs$late_age[wh.occs.taxon][wh.min]
    occs.Early <- as.character(occs$early_period[wh.occs.taxon][wh.max])
    occs.Late <- as.character(occs$late_period[wh.occs.taxon][wh.min])
    if (length(occs.max.ma) > 0L) {
      t.occs$early_age[i] <- occs.max.ma
      t.occs$early_period[i] <- occs.Early
      t.occs$late_age[i] <- occs.min.ma
      t.occs$late_period[i] <- occs.Late
    }
    # Update with PBDB (if extends range):
    wh.pbdb.taxon <- which(pbdb$taxon_name == taxon)
    if (length(wh.pbdb.taxon) == 0L) next
    taxon.pbdb <- pbdb[wh.pbdb.taxon, ]
    if (is.na(taxon.pbdb$firstapp_max_ma) &
        is.na(taxon.pbdb$lastapp_min_ma)) next
    max.ma <- max(taxon.pbdb$firstapp_max_ma, na.rm = TRUE)
    min.ma <- min(taxon.pbdb$lastapp_min_ma, na.rm = TRUE)
    if (max.ma > occs.max.ma) t.occs$early_age[i] <- max.ma
    if (min.ma < occs.min.ma) t.occs$late_age[i] <- min.ma
    # Assign to "level-4" ages
    Early <-
      as.character(l4s$interval_name[length(which(l4s$late_age < t.occs$early_age[i]))])
    Late <-
      as.character(l4s$interval_name[length(which(l4s$late_age <= t.occs$late_age[i]))])
    t.occs$late_period[i] <- Late
    t.occs$early_period[i] <- Early
  }
  
  write.csv(t.occs, file=paste("PBDBDates_", t.rank, ".csv", sep=""), row.names=FALSE)

  # If want to use a direct export from FileMakerPro. (Make sure the file details 
  # are the same as above.
  
  # t.occs <- read.csv("occs2.csv", header=TRUE)
  # t.occs$early_period <- as.character(t.occs$early_period); # t.occs$late_period <- as.character(t.occs$late_period)
  # head(t.occs)

  ## Construct diversity curves        (using 'Total Diversity' of Foote 2000)
  
  # Get midpoint age for PBDB subperiods
  mids <- apply(l4s[, 4:5], 1, mean)
  divs <- data.frame(interval = l4s$interval_name, base = l4s$early_age,
                     top = l4s$late_age, midpt = mids, div = NA)
  
  # Higher taxa diversity curve (using database occurrences above)
  for (t in 1:nrow(divs)) {
    FL <- length(which(t.occs$early_age > divs$base[t] &
                         t.occs$late_age < divs$top[t]))
    bL <- length(which(t.occs$early_age > divs$base[t] &
                         t.occs$late_age < divs$base[t] &
                         t.occs$late_age >= divs$top[t]))
    Ft <- length(which(t.occs$late_age < divs$top[t] &
                         t.occs$early_age <= divs$base[t] &
                         t.occs$early_age > divs$top[t]))
    bt <- length(which(t.occs$early_age <= divs$base[t] &
                         t.occs$late_age >= divs$top[t]))
    divs$div[t] <- FL + bL + Ft + bt
  }
  head(divs)
  summary(divs$div)
  
  # Plot diversity curve
  library(geoscale)
  geoscalePlot(divs$midpt, divs$div, units = c("Epoch", "Period"),
               tick.scale = "Period", boxes = "Epoch", cex.age = 0.65,
               cex.ts = 0.75, cex.pt = 1, age.lim = c(540, 0), data.lim = NULL,
               ts.col = TRUE, label = paste(t.rank, "diversity"),
               vers = "ICS2015", type = "l", lwd = 3)
}



## COMPARE TO PBDB TALLIES #############################################
# Get current numbers from PBDB, and compare to life habit database Note is
# comparing a marine & invertebrate-only life habit database to the entire PBDB
pbdb$taxon_name[which(pbdb$taxon_rank == "phylum")]
(l.pbdb <- length(pbdb$taxon_name[which(pbdb$taxon_rank == "phylum")]))
(lhdb <- length(unique(occs$Phylum)))
round(lhdb * 100 / l.pbdb, 2)

pbdb$taxon_name[which(pbdb$taxon_rank=="class")]
(l.pbdb <- length(pbdb$taxon_name[which(pbdb$taxon_rank=="class")]))
(lhdb <- length(unique(occs$Class)))
round(lhdb * 100 / l.pbdb, 2)

pbdb$taxon_name[which(pbdb$taxon_rank=="order")]
(l.pbdb <- length(pbdb$taxon_name[which(pbdb$taxon_rank=="order")]))
(lhdb <- length(unique(occs$Order)))
round(lhdb * 100 / l.pbdb, 2)

pbdb$taxon_name[which(pbdb$taxon_rank=="superfamily")]
(l.pbdb <- length(pbdb$taxon_name[which(pbdb$taxon_rank=="superfamily")]))
(lhdb <- length(unique(occs$Superfamily)))
round(lhdb * 100 / l.pbdb, 2)

pbdb$taxon_name[which(pbdb$taxon_rank=="family")]
(l.pbdb <- length(pbdb$taxon_name[which(pbdb$taxon_rank=="family")]))
(lhdb <- length(unique(occs$Family)))
round(lhdb * 100 / l.pbdb, 2)

pbdb$taxon_name[which(pbdb$taxon_rank=="genus")]
(l.pbdb <- length(pbdb$taxon_name[which(pbdb$taxon_rank=="genus")]))
(lhdb <- length(unique(occs$Genus)))
round(lhdb * 100 / l.pbdb, 2)

