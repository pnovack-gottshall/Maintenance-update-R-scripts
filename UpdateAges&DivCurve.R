## Update age ranges from Paleobiology Database and create diversity curves.
## Check for missing ID numbers. Check that extant/extincts are correct (but
## note false flag if a subgenus is extinct within a still-extant genus).

## ISSUES ######################################################################
## 1. Add routines that add in Sepkoski Genus Compendium ages.
## 2. Add routine that confirms extant status with WoRMS.
## 3. Silence the errors (and convert to warnings) so that doesn't die when trips.
################################################################################



## IMPORT AND PROCESS FILES ############################################

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")

## Download data directly from PaleobioDB and save to working directory (will be
## > 25 MB)

## Easier if paste link into URL and save manually
# pbdb.all <- read.csv("https://www.paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&interval=Phanerozoic&show=app&show=acconly&vocab=pbdb")
# If want forams too, use base_name=Metazoa,Retaria
pbdb.all <- read.csv("pbdb_data.csv")
head(pbdb.all)

# Extract only genera (to ensure only searching for sub/genus ranges; sometimes
# a genus, like Nuculoidea, has same name as a higher taxon)
pbdb <- pbdb.all[which(pbdb.all$taxon_rank == "genus" | 
                         pbdb.all$taxon_rank == "subgenus"), ]
nrow(pbdb)


## Run relevant code in SelectCols.R for UpdateAges&DivCurve.R to obtain
## following output.

## Use the following columns: IDNumber, Phylum, Class, Order, Superfamily,
## Family, Genus, Subgenus, Species, max_age, max_ma, min_age, min_ma
occs <- read.csv("occs.csv", header = TRUE)
ph.tbl <- table(occs$Phylum)
sort(ph.tbl, decreasing = FALSE)
cl.tbl <- table(occs$Class)
sort(cl.tbl, decreasing = FALSE)
or.tbl <- table(occs$Order)
sort(or.tbl, decreasing = FALSE)

# Confirm that max_age and min_age are factors
is.factor(occs$max_age)
is.factor(occs$min_age)
# If not, run following:
# occs$max_age <- as.factor(as.character(occs$max_age))
# occs$min_age <- as.factor(as.character(occs$min_age))
head(occs)

## Any duplicated ID numbers?
which(table(occs$IDNumber) > 1)

## Any missing ID numbers?
num <- seq(nrow(occs))
all(num %in% sort(occs$IDNumber)) # TRUE if nothing missing
which(num %in% sort(occs$IDNumber) == FALSE)

# Any duplicated genus entries?
if(length(table(table(occs$Genus))) > 1L) {
  foo <- which(table(occs$Genus) > 1L)
  print(foo)
  cat("There are", length(foo), "entries above.\n")
  stop("The above genus entries are entered twice. Delete the outdated entry/entries?")
}
# Ignore the 72 extant brachiopod genera with multiple species in the database
# (mostly extant species, some fossil species), cases where multiple subgenera
# are included in same genus, where there are ecologically quite different
# species in same genus, and homonyms.




## PREP TIME SCALE #####################################################

## Get updated PBDB intervals and ages
strat_names <-
  read.csv("https://www.paleobiodb.org/data1.2/intervals/list.csv?all_records&vocab=pbdb")
head(strat_names)
## "Level-4" "subperiods" (eons are level 1, eras=level 2, periods=3,
## subperiods=4, epochs=5)
l4s <- strat_names[which(strat_names$scale_level == 4),]
## Add in Ediacaran, too:
edia <- strat_names[which(strat_names$interval_name == "Ediacaran"),]
l4s <- rbind(l4s, edia)
## Series 3 now named Miaolingian (Zhao, et al. 2019, Episodes)
l4s$interval_name[which(l4s$interval_name == "Series 3")] <- "Miaolingian"
l4s[, 1:5]

## Are there any disallowed intervals in your database? (Allow blank ("") and
## Recent, although not official subperiods)
ints <- unique(c(levels(occs$max_age), levels(occs$min_age)))
ints[which(!ints %in% l4s$interval_name)]

## Manually replace the bad interval (one at a time, within FMP file [and
## occ.csv], manually changing 'bad' value and re-running until all replaced).
## But keep the listed interval (mya), if listed, which is higher resolution
## than summary subperiod

# bad <- "Botomian"; (wh.bad <- strat_names[which(strat_names$interval_name==bad),])

## With this one: 

# l4s[c(length(which(l4s$min_ma <= wh.bad$min_ma)), length(which(l4s$min_ma < wh.bad$max_ma))), 3:5]




## Identify potential homonym genera (used only if trouble-shooting for below)
which(table(pbdb$taxon_name) > 1L)



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
  override <- homonym <- FALSE
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
    if (occs$max_age[wh.occs.G] == "" &
        occs$min_age[wh.occs.G] == "" &
        !is.na(occs$max_ma[wh.occs.G]) &
        !is.na(occs$min_ma[wh.occs.G])) {
      occs$max_age[wh.occs.G] <-
        rep(as.character(l4s$interval_name[length(which(l4s$min_ma < occs$max_ma[wh.occs.G]))]), len.g)
      occs$min_age[wh.occs.G] <-
        rep(as.character(l4s$interval_name[length(which(l4s$min_ma <= occs$min_ma[wh.occs.G]))]), len.g)
    }
    # If only interval names are given, give correct range (be aware range could be overextended):
    if (occs$max_age[wh.occs.G] != "" &
        occs$min_age[wh.occs.G] != "" &
        is.na(occs$max_ma[wh.occs.G]) &
        is.na(occs$min_ma[wh.occs.G])) {
      if (occs$max_age[wh.occs.G] == "Recent") {
        occs$max_ma[wh.occs.G] <- rep(0, len.g)
      } else {
        occs$max_ma[wh.occs.G] <-
          rep(l4s$max_ma[which(l4s$interval_name == as.character(occs$max_age[wh.occs.G]))], len.g)
      }
      if (occs$min_age[wh.occs.G] == "Recent") { 
        occs$min_ma[wh.occs.G] <- rep(0, len.g)
      } else {
        occs$min_ma[wh.occs.G] <-
          rep(l4s$min_ma[which(l4s$interval_name == as.character(occs$min_age[wh.occs.G]))], len.g)
      }
    }
    # If no stratigraphic information is available:
    if (occs$max_age[wh.occs.G] == "" &
        occs$min_age[wh.occs.G] == "" &
        is.na(occs$max_ma[wh.occs.G]) &
        is.na(occs$min_ma[wh.occs.G])) {
      cat(paste("  - check ", gen, " (", occs$Class[wh.occs.G], 
        "): no age known in PBDB; check Treatise/Sepkoski\n", sep=""))
    }
  }
  
  if(override) next

  # In case of homonyms, reconfirm "by hand" (using PBDB web interface) so that
  # do not assign range to wrong taxon. (The PBDB uses the same 'accepted_name'
  # for synonyms, making it challenging in the raw data to distinguish homonyms
  # from synonyms, but synonyms are assigned the same strat ranges whereas
  # homonyms are not.)
  if (len.pbdb.G > 1L &
      (length(unique(pbdb$firstapp_max_ma[wh.pbdb.G])) > 1L |
       length(unique(pbdb$lastapp_min_ma[wh.pbdb.G])) > 1L)) {
    homonym <- TRUE
    cat(paste("         * Manually confirm range for possible homonym ",
        gen, " (", occs$Class[wh.occs.G], ")\n", sep = ""))
  }
  
  # Continue with those taxa with PBDB ranges (attempting to match homonym with
  # most similar stratigraphic range if at least one FAD or LAD already
  # provided):
  gen.pbdb <- pbdb[wh.pbdb.G,]
  if (homonym) {
    how.many.na <- c(is.na(occs$min_ma[wh.occs.G]), is.na(occs$max_ma[wh.occs.G]))
    sum.how.many <- sum(how.many.na)
    if (sum.how.many < 2L) {
      max.dev <- (occs$max_ma[wh.occs.G] - gen.pbdb$firstapp_max_ma) ^ 2
      min.dev <- (occs$min_ma[wh.occs.G] - gen.pbdb$lastapp_min_ma) ^ 2
      if (sum.how.many == 1L) {
        if (how.many.na[1])
          which.best <- which.min(max.dev)
        else
          which.best <- which.min(min.dev)
      }
      if (sum.how.many == 0L)
        which.best <- which.min(max.dev + min.dev)
      max.ma <- gen.pbdb$firstapp_max_ma[which.best]
      min.ma <- gen.pbdb$lastapp_min_ma[which.best]
    } else {
      # If not able to estimate the most likely homonym, use the first entered:
      max.ma <- gen.pbdb$firstapp_max_ma[1]
      min.ma <- gen.pbdb$lastapp_min_ma[1]
    }
  } else {
    max.ma <- gen.pbdb$firstapp_max_ma
    min.ma <- gen.pbdb$lastapp_min_ma
  }
  
  # Flag any discrepancies in extinct / extant tags
  if (any(occs$min_age[wh.occs.G] == "Recent") &
      any(gen.pbdb$is_extant == "extinct"))
    cat("+ Confirm extinct/extant status for", gen, "\n")
  if (any(occs$min_age[wh.occs.G] != "Recent") &
      any(gen.pbdb$is_extant == "extant"))
    cat("+ Confirm extinct/extant status for", gen, "\n")
  
  # Assign to "level-4" ages (overriding if previously assigned)
  Early <-
    as.character(l4s$interval_name[length(which(l4s$min_ma < max.ma))])
  Late <-
    as.character(l4s$interval_name[length(which(l4s$min_ma <= min.ma))])
  occs$max_ma[wh.occs.G] <- rep(max.ma, len.g)
  occs$max_age[wh.occs.G] <- rep(Early, len.g)
  
  # Implement pull-of-the-Recent
  if (any(occs$min_age[wh.occs.G] == "Recent") |
      any(gen.pbdb$is_extant == "extant")) {
    Late <- "Recent"
    min.ma <- 0
  }

  occs$min_ma[wh.occs.G] <- rep(min.ma, len.g)
  occs$min_age[wh.occs.G] <- rep(Late, len.g)
}

# Confirm no dates are out of order (e.g., FAD younger than LAD). This is
# because of manual entry errors by me in cases where a genus does not have
# occurrences in the PBDB. Thanks for Dave Bapst for pointing out error, which
# is triggered if try to build a time-tree.
if (any(occs$max_ma < occs$min_ma)) {
  cat(occs$Genus[which(occs$max_ma < occs$min_ma)])
  stop("Prior genera have the FAD and LAD switched.)")
}



# write.csv(occs, file = "PBDBDates.csv", row.names = FALSE)

# It is worthwhile to compare the original and updated ranges to troubleshoot
# for errors. If there is a significant change (e.g., > 100 Myr), it is possible
# the genus has a newly entered homonym or an incorrectly classified homonym, or
# a previously unrecognized homonym. In these cases, manually compare the PBDB
# entries and re-classify (or enter appropriate opinions) if incorrect. (If you
# get a "check extinct status" warning and the genus is correctly tagged, it
# usually means one of the species in the genus was improperly flagged. It's
# easiest to confirm on the "pbdb_data.csv" file rather than PBDB web
# interface.)

# Before importing ranges, manually delete 'NA's in strat ranges.

# Import "PBDBDates.csv", "updating matching records" using IDNumber as match
# and ONLY importing the age and interval fields.







## CONSTRUCT DIVERSITY CURVES ##########################################
## (using 'Total Diversity' of Foote 2000)
# X-FL, number of taxa w/ 1st and last appearance in interval (incl. singletons)
#       [including those that range through the entire interval but do not pass 
#       into adjacent intervals],
## X-bL, number of taxa crossing bottom boundary & last appear in interval,
## X-Ft, number of taxa crossing top boundary and first appear in interval,
## X-bt, number of taxa crossing both bottom and top interval boundaries,
##
## Total D, total number of taxa in interval (X-FL + X-bL + X-Ft + X-bt)

# Use next command if want to use a direct export from FileMakerPro instead of
# using the object above. (Make sure the file details are the same as above.)

colCl <- c(rep(NA, 9), "character", NA, "character", NA)
# occs <- read.csv("PBDBDates.csv", header = TRUE, stringsAsFactors=FALSE, colClasses=colCl)
# occs <- read.delim("PostSizes_withPBDB.tab", sep = "\t", header = TRUE, stringsAsFactors=FALSE, colClasses=colCl)
head(occs)


# Get midpoint age for PBDB subperiods
mids <- apply(l4s[ ,9:10], 1, mean)
divs <- data.frame(interval = l4s$interval_name, base = l4s$max_ma,
                   top = l4s$min_ma, midpt = mids, div = NA)

# Genus-level diversity curve (using database occurrences above)
for(t in 1:nrow(divs)) {
  FL <- length(which(occs$max_ma <= divs$base[t] &
                       occs$min_ma >= divs$top[t]))
  bL <- length(which(occs$max_ma > divs$base[t] &
                       occs$min_ma < divs$base[t] &
                       occs$min_ma >= divs$top[t]))
  Ft <- length(which(occs$min_ma < divs$top[t] &
                       occs$max_ma <= divs$base[t] &
                       occs$max_ma > divs$top[t]))
  bt <- length(which(occs$max_ma > divs$base[t] &
                       occs$min_ma < divs$top[t]))
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
    if (taxon == "Cyanobacteria") next
    if (taxon == "Chlorophyta") next
    wh.occs.taxon <- which(occs[[wh.col]] == taxon)
    t.occs[i, 1:(wh.col - 1)] <- occs[wh.occs.taxon[1], 2:wh.col]
    # Use current ranges before updating with PBDB:
    wh.max <- which.max(occs$max_ma[wh.occs.taxon])
    wh.min <- which.min(occs$min_ma[wh.occs.taxon])
    occs.max.ma <- occs$max_ma[wh.occs.taxon][wh.max]
    occs.min.ma <- occs$min_ma[wh.occs.taxon][wh.min]
    occs.Early <- as.character(occs$max_age[wh.occs.taxon][wh.max])
    occs.Late <- as.character(occs$min_age[wh.occs.taxon][wh.min])
    if (length(occs.max.ma) > 0L) {
      t.occs$max_ma[i] <- occs.max.ma
      t.occs$max_age[i] <- occs.Early
      t.occs$min_ma[i] <- occs.min.ma
      t.occs$min_age[i] <- occs.Late
    }
    # Update with PBDB (if extends range):
    wh.pbdb.taxon <- which(pbdb$taxon_name == taxon)
    # Note sometimes higher taxa have homonyms. Use most frequently used name as
    # the likely correct one. (Not not a perfect guarantee.)
    n.matches <- length(wh.pbdb.taxon)
    if (n.matches == 0L)
      next
    if (n.matches > 1L)
      best.match <- which.max(pbdb$n_occs[wh.pbdb.taxon])
    taxon.pbdb <- pbdb[wh.pbdb.taxon[best.match],]
    if (is.na(taxon.pbdb$firstapp_max_ma) &
        is.na(taxon.pbdb$lastapp_min_ma)) next
    max.ma <- max(taxon.pbdb$firstapp_max_ma, na.rm = TRUE)
    min.ma <- min(taxon.pbdb$lastapp_min_ma, na.rm = TRUE)
    if (taxon.pbdb$is_extant == "extant") min.ma <- 0
    if (max.ma > occs.max.ma) t.occs$max_ma[i] <- max.ma
    if (min.ma < occs.min.ma) t.occs$min_ma[i] <- min.ma
    # Assign to "level-4" ages
    Early <-
      as.character(l4s$interval_name[length(which(l4s$min_ma < t.occs$max_ma[i]))])
    Late <-
      as.character(l4s$interval_name[length(which(l4s$min_ma <= t.occs$min_ma[i]))])
    t.occs$min_age[i] <- Late
    t.occs$max_age[i] <- Early
  }
  
  write.csv(t.occs, file=paste("PBDBDates_", t.rank, ".csv", sep=""), row.names=FALSE)

  # If want to use a direct export from FileMakerPro. (Make sure the file details 
  # are the same as above.
  
  # t.occs <- read.csv("occs2.csv", header=TRUE)
  # t.occs$max_age <- as.character(t.occs$max_age); # t.occs$min_age <- as.character(t.occs$min_age)
  # head(t.occs)

  ## Construct diversity curves        (using 'Total Diversity' of Foote 2000)
  
  # Get midpoint age for PBDB subperiods
  mids <- apply(l4s[, 9:10], 1, mean)
  divs <- data.frame(interval = l4s$interval_name, base = l4s$max_ma,
                     top = l4s$min_ma, midpt = mids, div = NA)
  
  # Higher taxa diversity curve (using database occurrences above)
  for (t in 1:nrow(divs)) {
    FL <- length(which(t.occs$max_ma > divs$base[t] &
                         t.occs$min_ma < divs$top[t]))
    bL <- length(which(t.occs$max_ma > divs$base[t] &
                         t.occs$min_ma < divs$base[t] &
                         t.occs$min_ma >= divs$top[t]))
    Ft <- length(which(t.occs$min_ma < divs$top[t] &
                         t.occs$max_ma <= divs$base[t] &
                         t.occs$max_ma > divs$top[t]))
    bt <- length(which(t.occs$max_ma <= divs$base[t] &
                         t.occs$min_ma >= divs$top[t]))
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
# Get current numbers from PBDB, and compare to life habit database. Note is
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

