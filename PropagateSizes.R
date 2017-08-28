## PROPOGATE BODY SIZE CODINGS ACROSS ENTRIES, USING RELATIVES AS PROXIES

## BASIC LOGIC -------------------------------------------------------------

# "Relative' means the smallest inclusive taxonomic group the entry is related
# to whose body size has been coded. Use the 'EstAP', 'EstT', and 'EstDV' tags
# to know which are not measured directly (and can be over-ridden). The
# 'History_Size' field should be overridden as needed to explain how un-measured
# lengths were estimated.

# 1. If entry is measured at species- or genus- (or subgenus-) level AND all 3
# lengths are measured, skip to the next entry.
#
# 2. If entry at sp/g/subg-level AND 1 or more measures are missing, proceed to
# estimating using following logic:
#      A. Find closest relative with ALL 3 ATD.
#           1) if multiple relatives exist, pick the one (regardless of whether
#           sp or genus) that is closest to the same age (earlyRel - earlyEntry
#           + lateR - lateE, and pick using absolute difference). Why? Because
#           size is known to change through time (although unclear if shape does)
#
#      B. If all 3 lengths are missing (not possible if sp/g?), drop in all 3
#      measurements from relative.
#
#      C. If 2 lengths are missing, estimate the missing ones using the 
#      relative's shape.
#
#      D. If 1 length is missing, estimating the missing length using mean of
#      relative's shape (i.e., ratio of other lengths). (Using arithmetic mean
#      instead of geometric mean to best estimate an isometric shape; use of
#      geomean, in trials, does not produce equivalently shaped estimates.)
#
#      E. Update "estimate" check boxes and metadata accordinging, for each if()
#      step above.
#
# 3. If entry is not at genus or better level (i.e., subfamily or greater), find
# closest-aged relative and drop in all 3 measurements, updating metadata. Only
# in this case add tags for RefGenusSize and RefSpeciesSize.






## IMPORT DATA -------------------------------------------------------------

# Before exporting, (1) sort the entries so that BodySizeScale=Species is first,
# followed by Subgenus, Genus, etc., and second by BodyVolume (in descending
# order, with largest first).

# (2) Export following data fields as 'PreSizes.tab' tab-delimited format. DO
# NOT SAVE AS EXCEL FORMAT, AS DOING SO IDIOSYNCRATICALLY CHANGES NAs TO 0s AND
# 1s!! (MUST be tab-delimited, as several text fields contain commas!!!)

# IDNumber

# Taxonomy: Phylum, Subphylum, Class, Subclass, Order, Suborder, Superfamily,
# Family, Subfamily, Genus, Subgenus, Species

# Proxy fields: early_age, late_age, BodySizeScale, RefGenusSize,
# RefSpeciesSize, Enterer, DateEntered_Size, SizeChanged, History_Size,
# BodyMeasureReference

# Body size characters (can really be in any order, as called by name):
# APLength, TransverseLength, DVLength, PhotoAP, PhotoTransverse, PhotoDV,
# APScale, TransverseScale, DVScale, Est_AP, Est_T, Est_DV, AbsStratDist

# (Note "Intact" is excluded because only appropriate to the raw (observed) 
# species/genus-level entries.)

# (3) Open file in MSWord (turn off smart quotes are off: File > Options > 
# Proofing > Autocorrect Options > Autoformat As You Type > uncheck Smart 
# Quotes) and delete any hidden tabs and all problematic (i.e., double) 
# quotation marks (replacing "^t with " and replacing ^t" with " and replacing
# "" with ").

# (4) In Excel, add a row for headers and confirm the column headers are correct
# (and no cells are "hanging"). Then add a new column counting 'PhotoX' columns
# with values. Confirm all BodySizeScale = 'Sp/Subg/Gen' scales have at least
# one measurement.

# (5) Sort the BodySizeScale = 'Sp/Subg/Gen' by (1) number of PhotoX columns 
# (largest first) so entries with complete (all 3) size measurements are first 
# and most incomplete are last. That way those with best scales and 
# more-complete sizes are checked first, so that later entries can use the 
# largest available pool of relatives. (2) Second, sort item by AbsStratDist,
# with largest values first (so those with estimated AbsStratDists are
# considered first to propogate to those lacking them). TROUBLESHOOT: Confirm
# that all 'Sp/Subg/Gen' have at least 1 measurement!

# (6) Delete the added 'count' column and resave.


rm(list=ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/Documents/GSA (& NSF & NAPC)/2016GSA/GSA2016 analyses")

pre.input <- read.delim(file="preSizes.tab", stringsAsFactors=FALSE)
est.cols <- which(colnames(pre.input) == "SizeChanged" | 
    colnames(pre.input) == "Est_AP" | colnames(pre.input) == "Est_T" | 
    colnames(pre.input) == "Est_DV")
colCl <- c(rep(NA, ncol(pre.input)))
colCl[est.cols] <- "character"
rm(pre.input)
input <- read.delim(file="preSizes.tab", stringsAsFactors=FALSE, colClasses=colCl)
scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily",
  "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum", "", NA)
scales <- factor(scales, levels=scales, ordered=TRUE)
input$BodySizeScale <- factor(input$BodySizeScale, levels=scales, ordered=TRUE)
ATD.cols <- which(colnames(input) == "APLength" | colnames(input) == 
    "TransverseLength" | colnames(input) == "DVLength")
photo.cols <- which(colnames(input) == "PhotoAP" | colnames(input) == 
    "PhotoTransverse" | colnames(input) == "PhotoDV")
est.cols <- which(colnames(input) == "Est_AP" | colnames(input) == "Est_T" |
    colnames(input) == "Est_DV")
AP.cols <- which(colnames(input) == "APLength" | colnames(input) == "PhotoAP" |
    colnames(input) == "APScale")
T.cols <- which(colnames(input) == "TransverseLength" | colnames(input) ==
    "PhotoTransverse" | colnames(input) == "TransverseScale")
DV.cols <- which(colnames(input) == "DVLength" | colnames(input) == "PhotoDV" |
    colnames(input) == "DVScale")
AbsStratDist.col <- which(colnames(input) == "AbsStratDistance")
out <- input                    # Work with 'out', saving 'input' for reference
colnames(out[photo.cols])       # "PhotoAP", "PhotoTransverse", "PhotoDV"
colnames(out[est.cols])         # "Est_AP", "Est_T", "Est_DV"
colnames(out[AP.cols])          # "APLength", "PhotoAP", "APScale"
colnames(out[T.cols])           # "TrLength", "PhotoTr", "TrScale"
colnames(out[DV.cols])          # "DVLength", "PhotoDV", "DVScale"
str(input)
# Make sure the 4 "SizeChanged" and "Est_X" columns are input as characters. If 
# a column is blank, it by default is classed as a logical (which causes errors 
# below)
head(input)
table(input$BodySizeScale)

# Troubleshooting
if(any(is.na(input$early_age)) || any(is.na(input$late_age)))
  stop("Some entries have missing age ranges. Update ages from the Paleobiology
    Database before proceeding.\n")
if(length(table(table(input$Genus))) > 1L) {
  print(which(table(input$Genus) > 1L))
  stop("The above genus entries are entered twice. Delete the outdated entry/entries?")
}
# Ignore the 72 extant brachiopod genera with multiple species in the database
# (mostly extant species, some fossil species), cases where multiple subgenera
# are included in same genus, and where there are ecologically quite different
# species in same genus: brachiopod Eoplectodonta, polychaete Glycera, trilobite
# Isotelus, brachiopod Leangella, bivalve Modiolopsis, brachiopod Plectodonta,
# rugose Streptelasma, brachiopod Strophomena, and Tentaculites


## FUNCTIONS ---------------------------------------

## FUNCTION TO FIND RELATIVES OF SIMILAR AGE CODED AT BEST AVAILABLE RESOLUTION
## (BUT ADDING THOSE CODED AS GENERA, SUBGENERA, AND SPECIES WHEN POSSIBLE)
# x = data frame of all data.
# i = index (row number for taxon entry being considered).
# start = taxonomic level to start (default = subfamily, with 1=species and 14=NA)
# end = taxonomic level to end (default = NA, running higher up through all levels,
#     including unknowns).
# photo.cols and est.cols = which columns to use when calling 'any.missing' to
#     extract relatives with complete measurements.
# simtime = logical. If TRUE (default), uses similarity of geological range to
#     choose among multiple relatives. (If FALSE, returns all complete relatives)
find.rel <- function(x, i, start=4, end=12, photo.cols=NULL, est.cols=NULL, sim.time=TRUE) {
  if(any(is.null(photo.cols), is.null(est.cols)))
    stop("photo.cols and est.cols need to be specified\n")
  scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily",
    "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum")
  scales <- factor(scales, levels=scales, ordered=TRUE)
  others <- x[-i, ] # Entry cannot be its own relative
  for(e in start:end) {
    # Identify correct column for taxonomic scale being considered
    sc.col <- which(colnames(others) == scales[e])
    # Ignore if unassigned taxa
    if(x[i,sc.col] == "") next
    if(x[i,sc.col] == "UNCERTAIN") next
    # Identify relatives coded at species-, subgenus-, or genus-level AND with
    # all three measures
    rels <- others[which(others[ ,sc.col] == x[i,sc.col]), ]
    rels <- rels[which(rels$BodySizeScale == "Species" | rels$BodySizeScale ==
        "Subgenus" | rels$BodySizeScale == "Genus"), ]
    nr <- nrow(rels)
    if(nr == 0L) next
    poss.rels <- rels
    # Test if any relatives with all 3 measurements complete
    sq <- seq.int(nr)
    complete <- !sapply(sq, function(sq) any.missing(rels[sq, ], photo.cols,
      est.cols)$any)
    rels <- rels[complete, ]
    nr <- nrow(rels)
    if(nr == 0L) {
      # If not, do any relatives have at least 1 measured measurement?
      part.complete <- !sapply(sq, function(sq) any(is.na(poss.rels[sq,photo.cols])) ||
          any(poss.rels[sq,photo.cols] == ""))
      rels <- poss.rels[part.complete, ]
      nr <- nrow(rels)
    }
    if(nr > 0L) break
  }
  size.sc <- as.character(scales[e])
  # If multiple matches, pick one with most similar geologic range. If still
  # multiple matches, use character matching to find most likely type taxon.
  if(nr > 1L & sim.time) {
    age.dev <- (out$early_age[i] - rels$early_age) ^ 2 +
      (out$late_age[i] - rels$late_age) ^ 2
    if(length(which(age.dev == min(age.dev))) > 1L) {
      higher.taxon <- rels[1,which(colnames(rels)==scales[e])]
      sim.age.rels <- rels[which(age.dev == min(age.dev)),]
      type <- which.min(adist(sim.age.rels$Genus, higher.taxon,
        cost=list(ins=2, del=1, sub=10)))
      rel <- sim.age.rels[type, ]
    } else rel <- rels[which.min(age.dev), ]
  } else rel <- rels
  return(list(rel=rel, size.sc=size.sc))
}



## ARE ANY MEASUREMENTS MISSING OR PREVIOUSLY ABSENT, AND WHICH?
# cols  = identifies which columns to check across.
any.missing <- function(x, photo.cols, est.cols) {
  missing <- any(is.na(x[ ,photo.cols])) || any(x[ ,photo.cols] == "") ||
    (any(x[ ,est.cols] == "Estimated"))
  AP <- Trans <- DV <- NULL
  if(missing & (is.na(x[ ,photo.cols[1]]) | x[ ,photo.cols[1]] == "" | x[ ,est.cols[1]] ==
      "Estimated")) AP <- "AP"
  if(missing & (is.na(x[ ,photo.cols[2]]) | x[ ,photo.cols[2]] == "" | x[ ,est.cols[2]] ==
      "Estimated")) Trans <- "T"
  if(missing & (is.na(x[ ,photo.cols[3]]) | x[ ,photo.cols[3]] == "" | x[ ,est.cols[3]] ==
      "Estimated")) DV <- "DV"
  return(list(any=missing, which=c(AP, Trans, DV)))
}


## PREPARE TEXT FOR DOCUMENTING ESTIMATION HISTORY
# x = output from 'any.missing'
pre.text <- function(x) {
  if(length(x$which) == 1L) txt <- paste(x$which, "estimated from proportions")
  if(length(x$which) == 2L) txt <- paste(x$which[1], "and", x$which[2],
    "estimated from proportions")
  return(txt)
}


## IMPROVED ALL.EQUAL THAT JUST PRINTS THOSE THAT ARE CHANGED (IGNORING ROW 
## NUMBERS), USING SPECIFIED NUMBER OF SIGNIFICANT DIGITS AND SPECIFYING WHICH
## COLUMNS TO ROUND
better.all.equal <- function(a, b, sig.digits=2, nums=sort(c(AP.cols, T.cols, 
  DV.cols, AbsStratDist.col))) {
  if(!identical(dim(a), dim(b))) stop("data frames have different sizes\n")
  cn <- seq.int(ncol(a))
  row.names(a) <- row.names(b) <- NULL
  a.round <- a
  b.round <- b
  a.round[nums] <- signif(a[nums], sig.digits)
  b.round[nums] <- signif(b[nums], sig.digits)
  wh.diff <- sapply(cn, function(cn) !identical(a.round[ ,cn], b.round[ ,cn]))
  ab <- rbind(a, b)
  return(ab[ ,which(wh.diff), drop=FALSE])
}


## IDENTIFY HOW ABSOLUTE STRAT DISTANCE WAS CALCULATED, AND PROPOGATE USING SAME
## CALCULATION. CALCULATIONS USED INCLUDE 30, 45, & 60 DEGREES, and 25%, 33%, 
## 50%, 67%, 75%, 90%, 200%, and 400%. NOTE USES 3 SIGNIFICANT DIGITS FOR
## IDENTIFYING MATCHES (IN CASE OF INEXACT EQUALITY), BUT OUTPUTS ALL DIGITS,
## AND 'NA' IF NO MATCH)
# target = life habit data frame for target taxon to be estimated
# source = life-habit data frame containing best relative.
get.strat <- function(target, ref) {
  # Remember trig functions in R (and Excel) in radians
  angle.30 <- sin(30 * pi / 180)
  angle.45 <- sin(45 * pi / 180)
  angle.60 <- sin(60 * pi / 180)
  proportions <- c(.25, (1 / 3), 0.5, (2 / 3), 0.75, 0.9, 2.0, 4.0)
  ref_ms <- c(ref$APLength, ref$TransverseLength, ref$DVLength)
  ref_dists <- c(1, -1) %x% c(ref_ms, angle.30 * ref_ms, angle.45 * ref_ms, 
    angle.60 * ref_ms, proportions %x% ref_ms)
  wh.m <- match(signif(ref$AbsStratDistance, 3), signif(ref_dists, 3))
  target_ms <- c(target$APLength, target$TransverseLength, target$DVLength)
  target.strat.dist <- (c(1, -1) %x% c(target_ms, angle.30 * target_ms, 
    angle.45 * target_ms, angle.60 * target_ms, proportions %x% target_ms))[wh.m]
  return(target.strat.dist)
}



# Examples
i <- which(out$Genus=="Etoctenocystis")   # ctenocystoid Etoctenocystis
# i <- which(out$Genus=="Isogramma")      # brachiopod Isogramma
# i <- which(out$Genus=="Rhytimya")       # bivalve Rhytimya
# i <- which(out$Genus=="Bighornia")      # rugose coral Bighornia
out[i, c(1:16, 24:32, 36)]
any.missing(out[i, ], photo.cols, est.cols)
rel <- find.rel(out, i, photo.cols=photo.cols, est.cols=est.cols)
rel$size.sc
rel$rel[c(1:16, 24:32, 36)]
get.strat(target=out[i,], ref=rel$rel) # Only works if all 3 measures are in target
rels <- find.rel(out, i, photo.cols=photo.cols, est.cols=est.cols, sim.time=FALSE)$rel
nr <- 1:nrow(rels)
sapply(nr, function(nr) get.strat(out[i,], rels[nr,]))

rm(list=c("i", "rel"))





## PROPOGATE BODY SIZES --------------------------------------------

# Go through each record one by one, estimating body size measurements when
# missing. Because we are assuming an isometric (same-shape) model, measurements
# are not log-transformed. In future, if wish to implement a linear regression
# model for estimating sizes (i.e., calculate allometric power function among
# relatives to estimate missing measurements), will need to log-transform the
# measurements.

index <- seq(100, nrow(input), by=100) # For keeping track
today <- format(Sys.Date(), "%m/%d/%Y")
angle.30 <- sin(30 * pi / 180)
angle.45 <- sin(45 * pi / 180)
angle.60 <- sin(60 * pi / 180)
proportions <- c(.25, (1 / 3), 0.5, (2 / 3), 0.75, 0.9, 2.0, 4.0)
AbsStratDist.text <- c("AP.", "T.", "DV.",   "30 degrees (from horiz.) of AP, or half AP.", 
  "30 degrees (from horiz.) of T, or half AP.", 
  "30 degrees (from horiz.) of DV, or half AP.", "45 degrees (from horiz.) of AP.", 
  "45 degrees (from horiz.) of T.", "45 degrees (from horiz.) of DV.", 
  "60 degrees (from horiz.) of AP.", "60 degrees (from horiz.) of T.", 
  "60 degrees (from horiz.) of DV.", "25% of AP.", "25% of T.", "25% of DV.", 
  "33% of AP.", "33% of T.", "33% of DV.", "50% of AP (or 30 degrees).", 
  "50% of T (or 30 degrees).", "50% of DV (or 30 degrees).", "66.7% of AP.", "66.7% of T.",
  "66.7% of DV.", "75% of AP.", "75% of T.", "75% of DV.", "90% of AP.", "90% of T.", 
  "90% of DV.", "200% of AP.", "200% of T.", "200% of DV.", "400% of AP.", "400% of T.",
  "400% of DV.")
seq.AbsStratDist <- rep(seq.int(AbsStratDist.text), 2)
interactive <- TRUE   # If want to watch updates in real time
if(interactive) par("ask"=TRUE) else par("ask"=FALSE)
(start.t <- Sys.time())

for(i in 1:nrow(out)) {

  if(i %in% index) cat("record", i, "of", nrow(out), ":", out$Genus[i],
    out$Species[i], "\n")

  # Ignore if no higher taxonomic information at all
  if(all(input[i, 2:10] == "")) next
  
  # Ignore if already coded at species, subgenus or genus level AND complete
  this.scale <- out$BodySizeScale[i]
  missing <- any.missing(out[i, ], photo.cols, est.cols)
  missing.strat <- is.na(out$AbsStratDistance[i])
  if(!missing$any & !missing.strat &
      (this.scale == "Species" | this.scale == "Subgenus" | this.scale == "Genus")) next

  rel <- rels <- NULL
  change <- input$SizeChanged[i]

  # If entry is at species-, subgenus-, or genus-level AND 1 or more measures
  # are missing, proceed to estimating missing measurements:
  if(this.scale <= "Genus" & missing$any) {

    if(length(missing$any) == 3L) stop(paste("Entry", i, "(", out$Genus[i], ")",
      "is listed as species/subgenus/genus level, but is missing all measurements.\n"))

    rel <- find.rel(x=out, i=i, photo.cols=photo.cols, est.cols=est.cols)
    if(nrow(rel$rel) == 0L) next

    AP.DV <- rel$rel$APLength / rel$rel$DVLength
    AP.Tr <- rel$rel$APLength / rel$rel$TransverseLength
    DV.Tr <- rel$rel$DVLength / rel$rel$TransverseLength

    # If 2 measurements are missing, use shape of relative (with all 3
    # measurements) to estimating missing ones
    if(length(missing$which) == 2L) {

      # If A/P is only available measurement:
      if(!"AP" %in% missing$which) {
        out$TransverseScale[i]<- out$DVScale[i] <- out$APScale[i]
        out$Est_T[i] <- out$Est_DV[i] <- "Estimated"
        out$PhotoTransverse[i] <- out$TransverseScale[i] * out$APLength[i] / AP.Tr
        out$PhotoDV[i] <- out$DVScale[i] * out$APLength[i] / AP.DV
      }

      # If transverse is only available measurement:
      if(!"T" %in% missing$which) {
        out$APScale[i] <- out$DVScale[i] <- out$TransverseScale[i]
        out$Est_AP[i] <- out$Est_DV[i] <- "Estimated"
        out$PhotoAP[i] <- out$APScale[i] * out$TransverseLength[i] * AP.Tr
        out$PhotoDV[i] <- out$DVScale[i] * out$TransverseLength[i] * DV.Tr
      }

      # If D/V is only available measurement:
      if(!"DV" %in% missing$which) {
        out$TransverseScale[i] <- out$APScale[i] <- out$DVScale[i]
        out$Est_T[i] <- out$Est_AP[i] <- "Estimated"
        out$PhotoTransverse[i] <- out$TransverseScale[i] * out$DVLength[i] / DV.Tr
        out$PhotoAP[i] <- out$APScale[i] * out$DVLength[i] * AP.DV
      }
    }

    # If 1 measurement is missing, use shape of relative (with all 3
    # measurements) to estimating missing one
    if(length(missing$which) == 1L) {
      est1 <- est2 <- NA

      # If A/P is only missing measurement:
      if("AP" %in% missing$which) {
        out$APScale[i] <- out$TransverseScale[i]
        out$Est_AP[i] <- "Estimated"
        est1 <- out$APScale[i] * out$DVLength[i] * AP.DV
        est2 <- out$APScale[i] * out$TransverseLength[i] * AP.Tr
        out$PhotoAP[i] <- mean(c(est1, est2))
      }

      # If transverse is only missing measurement:
      if("T" %in% missing$which) {
        out$TransverseScale[i] <- out$APScale[i]
        out$Est_T[i] <- "Estimated"
        est1 <- out$TransverseScale[i] * out$APLength[i] / AP.Tr
        est2 <- out$TransverseScale[i] * out$DVLength[i] / DV.Tr
        out$PhotoTransverse[i] <- mean(c(est1, est2))
      }

      # If D/V is only missing measurement:
      if("DV" %in% missing$which) {
        out$DVScale[i] <- out$TransverseScale[i]
        out$Est_DV[i] <- "Estimated"
        est1 <- out$DVScale[i] * out$APLength[i] / AP.DV
        est2 <- out$DVScale[i] * out$TransverseLength[i] * DV.Tr
        out$PhotoDV[i] <- mean(c(est1, est2))
      }
    }

    change <- "maybe"
    out$History_Size[i] <- paste(pre.text(missing), "in same", rel$size.sc,
      as.character(rel$rel[which(colnames(rel$rel) == rel$size.sc)]), "of",
      rel$rel$BodyMeasureReference)
  }

  # If entry is NOT at genus-or-better level (i.e., subfamily or greater) [OR, 
  # if reported at genus or better level, but missing all 3 measurements], find 
  # closest-aged relative and drop in all 3 measurements, maintain original date
  # entered, and update metadata (and erasing history, in case previously
  # entered incorrectly). NOTE THIS REFERS TO PREVIOUSLY UPDATED DATA IN CASE
  # THERE IS A MORE SUITABLE RELATIVE.
  if(this.scale > "Genus" | (this.scale <= "Genus" & length(missing$which) == 3L)) {
    rel <- find.rel(x=out, i=i, photo.cols=photo.cols, est.cols=est.cols)
    if(nrow(rel$rel) == 0L) next
    out$RefGenusSize[i] <- rel$rel$RefGenusSize
    out$RefSpeciesSize[i] <- rel$rel$RefSpeciesSize
    out$BodySizeScale[i] <- rel$size.sc
    out[i,AP.cols] <- rel$rel[AP.cols]
    out[i,T.cols] <- rel$rel[T.cols]
    out[i,DV.cols] <- rel$rel[DV.cols]
    out[i,est.cols] <- "Estimated"
    if(ncol(better.all.equal(input[i, ], out[i, ], sig.digits=4)) > 0L) {
      # Only update metadata if actually changed (reference taxa and estimates
      # are always over-ridden in case there is new data)
      change <- "maybe"
      out$DateEntered_Size[i] <- rel$rel$DateEntered_Size
      out$Enterer[i] <- rel$rel$Enterer
      out$History_Size[i] <- ""
      }
  }

  # Update actual sizes. (Will be overridden by FileMakerPro, but need to update
  # because the raw lengths are used in estimating shape proportions.)
  out$APLength[i] <- out$PhotoAP[i] / out$APScale[i]
  out$TransverseLength[i] <- out$PhotoTransverse[i] / out$TransverseScale[i]
  out$DVLength[i] <- out$PhotoDV[i] / out$DVScale[i]

  # Propogate AbsStratDist 
  if(missing.strat) {
    # ... if missing AbsStratDist but available via ALL best relatives (that are
    # within same suborder [end=7] or lower resolution)
    rels <- find.rel(x=out, i=i, end=7, photo.cols=photo.cols, est.cols=est.cols, 
      sim.time=FALSE)$rel
    rels.with.strats <- 0L
    if(!is.null(rels)) rels.with.strats <- length(na.omit(rels$AbsStratDistance))
    if(missing.strat & rels.with.strats > 0L) {
      nr <- 1:nrow(rels)
      poss.strats <- as.vector(na.omit(sapply(nr, function(nr) get.strat(out[i,], 
        rels[nr,]))))
      # Discard all if some aren't a canonical orientation (because ignored in 'get.strat')
      if(length(poss.strats) != rels.with.strats) poss.strats <- NA
      if(length(unique(na.omit(poss.strats))) == 1L) out$AbsStratDistance[i] <- 
        poss.strats[1]
    }
  } else {
    # Or update AbsStratDist if previously existed, but some measurements were updated
    orig.ms <- unlist(input[i, ATD.cols])
    poss.dists <- c(1, -1) %x% c(orig.ms, angle.30 * orig.ms, angle.45 * orig.ms, 
      angle.60 * orig.ms, proportions %x% orig.ms)
    if(change == "maybe" & !missing.strat & 
        signif(input$AbsStratDistance[i], 3) %in% signif(poss.dists, 3))
      out$AbsStratDistance[i] <- get.strat(out[i,], input[i,])
  }

  # Add "check" tag if any size or AbsStratDist was changed (to 2 significant digits)
  if(!identical(signif(input[i, photo.cols], 2), signif(out[i, photo.cols], 2)) || 
      !identical(signif(input$AbsStratDistance[i], 2), signif(out$AbsStratDistance[i], 2))) {
    out$SizeChanged[i] <- "Check"
  }

  # Update history if AbsStratDist changed (using updated measurements)
  if(!identical(signif(input$AbsStratDistance[i], 2), signif(out$AbsStratDistance[i], 2))) {
    orig.ms <- unlist(out[i, ATD.cols])
    poss.dists <- c(1, -1) %x% c(orig.ms, angle.30 * orig.ms, angle.45 * orig.ms, 
      angle.60 * orig.ms, proportions %x% orig.ms)
    wh.m <- match(signif(out$AbsStratDistance[i], 3), signif(poss.dists, 3))
    if(out$History_Size[i] == "") out$History_Size[i] <- 
      paste("AbsStratDist estimated from", AbsStratDist.text[seq.AbsStratDist[wh.m]]) else 
        out$History_Size[i] <- paste("AbsStratDist estimated from ", 
          AbsStratDist.text[seq.AbsStratDist[wh.m]], " ", out$History_Size[i], sep="")
  }
  
  # Interactive mode (to observe how states are being propogated)
  if(interactive & ncol(better.all.equal(input[i, ], out[i, ], sig.digits = 3)) > 0L) {
    plot(1, 1, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
    text(1, 1, as.character(paste(out$Genus[i], out$Species[i])), cex=2)
    cat(as.character(paste(out$Genus[i], out$Species[i])), "\n")
    # print(better.all.equal(input[i, ], out[i, ]))
    print(better.all.equal(input[i,-c(20, ATD.cols)], out[i,-c(20, ATD.cols)], 
      nums=c(23:25, 32)))     # If want to mask date and calculated lengths
    cat("\n")
    }

}
(Sys.time() - start.t)    

round(table(input$BodySizeScale) * 100 / nrow(input), 1)
round(table(out$BodySizeScale) * 100 / nrow(out), 1)
table(input$BodySizeScale)
table(out$BodySizeScale)

rng <- range(na.omit(c(input$AbsStratDistance, out$AbsStratDistance)))
hist(out$AbsStratDistance, xlim=rng, col="white", border="white", n=50)
hist(out$AbsStratDistance, col="darkgray", border="white", add=TRUE, n=50)
hist(input$AbsStratDistance, col="transparent", border="black", add=TRUE, n=50)
abline(v=0, lwd=2, lty=2)


## EXPORT DATA -------------------------------------------------------------
write.table(out, file="PostSizes.tab", quote=FALSE, sep="\t", row.names=FALSE)

# (1) Open in Excel to confirm looks acceptable. Replace (matching entire cell 
# contents) "NA"s in body size data and AbsStratDist with blank cells.

# (2) Open in Word to remove quotation marks around the text entries, (replacing
# "^t with " and replacing ^t" with " and replacing "" with ").

# (3) Open FileMakerPro and import, updating records by matching names and using
# the IDNumber as the matching identifier. (Fine to not import the taxonomic
# names and geological ranges.)

## Manual trouble-shooting: Some propogations are known to be (potentially)
## incorrect. It would be more efficient it the following rules could be checked
## algorithmically, but that may be challenging because of the interpretative
## nuances involved. One possibility (for the size-related feeding and mobility
## rules, at least) is to add a code that uses some form of binned size
## regression analysis (within classes) to extrapolate the size-rules. Another
## option might be to use machine-learning (i.e., classification trees). Either
## way, would need to add an "exception" field when a coding defies the standard
## coding rules (for example, when crinoid sizes lack column lengths but a
## AbsStrat coding is still possible, or when the functional gill size in
## vermiculariids is best approximated by transverse width and A/P length).

# Once imported, run following manual corrections. (Note the RelStrat should not
# be deleted, but updated as needed, with other stratifications.)

# (1) Pelagic taxa given benthic AbsStratDists: Find Fluidic=1 & Insubstantial=1
# & AbsStratDist=">-10000" [ANY] & SizeChanged=CHECK and delete AbsStratDist (if
# needs correcting).

# (2) Supported taxa given self-supported AbsStratDists: Find Supported=1 & 
# AbsStratDist=">-10000" [ANY] & SizeChanged=CHECK and delete AbsStratDist (if
# needs correcting).

# (3) Epibiotic (but barely raised, so should be coded as self-supported) taxa 
# given incorrect benthic (as if not epibiotic) AbsStratDists: Find Biotic=1 &
# AbsStratDist=">-10000" [ANY] & SizeChanged=CHECK and delete AbsStratDist (if
# needs correcting).

# (4) Confirm all exclusively infaunal taxa have negative AbsStratDists and 
# exclusively epifaunal taxa have positive values. (But don't be surprised by 
# semi-infaunal taxa that are simultaneously epifaunal and infaunal.)
# Alternatively, confirm that all with negative AbsStratDists have
# WithinAbsStrat=1 and that all with positive AbsStratDists have
# AboveAbsStrat=1.

# (5) Confirm that taxa with AbsStratDist=">-100000" values [ANY] match the 
# correct AbsStrat coding. (Sort by AbsStratDist when checking manually.) When 
# checking this, also worth checking the RelStrat and AbsFoodStrat codings. In 
# general, if animal is epibenthic, self-supported, and filter-feeder, the 
# AbsFoodStrat will be same as AbsStrat; if mass-feeder, AbsFoodStrat will be 0;
# and if raptorial, AbsFoodStrat will be where food is located (often 0.25 
# coding). For RelStrat, useful to sort of Phylum>Class>DV or AP length. For
# RelStrat, recall that for semi-infaunals this will correspond to the animal's
# major axis and not the distance from sea floor.

# (6) Confirm RelFoodStrat for filter feeders, sorting by phyla and then either 
# AbsStratDist or A/P length. For ambient filter feeders (SPONGES and 
# suspension-feeding ECHINODERMS and CNIDARIANS) where flow rate depends on 
# distance from sea floor, confirm those 15-200 mm tall are 0.5; refer to 
# LifeHabitNotes.docs for additional categorizations. For active filter feeders,
# the coding depends on the size of the pumping organ. All BRYOZOANS, 
# GRAPTOLITES, and PTEROBRANCHS should be 0.25. BRACHIOPODS should be half the 
# A/P length. Filter-feeding mollusks (BIVALVES, ROSTROCONCHS, and GASTROPODS) 
# should be twice the A/P length. POLYCHAETES and TENTACULITIDS should be twice 
# the transverse length. Filter-feeding MICROCRUSTACEANS (amphipods, mysids, 
# etc.) generally should be 0.25. ASCIDIANS should generally be 0.5.

