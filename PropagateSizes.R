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
#
# A. Find closest relative with ALL 3 ATD. 1) if multiple relatives exist, pick
# the one (regardless of whether sp or genus) that is closest to the same age
# (earlyRel - earlyEntry + lateR - lateE, and pick using absolute difference).
# Why? Because size is known to change through time (although unclear if shape
# does). Note that this means that taxa missing a range will not get an
# estimated body size.
#
# B. If all 3 lengths are missing (not possible if SizeScale=sp/g?), drop in all
# 3 measurements from relative.
#
# C. If 2 lengths are missing, estimate the missing ones using the relative's
# shape.
#
# D. If 1 length is missing, estimating the missing length using mean of
# relative's shape (i.e., ratio of other lengths). (Using arithmetic mean
# instead of geometric mean to best estimate an isometric shape; use of geomean,
# in trials, does not produce equivalently shaped estimates.)
#
# E. Update "estimate" check boxes and metadata accordinging, for each if ()
# step above.
#
# 3. If entry is not at genus or better level (i.e., subfamily or greater), find
# closest-aged relative and drop in all 3 measurements, updating metadata. Only
# in this case add tags for RefGenusSize and RefSpeciesSize.





## EXPORT DATA -------------------------------------------------------------

# (0) Not required (because the code algorithmically updates), but if want a
# "fresh" propogation, consider first deleting all body size data for any
# estimates (EstAP, EstT, EstDV or non-species/subgenus/genus). (This is
# recommended if, for example, a species/subgenus/genus A entry has at least one
# estimated size measurement, and it might be used to estimate a missing size
# for species B, which was originally used to estimate the missing size for A.)
# IF DO THIS, PERFORM THE DELETIONS ON A COPY OF THE DATABASE RATHER THAN THE
# MASTER DATABASE ITSELF!

# (1) Before exporting, sort the entries so that BodySizeScale = Species is first,
# followed by Subgenus, Genus, etc., and second by AbsStratDistance (in
# descending order, with largest first), and third BodyVolume (with largest
# first). (This sort order means taxa with AbsStratDistances and all three
# measurements are placed before those missing one or more sizes.)

# (2) Run relevant code in SelectCols.R for PropogateSizes.R to obtain following
# output. Then continue with step 3.

# IDNumber

# Taxonomy: Phylum, Subphylum, Class, Subclass, Order, Suborder, Superfamily,
# Family, Subfamily, Genus, Subgenus, Species

# Proxy fields: max_ma, min_ma, BodySizeScale, RefGenusSize,
# RefSpeciesSize, Enterer, DateEntered_Size, SizeChanged, History_Size,
# BodyMeasureReference

# Body size characters (can really be in any order, as called by name):
# APLength, TransverseLength, DVLength, PhotoAP, PhotoTransverse, PhotoDV,
# APScale, TransverseScale, DVScale, Est_AP, Est_T, Est_DV, AbsStratDist

# (Note "Intact" is excluded because only appropriate to the raw (observed) 
# species/genus-level entries) and not the propogated ones.

# (3) Open file in MSWord (make sure smart quotes are off: File > Options >
# Proofing > Autocorrect Options > Autoformat As You Type > uncheck Smart
# Quotes) and delete any hidden tabs (tab within a text field) and all
# problematic (i.e., double) quotation marks (replacing [UNLESS THE QUOTATION
# MARK IS CORRECTLY AT THE END OF A TEXT FIELD] "^t with ^t and replacing ^t"
# with ^t and replacing "" with "). Re-save file in same format.

# (5) Open in Excel. Add a new column counting 'PhotoX' columns with values.
# (HINT: Use =MIN((3-COUNTA(Est_AP:Est_DV)),COUNT(APLength:DVLength)) that
# allows including the Est_X columns!) TROUBLESHOOT: Confirm that all
# 'Sp/Subg/Gen' have at least 1 measurement! Also delete any NAs in
# AbsStratDistance and size measures, and delete size measures that are 0.

# (6) Sort the BodySizeScale = 'Sp/Subg/Gen' rows by (1) number of PhotoX
# columns (largest first) so entries with complete (all 3) size measurements are
# first and most incomplete are last. That way those with best scales and
# more-complete sizes are checked first, so that later entries can use the
# largest available pool of relatives. (2) Second, sort item by AbsStratDist,
# with largest values first (so those with estimated AbsStratDists are
# considered first to propogate to those lacking them). (3) Third, sort by Body
# Size, with largest values first. (You can use the product of remaining values
# as a proxy.)

# (7) Delete the added 'PhotoX' column and resave.



## READ AND PROCESS DATA ---------------------------------------------------
rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/Documents/GSA (& NSF & NAPC)/2016GSA/GSA2016 analyses")

pre.input <- read.delim(file = "preSizes.tab", stringsAsFactors = FALSE)
# pre.input <- read.delim(file = "PreSizes_Constant_withPBDB.tab", stringsAsFactors = FALSE)
# pre.input <- read.delim(file = "EchinoPreSizes_withPBDB.tab", stringsAsFactors = FALSE)

# Following confirms that certain columns (with many empty cells) are treated as
# characters:
est.cols <- which(colnames(pre.input) == "SizeChanged" | 
                    colnames(pre.input) == "Est_AP" |
                    colnames(pre.input) == "Est_T" |
                    colnames(pre.input) == "Est_DV")
colCl <- c(rep(NA, ncol(pre.input)))
colCl[est.cols] <- "character"
rm(pre.input)

input <- read.delim(file = "preSizes.tab", stringsAsFactors = FALSE, colClasses = colCl)
# input <- read.delim(file = "PreSizes_Constant_withPBDB.tab", stringsAsFactors = FALSE)
# input <- read.delim(file = "EchinoPreSizes_withPBDB.tab", stringsAsFactors = FALSE)
scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily",
  "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum", "", NA)
scales <- factor(scales, levels = scales, ordered = TRUE)
input$BodySizeScale <- factor(input$BodySizeScale, levels = scales, ordered = TRUE)
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

# TROUBLESHOOT: Make sure the 4 "SizeChanged" and "Est_X" columns are input as
# characters. If a column is blank, it is classed by default as a logical (which
# causes errors below). If needed (such as when there are no "check"ed entries,
# use next lines to force to proper class.)

# input$SizeChanged <- replace(input$SizeChanged, which(is.na(input$SizeChanged)), "")

head(input)
tail(input)
table(input$BodySizeScale)

# Troubleshooting
if (any(is.na(input$max_ma)) || any(is.na(input$min_ma)))
  stop("Some entries have missing age ranges, which is used in body size propogation 
algorithm. Leaving ranges empty will mean that missing size measurements will not be propogated for these taxa. Update ages from the Paleobiology Database before proceeding.\n")

## Any duplicated ID numbers?
# Will match the incorrect life habits when importing
if (any(table(input$IDNumber) > 1)) {
  print(which(table(input$IDNumber) > 1))
  stop("There are duplicate IDNumbers. Fix before proceeding! See above for list.")
}



## FUNCTIONS ---------------------------------------

## FUNCTION TO FIND RELATIVES OF SIMILAR AGE CODED AT BEST AVAILABLE RESOLUTION
## (BUT ADDING THOSE CODED AS GENERA, SUBGENERA, AND SPECIES WHEN POSSIBLE)
#  x = data frame of all data.
#  i = index (row number for taxon entry being considered).
#  start = taxonomic level to start (default = subfamily, with 1=species and 14=NA)
#  end = taxonomic level to end (default = NA, running higher up through all levels,
#     including unknowns).
#  photo.cols and est.cols = which columns to use when calling 'any.missing' to
#     extract relatives with complete measurements.
#  all.3 = logical. If TRUE (default), the relatives ONLY contain values for all
#     three measurements. Recommend setting FALSE when propogating body sizes for
#     higher taxa (greater than genus level), where it is acceptable that there is
#     at least one available measurement, with other measurements estimated
#     using complete relatives, and when propagating AbsStratDistance.
#  sim.time = logical. If TRUE (default), uses similarity of geological range to
#     choose among multiple relatives. If no range available, uses all relatives.
#     (If FALSE, returns all complete relatives.)
find.rel <- function(x, i, start = 4, end = 12, photo.cols = NULL, 
                     est.cols = NULL, all.3 = TRUE, sim.time = TRUE) {
  if (any(is.null(photo.cols), is.null(est.cols)))
    stop("photo.cols and est.cols need to be specified\n")
  scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily",
    "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum")
  scales <- factor(scales, levels = scales, ordered = TRUE)
  others <- x[-i, ] # Entry cannot be its own relative
  for (e in start:end) {
    # Identify correct column for taxonomic scale being considered
    sc.col <- which(colnames(others) == scales[e])
    # Ignore if unassigned taxa
    if (x[i, sc.col] == "" | x[i, sc.col] == "UNCERTAIN" |
        is.na(x[i, sc.col])) next
    # Identify relatives coded at species-, subgenus-, or genus-level AND with
    # all three measures
    rels <- others[which(others[, sc.col] == x[i, sc.col]), ]
    rels <- rels[which(rels$BodySizeScale == "Species" | 
                         rels$BodySizeScale == "Subgenus" | 
                         rels$BodySizeScale == "Genus"), ]
    nr <- nrow(rels)
    if (nr == 0L) next
    poss.rels <- rels
    # Test if there are any relatives with all 3 complete measurements
    sq <- seq.int(nr)
    complete <- !sapply(sq, function(sq) any.missing(rels[sq, ], photo.cols,
      est.cols)$any)
    rels <- rels[complete, ]
    nr <- nrow(rels)
    if (nr == 0L & all.3 == FALSE) {
      # If not, do any relatives have at least 1 measured measurement?
      part.complete <- !sapply(sq, function(sq) any(is.na(poss.rels[sq,photo.cols])) ||
          any(poss.rels[sq,photo.cols] == ""))
      rels <- poss.rels[part.complete,]
      nr <- nrow(rels)
    }
    if (nr > 0L) break # Note this sets 'rels' to be NULL rather than blank matrix
  }
  size.sc <- as.character(scales[e])
  # If multiple matches, pick one with most similar geologic range. If still
  # multiple matches (or the reference genus lacks a temporal range), use
  # character matching to find most likely type taxon.
  if (nr > 1L & sim.time) {
    age.dev <- (out$max_ma[i] - rels$max_ma) ^ 2 +
      (out$min_ma[i] - rels$min_ma) ^ 2
    if (length(which(age.dev == min(age.dev))) > 1L | all(is.na(age.dev))) {
      higher.taxon <- rels[1, which(colnames(rels) == scales[e])]
      if (all(is.na(age.dev)))
        sim.age.rels <- rels else
        sim.age.rels <- rels[which(age.dev == min(age.dev)), ]
      type <- which.min(adist(sim.age.rels$Genus, higher.taxon,
        cost = list(ins = 2, del = 1, sub = 10)))
      rel <- sim.age.rels[type, ]
    } else rel <- rels[which.min(age.dev), ]
  } else rel <- rels
  return(list(rel = rel, size.sc = size.sc))
}



## ARE ANY MEASUREMENTS MISSING OR PREVIOUSLY ABSENT, AND WHICH?
#  photo.cols = which columns contain the PhotoAP, PhotoDV, and PhotoT
#     measurements, used to extract relatives with complete measurements.
#  est.cols = which columns contain the Est_AP, Est_DV, and Est_T measurements,
#     used to extract relatives with complete measurements.
any.missing <- function(x, photo.cols, est.cols) {
  missing <- any(is.na(x[ ,photo.cols])) || any(x[ ,photo.cols] == "") ||
    (any(x[ ,est.cols] == "Estimated"))
  AP <- Trans <- DV <- NULL
  if (missing & (is.na(x[ ,photo.cols[1]]) | x[ ,photo.cols[1]] == "" | 
                x[ ,est.cols[1]] == "Estimated")) AP <- "AP"
  if (missing & (is.na(x[ ,photo.cols[2]]) | x[ ,photo.cols[2]] == "" | 
                x[ ,est.cols[2]] == "Estimated")) Trans <- "T"
  if (missing & (is.na(x[ ,photo.cols[3]]) | x[ ,photo.cols[3]] == "" | 
                x[ ,est.cols[3]] == "Estimated")) DV <- "DV"
  return(list(any = missing, which = c(AP, Trans, DV)))
}


## PREPARE TEXT FOR DOCUMENTING ESTIMATION HISTORY
# x = output from 'any.missing'
pre.text <- function(x) {
  if (length(x$which) == 1L)
    txt <- paste(x$which, "estimated from proportions")
  if (length(x$which) == 2L)
    txt <- paste(x$which[1], "and", x$which[2],
                 "estimated from proportions")
  return(txt)
}


## IMPROVED ALL.EQUAL THAT JUST PRINTS THOSE THAT ARE CHANGED (IGNORING ROW 
## NUMBERS), USING SPECIFIED NUMBER OF SIGNIFICANT DIGITS AND SPECIFYING WHICH
## COLUMNS TO ROUND
better.all.equal <- function(a, b, sig.digits = 2, nums = sort(c(AP.cols, T.cols, 
  DV.cols, AbsStratDist.col))) {
  if (!identical(dim(a), dim(b)))
    stop("data frames have different sizes\n")
  cn <- seq.int(ncol(a))
  row.names(a) <- row.names(b) <- NULL
  a.round <- a
  b.round <- b
  a.round[nums] <- signif(a[nums], sig.digits)
  b.round[nums] <- signif(b[nums], sig.digits)
  wh.diff <- sapply(cn, function(cn) ! identical(a.round[, cn], b.round[, cn]))
  ab <- rbind(a, b)
  return(ab[ , which(wh.diff), drop = FALSE])
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



## Examples --------------------------------------------------------------------
# i <- which(out$Genus == "Etoctenocystis") # ctenocystoid Etoctenocystis
# i <- which(out$Genus == "Isogramma")      # brachiopod Isogramma
# i <- which(out$Genus == "Rhytimya")       # bivalve Rhytimya
# i <- which(out$Genus == "Bighornia")      # rugose coral Bighornia
i <- which(out$Genus == "Caturus")          # fish Caturus
out[i, c(1:16, 24:32, 36)]
any.missing(out[i, ], photo.cols, est.cols)
rel <- find.rel(out, i, photo.cols = photo.cols, est.cols = est.cols, all.3 = TRUE)
rel$size.sc
rel$rel[c(1:16, 24:32, 36)]
get.strat(target = out[i, ], ref = rel$rel) # Only works if all 3 measures are in target
rels <- find.rel(out, i, photo.cols = photo.cols, est.cols = est.cols, 
                 sim.time = FALSE, all.3 = TRUE)$rel
nr <- 1:nrow(rels)
sapply(nr, function(nr) get.strat(out[i,], rels[nr, ]))

rm(list = c("i", "rel"))





## PROPOGATE BODY SIZES --------------------------------------------

# Go through each record one by one, estimating body size measurements when
# missing. Because we are assuming an isometric (same-shape) model, measurements
# are not log-transformed. In future, if wish to implement a linear regression
# model for estimating sizes (i.e., calculate allometric power function among
# relatives to estimate missing measurements), will need to log-transform the
# measurements.

index <- seq(100, nrow(input), by = 100) # For keeping track
today <- format(Sys.Date(), "%m/%d/%Y")
angle.30 <- sin(30 * pi / 180)
angle.45 <- sin(45 * pi / 180)
angle.60 <- sin(60 * pi / 180)
proportions <- c(.25, (1 / 3), 0.5, (2 / 3), 0.75, 0.9, 2.0, 4.0)
AbsStratDist.text <- c("AP.", "T.", "DV.", "30 degrees (from horiz.) of AP, or half AP.", 
  "30 degrees (from horiz.) of T, or half T.", 
  "30 degrees (from horiz.) of DV, or half DV.", "45 degrees (from horiz.) of AP.", 
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
if (interactive) par("ask" = TRUE) else par("ask" = FALSE)
record.log <- TRUE
record.file <- "SizeLog.txt"
if (record.log) cat("Changes made to body sizes on", today, ":\n\n", file = record.file, append = FALSE)
(start.t <- Sys.time())

for (i in 1:nrow(out)) {
  
  if (i %in% index)
    cat("record", i, "of", nrow(out), ":", out$Genus[i], out$Species[i], "\n")
  
  # Ignore if no higher taxonomic information at all
  if (all(input[i, 2:10] == "")) next
  
  # Ignore if already coded at species, subgenus or genus level AND complete
  this.scale <- out$BodySizeScale[i]
  missing <- any.missing(out[i, ], photo.cols, est.cols)
  number.missing <- length(missing$which)
  missing.strat <- is.na(out$AbsStratDistance[i])
  # Note prior line should NOT check Est_AbsStratDistance here. Checks below instead.
  
  if (!missing$any & !missing.strat & (this.scale == "Species" | 
      this.scale == "Subgenus" | this.scale == "Genus")) next
  
  rel <- rels <- NULL
  change <- input$SizeChanged[i]
  
  # If entry is at species-, subgenus-, or genus-level AND 1 or more measures
  # are missing, proceed to estimating missing measurements:
  if (this.scale <= "Genus" & missing$any) {
    
    if (number.missing == 3L) stop(paste("Entry", i, "(", out$Genus[i], ")",
      "is listed as species/subgenus/genus level, but is missing all measurements.\n"))

    rel <- find.rel(x = out, i = i, photo.cols = photo.cols, 
                    est.cols = est.cols, all.3 = TRUE)
    if (nrow(rel$rel) == 0L) next
    
    AP.DV <- rel$rel$APLength / rel$rel$DVLength
    AP.Tr <- rel$rel$APLength / rel$rel$TransverseLength
    DV.Tr <- rel$rel$DVLength / rel$rel$TransverseLength
    
    # If 2 measurements are missing, use shape of relative (with all 3
    # measurements) to estimating missing ones
    if (number.missing == 2L) {
      
      # If A/P is only available measurement:
      if (!"AP" %in% missing$which) {
        out$TransverseScale[i] <- out$DVScale[i] <- out$APScale[i]
        out$Est_T[i] <- out$Est_DV[i] <- "Estimated"
        out$PhotoTransverse[i] <- out$TransverseScale[i] * out$APLength[i] / AP.Tr
        out$PhotoDV[i] <- out$DVScale[i] * out$APLength[i] / AP.DV
      }

      # If transverse is only available measurement:
      if (!"T" %in% missing$which) {
        out$APScale[i] <- out$DVScale[i] <- out$TransverseScale[i]
        out$Est_AP[i] <- out$Est_DV[i] <- "Estimated"
        out$PhotoAP[i] <- out$APScale[i] * out$TransverseLength[i] * AP.Tr
        out$PhotoDV[i] <- out$DVScale[i] * out$TransverseLength[i] * DV.Tr
      }

      # If D/V is only available measurement:
      if (!"DV" %in% missing$which) {
        out$TransverseScale[i] <- out$APScale[i] <- out$DVScale[i]
        out$Est_T[i] <- out$Est_AP[i] <- "Estimated"
        out$PhotoTransverse[i] <- out$TransverseScale[i] * out$DVLength[i] / DV.Tr
        out$PhotoAP[i] <- out$APScale[i] * out$DVLength[i] * AP.DV
      }
    }

    # If 1 measurement is missing, use shape of relative (with all 3
    # measurements) to estimating missing one
    if (number.missing == 1L) {
      est1 <- est2 <- NA
      
      # If A/P is only missing measurement:
      if ("AP" %in% missing$which) {
        out$APScale[i] <- out$TransverseScale[i]
        out$Est_AP[i] <- "Estimated"
        est1 <- out$APScale[i] * out$DVLength[i] * AP.DV
        est2 <- out$APScale[i] * out$TransverseLength[i] * AP.Tr
        out$PhotoAP[i] <- mean(c(est1, est2))
      }

      # If transverse is only missing measurement:
      if ("T" %in% missing$which) {
        out$TransverseScale[i] <- out$APScale[i]
        out$Est_T[i] <- "Estimated"
        est1 <- out$TransverseScale[i] * out$APLength[i] / AP.Tr
        est2 <- out$TransverseScale[i] * out$DVLength[i] / DV.Tr
        out$PhotoTransverse[i] <- mean(c(est1, est2))
      }

      # If D/V is only missing measurement:
      if ("DV" %in% missing$which) {
        out$DVScale[i] <- out$TransverseScale[i]
        out$Est_DV[i] <- "Estimated"
        est1 <- out$DVScale[i] * out$APLength[i] / AP.DV
        est2 <- out$DVScale[i] * out$TransverseLength[i] * DV.Tr
        out$PhotoDV[i] <- mean(c(est1, est2))
      }
    }
    
    change <- "maybe"
    out$History_Size[i] <- paste("On", today, pre.text(missing), "in same", rel$size.sc,
      as.character(rel$rel[which(colnames(rel$rel) == rel$size.sc)]), "of",
      rel$rel$BodyMeasureReference)
  }

  
  
  # If entry is NOT at genus-or-better level (i.e., subfamily or greater) [OR,
  # if is reported at genus or better level, but missing all 3 measurements],
  # find closest-aged relative and drop in all 3 measurements, maintain original
  # date entered, and update metadata (and erasing history, in case previously
  # entered incorrectly). NOTE THIS REFERS TO PREVIOUSLY UPDATED DATA IN CASE
  # THERE IS A MORE SUITABLE RELATIVE, USING all.3 = FALSE.
  if (this.scale > "Genus" |
      (this.scale <= "Genus" & number.missing == 3L)) {
    rel <- find.rel(x = out, i = i, photo.cols = photo.cols, est.cols = est.cols, 
                    sim.time = TRUE, all.3 = FALSE)
    if (is.null(rel$rel)) next
    out$RefGenusSize[i] <- rel$rel$RefGenusSize
    out$RefSpeciesSize[i] <- rel$rel$RefSpeciesSize
    out$BodySizeScale[i] <- rel$size.sc
    out[i, AP.cols] <- rel$rel[AP.cols]
    out[i, T.cols] <- rel$rel[T.cols]
    out[i, DV.cols] <- rel$rel[DV.cols]
    out[i, est.cols] <- "Estimated"
    if (ncol(better.all.equal(input[i,], out[i,], sig.digits = 4)) > 0L) {
      # Only update metadata if actually changed (reference taxa and estimates
      # are always over-ridden in case there is new data)
      change <- "maybe"
      # Tag with date the size is propogated for record-keeping:
      out$DateEntered_Size[i] <- today
      # But credit the original data enterer:
      out$Enterer[i] <- rel$rel$Enterer
      out$History_Size[i] <- ""
    }
  }
  
  # Update actual sizes. (Will be overridden by FileMakerPro, but need to update
  # because the raw lengths are used in estimating shape proportions.)
  out$APLength[i] <- out$PhotoAP[i] / out$APScale[i]
  out$TransverseLength[i] <- out$PhotoTransverse[i] / out$TransverseScale[i]
  out$DVLength[i] <- out$PhotoDV[i] / out$DVScale[i]

  # Propogate AbsStratDist (using closest relatives, regardless of whether
  # geologically contemporaneous or with missing some size measures)
  if (missing.strat) {
    # ... if missing AbsStratDist but available via ALL best relatives (that are
    # within same suborder [end = 7] or lower resolution)
    rels <- find.rel(x = out, i = i, end = 7, photo.cols = photo.cols, 
                     est.cols = est.cols, sim.time = FALSE, all.3 = FALSE)$rel
    rels.with.strats <- 0L
    if (!is.null(rels))
      rels.with.strats <- length(na.omit(rels$AbsStratDistance))
    if (missing.strat & rels.with.strats > 0L) {
      nr <- 1:nrow(rels)
      poss.strats <- as.vector(na.omit(sapply(nr, function(nr) get.strat(out[i, ], 
        rels[nr, ]))))
      # Discard all if some aren't a canonical orientation (because ignored in 'get.strat')
      if (length(poss.strats) != rels.with.strats)
        poss.strats <- NA
      if (length(unique(na.omit(poss.strats))) == 1L) {
        out$AbsStratDistance[i] <- poss.strats[1]
        out$Est_AbsStratDistance[i] <- "Estimated"
      }
    }
  } else {
    # Or update AbsStratDist if previously existed, but some measurements were updated
    orig.ms <- unlist(input[i, ATD.cols])
    poss.dists <- c(1, -1) %x% c(orig.ms, angle.30 * orig.ms, angle.45 * orig.ms, 
      angle.60 * orig.ms, proportions %x% orig.ms)
    if (change == "maybe" & !missing.strat &
        signif(input$AbsStratDistance[i], 3) %in% signif(poss.dists, 3))
      out$AbsStratDistance[i] <- get.strat(out[i, ], input[i, ])
  }

  # Add "check" tag if any size or AbsStratDist was changed (to 2 significant digits)
  if (!identical(signif(input[i, photo.cols], 2), signif(out[i, photo.cols], 2)) ||
      !identical(signif(input$AbsStratDistance[i], 2),
                 signif(out$AbsStratDistance[i], 2))) {
    out$SizeChanged[i] <- "Check"
    }
  
  # Update history if AbsStratDist added or re-calculated (using updated
  # measurements, if changed)
  if (out$Est_AbsStratDistance[i] == "Estimated") {
    orig.ms <- unlist(out[i, ATD.cols])
    poss.dists <- c(1, -1) %x% c(orig.ms, angle.30 * orig.ms, angle.45 * orig.ms, 
      angle.60 * orig.ms, proportions %x% orig.ms)
    wh.m <- match(signif(out$AbsStratDistance[i], 3), signif(poss.dists, 3))
    if (out$History_Size[i] == "")
      out$History_Size[i] <- paste0("AbsStratDist estimated ", today, " from ", 
            AbsStratDist.text[seq.AbsStratDist[wh.m]], " Prior AbsStratDist was ", 
            signif(input$AbsStratDistance[i], 3), ".") else
      out$History_Size[i] <- paste0("AbsStratDist updated ", today, " from ",
            AbsStratDist.text[seq.AbsStratDist[wh.m]], " Prior AbsStratDist was ", 
            signif(input$AbsStratDistance[i], 3), ". ", out$History_Size[i])
  }
  
  # Interactive mode (to observe how states are being propogated in real time).
  # Deprecating printing of History_Size because of false positives due to
  # simple change in date, and recalculations to ATD lengths.
  changes.made <- ncol(better.all.equal(input[i, -c(22, ATD.cols)], out[i, -c(22, ATD.cols)],
                                        sig.digits = 3, nums = c(23:28, 32))) > 0L
  if (interactive & changes.made) {
    plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    text(1, 1, as.character(paste(out$Genus[i], out$Species[i])), cex = 2)
    cat(as.character(paste(out$Genus[i], out$Species[i])), "\n")
    # print(better.all.equal(input[i, ], out[i, ]))
    print(better.all.equal(input[i, -c(20, ATD.cols)], out[i, -c(20, ATD.cols)],
                           nums = c(23:25, 32)))     # If want to mask date and calculated lengths
    cat("\n")
  }

  # If recording a log of changes made
  if (record.log & changes.made) {
    cat(as.character(paste(out$Genus[i], out$Species[i])), "\n", file = record.file,
        append = TRUE)
    # Deprecating printing of History_Size because of false positives due to
    # simple change in date.
    comps <- better.all.equal(input[i, -c(20, 22, ATD.cols)], 
                              out[i, -c(20, 22, ATD.cols)], nums = c(22:24, 31))
    write.table(comps, file = record.file, append = TRUE, sep = "\t\t", 
                quote = FALSE, row.names = FALSE)
    cat("\n\n", file = record.file, append = TRUE)
  }
  
}
(Sys.time() - start.t)
library(beepr)
beep(3)

# Note that occasionally a rounding error [caused by minor differences between
# the better.all.equal(sig.digits = 3) amd better.all.equal(sig.digits = 2)] occurs
# that triggers a false positive change. This results in a 'data frame with 0
# columns and 2 rows' result. You can ignore these results.

round(table(input$BodySizeScale) * 100 / nrow(input), 1)
round(table(out$BodySizeScale) * 100 / nrow(out), 1)
round(cumsum(table(out$BodySizeScale) * 100 / nrow(out)), 1)
table(input$BodySizeScale)
table(out$BodySizeScale)


rng <- range(na.omit(c(input$AbsStratDistance, out$AbsStratDistance)))
hist(out$AbsStratDistance, xlim = rng, col = "white", border = "white", n = 50)
hist(out$AbsStratDistance, col = "darkgray", border = "white", add = TRUE, n = 50)
hist(input$AbsStratDistance, col = "transparent", border = "black", add = TRUE, n = 50)
legend("topright", inset = .1, legend = c("after", "before"), pch = 22, 
       pt.bg = c("darkgray", "transparent"), col = c("white", "black"))
abline(v = 0, lwd = 2, lty = 2)

# If want to plot trends in body size evolution, see SizeTrends.R

## Any duplicated ID numbers?
# Will match the incorrect life habits when importing
if (any(table(input$IDNumber) > 1)) {
  print(which(table(input$IDNumber) > 1))
  stop("There are duplicate IDNumbers. Fix before proceeding! See above for list.")
}


## EXPORT DATA -------------------------------------------------------------
write.table(out, file = "PostSizes.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostSizes_Constant_withPBDB.tab", quote = FALSE, sep = "\t", row.names = FALSE)

# (1) Open in Excel to confirm looks acceptable. Replace (with Options = Match
# entire cell contents) "NA"s in taxonomic names, body size data, strat ranges,
# and AbsStratDist with blank cells. (Essentially the entire output.)

# (2) Open in Word to remove quotation marks around the text entries [UNLESS THE
# QUOTATION MARK IS CORRECTLY AT THE END OF A TEXT FIELD], (replacing "^t with
# ^t and replacing ^t" with " and replacing "" with ").

# (3) Open FileMakerPro and import, updating records by matching names and using
# the IDNumber as the matching identifier. (Fine to not import the taxonomic
# names and geological ranges, but import everything else.)

## Manual trouble-shooting: Some propogations are known to be (potentially)
## incorrect. It would be more efficient if the following rules could be checked
## algorithmically, but that may be challenging because of the interpretative
## nuances involved. One possibility (for the size-related feeding and mobility
## rules, at least) is to add code that uses some form of binned size regression
## analysis (within classes) to extrapolate the size-rules. Another option might
## be to use machine-learning (i.e., classification trees). Either way, would
## need to add an "exception" field when a coding defies the standard coding
## rules (for example, when crinoid sizes lack column lengths but a AbsStrat
## coding is still possible, or when the functional gill size in vermiculariids
## is best approximated by transverse width instead of A/P length).

# Once imported (BEFORE THE LIFE HABITS are propogated, but then again
# afterwards, too), run the following manual corrections. (Note the RelStrat
# should not be deleted, but updated as needed, with other stratifications.)

# For the first run-through (BEFORE propogating life habits), only need to check
# entries where ECOSCALE OR BODYSIZESCALE = "SPECIES" OR "GENUS". (The
# life-habit algorithm maintains the four body-size-scale-related states
# [AbsStrat, RelStrat, etc.] when EcoScale = Species/Genus, and uses the
# consensus across relatives when missing or not EcoScale = Species/Genus, but
# reverts back to the original codings when BodySizeScale = Species/Genus.)

# On the pre-life-habit propogation, focus on the scales above, and also the
# SizeChanged = Check tagged entries. On the post-life-habit propogation, focus
# on the SizeChanged = Check tagged entries, but also check ALL entries using
# the following criteria. (Can omit those coded at EcoScale = Species/Genus.)
# MAKE SURE THAT IF ADD/CHANGE A STATE FOR A EcoScale = SPECIES/GENUS, to tag as
# "Estimated."

# NOTE: When error-checking for a PBDB propogation, preferable to start with no.
# 7 (run through AbsStratDist to make sure AbsStrat, and sometimes RelStrat, is
# correct) and then no. 6 (RelStrat based on major body axes) because they could
# un-do other error-check changes. Similarly, because the combination of
# different life habit and body size can create novel body orientations, a
# useful convention is to only change RelStrat if it is not sensible with some
# orientation of the specified taxon's body size. (In other words, the
# propogated life habit may suggest a body orientation that the reference size
# taxon does not have in its life habit. This is an acceptable outcome of the
# propogation's "exploration" of ecospace.) An example is for colonial animals
# (bryozoans and tabulate corals) where a "self-supported" colony would be
# raised into water column whereas a "supported" colony would be oriented as
# encrusting some host. If the orientation could be multiple ones (e.g., the
# 'mode' treatments suggests one orientation but the 'constant' is not
# committed), best to leave the state uncoded. This is also common with bivalves
# and some brachiopods where the orientation depends on many factors.

# (1) Pelagic taxa given benthic AbsStratDists: Find Fluidic = 1 & Insubstantial
# = 1 & AbsStratDist = ">-10000" [ANY] & SizeChanged = CHECK and delete
# AbsStratDist (if needs correcting).

# (2) "Supported" taxa given self-supported (i.e., benthic) AbsStratDists: Find
# SupportedByOthers = 1 & AbsStratDist = ">-10000" [ANY] & SizeChanged = CHECK
# and delete AbsStratDist (if needs correcting). (The valid exceptions will
# typically have AbsStratDist values that do NOT correspond to the organism's
# major axes.)

# (3) Confirm that epifaunal "Self-supported" taxa have the same tier for
# AbsStrat and RelStrat. (And that "supported" taxa have different values.)
# (Note this only applies to epifaunal taxa because pelagic and infaunal
# organisms can be self-supported despite having different absolute and
# immediate stratifications.) Set fluidic = 0, above primary = 1, within primary
# = 0, and supported/self-supported as either 0/1 or 1/0. Then sort by AbsStrat
# > RelStrat. In many cases, the AbsStrat and AbsFoodStrat should also match. Be
# aware that there will be a few exceptions (e.g., very long crinoid
# Traumatocrinus that is attached to floating logs, crinoid Paracatillocrinus
# that has an unusual means of "wrapped" attachment that warrants being
# "attached" despite not being raised significantly higher into water column).
# During post-life-habit-propogation processing, add back in appropriate
# "self-supported" AbsStrat, and AbsFoodStrat (if filter-feeding) and
# AbsStratDist (if obvious), BUT ONLY FOR TAXA SCALED AT > GENUS/SPECIES! Be
# reminded that stemmed echinoderms (crinoids, blastoids, etc.) will frequently
# lack AbsStrat and AbsFoodStrat because of unknown stem lengths.

# (4) Epibiotic (but barely raised, so should be coded as "self-supported") taxa
# given incorrect benthic (as if not epibiotic) AbsStratDists: Find Biotic = 1 &
# SupportedByOthers = 0 & AbsStratDist = ">-10000" [ANY] & SizeChanged = CHECK &
# EstAbsStratDistance = Estimated and delete AbsStratDist (if needs correcting).
# (AbsStrat should be same as RelStrat.) (The valid exceptions will typically
# have AbsStratDist values that do NOT correspond to the organism's major axes.)

# (5) Confirm all exclusively infaunal taxa have negative AbsStratDists and
# exclusively epifaunal taxa have positive values. (But don't be surprised by
# semi-infaunal taxa that are simultaneously epifaunal and infaunal.) Test that
# all above = 1/in = 0 have positive AbsStratDists and all above = 0/in = 1 have
# negative ones. Converse, confirm that all with negative AbsStratDists have
# WithinAbsStrat = 1 and that all with positive AbsStratDists have AboveAbsStrat
# = 1.

# (6) Confirm that RelStrat corresponds to appropriate body axis. Best to sort
# by Phylum > Class (and for some groups, like brachiopods, subclass/order) >
# D/V / A/P because different taxa have different living orientations. If list
# is too long to run through, can limit to Sizechanged = "checked".
# (Alternatively, can find all taxa with all major axes in the same size range
# [e.g., 0.1-1], which should all have the same RelStrat.) Watch out for
# following exceptions: (1) Crinoids, blastoids, rhombiferans, pedunculately
# raised (Cambrian) lingulids, and other stemmed animals where the measured body
# part may not correspond to the intact animal. (2) Animals close to a
# size-boundary that are diagonally oriented (such as pedunculate brachiopods,
# and some bivalves and rugose corals). (3) Distinctive subtaxa with distinct
# orientations (such as agnostids scaled to 2/3 A/P and terrestrial birds and
# pterosaurs scaled to diagonally oriented A/P and flatfishes [Family Soleidae]
# scaled to transverse.). (4) Rhynchonellatan brachiopods and bivalves and other
# animals whose orientation is sometimes based on a diagonal orientation based
# on A/P length. (5) Corals and encrusting bryozoans where RelStrat is based on
# A/P. (6) Some bactritid and turrilitoid ancyloceratine cephalopods and sessile
# filter-feeding snails (Superfamilies Euomphaloidea and Macluritoidea and
# Cerithioidea [only families Siliquariidae and Turritellidae > only subfamily
# Vermiculariinae]) are vertically oriented, based on A/P length. (7) Some
# colonial animals (sponges, stromatoporoids, corals, and to a lesser extent
# bryozoans) are given larger AbsStrat & RelStrat values than the measured
# specimens provide, on the basis that the specimen is only one part of a
# substantially larger colony.

# (7) Confirm that taxa with AbsStratDist = ">-100000" values [ANY] match the
# correct AbsStrat coding. (Sort by AbsStratDist when checking manually.) When
# checking this, also worth checking the RelStrat and AbsFoodStrat codings. In
# general, if animal is epibenthic, self-supported, and filter-feeder, the
# AbsFoodStrat will be same as AbsStrat; if surficial mass-feeder, AbsFoodStrat
# will be 0; and if raptorial, AbsFoodStrat will be where food is located (often
# 0.25 coding if epibenthic prey). For RelStrat, useful to sort by
# Phylum>Class>DV or AP length. For RelStrat, recall that for semi-infaunals
# RelStrat will correspond to the animal's major axis and not the distance from sea
# floor.

# (8) Confirm RelFoodStrat for filter feeders, sorting by phyla and then either
# AbsStratDist or A/P length. For ambient filter feeders (PORIFERA and
# suspension-feeding ECHINODERMATA and CNIDARIA and CIRRIPEDIA) where flow rate
# depends on distance from sea floor, confirm those ~15-200 mm tall are 0.5;
# refer to LifeHabitNotes.docs for additional categorizations. For active filter
# feeders, the coding depends on the size of the pumping organ. All BRYOZOANS,
# GRAPTOLITES, PTEROBRANCHS, and CEPHALOCHORDATA should be 0.25. Filter-feeding
# MICROCRUSTACEANS (amphipods, mysids, etc.) and burrowing filter-feeding SHRIMP
# generally should be 0.25. ASCIDIANS should generally be 0.5. Filter-feeding
# fishes and whales should generally be 1.0. BRACHIOPODS should be half the A/P
# length. Filter-feeding mollusks (BIVALVES, ROSTROCONCHS, and GASTROPODS)
# should be twice the A/P length. POLYCHAETES, TENTACULITIDS, and other tubular
# taxa (Sorbeoconch snails in Cerithioidea [especially families Siliquariidae
# and Turritellidae, although propagations might include other cerithioids])
# should (generally) be twice the transverse length, instead. Tentaculate
# web-feeding (filter-feeding) ammonoids (such as those in Superfamily
# Turrilitoidea and Order Ceratitida) have many exceptions but are typically
# approximated by the A/P conch length. (Exceptions typically have the same
# scale for both life habit and size.)

# (9) Confirm that wholly infaunal filter-feeders (AboveAbsStrat = 0, WithinAbs
# = 1, Filter = 1 [rest 0]) have AbsFoodStrat at 0.25 (typically) and that
# AbsFood is "Above" the seafloor (with exceptions, like callianassoids that
# also ingest sediment from burrow wall, or Cenozoic cave-dwelling brachiopods,
# or thin productids that rested within the sediment with gape at seafloor, or
# echinoids like clypeasteroid mellitid Encope that emerge at surface to filter
# feed, or interstitial meiofauna).

# (10) Confirm that filter feeders (whether infaunal or epifaunal) that extend
# body above sea floor (AboveAbsStrat = 1 & FeedAbovePrimary = 1 & Filter = 1
# [rest = 0]) have AbsFoodStrat at level to which body extends (and that food is
# also "Above" the seafloor). Easiest to sort by AbsStrat > AbsFoodStrat >
# AbsStratDist, Exceptions include the relatively deeply buried semi-infaunals,
# where a large portion of body may be buried but only a small portion atop
# seafloor, some strophomenids/productids/chonetoideans where FoodStrat is
# closer to sediment, and some echinoderms (such as ophiuroids and stylophorans)
# that raise their filter-feeding arms into the water column when feeding.

# (11) Make sure that all "above primary" [large epifauna and demersal/nektonic]
# organisms (AbovePrimary = 1, WithinPrimary = 0) that have RelStrat = 1 also
# have AbsStrat = 1.

# (12) For filter-feeding echinoderms, clear the FilterDensity field if the
# entry is NOT a filter-feeder. Check FilterDensity = High/Medium/Low and
# FilterFeeder != 1. (If FilterFeeder is blank in the Constant data set, check
# the Mode propogation; if a FilterDensity coding was propogated in the Constant
# data set and the Mode data set has FilterFeeder = 1, then keep the
# FilterDensity and propogate its dependent FilterFeeder = 1 and check the
# Estimated box.)

# (13) Go through each character and confirm no "all-zeros" (i.e., if all
# dietary states are 0, etc.), removing some estimates as needed. All 1s are
# acceptable. If not sure which to delete, consult the alternative propagation
# method data set for a reference.

# (14) Need to write out rules for hunters, scavengers, and mass feeders,
# especially regarding food tier (half D/V for predators to account for smaller
# size of prey?) and sensory distances when scouting. (Speed can also be a proxy
# for this.) This might be best to sort by Phylum > Class > A/P / D/V because
# different taxa often have distinct foraging strategies.

# (15) Once these checks are run, re-run them and clear the SizeChanged = Check
# tags.