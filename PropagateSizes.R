## PROPAGATE BODY SIZE CODINGS ACROSS ENTRIES, USING RELATIVES AS PROXIES

## BASIC LOGIC -------------------------------------------------------------

# "Relative' means the smallest inclusive taxonomic group the entry is related
# to whose body size has been coded. Use the 'EstAP', 'EstT', and 'EstDV' tags
# to know which are not measured directly (and can be over-ridden). The
# 'History_Size' field should be overridden as needed to explain how un-measured
# lengths were estimated.

# 1. If entry is measured at species- or genus- (or subgenus-) level AND all 3
#    lengths are measured, skip to the next entry.
#
# 2. If entry at sp/g/subg-level AND 1 or more measures are missing, proceed to
#    estimating using following logic:
#
#   A. Find closest relative with ALL 3 ATD. 1) if multiple relatives exist, 
#      pick the one (regardless of whether sp or genus) that is closest to the 
#      same age [(earlyRel - earlyEntry) ^ 2 + (lateR - lateE) ^ 2], and pick  
#      match with smallest absolute difference. Why? Because size is known to  
#      change through time (although unclear if shape does). Note that this 
#      means that taxa missing a range will not get an estimated body size. (And 
#      no need to calculate square root here, which saves computational time.)
#
#   B. If all 3 lengths are missing (not possible if SizeScale = sp/g?), drop in
#      all 3 measurements from relative.
#
#   C. If 2 lengths are missing, estimate the missing ones using the relative's
#      shape.
#
#   D. If 1 length is missing, estimating the missing length using mean of
#      relative's shape (i.e., ratio of other lengths). (Using arithmetic mean
#      instead of geometric mean to best estimate an isometric shape; use of 
#      geomean, in trials, does not produce equivalently shaped estimates.)
#
#   E. Update "estimate" check boxes and metadata accordingly.
#
# 3. If entry is not at genus or better level (i.e., subfamily or greater), find
#    closest-aged relative and drop in all 3 measurements, updating metadata.
#    Only in this case add tags for RefGenusSize and RefSpeciesSize.





## EXPORT DATA -------------------------------------------------------------

# (0) Not recommended (because the code algorithmically updates and gives
#     priority to species/genera with complete measurements), but if want a 
#     "fresh" propagation, consider first deleting all body size data for any 
#     estimates (EstAP, EstT, EstDV or non-species/subgenus/genus). (This is 
#     recommended if, for example, a species/subgenus/genus A entry has at least 
#     one estimated size measurement, and it might be used to estimate a missing 
#     size for species B, which was originally used to estimate the missing size
#     for A.) IF DO THIS, PERFORM THE DELETIONS ON A COPY OF THE DATABASE RATHER 
#     THAN THE MASTER DATABASE ITSELF! If go this route, note that the 
#     History_Size output will be slightly off, because will imply there were 
#     prior estimates. It is easiest to clean the data set during step 5 (when 
#     it would be easiest to add a row number column so you can sort as needed, 
#     then return to the original row order.) However, be aware that removing 
#     estimated values has the negative side effect of meaning that AbsStratDist 
#     estimates are not likely to be propagated (because the get.strat() 
#     function matches the current AbsStratDist values against canonical values, 
#     and deleting estimated lengths means it may not find relevant matches.)

# (1) To save time, you only need to export one dataset. The "constant" database
#     is recommended, although both data sets should have identical body size
#     estimates (although there will be variation in the estimates for
#     AbsStratDist). Before exporting, sort the entries so that (i) 
#     BodySizeScale = Species is first, followed by Subgenus, Genus, etc., (ii) 
#     second by AbsStratDistance (in descending order, with largest first), and 
#     (iii) third BodyVolume (with largest first). (This sort order means taxa 
#     with AbsStratDistances and all three measurements are placed before those 
#     missing one or more sizes.)

# (2) Run relevant code in SelectCols.R for PropagateSizes.R to obtain following
#     output. Then continue with step 3.

#     IDNumber

#     Taxonomy: Phylum, Subphylum, Class, Subclass, Order, Suborder,
#     Superfamily, Family, Subfamily, Genus, Subgenus, Species

#     Proxy fields: max_ma, min_ma, BodySizeScale, RefGenusSize,
#     RefSpeciesSize, Enterer, DateEntered_Size, SizeChanged, History_Size,
#     BodyMeasureReference

#     Body-size characters (can really be in any order, as called by name):
#     APLength, TransverseLength, DVLength, PhotoAP, PhotoTransverse, PhotoDV,
#     APScale, TransverseScale, DVScale, Est_AP, Est_T, Est_DV, AbsStratDist

#     (Note "Intact" is excluded because only appropriate to the raw (observed) 
#     species/genus-level entries) and not the propagated ones.

# (3) Open file in MSWord (make sure smart quotes are off: File > Options >
#     Proofing > Autocorrect Options > Autoformat As You Type > uncheck Smart
#     Quotes) and delete any hidden tabs (tab within a text field) and all
#     problematic (i.e., double) quotation marks (replacing [UNLESS THE 
#     QUOTATION MARK IS CORRECTLY AT THE END OF A TEXT FIELD] "^t with ^t and 
#     replacing ^t"with ^t and replacing "" with "). Re-save file in same 
#     format.

# (5) Open in Excel. Add a new column named 'PhotoX' that counts measured 
#     'PhotoX' columns with values. 
#     (HINT: Use = MIN((3-COUNTA(Est_AP:Est_DV)),COUNT(APLength:DVLength)) , 
#     which is = MIN((3-COUNTA(AG2:AI2)),COUNT(X2:Z2)) that allows including 
#     the Est_X columns!) TROUBLESHOOT: Confirm that all 'Sp/Subg/Gen' have at 
#     least 1 measurement! Also delete any NAs in AbsStratDistance, SizeChanged, 
#     and size measures, and delete size or AbsStratDist measures that are 0 
#     (if not intentional).

# (6) Add a new column named 'BodySize' that estimates the body size, using 
#     the product of 3 'XLength' sizes (=PRODUCT(X2:Z2)).

# (6) Sort the BodySizeScale = 'Sp/Subg/Gen' rows by (1) number of PhotoX
#     columns (largest first) so entries with complete (all 3) size measurements 
#     are first and most incomplete are last. That way those with best scales
#     and more-complete sizes are checked first, so that later entries can use 
#     the largest available pool of relatives. (2) Second, sort item by 
#     AbsStratDist, with largest values first (so those with estimated 
#     AbsStratDists are considered first to propagate to those lacking them). 
#     (3) Third, sort by BodySize, with largest values first.

# (7) Delete the added 'PhotoX' and 'BodySize' columns and resave.



## READ AND PROCESS DATA ---------------------------------------------------
rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Documents/GSA (& NAPC)/2024NAPC/Higher taxa eco diversity")

pre.input <- read.delim(file = "preSizes.tab", stringsAsFactors = FALSE)
# pre.input <- read.delim(file = "PreSizes_Constant_Ostracodes.tab", stringsAsFactors = FALSE)
# pre.input <- read.delim(file = "PreSizes_Bradoriida&Aster&Echino.tab", stringsAsFactors = FALSE)
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
# input <- read.delim(file = "PreSizes_Constant_Ostracodes.tab", stringsAsFactors = FALSE)
# input <- read.delim(file = "PreSizes_Bradoriida&Aster&Echino.tab", stringsAsFactors = FALSE)
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
str(input)

# TROUBLESHOOT: Make sure the "SizeChanged" and 4 "Est_X" (including
# Est_AbsStratDistance) columns are input as characters and not logicals or all
# NAs. If a column is blank, it is classed by default as a logical (which causes
# errors below) and replaced with NA. If needed (such as when there are no
# "check"ed entries; i.e., when did a clean propagation), use next lines to
# force to proper class. Note that it is fine for AbsStratDist to have NAs for
# missing values (so long as the column is treated as a numeric). IT is also
# advisable to confirm the PhotoX and XLength columns are input as numerics.

## Following usually required:
input$SizeChanged <- replace(input$SizeChanged, which(is.na(input$SizeChanged)), "")

## Rest only if did a fresh propagation (lacking any estimates - not recommended!)
# input$Est_AP <- replace(input$Est_AP, which(is.na(input$Est_AP)), "")
# input$Est_T <- replace(input$Est_T, which(is.na(input$Est_T)), "")
# input$Est_DV <- replace(input$Est_DV, which(is.na(input$Est_DV)), "")
# input$Est_AbsStratDistance <- replace(input$Est_AbsStratDistance, which(is.na(input$Est_AbsStratDistance)), "")
str(input)

out <- input                    # Work with 'out', saving 'input' for reference
colnames(input[photo.cols])     # "PhotoAP", "PhotoTransverse", "PhotoDV"
colnames(input[est.cols])       # "Est_AP", "Est_T", "Est_DV"
colnames(input[AP.cols])        # "APLength", "PhotoAP", "APScale"
colnames(input[T.cols])         # "TrLength", "PhotoTr", "TrScale"
colnames(input[DV.cols])        # "DVLength", "PhotoDV", "DVScale"

head(out)
tail(out)

# Current taxonomic resolution of data set. Compare to out below to see if there
# has been improvement.
table(input$BodySizeScale)

# Troubleshooting
if (any(is.na(input$max_ma)) || any(is.na(input$min_ma)))
  stop("Some entries have missing age ranges, which is used in body size propagation 
algorithm. Leaving ranges empty will mean that missing size measurements will not be propagated for these taxa. Update ages from the Paleobiology Database before proceeding.\n")

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
#  start = taxonomic level to start (default = subfamily, with 1 = species and 14 = NA)
#  end = taxonomic level to end (default = NA, running higher up through all levels,
#     including unknowns).
#  photo.cols and est.cols = which columns to use when calling 'any.missing' to
#     extract relatives with complete measurements.
#  all.3 = logical. If TRUE (default), the relatives ONLY contain values for all
#     three measurements. Recommend setting FALSE when propagating body sizes for
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
    nr <- 0
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
    if (nr > 0L) break
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


## IDENTIFY HOW ABSOLUTE STRAT DISTANCE WAS CALCULATED, AND PROPAGATE USING SAME
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
# i <- which(out$Genus == "Caturus")        # fish Caturus
i <- which(out$Genus == "Schuchertia")      # sea star Schuchertia 
out[i, c(1:16, 24:32, 36)]
# Identify missing measurements
any.missing(out[i, ], photo.cols, est.cols)
# Find completely measured relative of the closest stratigraphic age
rel <- find.rel(out, i, photo.cols = photo.cols, est.cols = est.cols, 
                sim.time = TRUE, all.3 = TRUE)
rel$size.sc
rel$rel[c(1:16, 24:32, 36)]
get.strat(target = out[i, ], ref = rel$rel)
# Prior line only works if all 3 measures are in the target

# Find ALL completely measured relatives (regardless of age)
rels <- find.rel(out, i, photo.cols = photo.cols, est.cols = est.cols, 
                 sim.time = FALSE, all.3 = TRUE)$rel
rels[, 2:11]
nr <- 1:nrow(rels)
sapply(nr, function(nr) get.strat(out[i,], rels[nr, ]))
# Prior line only works if all 3 measures are in the target

rm(list = c("i", "nr", "rel", "rels"))





## PROPAGATE BODY SIZES --------------------------------------------

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
# interactive <- FALSE
if (interactive) par("ask" = TRUE) else par("ask" = FALSE)
# log actions so can trace changes
record.log <- TRUE
record.file <- "SizeLog.txt"
if (record.log) cat("Changes made to body sizes on", today, ":\n\n", file = record.file, append = FALSE)
(start.t <- Sys.time())

for (i in 1:nrow(out)) {
# for (i in 29220:nrow(out)) {
    
  if (i %in% index)
    cat("record", i, "of", nrow(out), ":", out$Genus[i], out$Species[i], "\n")
  
  # Ignore if no higher taxonomic information at all
  if (all(input[i, 2:10] == "")) next
  
  # Ignore if already coded at species, subgenus or genus level AND complete
  this.scale <- out$BodySizeScale[i]
  missing <- any.missing(out[i, ], photo.cols, est.cols)
  number.missing <- length(missing$which)
  missing.strat <- is.na(out$AbsStratDistance[i])
  # Note prior line should NOT check Est_AbsStratDistance here. Checks below
  # instead.
  
  if (!missing$any & !missing.strat & (this.scale == "Species" | 
      this.scale == "Subgenus" | this.scale == "Genus")) next
  
  # Next line sets 'rels' to be NULL rather than blank matrix (which is a
  # better behavior for below)
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
      
      # If AP is only available measurement:
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

      # If DV is only available measurement:
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
      
      # If AP is only missing measurement:
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

      # If DV is only missing measurement:
      if ("DV" %in% missing$which) {
        out$DVScale[i] <- out$TransverseScale[i]
        out$Est_DV[i] <- "Estimated"
        est1 <- out$DVScale[i] * out$APLength[i] / AP.DV
        est2 <- out$DVScale[i] * out$TransverseLength[i] * DV.Tr
        out$PhotoDV[i] <- mean(c(est1, est2))
      }
    }
    
    change <- "maybe"
    out$History_Size[i] <- paste("On", today, pre.text(missing), "in same", 
      tolower(rel$size.sc), as.character(rel$rel[which(colnames(rel$rel) == 
      rel$size.sc)]), "of", rel$rel$BodyMeasureReference)
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
    if (nrow(rel$rel) == 0L) next
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
      # Tag with date the size is propagated for record-keeping:
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

  # Propagate AbsStratDist (using closest relatives, regardless of whether
  # geologically contemporaneous or whether missing some size measures)
  if (missing.strat) {
    # ... if missing AbsStratDist but available via ALL best relatives (that are
    # within same suborder [end = 7] or lower resolution)
    rels <- find.rel(x = out, i = i, start = 4, end = 7, 
                     photo.cols = photo.cols, est.cols = est.cols, 
                     sim.time = FALSE, all.3 = FALSE)$rel
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
    # Or update AbsStratDist if previously existed, but some measurements were
    # updated
    orig.ms <- unlist(input[i, ATD.cols])
    poss.dists <- c(1, -1) %x% c(orig.ms, angle.30 * orig.ms, angle.45 * orig.ms, 
      angle.60 * orig.ms, proportions %x% orig.ms)
    if (change == "maybe" & !missing.strat &
        signif(input$AbsStratDistance[i], 3) %in% signif(poss.dists, 3))
      out$AbsStratDistance[i] <- get.strat(out[i, ], input[i, ])
  }

  # Add "check" tag if any size or AbsStratDist was changed (to 2 significant
  # digits)
  if (!identical(signif(input[i, photo.cols], 2), signif(out[i, photo.cols], 2)) ||
      !identical(signif(input$AbsStratDistance[i], 2),
                 signif(out$AbsStratDistance[i], 2))) {
    out$SizeChanged[i] <- "Check"
    }
  
  # Update history if AbsStratDist is added or re-calculated (using updated
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
  
  # Interactive mode (to observe how states are being propagated in real time).
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

  # If recording a log of changes made:
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
beepr::beep(3)

# Note that occasionally a rounding error [caused by minor differences between
# the better.all.equal(sig.digits = 3) and better.all.equal(sig.digits = 2)]
# occurs that triggers a false positive change. This results in a 'data frame
# with 0 columns and 2 rows' result. You can ignore these results.

# Has been an improvement in taxonomic resolution?
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
# write.table(out, file = "PostSizes_Ostracodes.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostSizes_Bradoriida&Aster&Echino.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostSizes_Constant_withPBDB.tab", quote = FALSE, sep = "\t", row.names = FALSE)

# (1) Open in Excel to confirm looks acceptable. Replace (with Options = Match
#     entire cell contents) "NA"s in taxonomic names, body size data, 
#     stratigraphic ranges, and AbsStratDist with blank cells. (Essentially the 
#     entire output.)

# (2) Open in Word to remove quotation marks around the text entries [UNLESS THE
#     QUOTATION MARK IS CORRECTLY AT THE END OF A TEXT FIELD], (replacing "^t 
#     with ^t and replacing ^t" with " and replacing "" with ").

# (3) Open FileMakerPro and import as a tab-delimited file, updating records by
#     matching names and using the IDNumber as the matching identifier. (Fine to 
#     not import the taxonomic names or geological ranges, but import  
#     everything else.) To set the column names as the field names, make sure 
#     the first entry row in the imported source (the column headings) are 
#     visible, then choose "Use as Field Names" for the imported source file. 
#     Then choose "Matching Names" in the Target Fields dropdown to ensure that 
#     the source and target fields match (but double-check that they are matched 
#     correctly). If using different propagations for the "constant" and "mode" 
#     databases, make sure to import the correct source file to the correct 
#     database version.




## MANUAL TROUBLE SHOOTING OF CODINGS ------------------------------------------

## Some propagations are known to be (potentially) incorrect. It would be more
## efficient if the following rules could be checked algorithmically, but that
## may be challenging because of the interpretative nuances involved. One
## possibility (for the size-related feeding and mobility rules, at least) is to
## add code that uses some form of binned size regression analysis (within
## classes) to extrapolate the size rules. Another option might be to use
## machine-learning (i.e., classification trees). Either way, would need to add
## an "exception" field when a coding defies the standard coding rules (for
## example, when crinoid sizes lack column lengths but an AbsStrat coding is
## still possible, or when the functional gill size in vermiculariids is best
## approximated by transverse width instead of the typical AP length for most
## filter-feeding snails).

# Once imported (BEFORE THE LIFE HABITS are propagated, but THEM AGAIN
# afterwards, too), run the following manual corrections. (Note the RelStrat
# should not be deleted, but updated as needed, with appropriate
# stratifications.) The current version of the FileMakerPro database includes
# these rules as pre-defined "saved finds."

# For the first run-through (BEFORE propagating life habits), only need to check
# entries where ECOSCALE OR BODYSIZESCALE = "SPECIES" OR "GENUS". (The
# life-habit algorithm maintains the four body-size-scale-related states
# [AbsStrat, RelStrat, etc.] when EcoScale = Species/Genus, and uses the
# consensus across relatives when missing or not EcoScale = Species/Genus, but
# reverts back to the original codings when BodySizeScale = Species/Genus.)

# On the pre-life-habit propagation, focus on the scales above, and also the
# SizeChanged = Check tagged entries. On the post-life-habit propagation, focus
# on the SizeChanged = Check tagged entries, but also check ALL entries using
# the following criteria. (Can omit those coded at EcoScale = Species/Genus.)
# MAKE SURE THAT IF ADD/CHANGE A STATE FOR AN ECOSCALE = SPECIES/GENUS, to tag
# as "Estimated."

# When checking with the pre-life-habit propagation, if a size-based coding (or
# any, really) is uncertain, it is better to leave blank than to guess. Leaving
# blank will allow a relative's state to be estimated when running
# PropagateLifeHabits.R, but filling in will tend to defer to the filled-in
# state instead.

# If error-checking both 'mode' and 'constant' propagations, it is more
# efficient to run through the 'mode' data set first because its typically
# greater proportion of filled-in states makes it easier to identify the
# taxon-specific rules.

# NOTE: When error-checking for a PBDB propagation, preferable to start with no.
# 1 (run through AbsStratDist to make sure AbsStrat, and sometimes RelStrat, is
# correct) and then no. 2 (RelStrat based on major body axes) because they could
# un-do other error-check changes. Similarly, because the combination of
# different life habit and body size can create novel body orientations, a
# useful convention is to only change RelStrat if it is not sensible with some
# orientation of the specified taxon's body size. (In other words, the
# propagated life habit may suggest a body orientation that the reference size
# taxon does not have in its life habit. This is an acceptable outcome of the
# propagation's "exploration" of ecospace.) An example is for colonial animals
# (bryozoans and tabulate corals) where a "self-supported" colony would be
# raised into water column whereas a "supported" colony would be oriented as
# encrusting some host. If the orientation could be multiple ones (e.g., the
# 'mode' treatments suggests one orientation but the 'constant' is not
# committed), best to leave the state uncoded. This is also common with bivalves
# and some brachiopods where the orientation depends on many factors.

# (1) Confirm that taxa with AbsStratDist = ">-100000" values [ANY] match the
# correct AbsStrat coding. (Sort by AbsStratDist when checking manually.) When
# checking this, also worth checking the RelStrat and AbsFoodStrat codings. In
# general, if animal is epibenthic, self-supported, and filter-feeder, the
# AbsFoodStrat will be same as AbsStrat; if surficial mass-feeder, AbsFoodStrat
# will be 0; and if raptorial, AbsFoodStrat will be where food is located (often
# 0.25 coding if epibenthic prey). For RelStrat, useful to sort by Phylum >
# Class > DV or AP length. For RelStrat, recall that for semi-infaunals RelStrat
# will correspond to the animal's major axis and not the distance from sea
# floor. If states are inconsistent, usually give primacy to the AbsStratDist,
# which is based on an actual specimen. Check LifeHabitNotes.docx for
# taxon-specific conventions.

# (2) Confirm that RelStrat corresponds to appropriate body axis. Best to sort
# by Phylum > Class (and for some groups, like brachiopods, subclass/order) > DV
# / AP because different taxa have different living orientations. If list is too
# long to run through, can limit to Sizechanged = "checked". (Alternatively, can
# find all taxa with all major axes in the same size range [e.g., 0.1-1], which
# should all have the same RelStrat.) Watch out for following exceptions: (1)
# CRINOIDS, BLASTOIDS, RHOMBIFERANS, pedunculately raised (Cambrian)
# LINGULIDANS, and other stemmed animals where the measured body part may not
# correspond to the intact animal. (2) Animals close to a size-boundary that are
# diagonally oriented (such as pedunculate BRACHIOPODS, and some BIVALVES and
# RUGOSE corals). (3) Distinctive subtaxa with distinct orientations (such as
# Order AGNOSTIDA and Order EODISCIDA scaled to 2/3 AP and [terrestrial ONLY!]
# Class AVES and Order PTEROSAURIA scaled to diagonally oriented AP [14.2-141 as
# 0.5, 142-1414 as 0.75, >1415 as 1] and flatfishes [Family SOLEIDAE] scaled to
# transverse.). (4) RHYNCHONELLATAN brachiopods and BIVALVES and other animals
# whose orientation is sometimes based on a diagonal orientation based on AP
# length. (5) CORALS and encrusting BRYOZOANS where RelStrat is based on AP. (6)
# Some BACTRICID and TURRILITOID ANCYLOCERATINE cephalopods and sessile
# filter-feeding snails (Superfamilies EUOMPHALOIDEA and MACLURITOIDEA and
# CERITHIOIDEA [only families SILIQUARIIDAE and TURRITELLIDAE & only subfamily
# VERMICULARIINAE]) are vertically oriented, based on AP length. (7) Some
# colonial animals (SPONGES, STROMATOPOROIDS, CORALS, and to a lesser extent
# BRYOZOANS) are given larger AbsStrat & RelStrat values than the measured
# specimens provide, on the basis that the specimen is only one part of a
# substantially larger colony. (8) Sessile polychaetes (i.e., SEDENTARIA) that
# present many different orientations, even within individual families.

# (3) Are there any entries where, through process of elimination, the life
# habit states can be coded? For example, search for any entries that are Biotic
# = 0 + Lithic = 0 but omit Fluidic = 1; if so, then fluidic should be coded as
# 1. Run this check across all 5 such characters (substrate composition,
# substrate consistency, diet, physical condition of food, and foraging habit)
# BEFORE running subsequent checks, because it can introduce new life habit
# codings. If adding a state to a Sp/Gen-level entry, make sure to "Check" that
# the added state is "Estimated."

# (4) Pelagic taxa given benthic AbsStratDists: Find Fluidic = 1 & Insubstantial
# = 1 & AbsStratDist = ">-10000" [ANY] & SizeChanged = CHECK and delete
# AbsStratDist (if needs correcting).

# (5) "Supported" taxa given self-supported (i.e., benthic) AbsStratDists: Find
# SupportedByOthers = 1 & AbsStratDist = ">-10000" [ANY] & SizeChanged = CHECK
# and delete AbsStratDist (if needs correcting). (The valid exceptions will
# typically have AbsStratDist values that do NOT correspond to the organism's
# major axes, or are semi-infaunal in which case the AbsStrat and RelStrat may
# differ, or have a typically known raised distance [such as encrusters of known
# hosts]. Easiest to confirm if sort by History_Size.)

# (6) Confirm that epifaunal "Self-supported" taxa have the same tier for
# AbsStrat and RelStrat. (And that "supported" taxa have different values.)
# (Note this only applies to epifaunal taxa because pelagic and infaunal
# organisms can be self-supported despite having different absolute and
# immediate stratifications.) Set fluidic = 0, above primary = 1, within primary
# = 0, and supported/self-supported as either 0/1 or 1/0. Then sort by AbsStrat
# > RelStrat > AbsStratDist. In many cases, the AbsStrat and AbsFoodStrat should
# also match, but confirm that any propagated AbsStratDist values are also
# sensible. Be aware that there will be a few exceptions (e.g., very long
# CRINOIDS Traumatocrinus that is attached to floating logs and
# Paracatillocrinus that has an unusual means of "wrapped" attachment that
# warrants being "attached" despite not being raised significantly higher into
# water column). During post-life-habit-propagation processing, add back in
# appropriate "self-supported" AbsStrat, and AbsFoodStrat (if filter-feeding)
# and AbsStratDist (if obvious), BUT ONLY FOR TAXA SCALED AT > GENUS/SPECIES! Be
# reminded that stemmed echinoderms (CRINOIDS, BLASTOIDS, etc.) will frequently
# lack AbsStrat and AbsFoodStrat because of unknown stem lengths. If need to
# change, use the bulk of the evidence to find the parsimonious solution. (E.g.,
# if AbsStrat and AbsFoodStrat and RelStrat and AbsStratDistance are all
# consistent, it is more parsimonious to change a "self-supported" coding to
# "supported" than to change the size-related categories. Usually give primacy
# to the AbsStratDist, which is based on an actual specimen.)

# (7) Epibiotic (but barely raised, so should be coded as "self-supported") taxa
# given incorrect benthic (as if not epibiotic) AbsStratDists: Find Biotic = 1 &
# SupportedByOthers = 0 & AbsStratDist = ">-10000" [ANY] & SizeChanged = CHECK &
# EstAbsStratDistance = Estimated and delete AbsStratDist (if needs correcting).
# (AbsStrat should be same as RelStrat.) (The valid exceptions will typically
# have AbsStratDist values that do NOT correspond to the organism's major axes.
# Easiest to confirm if sort by History_Size.)

# (8) Confirm all exclusively infaunal taxa have negative AbsStratDists and
# exclusively epifaunal taxa have positive values. (But don't be surprised by
# semi-infaunal taxa that are simultaneously epifaunal and infaunal.) Test that
# all above = 1/in = 0 have positive AbsStratDists and all above = 0/in = 1 have
# negative ones. Conversely, confirm that all with negative AbsStratDists have
# WithinAbsStrat = 1 and that all with positive AbsStratDists have AboveAbsStrat
# = 1.

# (9) Confirm RelFoodStrat for filter feeders, sorting by phyla and then either
# AbsStratDist or AP length. For ambient filter feeders (PORIFERA and
# suspension-feeding ECHINODERMATA and CNIDARIA and CIRRIPEDIA) where flow rate
# depends on distance from sea floor, confirm those ~14-191 mm tall are 0.5;
# refer to LifeHabitNotes.docs for additional categorizations. But note that for
# sponges, if just at or slightly above 190 mm, downcoding to 0.5 because
# incurrent flow occurs in lower ostia. For active filter feeders, the coding
# depends on the size of the pumping organ. All BRYOZOANS, GRAPTOLITES,
# PTEROBRANCHS, and CEPHALOCHORDATA should be 0.25. Filter-feeding
# MICROCRUSTACEANS (AMPHIPODS, MYSIDS, etc.) and burrowing filter-feeding SHRIMP
# generally should be 0.25. ASCIDIANS should generally be 0.5. Filter-feeding
# FISHES and WHALES should generally be 1.0. BRACHIOPODS should be half the AP
# length, and HYOLITHS the transverse width. Filter-feeding mollusks (BIVALVES,
# ROSTROCONCHS, and GASTROPODS) should be twice the AP length. POLYCHAETES,
# TENTACULITIDS, and other tubular taxa (SORBEOCONCH snails in CERITHIOIDEA
# [especially families SILIQUARIIDAE and TURRITELLIDAE, although propagations
# might include other cerithioids]) should (generally) be twice the transverse
# length, instead. Tentaculate web-feeding (filter-feeding) AMMONOIDS (such as
# those in Superfamily TURRILITOIDEA and Order CERATITIDA) have many exceptions
# but are typically approximated by the AP conch length. (Exceptions typically
# have the same scale for both life habit and size.)

# (10) Confirm that wholly infaunal filter-feeders (AboveAbsStrat = 0, WithinAbs
# = 1, Filter = 1 [rest 0]) have AbsFoodStrat at 0.25 (typically) and that
# AbsFood is "Above" the seafloor (with exceptions, like CALLIANASSOIDS that
# also ingest sediment from burrow wall, or [Cenozoic] cave-dwelling
# BRACHIOPODS, or thin PRODUCTIDS that rested within the sediment with gape at
# seafloor, or ECHINOIDS like clypeasteroid mellitid Encope that emerge at
# surface to filter feed, or interstitial meiofauna).

# (11) Confirm that filter feeders (whether infaunal or epifaunal) that extend
# body above sea floor (AboveAbsStrat = 1 & FeedAbovePrimary = 1 & Filter = 1
# [rest = 0]) have AbsFoodStrat at level to which body extends (and that food is
# also "Above" the seafloor). Easiest to sort by AbsStrat > AbsFoodStrat >
# AbsStratDist, Exceptions include the relatively deeply buried semi-infaunals,
# where a large portion of body may be buried but only a small portion atop
# seafloor, some STROPHOMENIDS/PRODUCTIDS/CHONETOIDEANS where FoodStrat is
# closer to sediment, and some echinoderms (such as OPHIUROIDS and STYLOPHORANS)
# that raise their filter-feeding arms into the water column when feeding.

# (12) Make sure that all "above primary" [large epifauna and demersal/nektonic]
# organisms (AbovePrimary = 1, WithinPrimary = 0) that have RelStrat = 1 also
# have AbsStrat = 1.

# (13) Confirm that entries that are definitely benthic (whether infaunal or
# epifaunal) are not listed as fluidic or insubstantial. Search for AbsStratDist
# = ">-100000" values [ANY], omitting those with Fluidic = 0 or Insubstantial =
# 0. Sort by AbovePrimary > Phylum > Class. If warranted, set Fluidic = 0 and
# Insubstantial = 0. If both of these "aquatic" states are checked, they are
# normally the only states checked for these characters. (Exceptions include
# some amphibious tetrapods that regularly rest on land and within water, such
# as early CETACEANS, CROCODYLIANS, PINNIPEDS, and other coastal tetrapods, and
# some floating marine BIRDS, which can be both fluidic and Soft-substrate
# resters.) If other substrates are checked, reconcile the conflict by setting
# the correct state combinations. If only a single substrate type of blank, that
# state can be added as the likely substrate type.

# (14) For filter-feeding ECHINODERMS, clear the FilterDensity field if the
# entry is NOT a filter-feeder. Check FilterDensity = High/Medium/Low and
# FilterFeeder != 1. (If FilterFeeder is blank in the Constant data set, check
# the Mode propagation; if a FilterDensity coding was propagated in the Constant
# data set and the Mode data set has FilterFeeder = 1, then keep the
# FilterDensity and propagate its dependent FilterFeeder = 1 and check the
# Estimated box. But if the Mode data set has FilterFeeder = 0, then remove the
# FilterDensity state in the Constant data set.)

# (15A) Confirm AbsFoodStrat and RelFoodStrat for "predators" (really, all
# raptorial feeders, including scavengers). Search for Raptor = 1 only and sort
# by Fluidic (descending, so swimming first!) > Microbivore > Phylum > Class >
# AP / DV. Be aware the same higher taxon can have widely varying codings among
# its genera, depending on many subtle factors, such as prey specializations,
# body size, foraging habits, and sensory differences (e.g., parasites,
# durophagous predators, predators of infaunal prey, etc.). See
# LifeHabitNotes.docx for details. Best to run each check below separately for
# each higher taxon.

#     (1) Use the following conventions for FISHES, non-shelled cephalopods 
#     (e.g., COLEOIDS, including BELEMNITES), and other SWIMMING predators (and 
#     generally crawling benthic predators, too):

#       (a) NEKTONIC (and PLANKTONIC) forms feed in the water column 
#       (AbsFoodStrat typically = 1, RelFood = 1/1 for being above and 
#        "within").

#       (b) BENTHOPELAGIC and DEMERSAL forms swim in water column and often 
#       (but not always) have benthic prey (RelFood = 0/1 for being only 
#       "within/below" their swimming position and AbsFoodStrat ~ 1/2 DV height 
#       if epibenthic or shallow infaunal prey, but many exceptions).

#       (c) NEKTOBENTHIC (and the typical convention for non-swimming benthic 
#       predators) forms rest on (or within) the seafloor and also typically 
#       have benthic prey (with AbsFoodStrat ~ 1/2 DV height), but RelFood will 
#       depend on whether the prey is epifaunal and/or infaunal.

#     In all cases, RelFoodStrat estimated by distance the predator (or their 
#     organ, such as an introvert, proboscis, or tentacles) moves to ensnare 
#     prey. If swimming or moving to attack, RelFoodStrat scales with AP length 
#     (<1X to 1X for crawlers, 2X for swimmers, 4X for flyers), but with many 
#     exceptions. Can usually distinguish crawlers from swimmers by sorting by 
#     fluidic substrate.)

#     (2) RelFoodStrat rules for crawling predators:

#       (a) ASTEROIDS = 0.25.

#       (b) DECAPODS ~ 1 X AP or DV scale, depending on locomotive type (and as 
#       long as don't swim when foraging).

#       (c) Small scavenging crustaceans (some AMPHIPODS, TANAIDACEANS, 
#       ISOPODS), use 1 X AP if crawling to "bulky" herbivorous or carnivorous 
#       food or 1 X DV if generally immobile and feeding on detritus, surficial 
#       algae, or other microbial food.

#       (d) SNAILS = 0 slower (often mass-feeding) forms and 0.25 for fastest 
#       ones (often predators).

#       (e) For macroalgae feeders (raptors on bulk microbes), generally code 
#       RelFoodStrat as a crawler based on size, but downgrading one level to 
#       account for reduced mobility because of immobile food.

#     (3) Ambush-hunting (typically infaunal) ANNELIDS, PRIAPULIDS, NEMERTEANS, 
#     and other vermiform predators: RelFoodStrat ~ length of introvert/proboscis
#     (typically longer than transverse diameter, and often a substantial 
#     portion of A/P).

#     (4) Predatory benthic TRILOBITES: AbsFoodStrat ~ 1/2 DV height and 
#     RelFoodStrat ~ 1 X AP. Common exceptions include highly visual taxa (coded
#     with 2 X AP) or ambush-hunting trilobites with eye stalks (typically 
#     coding 1 X AP) (e.g., several ASAPHIDS, PHACOPIDS [especially PHACOPINES, 
#     which are nearly all coded at 2 X AP], ODONTOPLEURIDS, and HOMALONOTID and
#     ENCRINURID PHACOPIDS) and pelagic trilobites (e.g., CYCLOPYGIDS, coding 
#     2 X AP). Trilobites generally coded as feeding on "epifaunal" prey (unless
#     evidence of Rusophycus, which gets coded as eating prey both above and 
#     below the sediment-water interface).

#     (5) For CNIDARIANS and sedentary carnivores, RelFoodStrat ~ tentacle 
#     length (= corallite diameter) and AbsFoodStrat = AbsStrat.

#     (6) Carnivorous crinoids (some TAXOCRINIDS and SAGENOCRINIDS) and 
#     ECHINOIDS (e.g., Histocidaris, but be alert for podia-grabbing 
#     microbivorous "raptors" whose RelFoodStrat is based instead on podial 
#     length) have RelFoodStrat = 0.25 and AbsFoodStrat = AbsStrat.

#     (7) For shelled cephalopods (AMMONOIDS, NAUTILOIDS, etc., including order 
#     SPIRULIDA), use 1 X T as an estimate of tentacle length. For (swimming) 
#     non-shelled COLEOIDS (except shelled spirulids), use 2 X AP.

#     (15B) Run separate check on scan-and-trap "raptorial" plankton (Search for
#     Order = AGNOSTIDA, Order = EODISCIDA, Subclass = DIPLOSTRACA, Class =
#     OSTRACODA (but only those coded as raptorial!), Class = COPEPODA and tiny 
#     trilobite subfamily = LEPTOPLASTINAE) without any life habit codings and 
#     sort by DV): RelFoodStrat ~ 1 X DV.

#     (16) Confirm AbsFoodStrat and RelFoodStrat for mass feeders (both detritus
#     feeders, algae scrapers, and some scavengers). Search for Mass = 1 and rest
#     0s, and sort by FoodAbovePrimary > Carnivore > Phylum > Class > DV/T to
#     distinguish epifaunal from infaunal foods:

#     (16A) "Deposit" and detritus feeders eating WITHIN sediment: AbsFoodStrat
#     based on location of food, typically corresponding to burial depth. For
#     surficial "deposit-feeding" TRILOBITES, use 1/2 X transverse width (or 1 X 
#     D/V depth, if smaller value) as proxy for depth of sediment churned. Code
#     RelFoodStrat = 0 unless there is a cephalic shield present (i.e.,
#     TRINUCLEOIDEANS, BATHYURIDS, HARPINES, and several BATHYCHEILID,
#     BRACHYMETOPID, and PROETOIDEAN genera), in which case RelFoodStrat = 1/2 X 
#     D/V (typically = 0.25). For consistency, using the same conventions for
#     non-trilobites (e.g., mud-grubbing early FISHES, PHYLLOCARIDS, CUMACEANS, 
#     and modified for OSTRACODES). Non-selective sediment-feeders (e.g., 
#     ECHINOIDS, ASTEROIDS, HOLOTHUROIDEANS, SOMASTEROIDEANS, STENUROIDEANS, 
#     OPHIUROIDS and POLYCHAETES, and PROTOBRANCH bivalves) will have 
#     RelFoodStrat = 0 unless they use an organ (palp proboscides, podia, 
#     tentacles, etc.) to collect and ingest sediment, in which case 
#     RelFoodStrat = the length of this organ. In the case of NUCULID and 
#     NUCULANID (only) PROTOBRANCHS, RelFoodStrat = 0.25 (technically, 25% X 
#     D/V). Because these distinctions are unlikely to be known for most 
#     fossils, it is often most prudent to let the propagation algorithm infer 
#     the missing states, and then to not change them unless there is certainty.

#     (16B) Surficial mass (typically algal) feeders (including those that use
#     body mucus to capture seston): Animals scraping or eating algal films / 
#     detritus ON/ABOVE substrate (e.g., SNAILS, MONOPLACOPHORANS, CHITONS, a 
#     few infaunal TELLINID BIVALVES, JAWLESS and EARLY FISHES, some 
#     POLYCHAETES, ECHINOIDS, ASTEROIDS, OPHIUROIDS, and OSTRACODES), typically 
#     are coded as AbsFoodStrat = 0. (Note macroalgae feeders are excluded here 
#     because they are bulk feeders and often raptorial.) RelFoodStrat depends 
#     on where the feeding organ is located:

#       (A) If the mouth is emplaced against sediment in resting (non-feeding) 
#       position, RelFoodStrat = 0.

#       (B) If the head is raised substantially and a proboscis is used for 
#       feeding (or the animal swims), then the distance will correspond to the
#       distance from the organism to the food. Similar logic is used if the 
#       animal is infaunal (TELLINIDS, POLYCHAETES) and feed using extensible 
#       feeding organs like palps or siphons or introverts. RelFoodAbove/Within 
#       coding depends on whether the organism considers the food above or below 
#       their level. For most animals, the surficial film with be "above" their 
#       immediate microhabitat, but animals with long feeding organs or demersal 
#       animals may be coded as below their immediate microhabitat. For 
#       mass-feeding OSTRACODES and similar animals (such as BRADORIIDS and 
#       PHYLOCARIDS) using appendages to collect food particles (usually within 
#       sediment, but sometimes above it), RelFoodStrat will generally 
#       correspond with appendage length, approximated by D/V, a minimal 
#       modification (because of lateral compression) from the logic used for 
#       trilobites. Surficial mass-feeding, carnivorous (or macro-algal) 
#       scavengers (e.g., ASTEROIDS, OPHIUROIDS, STENUROIDS and some ECHINOIDS) 
#       are typically coded as RelFoodStrat = 0.25 because of presumably larger 
#       food; however, those known to be especially fast (e.g., Ophiura and some 
#       other ophiuroids) can be coded more like an active scavenger (with 
#       RelFoodStrat based on 1 X AP). Animals that ingest bodily-secreted mucus 
#       ensnared with seston (e.g., a few ASTEROIDS) are coded AbsFoodStrat as 
#       the uppermost body surface and RelFoodStrat = 0.
#
# (17) Confirm RelFoodStrat and AbsFoodStrat are correct for absorptive feeders,
# incorporeal feeders, and autotrophs, running each separately (the state = 1,
# rest = 0). Sort by RelFoodStrat > AbsStrat > RelStrat > Phylum > Class.
# Ambient and incorporeal feeders should have RelFoodStrat = 0 and AbsStrat =
# RelStrat, and no animals should be listed as an autotroph (but animals can be
# ambient- and/or incorporeal-feeders).

# (18) Confirm RelFoodStrat and AbsFoodStrat are correct for attachment feeders
# and solution feeders, running each separately (the state = 1, rest = 0). For
# attachment feeders, RelFoodStrat will depend on depth of penetration. Best
# strategy is to review propagated values and use logic from relatives to fill
# in blank states.

# (19) Go through each character and confirm no "all-zeros" (i.e., if all
# dietary states are 0, etc.), removing some estimates as needed. All 1s are
# acceptable. If not sure which to delete, consult the alternative propagation
# method data set for a reference. (Do this check BEFORE subsequent ones because
# it can lead to new entries with a particular feeding mode.)

# (20) Once these checks are run, re-run them (except no. 1 that checks every
# one) and clear the SizeChanged = Check tags for each check. Then do a final
# search for any that are still checked, focusing on the size-related
# characters, to confirm sensibility, clearing them at end.
