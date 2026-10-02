## REVERSE ENGINEER NOEL HEIM's (et al., 2015) SHAPE-BASED BIOVOLUMES INTO ATD
## MAJOR AXES

## ISSUES ######################################################################
##
## 1. For incertae sedis (like Bunaia), it might make more sense to match age
##    AND size rather than just age. Given how rare these cases are (and because
##    the vast majority are approximated by very close relatives), leaving this
##    for a future release.
##
################################################################################

## BASIC LOGIC -------------------------------------------------------------

# A one-time (bespoke) code based on PropagateSizes.R that take Heim, et al.'s
# (2015) shape-based (cones and ellipses) biovolumes and rescales them using
# relatives with known major axis (ATD) measurements to reverse engineer what
# their original raw data measurements were.

# See SI for Heim, et al. (2015) for logic used when choosing shapes and
# calculating biovolumes. See SizesToImport_PublishedSITables.xlsx for the
# pre-processed data used herein. See Heim_rescaling_ATD_solution.xlsx for proof
# the algorithm below works as intended.

# "Relative' means the smallest inclusive taxonomic group the entry is related
# to whose body size has been coded. Use the 'EstAP', 'EstT', and 'EstDV' tags
# to know which are not measured directly (and can be over-ridden). The
# 'History_Size' field should be overridden as needed to explain how un-measured
# lengths were estimated.

# 1. Find closest relative in core database with ALL 3 ATD. 1) if multiple 
#    relatives exist, pick the one (regardless of whether sp, subgenus, or
#    genus) that is closest to the same age [(earlyRel - earlyEntry) ^ 2 +
#    (lateR - lateE) ^ 2], and pick match with smallest absolute difference.
#    Why? Because size is known to change through time (although unclear if
#    shape does). Note that this means that taxa missing a range will not get an
#    estimated body size based on temporal similarity (and will only be chosen
#    based on closest relative). (And no need to calculate square root in this
#    case, which saves computational time.)
#
# 2. Use (conical or elliptical) shape approximation to extract ATD product from 
#    Heim, et al. (2015) database. Calculate the scaling factor as the cube root
#    of Heim's ATD to the ATD product of temporally closest relative.
#
# 3. Update "estimate" check boxes and metadata accordingly.
#
# 4. Update AbsStratDist using similar logic.




## EXPORT DATA -------------------------------------------------------------

# (0) Use the same export and pre-processing steps as in PropagateSizes.R (and 
#     using SelectCols.R to select columns), but only need to export entries at
#     species/subgenus/genus scale.

# (1) Open file in MSWord (make sure smart quotes are off: File > Options >
#     Proofing > Autocorrect Options > Autoformat As You Type > uncheck Smart
#     Quotes) and delete any hidden tabs (tab within a text field) and all
#     problematic (i.e., double) quotation marks (replacing [UNLESS THE 
#     QUOTATION MARK IS CORRECTLY AT THE END OF A TEXT FIELD] "^t with ^t and 
#     replacing ^t"with ^t and replacing "" with "). Re-save file in same 
#     format.
#
# (2) Add a new column named 'BodySize' that estimates the body size, using 
#     the product of 3 'XLength' sizes (= PRODUCT(AF2:AH2)).
#
# (3) Sort the BodySizeScale = 'Sp/Gen/Subg' rows (AND ONLY THESE ROWS!) by:
#
#     (a) Number of PhotoX columns (with largest values first) so entries with 
#         complete (all 3) size measurements are first and most incomplete are 
#         last. That way those entries with best scales and more-complete sizes 
#         are checked first, and so later entries can use the largest available 
#         pool of relatives.
#
#     (b) Sort item by AbsStratDist (with largest values first) so entries with 
#         estimated AbsStratDists are considered first to propagate to those 
#         lacking them.
#
#     (c) Sort by BodySize (with largest values first).
#
# (4) Delete the added 'PhotoX' and 'BodySize' columns and resave.



## READ AND PROCESS DATA ---------------------------------------------------
rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")

pre.input <- read.delim(file = "preSizes.tab", stringsAsFactors = FALSE)

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
scales <- c("Species", "Subgenus", "Genus", "Tribe", "Subfamily", "Family", 
            "Superfamily", "Subsection", "Section", "Infraorder", "Suborder", 
            "Order", "Superorder", "Infraclass", "Subclass", "Class", 
            "Superclass", "Subphylum", "Phylum", "", NA)
scales <- factor(scales, levels = scales, ordered = TRUE)
input$BodySizeScale <- factor(input$BodySizeScale, levels = scales, ordered = TRUE)
ATD.cols <- which(colnames(input) == "APLength" | colnames(input) == 
                    "TransverseLength" | colnames(input) == "DVLength")
photo.cols <- which(colnames(input) == "PhotoAP" | colnames(input) == 
                      "PhotoTransverse" | colnames(input) == "PhotoDV")
est.cols <- which(colnames(input) == "Est_AP" | colnames(input) == "Est_T" |
                    colnames(input) == "Est_DV")
ATD.scale.cols <- which(colnames(input) == "APScale" | colnames(input) ==
                          "TransverseScale" | colnames(input) == "DVScale")
AP.cols <- which(colnames(input) == "APLength" | colnames(input) == "PhotoAP" |
                   colnames(input) == "APScale")
T.cols <- which(colnames(input) == "TransverseLength" | colnames(input) ==
                  "PhotoTransverse" | colnames(input) == "TransverseScale")
DV.cols <- which(colnames(input) == "DVLength" | colnames(input) == "PhotoDV" |
                   colnames(input) == "DVScale")
AbsStratDist.col <- which(colnames(input) == "AbsStratDistance")
date.col <- which(colnames(input) == "DateEntered_Size")
hist.col <- which(colnames(input) == "History_Size")
str(input)

# TROUBLESHOOT: Make sure the "SizeChanged", "History_Size", and 4 "Est_X"
# (including Est_AbsStratDistance) columns are input as characters and not
# logicals or all NAs. (If a column is blank, it is classed by default as a
# logical (which causes errors below) and replaced with NAs.) If needed (such as
# when there are no "check"ed entries; i.e., when did a clean propagation), use
# next lines to force to proper class. Note that it is fine for AbsStratDist to
# have NAs for missing values (so long as the column is treated as a numeric).
# It is also advisable to confirm the PhotoX and XLength columns are input as
# numerics.

## Following usually required:
input$SizeChanged <- replace(input$SizeChanged, which(is.na(input$SizeChanged)), "")
input$History_Size <- replace(input$History_Size, which(is.na(input$History_Size)), "")

## Rest needed only if did a fresh propagation (i.e., lacking any estimates -
## not typically recommended!)
# input$Est_AP <- replace(input$Est_AP, which(is.na(input$Est_AP)), "")
# input$Est_T <- replace(input$Est_T, which(is.na(input$Est_T)), "")
# input$Est_DV <- replace(input$Est_DV, which(is.na(input$Est_DV)), "")
# input$Est_AbsStratDistance <- replace(input$Est_AbsStratDistance, which(is.na(input$Est_AbsStratDistance)), "")
str(input)

out <- input                    # Work with 'out', saving 'input' for reference
colnames(input[photo.cols])     # "PhotoAP", "PhotoTransverse", "PhotoDV"
colnames(input[est.cols])       # "Est_AP", "Est_T", "Est_DV"

head(out)
tail(out)

# Current taxonomic resolution of data set. Compare to 'out' below to see if
# there has been improvement. (There shouldn't be for this algorithm.)
table(input$BodySizeScale)

# Troubleshooting
if (any(is.na(input$max_ma)) || any(is.na(input$min_ma)))
  stop("Some entries have missing age ranges, which is used in body size propagation 
algorithm. Leaving ranges empty will mean that missing size measurements will not use age-matched relatives for these entries. Update ages from the Paleobiology Database before proceeding.\n")

## Any duplicated ID numbers?
# Will match the incorrect life habits when importing
if (any(table(input$IDNumber) > 1)) {
  print(which(table(input$IDNumber) > 1))
  stop("There are duplicate IDNumbers. Fix before proceeding! See above for list.")
}

## Import processed Heim, et al. (2015) dataset
Heim <- read.csv(file = "HeimShapePropagations.csv", header = TRUE)
head(Heim)

# Confirm all IDNumbers in Heim are also in the core database. (IDNumbers are
# what are used to match the two data sets.)
if (isFALSE(all(Heim$IDNumber %in% out$IDNumber)))
  stop("Compare two data sets to ensure the IDNumbers are correctly shared.")




## FUNCTIONS ---------------------------------------

## FUNCTION TO FIND RELATIVES OF SIMILAR AGE CODED AT BEST AVAILABLE RESOLUTION
## (BUT ADDING THOSE CODED AS GENERA, SUBGENERA, AND SPECIES WHEN POSSIBLE)
#  x = data frame of all data.
#  i = index (row number for taxon entry being considered).
#  start = taxonomic level to start (default = tribe, with 1 = species and 20 = NA)
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
find.rel <- function(x, i, start = 4, end = 21, photo.cols = NULL, 
                     est.cols = NULL, all.3 = TRUE, sim.time = TRUE) {
  if (any(is.null(photo.cols), is.null(est.cols)))
    stop("photo.cols and est.cols need to be specified\n")
  scales <- c("Species", "Subgenus", "Genus", "Tribe", "Subfamily", "Family", 
              "Superfamily", "Subsection", "Section", "Infraorder", "Suborder", 
              "Order", "Superorder", "Infraclass", "Subclass", "Class", 
              "Superclass", "Subphylum", "Phylum", "", NA)
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
    # Confirm relatives are within correct higher taxon (in case of homonyms)
    if (NROW(unique(rels[, 3:sc.col])) > 1L) {
      higher.taxon <- apply(rels[, 3:sc.col], 1, paste0, collapse = "")
      rels <- 
        rels[which(higher.taxon == paste0(out[i, 3:sc.col], collapse = "")), ]
    }
    poss.rels <- rels
    # Test if there are any relatives with all 3 complete measurements
    sq <- seq.int(nr)
    complete <- !sapply(sq, function(sq) any.missing(rels[sq, ], photo.cols,
                                                     est.cols)$any)
    rels <- rels[complete, ]
    nr <- nrow(rels)
    if (nr == 0L & all.3 == FALSE) {
      # If not, do any relatives have at least 1 measured measurement?
      part.complete <- !sapply(sq, function(sq) 
        any(is.na(poss.rels[sq, photo.cols])) || 
          any(poss.rels[sq, photo.cols] == ""))
      rels <- poss.rels[part.complete, ]
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


## IMPROVED ALL.EQUAL THAT JUST PRINTS THOSE THAT ARE CHANGED (IGNORING ROW 
## NUMBERS), USING SPECIFIED NUMBER OF SIGNIFICANT DIGITS AND SPECIFYING WHICH
## COLUMNS TO ROUND
#  sig.digits = how many significant digits should 'nums' values be rounded?
#  nums = which columns in 'a' and 'b' should be rounded?
better.all.equal <- function(a, b, sig.digits = 2, 
                             nums = sort(c(AP.cols, T.cols, DV.cols, 
                                           AbsStratDist.col))) {
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
  proportions <- c(.25, (1 / 3), 0.5, (2 / 3), 0.75, 0.9, 2.0, 4.0, 5.0)
  ref_ms <- c(ref$APLength, ref$TransverseLength, ref$DVLength)
  ref_dists <- c(1, -1) %x% c(ref_ms, angle.30 * ref_ms, angle.45 * ref_ms, 
                              angle.60 * ref_ms, proportions %x% ref_ms)
  wh.m <- match(signif(ref$AbsStratDistance, 3), signif(ref_dists, 3))
  target_ms <- c(target$APLength, target$TransverseLength, target$DVLength)
  target.strat.dist <- (c(1, -1) %x% c(target_ms, angle.30 * target_ms, 
                                       angle.45 * target_ms, angle.60 * target_ms, proportions %x% target_ms))[wh.m]
  return(target.strat.dist)
}



## PROPAGATE BODY SIZES --------------------------------------------

# Go through each record in Heim one by one (matching with IDNumber), rescaling
# Heim's biovolumes to estimate original ATDs. Because we are assuming an
# isometric (same-shape) model, measurements are not log-transformed. In future,
# if wish to implement a linear regression model for estimating sizes (i.e.,
# calculate allometric power function among relatives to estimate missing
# measurements), will need to log-transform the measurements.

index <- seq(100, nrow(Heim), by = 100) # For keeping track
today <- format(Sys.Date(), "%m/%d/%Y")
angle.30 <- sin(30 * pi / 180)
angle.45 <- sin(45 * pi / 180)
angle.60 <- sin(60 * pi / 180)
proportions <- c(.25, (1 / 3), 0.5, (2 / 3), 0.75, 0.9, 2.0, 4.0, 5.0)
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
# Choose what rank to end searching for relatives? (all ranks used). (The start
# is defined below, based on whether there is a subgenus.)
scale.end <- length(scales)
# Choose what rank to end searching for relatives for AbsStratDist? (order)
AbsStratDist.scale.end <- which(scales == "Order")
# interactive <- TRUE   # If want to watch updates in real time
interactive <- FALSE
if (interactive) par("ask" = TRUE) else par("ask" = FALSE)
# log actions so can trace changes
record.log <- TRUE
record.file <- "SizeLog_Heim.txt"
if (record.log) cat("Changes made to body sizes on", today, ":\n\n", file = record.file, append = FALSE)
(start.t <- Sys.time())

for (i in 1:nrow(Heim)) {
  # Errors typically occur when there are no available relatives
  # for (i in 952:nrow(out)) {
  
  # Match records
  wh <- which(out$IDNumber == Heim$IDNumber[i])
  
  if (i %in% index)
    cat("record", i, "of", nrow(Heim), ":", out$Genus[wh], "\n")
  
  # Tag if taxon is a subgenus. (Should use measured congenerics as relatives,
  # if so.)
  is.subgenus <- ifelse(out$Subgenus[wh] != "", TRUE, FALSE)
  
  # Set starting rank for finding relatives (genus if subgenus or tribe if
  # anything else)
  scale.start <- ifelse(is.subgenus, which(scales == "Genus"), 
                        which(scales == "Tribe"))
  missing <- any.missing(out[wh, ], photo.cols, est.cols)
  number.missing <- length(missing$which)
  missing.strat <- is.na(out$AbsStratDistance[wh])
  # Note prior line should NOT check Est_AbsStratDistance here. Checks below
  # instead.
  
  # Next line sets 'rels' to be NULL rather than blank matrix (which is a
  # better behavior for below)
  rel <- rels <- NULL
  change <- input$SizeChanged[wh]
  
  # Find (temporally) closest relatives. Note acceptable to NOT require all 3
  # measurements are measured. In this case, better to train off of closest
  # relative with only 2 (or even 1) confirmed measurement.
  rel <- find.rel(x = out, i = wh, start = scale.start, end = scale.end, 
                  photo.cols = photo.cols, est.cols = est.cols, all.3 = FALSE)

  # Should never trigger here, but just in case:
  if (nrow(rel$rel) == 0L)
    stop("Double check why there are no available relatives.")
  
  # Rescale Heim's shape-based biovolume to relative's shape. Because rescaling
  # based only on ATD, only need to divide Heim's volume by non-ATD shape
  # equation factor. See Heim_rescaling_ATD_solution.xlsx for proof the
  # algorithm below works as intended.

  # Most are ellipses
  shape <- Heim$Shape[i]
  ATD_Heim <- Heim$BVOL[i] / ifelse (shape == "conical", pi / 12, pi / 6)

  # ATD product for relative
  ATD_rel <- prod(rel$rel[, ATD.cols])
  
  # Rescale ATDs based on cube root of ratio of ATD products
  scaling_factor <- (ATD_Heim / ATD_rel) ^ (1 / 3)
  
  # Record rescaled ATDs
  out[wh, photo.cols] <- rel$rel[, ATD.cols] * scaling_factor
  out[wh, ATD.scale.cols] <- c(1, 1, 1)
  out[wh, est.cols] <- rep("Estimated", 3)
  
  change <- "maybe"
  out$History_Size[wh] <- paste("On", today, "ATD estimated from rescaled Heim, et al.'s (2015)", 
                                shape, "approximation from proportions in same", tolower(rel$size.sc), 
                                as.character(rel$rel[which(colnames(rel$rel) == rel$size.sc)]), 
                                "of", rel$rel$BodyMeasureReference)

  # Update actual sizes. (Will be overridden by FileMakerPro, but need to update
  # because the raw lengths are used in estimating AbsStratDist.)
  out$APLength[wh] <- out$PhotoAP[wh] / out$APScale[wh]
  out$TransverseLength[wh] <- out$PhotoTransverse[wh] / out$TransverseScale[wh]
  out$DVLength[wh] <- out$PhotoDV[wh] / out$DVScale[wh]
  
  # Propagate AbsStratDist (using closest relatives, regardless of whether
  # geologically contemporaneous or whether missing some size measures)
  if (missing.strat) {
    # ... if missing AbsStratDist but available via ALL best relatives (that are
    # within same order [end = 12] or lower resolution)
    rels <- find.rel(x = out, i = wh, start = scale.start, 
                     end = AbsStratDist.scale.end, photo.cols = photo.cols, 
                     est.cols = est.cols, sim.time = FALSE, all.3 = FALSE)$rel
    rels.with.strats <- 0L
    if (!is.null(rels))
      rels.with.strats <- length(na.omit(rels$AbsStratDistance))
    if (missing.strat & rels.with.strats > 0L) {
      nr <- 1:nrow(rels)
      poss.strats <- as.vector(na.omit(sapply(nr, function(nr) get.strat(out[wh, ], 
                                                                         rels[nr, ]))))
      # Discard all if some aren't a canonical orientation (because ignored in 'get.strat')
      if (length(poss.strats) != rels.with.strats)
        poss.strats <- NA
      if (length(unique(na.omit(poss.strats))) == 1L) {
        out$AbsStratDistance[wh] <- poss.strats[1]
        out$Est_AbsStratDistance[wh] <- "Estimated"
      }
    }
  } else {
    # Or update AbsStratDist if previously existed, but some measurements were
    # updated
    orig.ms <- unlist(input[wh, ATD.cols])
    poss.dists <- c(1, -1) %x% c(orig.ms, angle.30 * orig.ms, angle.45 * orig.ms, 
                                 angle.60 * orig.ms, proportions %x% orig.ms)
    if (change == "maybe" & !missing.strat &
        signif(input$AbsStratDistance[wh], 3) %in% signif(poss.dists, 3))
      out$AbsStratDistance[wh] <- get.strat(out[wh, ], input[wh, ])
  }
  
  # Add "check" tag if any size or AbsStratDist was changed (to 2 significant
  # digits)
  if (!identical(signif(input[wh, photo.cols], 2), signif(out[wh, photo.cols], 2)) ||
      !identical(signif(input$AbsStratDistance[wh], 2),
                 signif(out$AbsStratDistance[wh], 2))) {
    out$SizeChanged[wh] <- "Check"
  }
  
  # Update history if AbsStratDist is added or re-calculated (using updated
  # measurements, if changed)
  if (out$Est_AbsStratDistance[wh] == "Estimated") {
    orig.ms <- unlist(out[wh, ATD.cols])
    poss.dists <- c(1, -1) %x% c(orig.ms, angle.30 * orig.ms, angle.45 * orig.ms, 
                                 angle.60 * orig.ms, proportions %x% orig.ms)
    wh.m <- match(signif(out$AbsStratDistance[wh], 3), signif(poss.dists, 3))
    if (out$History_Size[wh] == "")
      out$History_Size[wh] <- paste0("AbsStratDist estimated ", today, " from ", 
                                    AbsStratDist.text[seq.AbsStratDist[wh.m]], " Prior AbsStratDist was ", 
                                    signif(input$AbsStratDistance[wh], 3), ".") else
                                      out$History_Size[wh] <- paste0("AbsStratDist updated ", today, " from ",
                                                                    AbsStratDist.text[seq.AbsStratDist[wh.m]], " Prior AbsStratDist was ", 
                                                                    signif(input$AbsStratDistance[wh], 3), ". ", out$History_Size[wh])
  }
  
  # Interactive mode (to observe how states are being propagated in real time).
  # Deprecating printing of History_Size because of false positives due to
  # simple change in date, and recalculations of ATD lengths. Note 'nums' is
  # updated to the index when excluded columns (i.e., hist.col and ATD.cols) are
  # removed.
  changes.made <- ncol(better.all.equal(input[wh, -c(hist.col, ATD.cols)],
                                        out[wh, -c(hist.col, ATD.cols)], sig.digits = 3, 
                                        nums = c(photo.cols, AbsStratDist.col) - length(c(hist.col, ATD.cols)))) > 0L
  if (interactive & changes.made) {
    plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    text(1, 1, as.character(paste(out$Genus[wh], out$Species[wh])), cex = 2)
    cat(as.character(paste(out$Genus[wh], out$Species[wh])), "\n")
    # print(better.all.equal(input[wh, ], out[wh, ]))
    # If want to mask date and calculated lengths:
    print(better.all.equal(input[wh, -c(date.col, ATD.cols)],
                           out[wh, -c(date.col, ATD.cols)], 
                           nums = c(photo.cols, AbsStratDist.col) - 
                             length(c(date.col, ATD.cols))))
    cat("\n")
  }
  
  # If recording a log of changes made:
  if (record.log & changes.made) {
    cat(as.character(paste(out$Genus[wh], out$Species[wh])), "\n", file = record.file,
        append = TRUE)
    # Deprecating printing of date and History_Size, because of false positives
    # due to simple change in date.
    comps <- better.all.equal(input[wh, -c(date.col, hist.col, ATD.cols)], 
                              out[wh, -c(date.col, hist.col, ATD.cols)], 
                              nums = c(photo.cols, AbsStratDist.col) - 
                                length(c(date.col, hist.col, ATD.cols)))
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

round(cumsum(table(input$BodySizeScale) * 100 / nrow(input)), 1)
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
write.table(out, file = "PostSizes_Heim.tab", quote = FALSE, sep = "\t", row.names = FALSE)

# (1) Open in Excel to confirm looks acceptable. Replace (with Options = Match
#     entire cell contents) "NA"s in taxonomic names, body size data, 
#     stratigraphic ranges, and AbsStratDist with blank cells. (Essentially the 
#     entire output, except for PBDB_GSG_Number, if not in the PBDB.)

# (2) Open in Word to remove quotation marks around the text entries [UNLESS THE
#     QUOTATION MARK IS CORRECTLY AT THE END OF A TEXT FIELD], (replacing "^t 
#     with ^t and replacing ^t" with " and replacing "" with ").

# (3) Open FileMakerPro and import as a tab-delimited file, updating records by
#     matching (ONLY!) IDNumber as the matching identifier. Do not use the PBDB
#     Number or Genus/Subgenus names to match, because this will cause false
#     matches for entries missing PBDB numbers or for multi-subgenus genera.
#     (Fine to not import the taxonomic names or geological ranges or
#     [especially the] BodyMeasureReference [which will override any non-latin
#     characters], but import everything else.) To set the column names as the
#     field names, make sure the first entry row in the imported source (the
#     column headings) are visible, then choose "Use as Field Names" for the
#     imported source file. Then choose "Matching Names" in the Target Fields
#     dropdown to ensure that the source and target fields match (but
#     double-check that they are matched correctly).