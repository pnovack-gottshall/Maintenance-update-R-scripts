## PROPOGATE LIFE HABIT CODINGS ACROSS ENTRIES, USING RELATIVES AS PROXIES

## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
##
## !!!NONE!!!
##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
##
## FUTURE FEATURE REQUEST? Add AbsStratDist here instead, so that propogated
## based not only on size, but also on life habit coding? Would require
## exporting canonical body size lengths!

## BASIC LOGIC -------------------------------------------------------------

# "Relative' means the smallest inclusive taxonomic group the entry is related
# to whose life habit has been coded.

# 1. If an entry is already coded at species-, subgenus-, or genus-level, skip;
# unless life habits are partly blank (uncoded), in which case use closest
# relatives to populate remaining codings. (In these cases, tag the states as
# "Estimated" and record the changes in the History_Ecology field.)

# 2. If an entry is above genus-level, use closest relatives to update
# (override) the pre-existing codings. If unchanged (identical to relative),
# abort and go to the next entry, leaving the 'DateEntered_Ecology' and
# 'EcoScale' fields unchanged. If multiple relatives exist, enter the value (if
# constant) or 'NA' (if varies). (Record the relative as "Higher taxon indet."
# NO LONGER USED (July 2018): Use 'grep''s 'adist' to approximate type taxon for
# reference species when multiple relatives exist.) If any states remain
# uncoded, go to higher taxa in case they have codings for these states, using
# the same logic as step 1.

# The four body-size-related states (ABsStrat, RelStrat, etc.) are only
# over-written when missing or EcoScale > Species/Genus. If any of these four
# states are changed AND the BodySizeScale was Species/Genus (implying they were
# originally checked after the Propogatesizes.R algorithm), add a "check" tag to
# force a manual check. If any life habit codings are changed, add (or override)
# record-keeping text to History_Ecology field. This documents a history of life
# habit changes that can be restored using earlier back-up database copies. If
# there is no date in the 'DateEntered_Ecology' date field (i.e., this is the
# first update), just update the date (without an update to the history text).
# But if piecemeal states are changed (that are different than the consensus),
# record those changes to the History field.

# A benefit of this update-higher-taxon approach is that as we get more data, we
# can feel more confident in codings when there are many relatives (because it
# is still a strict consensus). If a higher taxon has lots of inherent
# variability, the NAs get up-voted based on actual variability across the
# higher taxon. So it's OK to "downgrade" the data (add more NAs) as we get more
# data, because that reflects the reality of natural variability. (Recall that
# entries coded at genus and species-level will not be overriden.)



## IMPORT DATA -------------------------------------------------------------

# Make sure the stratifications are updated from the size propogation BEFORE
# running this algorithm! (The algorithm reverts to original stratifications
# when size was coded as genus or species level, but still adds a size "Check"
# to trigger a second look-over afterwards.)

# (1) Before exporting, sort the entries so that EcoScale=Species is first,
# followed by Subgenus, Genus, etc. That way those with best scales are checked
# for completeness first.

# (2) Run relevant code in SelectCols.R for PropogateSizes.R to obtain following
# output. Then continue with step 3.

# IDNumber

# Taxonomy: Phylum, Subphylum, Class, Subclass, Order, Suborder, Superfamily,
# Family, Subfamily, Genus, Subgenus, Species

# Proxy fields: EcologyScale, RefGenusEco, RefSpeciesEco, DateEntered_Ecology,
# SizeChanged, BodySizeScale, History_Ecology

# Life habit characters (can really be in any order, as called by name):
# AboveImmediate, AbovePrimary, AbsFoodStratification, AbsStratification,
# AmbientFeeder, Asexual, Attached, AttachmentFeeder, Autotroph, Biotic,
# BulkFeeder, Carnivore, FeedingAboveImm, FeedingAbovePrimary, FeedingWithinImm,
# FeedingWithinPrimary, [FilterDensity field (only used with suspension-feeding
# echinoderms)], FilterFeeder, Fluidic, FreeLiving, HardSubstratum, Herbivore,
# Incorporeal, Insubstantial, Lithic, MassFeeder, Microbivore, Mobility,
# ParticleFeeder, RaptorFeeder, RelFoodStratification, RelStratification,
# SelfSupport, Sexual, SoftSubstratum, SolutionFeeder, Supported,
# WithinImmediate, WithinPrimary

# The same characters, with in Est_X form (excluding Est_AbsStratDist, Est_AP,
# Est_DV, and EstT).

# (3) Open in MSWord and delete all quotation marks (replacing "^t with ^t, ^t"
# with ^t, and "" with ").



rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/Documents/GSA (& NSF & NAPC)/2016GSA/GSA2016 analyses")
input <- read.delim(file = "preLH.tab", colClasses = "character")
# input <- read.delim(file = "preLH_withPBDB.tab", colClasses = "character")
scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily", 
  "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum", "", NA)
scales <- factor(scales, levels = scales, ordered = TRUE)
input$EcologyScale <- factor(input$EcologyScale, levels = scales, ordered = TRUE)
input$BodySizeScale <- factor(input$BodySizeScale, levels = scales, ordered = TRUE)
out <- input      # Work with 'out', saving 'input' for reference
str(input)
head(input)
table(input$EcologyScale)

## Any duplicated ID numbers?
if (any(table(input$IDNumber) > 1)) {
  print(which(table(input$IDNumber) > 1))
  stop("There are duplicate IDNumbers. Fix before proceeding! See above for list.")
}



## FUNCTIONS ---------------------------------------

## FUNCTION TO FIND RELATIVES CODED AT BEST AVAILABLE RESOLUTION (BUT INCLUDING 
## GENERA, SUBGENERA, AND SPECIES WHEN POSSIBLE)
# x = data frame of all data
# i = index (row number for taxon entry being considered)
# start = taxonomic level to start (default = subfamily, with 1=species and
#   14=NA). Note should never be '1' or else 'relatives' will include unrelated
#   taxa sharing the same species epithet.
# end = taxonomic level to end (default = NA, running higher up through all
#    levels, including unknowns)
# min.rels = minimum number of relatives needed in output
# ref.g.col = column for reference genus name
# ref.sp.col = column for reference species epithet name
# eco.col = columns to check for life habit traits
find.rels <- function(x, i, min.rels = 1, start = 4, end = 12, ref.g.col = 15, 
  ref.sp.col = 16, eco.col = 21:59) {
  if (start == 1L)
    stop("'start' must be greater than 1 (species-level)")
  scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily", 
    "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum")
  scales <- factor(scales, levels = scales, ordered = TRUE)
  others <- x[-i, ] # Entry cannot be its own relative
  low.res <- NA
  for (e in start:end) {
    # Identify correct column for taxonomic scale being considered
    sc.col <- which(colnames(others) == scales[e])
    # Ignore if unassigned taxa 
    # (Use index [1] as safeguard in case working with multiple 'i's, such as in
    # example)
    if (x[i, sc.col][1] == "") next
    if (x[i, sc.col][1] == "UNCERTAIN") next
    # Extract out relatives in taxon coded at species, subgenus, and genus-scale
    rels <- others[which(others[, sc.col] == x[i, sc.col]), ]
    rels <- rels[which(rels$EcologyScale == "Species" |
      rels$EcologyScale == "Subgenus" |
      rels$EcologyScale == "Genus"), ]
    if (nrow(rels) >= min.rels) break
  }
  eco.sc <- as.character(scales[e])
  return(list(rels = rels, eco.sc = eco.sc))
}


## CALCULATE THE MODE AMONG OBSERVED CHARACTER STATES
# NAs (and missing "" data) can be omitted (with default to include to match
# behavior of mean() and median(). The standard 'mode' solution in case of ties
# (an intermediate value) is nonsensical when dealing with discrete states.
# Here, ties are won based on which state is listed first in the input data,
# which is a pseudorandom way to 'flip a coin' to break a tie.
Mode <- function(x, na.rm = FALSE) {
  if (na.rm)
    ux <- unique(x[!is.na(x) & x != ""])
  else
    ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


## FIND CONSENSUS LIFE HABIT STATES AMONG RELATIVES (USING UNANIMITY, OR NA IF
## VARIABLE)
# rels = the data frame containing identified relatives.
# cols  = identifies which columns to check across.
# na.rm = should NAs and "" missing data be removed? Default is TRUE
consensus <- function(rels, cols, method = "constant", na.rm = TRUE) {
  method <- tolower(method) # In case capitalized
  cs <- data.frame(rels[1, cols])
  row.names(cs) <- NULL
  cs[, ] <- ""
  for (c in seq_along(cols)) {
    if (na.rm) {
      state.c <- rels[, cols[c]]
      state.c <- state.c[!is.na(state.c) & state.c != ""]
    } else
      state.c <- rels[, cols[c]]
    if (method == "constant") {
      if (length(unique(state.c)) == 1L)
        cs[c] <- state.c[1]
      else
        cs[c] <- NA
    }
    if (method == "mode")
      cs[c] <- Mode(state.c, na.rm = na.rm)
  }
  return(cs)
}


## WITH NEW CODING PHILOSOPHY, THE FOLLOWING MAY NO LONGER BE NEEDED:

## FIND THE MOST SIMILAR REFERENCE TAXON TO TYPICAL STATES, WHERE 'MOST SIMILAR'
## IS CALCULATED USING EUCLIDEAN DISTANCE TO THE MODAL STATES
# (Unclear how mobility is treated, but doesn't really matter)
# rels = the data frame containing identified relatives. Make sure that the columns
#      match those in cs.
# cols  = identifies which columns to check across.
# scale  = which column has the ecological scale? (so that has best chance of
#      finding type taxon using grep's 'adist' in case of ties)
# (Consider altering in future: if multiple best matches (i.e., ties in min),
# pick the one closest in time? or with the more similar body size? or the more
# similar shape? Size is probably the easiest to code, but requires propogating
# the body sizes first!)
best.ref <- function(rels, cols, scale = NULL) {
  if (is.null(scale))
    stop("scale not specified, with no default\n")
  scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily", 
    "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum")
  wh.min <- 1
  if (nrow(rels) > 1L) {
    csm <- consensus(rels = rels, cols = cols, method = "mode", na.rm = TRUE)
    d <- dist(rbind(csm, rels[, cols]))
    dm <- as.matrix(d)[-1, 1]
    # If multiple best matches, use text matching to identify most likely type 
    # taxon (where deletions are penalized less than insertions, both of which
    # are penalized much less than substitutions)
    if (length(dm == min(dm)) > 1) {
      higher.taxon <-
        rels[1, which(colnames(rels) == scales[which(scales == scale)])]
      wh.min <- which.min(adist(rels$Genus, higher.taxon, 
        cost = list(ins = 2, del = 1, sub = 10)))
    } else
      wh.min <- which.min(dm)
  }
  return(as.integer(row.names(rels)[wh.min]))
}


## ARE ANY STATES NA OR ABSENT, AND WHICH?
# cols  = identifies which columns to check across.
any.missing <- function(x, cols) {
  out <- any(is.na(x[, cols])) || any(x[, cols] == "")
  if (out) {
    wh <-
      cols[which(is.na(x[, cols]) | x[, cols] == "")]
  } else {
    wh = NA
  }
  return(list(any = out, which = wh))
}


## ARE ANY STATES ESTIMATED, AND WHICH?
#  est.col = which columns contain the Est_X tags, used to identify life habit
#     states to propogate.
# Note that estimates are placed in the same order as est.col, so if need a
#   particular sort order for est.col, sort PRIOR to running the code.
any.est <- function(x, cols) {
  missing <- x[, cols] == "Estimated"
  if (any(missing)) {
    wh <- cols[which(missing)]
  } else {
    wh <- NA
  }
  return(list(any = any(missing), which = wh))
}


## COMBINE RESULTS FOR any.missing() AND any.est()
# Combine the results of the checking functions into a single list
combined.any.missing <- function(missing, estimated) {
  return(list(any = any(missing$any, estimated$any), 
              which = sort(unique(missing$which, estimated$which))))
}


## IMPROVED ALL.EQUAL THAT JUST PRINTS THOSE THAT ARE CHANGED (IGNORING ROW NUMBERS)
#  NAs, blanks, and character("NA") are all treated as identical.
better.all.equal <- function(a, b) {
  if (!identical(dim(a), dim(b)))
    stop("data frames have different sizes\n")
  cn <- seq.int(ncol(a))
  row.names(a) <- row.names(b) <- NULL
  # Replace empty cells with "NA" because empty and logical(NA) is not same as
  # character("NA") (as used as a factor level), allowing false positives
  a <- replace(a, a == "", "NA")
  b <- replace(b, b == "", "NA")
  a <- replace(a, is.na(a), "NA")
  b <- replace(b, is.na(b), "NA")
  wh.diff <- sapply(cn, function(cn) !identical(a[, cn], b[, cn]))
  ab <- rbind(a, b)
  return(ab[, which(wh.diff), drop = FALSE])
}



## Examples --------------------------------------------------------------------
cols <- 21:59 # LH cols
est.cols <- 60:98 # Est_X cols
i <- which(out$Genus == "Murravia") # Brachiopod Murravia
out[i, cols]
(missings <- any.missing(out[i, ], cols))
(estimateds <- any.est(out[i,], est.cols))
combined.any.missing(missings, estimateds)
(r <- find.rels(input, i))
rels <- r$rels
(cs.c <- consensus(rels = rels, cols = cols, method = "constant"))
(cs.m <- consensus(rels = rels, cols = cols, method = "mode"))
best <- best.ref(rels=rels, cols=cols, scale=r$eco.sc)
input[best, ] # Notice the proper type taxon chosen that is in same higher taxon
rbind(out[i, cols], cs.c, cs.m, input[best, cols])
r$eco.sc

# Compare proxy methods:
proxy1 <- consensus(rels, cols=cols, method="mode")
proxy2 <- consensus(rels, cols=cols, method="constant")
rbind(proxy1, proxy2)

rel.1 <- find.rels(input, i, min.rels = 1)
rel.5 <- find.rels(input, i, min.rels = 5)
rel.10 <- find.rels(input, i, min.rels = 10)
rel.20 <- find.rels(input, i, min.rels = 20)
rel.50 <- find.rels(input, i, min.rels = 50)
rel.1$rels[ ,5:14]; rel.1$eco.sc; nrow(rel.1$rels)
rel.5$rels[ ,5:14]; rel.5$eco.sc; nrow(rel.5$rels)
rel.10$rels[ ,5:14]; rel.10$eco.sc; nrow(rel.10$rels)
rel.20$rels[ ,5:14]; rel.20$eco.sc; nrow(rel.20$rels)
rel.50$rels[ ,5:14]; rel.50$eco.sc; nrow(rel.50$rels)

rm(list = c("i", "r", "cols", "est.cols", "missings", "estimateds", "cs.c", 
            "cs.m", "best", "proxy1", "proxy2", "rels", "rel.1", "rel.5", 
            "rel.10", "rel.20", "rel.50"))



## PROPOGATE LIFE HABIT CODINGS --------------------------------------------

# Go through each record and life-habit state one by one, propogating codings as
# relevant.
index <- seq(100, nrow(input), by=100) # For keeping track
today <- format(Sys.Date(), "%m/%d/%Y")

# Confirm correct columns for life habit codings (match manually):
eco.col <- which(colnames(input) == "AboveImmediate" |
    colnames(input) == "AbovePrimary" |
    colnames(input) == "AbsFoodStratification" |
    colnames(input) == "AbsStratification" |
    colnames(input) == "AmbientFeeder" |
    colnames(input) == "Asexual" |
    colnames(input) == "Attached" |
    colnames(input) == "AttachmentFeeder" |
    colnames(input) == "Autotroph" |
    colnames(input) == "Biotic" |
    colnames(input) == "BulkFeeder" |
    colnames(input) == "Carnivore" |
    colnames(input) == "FeedingAboveImm" |
    colnames(input) == "FeedingAbovePrimary" |
    colnames(input) == "FeedingWithinImm" |
    colnames(input) == "FeedingWithinPrimary" |
    colnames(input) == "FilterDensity" |    # Only for filter-feeding echinoderms
    colnames(input) == "FilterFeeder" |
    colnames(input) == "Fluidic" |
    colnames(input) == "FreeLiving" |
    colnames(input) == "HardSubstratum" |
    colnames(input) == "Herbivore" |
    colnames(input) == "Incorporeal" |
    colnames(input) == "Insubstantial" |
    colnames(input) == "Lithic" |
    colnames(input) == "MassFeeder" |
    colnames(input) == "Microbivore" |
    colnames(input) == "Mobility" |
    colnames(input) == "ParticleFeeder" |
    colnames(input) == "RaptorFeeder" |
    colnames(input) == "RelFoodStratification" |
    colnames(input) == "RelStratification" |
    colnames(input) == "SelfSupport" |
    colnames(input) == "Sexual" |
    colnames(input) == "SoftSubstratum" |
    colnames(input) == "SolutionFeeder" |
    colnames(input) == "Supported" |
    colnames(input) == "WithinImmediate" |
    colnames(input) == "WithinPrimary"
    )

# Automate the Est_X assignments so in same order as raw columns:
ec <- seq_along(eco.col)
est.col.names <- sapply(ec, function(ec) paste0("Est_", colnames(input)[eco.col][ec]))
est.col <- match(est.col.names, colnames(input))

# Life habit columns with ordered numeric characters are treated separately:
size.col <- which(colnames(input[ ,eco.col]) == "AbsFoodStratification" | 
    colnames(input[ ,eco.col]) == "AbsStratification" | 
    colnames(input[ ,eco.col]) == "RelFoodStratification" |  
    colnames(input[ ,eco.col]) == "RelStratification")

# Confirm assignments
if (length(eco.col) != 39)
  stop("double-check the life habit column assignments")
colnames(input)[eco.col]              # AboveImmediate  < ----- >  WithinPrimary
colnames(input)[est.col]              # Est_AboveImmediate  < -- >  Est_WithinPrimary
if (length(eco.col) != length(est.col))
  stop("the raw and Est_X assignments have different lengths")
if (length(eco.col) != 39)
  stop("double-check the life habit column assignments")
colnames(input)[eco.col]              # AboveImmediate  < ----- >  WithinPrimary
colnames(input)[est.col]              # Est_AboveImmediate  < -- >  Est_WithinPrimary
if (length(eco.col) != length(est.col))
  stop("the raw and Est_X assignments have different lengths")
cbind(colnames(input[eco.col]), colnames(input[est.col])) # These should match
colnames(input)[eco.col[size.col]]    #  Four size-related stratification states
colnames(input)[-c(eco.col, est.col)] #      IDNumber   < ---- >  History_Ecology

# Which consensus method ('constant' or 'mode' to use for propogating from relatives
method <- "constant"
# method <- "mode"
interactive <- TRUE   # If want to watch updates in real time
# interactive <- FALSE
if (interactive) par("ask" = TRUE) else par("ask" = FALSE)
ncs <- 14:98          # For printing interactive data
(start.t <- Sys.time())

for(i in 1:nrow(out)) {
  if (i %in% index)
    cat("record", i, "of", nrow(out), ":", out$Genus[i], out$Species[i], "\n")
  
  # Ignore if no higher taxonomic information at all
  if (all(input[i, 2:10] == "")) next
  
  # Ignore if already coded at species, subgenus or genus level AND complete
  this.scale <- out$EcologyScale[i]
  if (combined.any.missing(any.missing(out[i, ], eco.col), 
                           any.est(out[i, ], est.col))$any == FALSE &
      (this.scale == "Species" | this.scale == "Subgenus" | 
       this.scale == "Genus")) next

  rels <- cs <- eco.sc <- char.changed <- NA
  # exemplar <- NA # Deprecated
  wh.changed <- FALSE

  # Propogate (and update, if needed) life habit codings if higher taxon
  if (this.scale > "Genus") {
    rels <- find.rels(x = out, i = i, eco.col = eco.col, start = 4, 
                      end = min(12, which(scales == this.scale)))
    # WITH NEW CODING PHILOSOPHY, THE FOLLOWING MAY NO LONGER BE NEEDED:
    # If entry is sole record for higher taxon and has at least 33% states
    # coded, include as a reasonable life habit. (In other words, it is
    # essentially coded at genus/species scale already.)
    if (this.scale < "" & nrow(rels$rels) == 0L &
        length(combined.any.missing(any.missing(out[i, ], eco.col),
                any.est(out[i, ], est.col))$which) < 0.33 * length(eco.col)) {
      rels$rels <- out[i,]
    }
    
    if (nrow(rels$rels) < 1L)
      warning(paste("no suitable relatives exist for taxon", input$Genus[i], "\n"))
    if (nrow(rels$rels) < 1L)
      next

    # WITH NEW CODING PHILOSOPHY, SOME OF THE FOLLOWING (LIKE REFERENCING A
    # 'BEST' EXEMPLAR RELATIVE) MAY NO LONGER BE NEEDED:
    
    # Propogate codings, identify exemplar relative, and update metadata (but
    # only if non-size states changed)
    cs <- consensus(rels = rels$rels, cols = eco.col, method = method, 
                    na.rm = TRUE)
    # Following lines works but deprecated for now 
    # exemplar <- best.ref(rels = rels$rels, cols = eco.col, scale = rels$eco.sc)
    # If old reference taxon is no longer a good relative, pick different
    # exemplar (if possible)
    # if (!out$RefGenusEco[i] %in% rels$rels$Genus & nrow(rels$rels) > 1L) {
    #  wh.row <- which(rels$rels$Genus == out$Genus[i])
    #  exemplar <- best.ref(rels = rels$rels[-2, ], cols = eco.col, 
    #                       scale = rels$eco.sc)
    # }

    # When proxies are consensus across multiple higher taxa, use the higher
    # taxon as the reference taxon (or the sole related species is only single
    # reference relative)
    if (nrow(rels$rels) > 1L){
      out$RefGenusEco[i] <- rels$rels[1, which(colnames(rels$rels) == rels$eco.sc)]
      out$RefSpeciesEco[i] <- "indet."
    } else {
      out$RefGenusEco[i] <- rels$rels$Genus
      out$RefSpeciesEco[i] <- rels$rels$Species
      }
    out$EcologyScale[i] <- rels$eco.sc
    
    # Confirm true states changed (i.e., ignore if the only change was a
    # size-related coding to NA)
    what.sizes.changed <-
      better.all.equal(input[i, eco.col[size.col]], cs[size.col])
    size.changed <- ncol(what.sizes.changed) > 0L
    size.changed.to.NAs <- all(is.na(what.sizes.changed[2, ]))
    # Revert to original size-related states if input size was coded at genus or
    # better (but trigger "check" to manually confirm coded correctly)
    revert <- size.changed & input$BodySizeScale[i] <= "Genus"
    if (revert)
      cs[size.col] <- input[i, eco.col[size.col]]
    others.changed <- nrow(unique(rbind(input[i, eco.col[-size.col]],
                                        cs[-size.col]))) > 1L
    num.changed <- ncol(better.all.equal(input[i, eco.col], cs))
    
    if (size.changed | others.changed) {
      out[i, eco.col] <- cs
      if (!size.changed.to.NAs | revert)
        out$SizeChanged[i] <- "Check"
      if (num.changed > 0L) {
        out$DateEntered_Ecology[i] <- today
        if (input$RefGenusEco[i] == "") {
          out$History_Ecology[i] <- ""
        } else {
          out$History_Ecology[i] <- paste0(num.changed, " states updated ",
              today, " to ", rels$eco.sc, " ", out[i, which(colnames(out) == rels$eco.sc)],
              " from ", input$RefGenusEco[i], " ", input$RefSpeciesEco[i], 
              " in same ", tolower(this.scale), ", last updated on ", 
              input$DateEntered_Ecology[i], ".")
        }
      }
    }
  }

  
  # Go through any remaining missing / unknowns / previously estimated, and
  # propogate using higher taxa, if constant across at least 5 relatives, but
  # ignoring size-related characters if size was coded at species/genus. (not
  # recorded in history because constant across the higher taxon, but tagged in
  # "Est_X" fields)
  if (input$BodySizeScale[i] <= "Genus") {
    still.missing <-
      combined.any.missing(any.missing(out[i,], eco.col[-size.col]),
                           any.est(out[i,], est.col[-size.col]))
  } else {
    still.missing <-
      combined.any.missing(any.missing(out[i,], eco.col),
                           any.est(out[i,], est.col))
  }

  if (still.missing$any) {
    higher.rels <- find.rels(x = out, i = i, eco.col = eco.col, min.rels = 5,
      start = max(2, which(scales == as.character(out$EcologyScale[i]))), end = 12)
    higher.cs <- consensus(rels = higher.rels$rels, cols = still.missing$which, 
      method = method, na.rm = TRUE)
    # Drop size columns, if 'reverted' above
    # Ignore if consensus is missing or NA: 
    l.cs <- seq_along(still.missing$which)
    wh.changed <- !sapply(l.cs, function(l.cs) is.na(higher.cs[l.cs]) | 
                    higher.cs[l.cs] == "")
    if (any(wh.changed))
      out[i, still.missing$which[wh.changed]] <- higher.cs[wh.changed]
  }

  # 'Check' whether the size-related characters were changed:
  size.changed.yet.again <-
    ncol(better.all.equal(input[i, eco.col[size.col]],
                          out[i, eco.col[size.col]])) >= 1L
  if (size.changed.yet.again)
    out$SizeChanged[i] <- "Check"
  
  # If changed, tag any changes as "Estimated" (or remove if no longer
  # estimated) and add changes to history:
  if (any(wh.changed)) {
    char.changed <-
      colnames(better.all.equal(input[i, eco.col], out[i, eco.col]))
    changed.col <- match(char.changed, colnames(input[i, eco.col]))
    out[i, est.col[changed.col]] <- "Estimated"
    dropped <-
      is.na(out[i, eco.col[changed.col]]) | out[i, eco.col[changed.col]] == ""
    if (any(dropped))
      out[i, est.col[changed.col[which(dropped)]]] <- ""
    # Note: Those entered today will be propogated at a level > genus, and so
    # there is no need to continue documenting previous history of life-habit
    # proxies. Remainder of changes (not tagged with updated date) are updating
    # empty cells using appropriate higher taxa, either for a genus/species
    # entry or for higher taxon proxy. In these cases, it is valuable to record
    # the history of estimatations.
    if (out$DateEntered_Ecology[i] != today) {
      out$History_Ecology[i] <- paste0(length(which(wh.changed)), 
                          " additional states updated ", today,
                          " based on consensus of ", higher.rels$eco.sc, " ", 
                          out[i, which(colnames(out) == higher.rels$eco.sc)], 
                          ". ", out$History_Ecology[i])
    } else {
      out$History_Ecology[i] <- paste0(length(which(wh.changed)), 
                          " additional states updated ", today,
                          " based on consensus of ", higher.rels$eco.sc, " ", 
                          out[i, which(colnames(out) == higher.rels$eco.sc)], ".")
    }
  }
  
  # Interactive mode (to observe how states are being propogated)
  if (interactive) {
    same <- identical(input[i, ], out[i, ])
    if (!same) {
      plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
           ylab = "")
      text(1, 1, as.character(paste(out$Genus[i], out$Species[i])), cex = 2)
      cat(as.character(paste(out$Genus[i], out$Species[i])), "\n")
      print(better.all.equal(input[i, ], out[i, ]))
      cat("\n")
    }
  }
  
}
(Sys.time() - start.t)
library(beepr)
beep(3)

# Note that if the only change is "SizeChanged", that means the size codings
# (AbsStrat, RelStrat, etc.) were reverted from the consensus state "back" to
# the state input into the database based on PropogateSizes.R updates for
# entries with size values at species/genus level.

warnings()
# Warnings about "no suitable relatives" typically means the EcoScale in
# "PreLH.tab" is incorrect. Check and re-run, if so. (Otherwise it means there
# are literally no other relatives, not even other members of their phylum.)

round(table(input$EcologyScale) * 100 / nrow(input), 1)
round(table(out$EcologyScale) * 100 / nrow(out), 1)
table(input$EcologyScale)
table(out$EcologyScale)

## Any duplicated ID numbers?
# Will match the incorrect life habits when importing
if (any(table(input$IDNumber) > 1)) {
  print(which(table(input$IDNumber) > 1))
  stop("There are duplicate IDNumbers. Fix before proceeding! See above for list.")
}

## EXPORT DATA -------------------------------------------------------------
write.table(out, file="PostLH_constant.tab", quote=FALSE, sep="\t", row.names=FALSE)
# write.table(out, file="PostLH_mode.tab", quote=FALSE, sep="\t", row.names=FALSE)
# write.table(out, file="PostLH_withPBDB_mode.tab", quote=FALSE, sep="\t", row.names=FALSE)
# write.table(out, file="PostLH_withPBDB_constant.tab", quote=FALSE, sep="\t", row.names=FALSE)

# (1) Open in Excel to confirm looks acceptable. Replace (matching entire cell
# contents) "NA"s in life habit data.

# (2) Open in Word to remove quotation marks around the text entries, (replacing
# "^t with ^t and ^t" with ^t and "" with ^t and ").

# (3) Open FileMakerPro and import, updating records by matching names and using
# the IDNumber as the matching identifier. (Fine to not import the taxonomic
# names.)

# (4) Refer to PropogateSizes.R for common troubleshooting corrections to run
# through in case of manual overrides.

# On the post-life-habit propogation, focus on the SizeChanged=Check tagged
# entries, but also check ALL entries using the following criteria. (Can omit
# those coded at EcoScale=Species/Genus.) MAKE SURE THAT IF ADD/CHANGE A STATE
# FOR A EcoScale=SPECIES/GENUS, to tag as "Estimated."


