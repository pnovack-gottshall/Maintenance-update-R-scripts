## PROPOGATE LIFE HABIT CODINGS ACROSS ENTRIES, USING RELATIVES AS PROXIES



## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##

## 1. The count of number of states seems to be off by 1 or 2 (primarily when
## converting blanks to values?). Check how counted.
##
## 2. When a blank life habit is propogated, the paste() doesn't work properly.
## Add switch for when no pre-existing RefGenusEco?
##
## 3. When propogating higher taxa (e.g., Xanthoidea indet. for Carcinoplax),
## should the EcoRef be the consensus best reference (e.g., Dromia sp.), or
## switch to "Xanthoidea indet."? As currently done, not really copying Dromia,
## but copying the consensus across that taxon's relatives instead.
##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##

## FUTURE FEATURE REQUEST? Add AbsStratDist here instead, so that propogated
## based not only on size, but also on life habit coding? Would require
## exporting canonical body size lengths!








## BASIC LOGIC -------------------------------------------------------------

# "Relative' means the smallest inclusive taxonomic group the entry is related
# to whose life habit has been coded.

# 1. If an entry is already coded at species-, subgenus-, or genus-level, skip;
# unless life habits are partly blank (uncoded), in which case use closest
# relatives to populate remaining codings.

# 2. If an entry is above genus-level, use closest relatives to update
# (override) the pre-existing codings. If unchanged (identical to relative),
# abort and go to the next entry, leaving the 'DateEntered_Ecology' and
# 'EcoScale' fields unchanged. If multiple relatives exist, enter the value if
# constant or 'NA' if varies. (Use 'grep''s 'adist' to approximate type taxon
# for reference when multiple relatives exist.) If any states remain uncoded, go
# to higher taxa in case they have codings for these states, using the same
# logic.

# If any life habit codings are changed, add record-keeping text to
# History_Ecology field. This documents a history of life habit changes that can
# be restored using earlier back-up database copies. Add this new tag IN FRONT
# OF any prior tag via 'paste' to maintain a complete history. If there is no
# date is in the 'DateEntered_Ecology' date field (i.e., this is the first
# update), just update the date (without an update to the history text).

# A benefit of this update-higher-taxon approach is that as we get more data, we
# can feel more confident in codings when there are many relatives (because it
# is still a strict consensus). If a higher taxon has lots of inherent
# variability, the NAs get up-voted based on actual variability across the
# higher taxon. So it's OK to "downgrade" the data (add more NAs) as we get more
# data, because that reflects reality! (Recall that entries coded at genus and
# species-level will not be overriden.)



## IMPORT DATA -------------------------------------------------------------

# Make sure the stratifications are updated from the size propogation BEFORE
# running this algorithm! (The algorithm reverts to original stratifications
# when size was coded as genus or species level, but still adds a size "Check"
# to trigger a second look-over afterwards.)

# (1) Before exporting, sort the entries so that EcoScale=Species is first,
# followed by Subgenus, Genus, etc. That way those with best scales are checked
# for completeness first.

# (2) Export following data fields as 'PreLH.tab' tab-delimited format. Open and
# add headers (easiest to do saving crap file in Excel format). DO NOT SAVE AS
# EXCEL FORMAT, AS DOING SO IDIOSYNCRATICALLY CHANGES NAs TO 0s AND 1s!! (MUST
# be tab-delimited, as several text fields contain commas!!!) Best to then open
# in MSWord and delete all quotation marks (replacing "^t with ^t, ^t" with ^t,
# and "" with ").

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



## FUNCTIONS ---------------------------------------

## FUNCTION TO FIND RELATIVES CODED AT BEST AVAILABLE RESOLUTION (BUT INCLUDING 
## GENERA, SUBGENERA, AND SPECIES WHEN POSSIBLE)
# x = data frame of all data
# i = index (row number for taxon entry being considered)
# start = taxonomic level to start (default = subfamily, with 1=species and 14=NA)
# end = taxonomic level to end (default = NA, running higherup through all levels, 
#     including unknowns)
find.rels <- function(x, i, min.rels = 1, start = 4, end = 12, ref.g.col = 15, 
  ref.sp.col = 16, eco.col = 21:58) {
  scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily", 
    "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum")
  scales <- factor(scales, levels = scales, ordered = TRUE)
  others <- x[-i, ] # Entry cannot be its own relative
  low.res <- NA
  for (e in start:end) {
    # Identify correct column for taxonomic scale being considered
    sc.col <- which(colnames(others) == scales[e])
    # Ignore if unassigned taxa
    if (x[i, sc.col] == "") next
    if (x[i, sc.col] == "UNCERTAIN") next
    # Identify taxonomic relatives (removing duplicates)
    rels <- others[which(others[, sc.col] == x[i, sc.col]), ]
    rels <- rels[which(rels$EcologyScale != ""), ]
    u.rels <- unique(rels[, c(ref.g.col, ref.sp.col, eco.col)])
    if (nrow(u.rels) < min.rels) next
    # Identify those relatives coded at best resolution
    low.res <- min(rels$EcologyScale)
    # Discard if equal to or higher than entry being considered (i.e., entry is 
    # their best proxy, or equally as well known), or if not including all 
    # potential relatives [i.e., ending too early for specified scale]
    if (low.res < x$EcologyScale[i] &
        as.character(low.res) <= scales[e]) break
  }
  # Also include genera and subgenera (in addition to species coding, if present)
  rels <- rels[which(rels$EcologyScale == as.character(low.res) |
      rels$EcologyScale == "Species" |
      rels$EcologyScale == "Subgenus" |
      rels$EcologyScale == "Genus"), ]
  u.rels <- unique(rels[, c(ref.g.col, ref.sp.col, eco.col)])
  rels <- x[as.integer(row.names(u.rels)), ]
  eco.sc <- as.character(scales[e])
  return(list(rels = rels, eco.sc = eco.sc))
}


## CALCULATE THE MODE AMONG OBSERVED CHARACTER STATES
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


## FIND CONSENSUS LIFE HABIT STATES AMONG RELATIVES (USING UNANIMITY, OR NA IF
## VARIABLE)
# rels = the data frame containing identified relatives.
# cols  = identifies which columns to check across.
consensus <- function(rels, cols, method = "constant") {
  cs <- data.frame(rels[1, cols])
  row.names(cs) <- NULL
  cs[, ] <- ""
  for (c in seq_len(length(cols))) {
    if (method == "constant") {
      if (length(unique(rels[, cols[c]])) == 1L)
        cs[c] <- rels[, cols[c]][1]
      else
        cs[c] <- NA
    }
    if (method == "mode")
      cs[c] <- Mode(rels[, cols[c]])
  }
  return(cs)
}


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
    csm <- consensus(rels = rels, cols = cols, method = "mode")
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
#  est.cols = which columns contain the Est_X tags, used to identify life habit
#     states to propogate.
# Note that estimates are placed in the same order as est.cols, so if need a
#   particular sort order for est.cols, sort PRIOR to running the code.
any.est <- function(x, est.cols) {
  missing <- x[, est.cols] == "Estimated"
  if (any(missing)) {
    wh <- est.cols[which(missing)]
  } else {
    wh <- NA
  }
  return(list(any = any(missing), which = wh))
}


## IMPROVED ALL.EQUAL THAT JUST PRINTS THOSE THAT ARE CHANGED (IGNORING ROW NUMBERS)
better.all.equal <- function(a, b) {
  if (!identical(dim(a), dim(b)))
    stop("data frames have different sizes\n")
  cn <- seq.int(ncol(a))
  row.names(a) <- row.names(b) <- NULL
  wh.diff <- sapply(cn, function(cn) !identical(a[, cn], b[, cn]))
  ab <- rbind(a, b)
  return(ab[, which(wh.diff), drop = FALSE])
}



## Examples --------------------------------------------------------------------
cols <- 21:59 # LH cols
est.cols <- 60:98 # Est_X cols
i <- which(out$Genus == "Menippe") # Crap Menippe
out[i, cols]
(r <- find.rels(input, i))
rels <- r$rels
(cs <- consensus(rels = rels, cols = cols, method = "constant"))
any.missing(cs, seq.int(ncol(cs)))
any.est(cs, seq.int(ncol(cs)))
best <- best.ref(rels=rels, cols=cols, scale=r$eco.sc)
input[best, ] # Notice the proper type taxon chosen that is in same higher taxon
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

rm(list=c("i", "r", "cols", "cs", "best", "proxy1", "proxy2", "rel.1", 
  "rel.5", "rel.10", "rel.20"))





## PROPOGATE LIFE HABIT CODINGS --------------------------------------------

# Go through each record and life-habit state one by one, propogating codings as
# relevant
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
    colnames(input) == "WithinPrimary")

# Automate the Est_X assignments so in same order as raw columns:
ec <- seq.int(length(eco.col))
est.col.names <- sapply(ec, function(ec) paste0("Est_", colnames(input)[eco.col][ec]))
est.col <- match(est.col.names, colnames(input))

# Life habit columns with ordered numeric characters are treated separately:
size.col <- which(colnames(input[ ,eco.col]) == "AbsFoodStratification" | 
    colnames(input[ ,eco.col]) == "AbsStratification" | 
    colnames(input[ ,eco.col]) == "RelFoodStratification" |  
    colnames(input[ ,eco.col]) == "RelStratification")

# Confirm assignments
if (length(eco.col) != 39)
  stop("double-check the life habit column assignments!")
colnames(input)[eco.col]              # AboveImmediate  < ----- >  WithinPrimary
colnames(input)[est.col]              # Est_AboveImmediate  < -- >  Est_WithinPrimary
if (length(eco.col) != length(est.col))
  stop("the raw and Est_X assignments have different lengths!")
if (length(eco.col) != 39)
  stop("double-check the life habit column assignments!")
colnames(input)[eco.col]              # AboveImmediate  < ----- >  WithinPrimary
colnames(input)[est.col]              # Est_AboveImmediate  < -- >  Est_WithinPrimary
if (length(eco.col) != length(est.col))
  stop("the raw and Est_X assignments have different lengths!")
cbind(colnames(input[eco.col]), colnames(input[est.col])) # These should match
colnames(input)[eco.col[size.col]]    #  Four size-related stratification states
colnames(input)[-c(eco.col, est.col)] #      IDNumber   < ---- >  History_Ecology

# Which consensus method ('constant' or 'mode' to use for propogating from relatives
method <- "constant"
# method <- "mode"
interactive <- TRUE   # If want to watch updates in real time
# interactive <- FALSE
if (interactive) par("ask" = TRUE) else par("ask" = FALSE)
ncs <- 14:98 # For printing interactive data
(start.t <- Sys.time())

for(i in 1:nrow(out)) {
  if(i %in% index) cat("record", i, "of", nrow(out), ":", out$Genus[i], 
    out$Species[i], "\n")

  # Ignore if no higher taxonomic information at all
  if(all(input[i, 2:10] == "")) next
  
  # Ignore if already coded at species, subgenus or genus level AND complete
  this.scale <- out$EcologyScale[i]
  if(any.missing(out[i, ], eco.col)$any == FALSE & (this.scale == "Species" |
      this.scale == "Subgenus" | this.scale == "Genus")) next
  rels <- cs <- eco.sc <- exemplar <- NA
  # rels <- data.frame(out[1, ])
  # rels[ , ] <- ""
  
  # Propogate (and update, if needed) life habit codings if higher taxon
  if(this.scale > "Genus") {
    rels <- find.rels(x=out, i=i, start=4, end=min(12, which(scales == this.scale)))
    # If entry is sole record for higher taxon and has at least 33% states
    # coded, include as a reasonable life habit
    if(this.scale < "" & nrow(rels$rels) == 0L & length(any.missing(out[i, ], 
      eco.col)$which) < 0.33 * length(eco.col)) {
      rels$rels <- out[i, ]
      }
    
    if(nrow(rels$rels) < 1L) warning(paste("no relatives for", i, "\n"))
    if(nrow(rels$rels) < 1L) next

    # Propogate codings, identify exemplar relative, and update metadata (but
    # only if non-size states changed)
    cs <- consensus(rels=rels$rels, cols=eco.col, method=method)
    exemplar <- best.ref(rels=rels$rels, cols=eco.col, scale=rels$eco.sc)
    # If old reference taxon is no longer a good relative, pick different
    # exemplar (if possible)
    if(!out$RefGenusEco[i] %in% rels$rels$Genus & nrow(rels$rels) > 1) {
      wh.row <- which(rels$rels$Genus == out$Genus[i])
      exemplar <- best.ref(rels=rels$rels[-2, ], cols=eco.col, 
        scale=rels$eco.sc)
      }
    out$RefGenusEco[i] <- out$RefGenusEco[exemplar]
    out$RefSpeciesEco[i] <- out$RefSpeciesEco[exemplar]
    out$EcologyScale[i] <- rels$eco.sc
    
    # Confirm true states changed (i.e., ignore if only change was a
    # size-related coding to NA)
    what.sizes.changed <- better.all.equal(input[i,eco.col[size.col]], 
      cs[size.col])
    size.changed <- ncol(what.sizes.changed) > 0L
    size.changed.to.NAs <- all(is.na(what.sizes.changed[2, ]))
    # Revert to original size-related states if size was coded at genus or
    # better (but trigger "check" to confirm correct)
    revert <- size.changed & input$BodySizeScale[i] <= "Genus"
    if(revert) cs[size.col] <- input[i,eco.col[size.col]]
    others.changed <- nrow(unique(rbind(input[i,eco.col[-size.col]], 
      cs[-size.col]))) > 1L
    num.changed <- ncol(better.all.equal(input[i,eco.col], cs))
    
    if(size.changed | others.changed) {
      out[i,eco.col] <- cs
      if(!size.changed.to.NAs | revert) out$SizeChanged[i] <- "Check"
      if(num.changed > 0L) out$DateEntered_Ecology[i] <- today
      if(num.changed > 0L & out$DateEntered_Ecology[i] != "" & 
          (any(!is.na(input$History_Ecology[i]), 
            input$History_Ecology[i] != "")==FALSE)) {
        out$History_Ecology[i] <- paste(num.changed, " states updated ", today, 
          " to ", rels$eco.sc, " ", out[i, which(colnames(out) == rels$eco.sc)], 
          " from ", input$RefGenusEco[i], " ", input$RefSpeciesEco[i], " in same ", 
          tolower(this.scale),", last updated on ", input$DateEntered_Ecology[i], 
          ". ", input$History_Ecology[i], sep="")
      }
      if(num.changed > 0L & out$DateEntered_Ecology[i] != "" & 
          (is.na(input$History_Ecology[i]) | input$History_Ecology[i] == "")) {
        out$History_Ecology[i] <- paste(num.changed, " states updated ", today, 
          " to ", rels$eco.sc, " ", out[i, which(colnames(out) == rels$eco.sc)], 
          " from ", input$RefGenusEco[i], " ", input$RefSpeciesEco[i], " in same ", 
          tolower(this.scale), ", last updated on ", input$DateEntered_Ecology[i], 
          ".", sep="")
      }
    }
  }

  # Go through any remaining missing / unknowns, and propogate using higher taxa,
  # if constant (not recorded in history because constant across this higher taxon)
  still.missing <- any.missing(out[i, ], cols=eco.col)
  if(still.missing$any) {
    higher.rels <- find.rels(x=out, i=i, 
      start=which(scales==as.character(out$EcologyScale[i])), end=12, min.rels=5)
    higher.cs <- consensus(rels=higher.rels$rels, cols=still.missing$which, 
      method=method)
    if(!all(is.na(higher.cs))) out[i,still.missing$which] <- higher.cs
  }
  size.changed.yet.again <- ncol(better.all.equal(input[i,eco.col[size.col]], 
    out[i,eco.col[size.col]])) > 1L
  if(size.changed.yet.again) out$SizeChanged[i] <- "Check"
  
  # Interactive mode (to observe how states are being propogated)
  if(interactive) {
    same <- identical(input[i, ], out[i, ])
    if(!same) {
      plot(1, 1, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
      text(1, 1, as.character(paste(out$Genus[i], out$Species[i])), cex=2)
      cat(as.character(paste(out$Genus[i], out$Species[i])), "\n")
      print(better.all.equal(input[i, ], out[i, ])) 
      cat("\n")
    }
  }
  
}
(Sys.time() - start.t)    


round(table(input$EcologyScale) * 100 / nrow(input), 1)
round(table(out$EcologyScale) * 100 / nrow(out), 1)
table(input$EcologyScale)
table(out$EcologyScale)

  
## EXPORT DATA -------------------------------------------------------------
# write.table(out, file="PostLH_constant.tab", quote=FALSE, sep="\t", row.names=FALSE)
# write.table(out, file="PostLH_mode.tab", quote=FALSE, sep="\t", row.names=FALSE)
# write.table(out, file="PostLH_withPBDB_mode.tab", quote=FALSE, sep="\t", row.names=FALSE)
write.table(out, file="PostLH_withPBDB_constant.tab", quote=FALSE, sep="\t", row.names=FALSE)

# Open in Excel to confirm looks acceptable. Replace (matching entire cell 
# contents) "NA"s in life habit data.

# Next open in Word to remove quotation marks around the text entries,
# (replacing "^t with ^t and ^t" with ^t and "" with ^t and ").

# Open FileMakerPro and import, updating records by matching names and using the
# IDNumber as the matching identifier. (Fine to not import the taxonomic names
# and geological ranges.)

# Refer to PropogateSizes.R for common troubleshooting corrections to override.

  
  
## DIAGNOSTIC TESTING ------------------------------------------------------ 
  
# Good entries (i) to use for testing: 1458 (Adelphobolbina, nothing should 
# change), 1664 (Bembexia, scale should improve), 1908 (Echinarachnius, codings 
# should fill in), 2518 (Caprina, some should fill in), 2560 (Atreta, consensus
# has missing values)
identical(input[i, ], out[i, ])
better.all.equal(input[i, ], out[i, ])
