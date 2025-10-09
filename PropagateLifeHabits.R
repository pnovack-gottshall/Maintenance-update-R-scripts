## PROPAGATE LIFE HABIT CODINGS ACROSS ENTRIES, USING RELATIVES AS PROXIES

## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
##
## !!!NONE!!!
##
##  triple check that the new (June 2024) switch in lines 590-593 works as 
##  intended
##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ## ERRORS TO FIX! ##
##
## FUTURE FEATURE REQUEST? Add AbsStratDist here instead, so that propagated
## based not only on size, but also on life habit coding? Would require
## exporting canonical body size lengths!

## BASIC LOGIC -------------------------------------------------------------

# "Relative' means the smallest inclusive taxonomic group the entry is related
# to whose life habit has been coded.

# 1. If an entry is already coded at species-, subgenus-, or genus-level, skip,
#    unless life habits are partly blank (uncoded), in which case use closest
#    relatives to estimate remaining states. (In these cases, tag the states as
#    "Estimated" and record the changes in the History_Ecology field.)

# 2. If an entry is above genus-level, use closest relatives to update
#    (override) the pre-existing codings. If unchanged (identical to relative),
#    abort and go to the next entry, leaving the 'DateEntered_Ecology' and
#    'EcoScale' fields unchanged. If multiple relatives exist, enter the value,
#    using the specified propagation method. In the 'constant' method, propagate
#    any state that is invariant (i.e., shared) among ALL relatives, and 'NA' if
#    the state varies. In the 'mode' method, propagate states that are most
#    frequently occurring for each state. In both cases, record the relative as
#    "Higher taxon indet." If any states remain uncoded, go to the next 
#    inclusive higher taxa in case they have codings for these states, using the 
#    same logic as step 1.

#    The four body-size-related states (AbsStrat, RelStrat, AbsFoodStrat, and
#    RelFoodStrat) are only over-written when the states are missing or when
#    EcoScale > Species/Genus. If any of these four states are changed AND the
#    BodySizeScale was Species/Genus (implying they were originally checked 
#    after the PropogateSizes.R algorithm), add a "check" tag to force a manual 
#    re-check. If any life habit codings are changed, add (or override) the 
#    record-keeping text to History_Ecology field. This documents a history of 
#    life-habit changes that can be restored using earlier back-up database 
#    copies. If there is no date in the 'DateEntered_Ecology' date field (i.e., 
#    this is the first update), just update the date (without an update to the 
#    history text). If secondarily filled-in states are changed (that are 
#    different than the chosen relative), record those changes to the History 
#    field.

#    A benefit of this update-higher-taxon approach is that as we get more data, 
#    we can feel more confident in codings when there are many relatives 
#    (because it is still a strict consensus). If a higher taxon has lots of 
#    inherent variability, the NAs get up-voted based on actual variability 
#    across the higher taxon. So it's OK to "downgrade" the data (add more NAs) 
#    as we get more data, because that reflects the reality of biological 
#    variability. (Recall that individual states coded at genus and 
#    species-level will not be overriden during this algorithm, only the unknown 
#    states.)



## IMPORT DATA -------------------------------------------------------------

# Make sure the stratifications are updated from the size propagation
# (PropagateSizes.R) BEFORE running this algorithm! (The algorithm reverts to
# original stratifications when size was coded at genus or species level, but
# still adds a size "Check" to trigger a second look-over afterwards.)

# If updating both previously propagated "Mode" and "Constant" datasets, then
# each should be run separately, as the reference population for each algorithm
# will have different life-habit states for varying taxa.

# (0) Not recommended (because the code algorithmically updates and gives
#     priority to species/genera with complete codings), but if want a "fresh"
#     propagation, consider first deleting all life-habit codings for any 
#     estimates (EstLithic, EstBiotic, or all codings for 
#     non-species/subgenus/genus). (This is recommended if, for example, a 
#     species/subgenus/genus A entry has at least one estimated life-habit 
#     state, and it might be used to estimate a missing state for species B, 
#     which was originally used to estimate the missing state for A.) IF DO 
#     THIS, PERFORM THE DELETIONS ON A COPY OF THE DATABASE RATHER THAN THE 
#     MASTER DATABASE ITSELF! If do this, it is most efficient to export the 
#     "constant" database entries, which have fewer estimated states (i.e., 
#     there will be less to delete). If go this route, note that the History_Eco
#     output will be slightly off, because will imply there were prior 
#     estimates. (It is easiest to clean the data set by adding a row number 
#     column so you can sort as needed, then return to the original row order.)

# (1) Before exporting, sort the entries so that EcoScale = Species is first,
#     followed by Subgenus, Genus, etc. That way those with best scales are 
#     checked for completeness first.

# (2) Run relevant code in SelectCols.R for PropogateLifeHabits.R to obtain 
#     following output. Then continue with step 3.

#     IDNumber

#     Taxonomy: Phylum, Subphylum, Class, Subclass, Order, Suborder, 
#     Superfamily, Family, Subfamily, Genus, Subgenus, Species

#     Proxy fields: EcologyScale, RefGenusEco, RefSpeciesEco, 
#     DateEntered_Ecology, SizeChanged, BodySizeScale, History_Ecology

#     Life habit characters (can really be in any order, as called by name):
#     AboveImmediate, AbovePrimary, AbsFoodStratification, AbsStratification,
#     AmbientFeeder, Asexual, Attached, AttachmentFeeder, Autotroph, Biotic,
#     BulkFeeder, Carnivore, FeedingAboveImm, FeedingAbovePrimary, 
#     FeedingWithinImm, FeedingWithinPrimary, [FilterDensity field (only used 
#     with filter-feeding echinoderms)], FilterFeeder, Fluidic, FreeLiving, 
#     HardSubstratum, Herbivore, Incorporeal, Insubstantial, Lithic, MassFeeder, 
#     Microbivore, Mobility, ParticleFeeder, RaptorFeeder, 
#     RelFoodStratification, RelStratification, SelfSupport, Sexual, 
#     SoftSubstratum, SolutionFeeder, Supported, WithinImmediate, WithinPrimary

#     The same characters, in Est_X form (excluding Est_AbsStratDist, Est_AP,
#     Est_DV, and EstT, which were propagated in PropagateSizes.R).

# (3) Open in Excel and delete NAs for columns (typically SizeChanged, 
#     Est_FilterDensity and other Est_Xs) when they are ALL NAs. (You don't 
#     need to do this for all NAs, only those where the entire column is NAs.)

# (4) Open in MSWord and delete all quotation marks (replacing "^t with ^t, ^t"
#     with ^t, and "" with ").



rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Documents/GSA (& NAPC)/2024NAPC/Higher taxa eco diversity")
input <- read.delim(file = "PreLH_constant_Ostracodes.tab", colClasses = "character")
# input <- read.delim(file = "PreLH_Isabel.tab", colClasses = "character")
# input <- read.delim(file = "PreLH_mode_Ostracodes.tab", colClasses = "character")
# input <- read.delim(file = "PreLH_constant_Bradoriida&Aster&Echino.tab", colClasses = "character")
# input <- read.delim(file = "preLH_mode_Bradoriida&Aster&Echino.tab", colClasses = "character")
# input <- read.delim(file = "preLH_constant.tab", colClasses = "character")
# input <- read.delim(file = "preLH_mode.tab", colClasses = "character")
# input <- read.delim(file = "preLH_mode_PBDB.tab", colClasses = "character")
# input <- read.delim(file = "preLH_constant_PBDB.tab", colClasses = "character")
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
# start = taxonomic level to start (default = subfamily, with 1 = species and 
#   14 = NA). Note should never be '1' or else 'relatives' will include 
#   unrelated taxa sharing the same species epithet
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
  rels <- array(NA, dim = 0)
  for (e in start:end) {
    nr <- 0
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
# method = which consensus method to use? 'constant' (detault) returns a value
#           only if unanimous (and NA otherwise). 'mode' returns the most 
#           frequent value.
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
#     states to propagate.
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
# Coral Stylophyllum, brachiopods Dallithyris and Murravia, asteroid Schuchertia
i <- which(out$Genus == "Schuchertia")[1] 
out[i, cols]
(missings <- any.missing(out[i, ], cols))
(estimateds <- any.est(out[i,], est.cols))
combined.any.missing(missings, estimateds)
(r <- find.rels(input, i))
rels <- r$rels
(cs.c <- consensus(rels = rels, cols = cols, method = "constant"))
(cs.m <- consensus(rels = rels, cols = cols, method = "mode"))
rbind(out[i, cols], cs.c, cs.m)
r$eco.sc

# Compare proxy methods:
proxy1 <- consensus(rels, cols = cols, method = "mode")
proxy2 <- consensus(rels, cols = cols, method = "constant")
rbind(proxy1, proxy2)

rel.1 <- find.rels(input, i, min.rels = 1)
rel.5 <- find.rels(input, i, min.rels = 5)
rel.10 <- find.rels(input, i, min.rels = 10)
rel.20 <- find.rels(input, i, min.rels = 20)
rel.50 <- find.rels(input, i, min.rels = 50)
rel.1$rels[ ,4:14]; rel.1$eco.sc; nrow(rel.1$rels)
rel.5$rels[ ,4:14]; rel.5$eco.sc; nrow(rel.5$rels)
rel.10$rels[ ,4:14]; rel.10$eco.sc; nrow(rel.10$rels)
rel.20$rels[ ,4:14]; rel.20$eco.sc; nrow(rel.20$rels)
rel.50$rels[ ,4:14]; rel.50$eco.sc; nrow(rel.50$rels)

rm(list = c("i", "r", "cols", "est.cols", "missings", "estimateds", "cs.c", 
            "cs.m", "proxy1", "proxy2", "rels", "rel.1", "rel.5", "rel.10", 
            "rel.20", "rel.50"))






## PROPAGATE LIFE HABIT CODINGS --------------------------------------------

# Go through each record and life-habit state one by one, propagating codings as
# relevant.
index <- seq(0, nrow(input), by = 100) # For keeping track
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
size.col <- which(colnames(input[, eco.col]) == "AbsFoodStratification" | 
    colnames(input[, eco.col]) == "AbsStratification" | 
    colnames(input[, eco.col]) == "RelFoodStratification" |  
    colnames(input[, eco.col]) == "RelStratification")

# Confirm assignments
if (length(eco.col) != 39)
  stop("double-check the life habit column assignments")
colnames(input)[eco.col]              #  AboveImmediate   < ----- >  WithinPrimary
colnames(input)[est.col]              #  Est_AboveImmediate  < -- >  Est_WithinPrimary
if (length(eco.col) != length(est.col))
  stop("the raw and Est_X assignments have different lengths")
if (length(eco.col) != 39)
  stop("double-check the life habit column assignments")
if (length(eco.col) != length(est.col))
  stop("the raw and Est_X assignments have different lengths")
cbind(colnames(input[eco.col]), colnames(input[est.col])) # These should match
colnames(input)[eco.col[size.col]]    #  Four size-related stratification states
colnames(input)[-c(eco.col, est.col)] #  IDNumber   < ---- >  History_Ecology

# Which consensus method ('constant' or 'mode' to use for propagating from relatives
method <- "constant"
# method <- "mode"
interactive <- TRUE   # If want to watch updates in real time
# interactive <- FALSE
if (interactive) par("ask" = TRUE) else par("ask" = FALSE)
ncs <- 14:98          # For printing interactive data
(start.t <- Sys.time())

for(i in 1:nrow(out)) {
# for(i in 4665:nrow(out)) {

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
  higher.rels <- list(rels = NULL, eco.sc = "Species")
  wh.changed <- FALSE

  # Propogate (and update, if needed) life habit codings, if higher taxon
  if (this.scale > "Genus") {
    
    rels <- find.rels(x = input, i = i, eco.col = eco.col, start = 4, 
                      end = min(12, which(scales == this.scale)))

    # Before rejecting for having no relatives, consider whether the
    # 'EcologyScale' was incorrect (i.e., perhaps the higher taxonomic
    # assignments have changed) and there actually are other relatives (coded at
    # species, subgenus, and genus-scale) available to use
    if (nrow(rels$rels) < 1L)
      rels <- find.rels(x = input, i = i, eco.col = eco.col, start = 4, end = 12)
    
    # But reject (with a warning) if there truly are no available relatives
    nr <- nrow(rels$rels)
    if (nr < 1L)
      warning(paste("no suitable relatives exist for taxon", input$Genus[i], "\n"))
    if (nr < 1L)
      next

    # Propogate codings and update metadata (but only if non-size states
    # changed)
    cs <- consensus(rels = rels$rels, cols = eco.col, method = method, 
                    na.rm = TRUE)

    # When proxies are consensus across multiple higher taxa, use the higher
    # taxon as the reference taxon (or the sole related species if there is only
    # one reference relative)
    if (nr > 1L){
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
              today, " to ", tolower(rels$eco.sc), " ", 
              out[i, which(colnames(out) == rels$eco.sc)], " from ", 
              input$RefGenusEco[i], " ", input$RefSpeciesEco[i], " in same ", 
              tolower(this.scale), ", last updated on ", 
              input$DateEntered_Ecology[i], ".")
        }
      }
    }
  }

  
  # Go through any remaining missing / unknowns / previously estimated, and
  # propagate using higher taxa where at least 5 coded relatives exist, but
  # ignoring size-related characters if size was coded at species/genus. (Not
  # recorded in history because constant across the higher taxon, but tagged in
  # "Est_X" fields)

  # Note these "higher" propagations work recursively, working up higher ranks
  # to fill in as many un-coded states as possible. (E.g., it may not be known
  # whether any Order Agnostida reproduced sexually or asexually, but all extant
  # Phylum Arthropoda reproduce sexually.)
  
  repeat {

    if (input$BodySizeScale[i] <= "Genus") {
      still.missing <-
        combined.any.missing(any.missing(out[i, ], eco.col[-size.col]),
                             any.est(out[i, ], est.col[-size.col]))
    } else {
      still.missing <-
        combined.any.missing(any.missing(out[i, ], eco.col),
                             any.est(out[i, ], est.col))
    }
    
    # Add a 'break' to end 'repeat' loop when no more missing states, or have
    # exhausted all possible 'higher' relatives:
    if (!still.missing$any | higher.rels$eco.sc == "Phylum")
      break

    # Remove FilterDensity (column 37) from propagation if definitively known to
    # NOT be a filter feeder (but allow to be propagated if unknown whether
    # might be)
    if (37 %in% still.missing$which & out$FilterFeeder[i] == "0" & 
        !is.na(out$FilterFeeder[i])) {
      still.missing$which <-
        still.missing$which[-which(still.missing$which == "37")]
      if (length(still.missing$which) < 1L)
        still.missing$any <- FALSE
    }
    
    # If FilterDensity was the only missing state (and not relevant from prior
    # check), then 'break'
    if (!still.missing$any)
      break
      
    # Identify consensus states among higher taxa:
    if (still.missing$any) {
      start.scale <- max(which(scales == as.character(out$EcologyScale[i])), 
                         (which(scales == as.character(higher.rels$eco.sc))) + 1)
      higher.rels <- find.rels(x = input, i = i, eco.col = eco.col, min.rels = 5, 
                               start = start.scale, end = 12)
      # Only proceed if there are relatives to work with (maintaining prior
      # scale states, which will be unchanged from prior scale, so essentially
      # ignored):
      if (nrow(higher.rels$rels) > 0L) {
        higher.cs <- consensus(rels = higher.rels$rels, cols = still.missing$which, 
                               method = method, na.rm = TRUE)
      }

      # Ignore if the consensus is missing or NA: 
      l.cs <- seq_along(still.missing$which)
      wh.changed <- !sapply(l.cs, function(l.cs) is.na(higher.cs[l.cs]) | 
                      higher.cs[l.cs] == "")
      
      # Fill in consensus states
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

      # Note: Those "entered" today will be propagated at a level > genus, and
      # so there is no need to continue documenting previous history of
      # life-habit proxies. Remainder of changes (not tagged with updated date)
      # are updating empty cells using appropriate higher taxa, either for a
      # genus/species entry or for higher taxon proxy. In these cases, it is
      # valuable to record the history of estimations.
      if (out$DateEntered_Ecology[i] != today) {
        out$History_Ecology[i] <- paste0(length(which(wh.changed)), 
                            " additional states updated ", today,
                            " based on consensus of ", 
                            tolower(higher.rels$eco.sc), " ", 
                            out[i, which(colnames(out) == higher.rels$eco.sc)], 
                            ". ", out$History_Ecology[i])
      } else {
        out$History_Ecology[i] <- paste0(length(which(wh.changed)), 
                            " additional states updated ", today,
                            " based on consensus of ", 
                            tolower(higher.rels$eco.sc), " ", 
                            out[i, which(colnames(out) == higher.rels$eco.sc)], ".")
      }
    }
    
  }
  
  # Interactive mode (to observe how states are being propagated)
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
beepr::beep(3)

# Note that if the only change is "SizeChanged", that means the size codings
# (AbsStrat, RelStrat, etc.) were reverted from the consensus state "back" to
# the input state for size values at species/genus level (which was pre-checked
# after running PropogateSizes.R).

warnings()
# Warnings about "no suitable relatives" typically means the EcoScale in
# "PreLH.tab" is incorrect. Check and re-run, if so. (Otherwise it means there
# are literally no other relatives, not even other members of their phylum.)

round(table(input$EcologyScale) * 100 / nrow(input), 1)
round(table(out$EcologyScale) * 100 / nrow(out), 1)
round(cumsum(table(input$EcologyScale) * 100 / nrow(input)), 1)
round(cumsum(table(out$EcologyScale) * 100 / nrow(out)), 1)
table(input$EcologyScale)
table(out$EcologyScale)

## Any duplicated ID numbers?
# Will match the incorrect life habits when importing
if (any(table(input$IDNumber) > 1)) {
  print(which(table(input$IDNumber) > 1))
  stop("There are duplicate IDNumbers. Fix before proceeding! See above for list.")
}

## EXPORT DATA -------------------------------------------------------------
write.table(out, file = "PostLH_Isabel.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_constant_Ostracodes.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_mode_Ostracodes.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_constant_Bradoriida&Aster&Echino.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_mode_Bradoriida&Aster&Echino.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_constant.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_mode.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_withPBDB_mode.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(out, file = "PostLH_withPBDB_constant.tab", quote = FALSE, sep = "\t", row.names = FALSE)

# (1) Open in Excel to confirm looks acceptable. Delete (matching entire cell
#     contents) all "NA"s in life habit data.

# (2) Open in Word to remove quotation marks around the text entries [UNLESS THE
#     QUOTATION MARK IS CORRECTLY AT THE END OF A TEXT FIELD], (replacing "^t 
#     with ^t and replacing ^t" with " and replacing "" with ").

# (3) Open FileMakerPro and import as a tab-delimited file, updating records by
#     matching names and using the IDNumber as the matching identifier. (Fine to 
#     not import the taxonomic names, but import  everything else.) To set the 
#     column names as the field names, make sure the first entry row in the 
#     imported source (the column headings) are visible, then choose "Use as 
#     Field Names" for the imported source file. Then choose "Matching Names" in
#     the Target Fields dropdown to ensure that the source and target fields 
#     match (but double-check that they are matched correctly). If using 
#     different propagations for the "constant" and "mode" databases, make sure 
#     to import the correct source file to the correct database version.

# (4) Refer to PropogateSizes.R for common troubleshooting corrections to run
#     through in case of manual overrides.

#     On the post-life-habit propagation, focus on the SizeChanged = Check 
#     tagged entries, but also check ALL entries using the specified criteria. 
#     (Can omit those coded at EcoScale = Species/Genus.) MAKE SURE THAT IF 
#     ADD/CHANGE A STATE FOR A EcoScale = SPECIES/GENUS, to tag as "Estimated" 
#     and to update the History accordingly.





## When run for all PBDB genera, need to add a new script that adds notes for
## subjective synonyms, nomen dubia, and form taxa

## ADD SUBJECTIVE SYNONYMS AND NOMEN DUBIA

# Because the 'parent' of subjective synonyms and nomen dubia is their
# accepted_name (and not a higher taxon), need to hold these genera until
# everything else (i.e., body sizes and life habits) is propagated. To maintain
# consistency in the PBDB "namespace," these names were set aside before
# propagating. Add them back in (1) after updating the stratigraphic ranges
# (including with WoRMS for adding extant ranges and with the Sepkoski
# Compendium), (2) after propagating the body sizes (which rely on the
# stratigraphic ranges), and (3) after propagating the life habits. Only then
# will we (4) copy the taxonomic classification of their accepted parent, and
# (5) add one of the following tags to the TaxonomyReference field (inserting
# the correct parent name:

# Subjective synonym: "Possibly invalid taxon. PBDB treats Stellaria as a
# subjective synonym of Chancelloria.".

# Nomen dubium: "Possibly invalid taxon. PBDB treats Protosphaerites as a
# subjective synonym of Archaeooides.".

# Form taxon: "Possibly invalid taxon. PBDB treats Choiaella as a form taxon.".
# Note that this tag would require going back to the main PBDB genus list file.
