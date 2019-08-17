## CONFIRM RAW DATA IN 'MODE' AND 'CONSTANT' DATABASES ARE IDENTICAL

# Explanation: When dealing with two databases in which some states are
# estimated by different propagation algorithms, it is critical that the raw,
# manually entered data is identical in both databases. This code scrubs out any
# estimated states, allowing a simple test for identity in the remaining raw
# data.

# Before running: Export data from FMP in Excel format with headers. (Saving to
# .xlsx does a better job maintaining the text in EcologyNotes. The annoying
# double-quotes are fine to keep, if present, as they should be identical in
# both data sets.) The mode file should be named 'AllMode' and the constant file
# should be 'AllConstant'. Then open in Excel, find-and-replaces all entire-cell
# NAs and resave as tab-delimited (.tab) files. Make sure ALL columns are
# exported (including EcologyNotes and all body size cells), in any order. The
# data below will sort the columns and rows.

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

mode <- read.delim(file = "AllMode.tab", stringsAsFactors = FALSE)
cons <- read.delim(file = "AllConstant.tab", stringsAsFactors = FALSE)
head(mode, 2)
head(cons, 2)

# Confirm same dimensions and same columns
if(nrow(cons) != max(cons$IDNumber))
  stop("There is a hidden tab or paragraph in a 'constant's' text field. Open in Excel to identify and correct.\n")
if(nrow(mode) != max(mode$IDNumber))
  stop("There is a hidden tab or paragraph in a 'mode's' text field. Open in Excel to identify and correct.\n")
if(!identical(dim(mode), dim(cons)))
  stop("The two data files have different dimensions.\n")
if(!identical(sort(colnames(mode)), sort(colnames(cons))))
  stop("The two data files have different columns.\n")

# Make in same order (and re-assign row names):
mode <- mode[order(mode$IDNumber), order(colnames(mode))]
cons <- cons[order(cons$IDNumber), order(colnames(cons))]
rownames(mode) <- rownames(cons) <- seq.int(cons$IDNumber)

# Confirm columns for life habit codings:
eco.col <- which(colnames(cons) == "AboveImmediate" |
                   colnames(cons) == "AbovePrimary" |
                   colnames(cons) == "AbsFoodStratification" |
                   colnames(cons) == "AbsStratification" |
                   colnames(cons) == "AbsStratDistance" |
                   colnames(cons) == "AmbientFeeder" |
                   colnames(cons) == "Asexual" |
                   colnames(cons) == "Attached" |
                   colnames(cons) == "AttachmentFeeder" |
                   colnames(cons) == "Autotroph" |
                   colnames(cons) == "Biotic" |
                   colnames(cons) == "BulkFeeder" |
                   colnames(cons) == "Carnivore" |
                   colnames(cons) == "FeedingAboveImm" |
                   colnames(cons) == "FeedingAbovePrimary" |
                   colnames(cons) == "FeedingWithinImm" |
                   colnames(cons) == "FeedingWithinPrimary" |
                   colnames(cons) == "FilterDensity" |    # Only for filter-feeding echinoderms
                   colnames(cons) == "FilterFeeder" |
                   colnames(cons) == "Fluidic" |
                   colnames(cons) == "FreeLiving" |
                   colnames(cons) == "HardSubstratum" |
                   colnames(cons) == "Herbivore" |
                   colnames(cons) == "Incorporeal" |
                   colnames(cons) == "Insubstantial" |
                   colnames(cons) == "Lithic" |
                   colnames(cons) == "MassFeeder" |
                   colnames(cons) == "Microbivore" |
                   colnames(cons) == "Mobility" |
                   colnames(cons) == "ParticleFeeder" |
                   colnames(cons) == "RaptorFeeder" |
                   colnames(cons) == "RelFoodStratification" |
                   colnames(cons) == "RelStratification" |
                   colnames(cons) == "SelfSupport" |
                   colnames(cons) == "Sexual" |
                   colnames(cons) == "SoftSubstratum" |
                   colnames(cons) == "SolutionFeeder" |
                   colnames(cons) == "Supported" |
                   colnames(cons) == "WithinImmediate" |
                   colnames(cons) == "WithinPrimary"
)

# Automate the Est_X assignments so in same order as raw columns:
ec <- seq_along(eco.col)
est.col.names <- sapply(ec, function(ec) paste0("Est_", colnames(cons)[eco.col][ec]))
est.col <- match(est.col.names, colnames(mode))

# Confirm match
cbind(colnames(mode)[eco.col], colnames(cons)[est.col])

# Remove any estimated states
for(i in seq.int(eco.col)) {
  wh.est <- which(mode[[est.col[i]]] == "Estimated")
  mode[[eco.col[i]]][wh.est] <- ""
  mode[[est.col[i]]][wh.est] <- ""
  wh.na <- which(is.na(mode[[eco.col[i]]]))
  mode[[eco.col[i]]][wh.na] <- ""
  wh.est <- which(cons[[est.col[i]]] == "Estimated")
  cons[[eco.col[i]]][wh.est] <- ""
  cons[[est.col[i]]][wh.est] <- ""
  wh.na <- which(is.na(cons[[eco.col[i]]]))
  cons[[eco.col[i]]][wh.na] <- ""
}


# function that returns values for different fields
better.all.equal <- function(a, b) {
  if (!identical(dim(a), dim(b)))
    stop("data frames have different sizes\n")
  cn <- seq.int(ncol(a))
  row.names(a) <- row.names(b) <- NULL
  wh.diff <- sapply(cn, function(cn) ! identical(a[, cn], b[, cn]))
  ab <- rbind(a, b)
  return(ab[ , which(wh.diff), drop = FALSE])
}


# Check entries coded at species/genus rank (where mostly everything should be
# identical)
cores <-  which(mode$EcologyScale == "Genus" | mode$EcologyScale == "Subgenus" |
                  mode$EcologyScale == "Species")

# But some columns are allowed to have differences for this rank
ignores <- which(colnames(mode) == "History_Ecology" |
                   colnames(mode) == "SizeChanged")

for(i in seq.int(cores)) {
  if (!isTRUE(all.equal(cons[cores[i], -ignores], mode[cores[i], -ignores]))) {
    cat(mode$Genus[cores[i]], ":")
    cat(colnames(better.all.equal(cons[cores[i],-ignores], mode[cores[i],-ignores])), "\n\n")
  }
}

# Manual check
x <- which(cons$Genus == "Eugasterella")
better.all.equal(cons[x, -ignores], mode[x, -ignores])




# Check entries coded at higher ranks (where many more differences allowed)
non.cores <-  which(mode$EcologyScale != "Genus" & mode$EcologyScale != "Subgenus" &
                  mode$EcologyScale != "Species")

# But some columns are allowed to have differences for this rank
ignores <- which(colnames(mode) == "History_Ecology" | 
                   colnames(mode) == "SizeChanged" | 
                   colnames(mode) == "DateEntered_Ecology")
ignores <- sort(c(ignores, eco.col, est.col))

for(i in seq.int(non.cores)) {
  if (!isTRUE(all.equal(cons[non.cores[i], -ignores], mode[non.cores[i], -ignores]))) {
    cat(mode$Genus[non.cores[i]], ":")
    cat(colnames(better.all.equal(cons[non.cores[i],-ignores], mode[non.cores[i],-ignores])), "\n\n")
  }
}

# Manual check
x <- which(cons$Genus == "Ropalospongia")
better.all.equal(cons[x, -ignores], mode[x, -ignores])
