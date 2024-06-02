## CODE FOR COMBINING SEPKOSKI COMPENDIUM, THE CLEANED-UP SEPKOSKI COMPENDIUM IN
## THE FOSSILBRUSH PACKAGE, AND THE LIST OF GENERA IN HEIM, ET AL. (2015,
## SCIENCE) THAT HAVE BODY SIZES (AND SEPKOSKI RANGES).

# Goals:

# 1. Use PBDB (which includes Sepkoski Compendium genera, but with updated
#    taxonomy and occurrences) for the core list of genera and stratigraphic
#    ranges.

# 2. Use Heim, et al. (2015) list for body sizes. Requires updates to downstream
#    body size propagation algorithm: for chordates (where maximum [A/P] length 
#    is provided), record A/P and propagate missing D/T and T. For rest (where 
#    only biovolume is provided), use stratigraphically close relatives to 
#    isometrically extrapolate all 3 cardinal axes. To avoid over-riding these 
#    in future propagations, this will require adding a switch to look for 
#    SizeScale = Genus & Size ref starts with "Heim" [i.e., 
#    substr(BodyMeasureReference, 1, 4) == "Heim"] to use the body volume as 
#    the unchanged value.

# 3. Use raw stratigraphic ranges from Heim and fossilbrush when not in PBDB
# (preferentially using the interval name instead of absolute age, so that can
# be updated with future timescales.)

op <- par()
rm(list = ls())

setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

## LIBRARIES
library(palaeoverse)      # v. 1.0.0
# INSTALL FOSSILBRUSH LAST TO FORCE PLACING ITS GTS2020 (which is better than
# palaeoverse's) IN THE WORKING NAMESPACE!
library(fossilbrush)      # v. 1.0.3
library(beepr)            # v. 1.3

## FUNCTIONS ###################################################################

# Function to extract subgenus name within parentheses.
#  x           character formatted as "Genus (Subgenus)".
get_subgenus <- function(x) {
  subgenus_part <- 
    unlist(gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))))
  return(subgenus_part)
}
# Example
get_subgenus("Genus (Subgenus)")



## DOWNLOAD ALL GENERA (AND SUBGENERA) IN PBDB
## Easier if paste link into URL and save manually.
# pbdb <- read.csv("https://paleobiodb.org/data1.2/taxa/list.csv?datainfo&rowcount&base_name=Metazoa&rank=genus,subgenus&variant=all&pres=regular&private")
# If want forams too, use base_name=Metazoa,Retaria .
# Because taxa may be incorrectly tagged, later use "all" and then manually (via
# code) remove the ichnofossils (tagged with "flags = "I" or "IF").
pbdb <- read.csv("pbdb_genera.csv")
head(pbdb, 3)
nrow(pbdb)
# Note: includes subgenera in column = taxon_name and accepted_name as (X) X

# Add new column with subgenus elevated to genus
pbdb$subgenus_name <- NA
for (i in 1:nrow(pbdb)) {
  if (pbdb[i, "taxon_rank"] == "subgenus") {
    pbdb$subgenus_name[i] <- get_subgenus(pbdb$taxon_name[i])
  }
}
pbdb[which(!is.na(pbdb$subgenus_name))[1:10], c(6, 15)]
beepr::beep()
# When update strat ranges, use the subgenus ranges instead of aggregate genus

# If want to only use those without any occurrences (more likely to be those
# only with ranges from Compendium, but not guaranteed).
which_Sepkoski_only <- which(pbdb$n_occs == 0)
no_ranges <- pbdb[which_Sepkoski_only, ]



## IMPORT CLEANED-UP SEPKOSKI COMPENDIUM FROM 'FOSSILBRUSH'
data(sepkoski)
sepkoski_clean <- sepkoski
# write.csv(sepkoski_clean, file = "fossilbrush_sepkoski.csv")
head(sepkoski_clean)
nrow(sepkoski_clean)
# Note: treats subgenera as separate genera

## WHICH FOSSILBRUSH SEPKOSKI NAMES ARE NOT IN PBDB (AND THEREFORE HAVE LIKELY
## BEEN ALTERED DURING CLEANING)?
wh <- which(!(sepkoski_clean$GENUS %in% pbdb$taxon_name | 
                sepkoski_clean$GENUS %in% pbdb$accepted_name |
                sepkoski_clean$GENUS %in% pbdb$subgenus_name))
length(wh)
# ~3,607! 

# But good news, most are protists (which I can ignore), leaving < 800
table(sepkoski_clean$PHYLUM[wh])
  
# Let's take a look:
sepkoski_clean[wh[1:10], ]

# Many of these are mis-spellings (either in PBDB or Sepkoski Compendium or in
# the apparently cleaned version. For consistency, they should all be fixed :( )

# Examples, Sepkoski mis-spelled bivalve Afghanodesma as Afganodesma, and
# mis-spelled Alococoncha as Aloconcha.

# It's worth the time to fix as many of these as possible.

# Note that when fossilbrush finds likely homonyms (such as the bivalve and
# cephalopod Rollieria, it distinguishes them as RollieriaA for the bivalve and
# RollieriaB for the cephalopod.) These can be identified here:

ends_in_A <- which(substr(sepkoski_clean$GENUS,
                          nchar(sepkoski_clean$GENUS),
                          nchar(sepkoski_clean$GENUS)) == "A")
sepkoski_clean[ends_in_A, 1:4]
length(ends_in_A) # 101

ends_in_B <- which(substr(sepkoski_clean$GENUS,
                          nchar(sepkoski_clean$GENUS),
                          nchar(sepkoski_clean$GENUS)) == "B")
length(ends_in_B) # 101, as must be

ends_in_C <- which(substr(sepkoski_clean$GENUS,
                          nchar(sepkoski_clean$GENUS),
                          nchar(sepkoski_clean$GENUS)) == "C")
length(ends_in_C) # only 2 triple homonyms
sepkoski_clean[ends_in_C, 1:4]

ends_in_D <- which(substr(sepkoski_clean$GENUS,
                          nchar(sepkoski_clean$GENUS),
                          nchar(sepkoski_clean$GENUS)) == "D")
length(ends_in_D) # 0 quadruple homonyms

# This means there are only approximately 600 current mis-spellings (or other
# mis-matches to deal with)

# Sort alphabetically (because easiest to see homonyms and to more efficiently
# match with the alphabetical listing of Sepkoski's BAP Compendium, starting on
# page 364.)
mismatches <- sepkoski_clean[wh, ]
# Remove protists
mismatches <- mismatches[which(mismatches$PHYLUM != "Protista"), ]
alpha.order <- order(mismatches$GENUS)
mismatches[alpha.order[1:10], 1:4]

# Some of these are also errors because the PBDB genus was flagged as a "form"
# genus. E.g., Abyssocythere. Fix these, too, if can confirm correctly NOT a
# form taxon.)



## DOWNLOAD HEIM, KNOPE, SCHAAL, WANG, & PAYNE (2015 Science) SI (converted from
## txt to csv). Data reposited at https://purl.stanford.edu/rf761bx8302
heim <- read.csv("HeimSizes_supplementary_data_file.csv")
head(heim, 3)
nrow(heim)
# Note: includes subgenera in column = taxon_name as (X) X (and also provides
# PaleoDB_taxon_no). For consistency, using the provided names.

# Convert Sepkoski and Heim to "accepted_name" (searching for names in both
# genus and subgenus form).

# Although > 80% of Heim's list is tied to PBDB taxon numbers, the numbers may
# no longer correct. Instead, using the raw names (which may include homonyms).
# Although faster to use 'match()', that function only uses the first match
# (instead of all matches), which may not be the accepted version. Using a
# slower loop instead to circumvent this.  Adding 'accepted rank' for genera
# that are nomina nuda or otherwise invalid genera (which should not be
# propagated to the life habit database.)

# THIS IS MUCH TOO MUCH WORK! For now, use PBDB names as the primary list, ONLY
# using Heim and cleaned Sepkoski for updating ranges

heim$PBDB_accepted_name <- NA
heim$PBDB_accepted_rank <- NA
for (i in 1:nrow(heim)) {
  if (heim$taxon_name[i] %in% pbdb$taxon_name) {
    wh_genus <- which(pbdb$taxon_name == heim$taxon_name[i])
    if (length(wh_genus) > 1L) {
      if (length(unique(pbdb$accepted_name[wh_genus])) == 1L) {
        heim$PBDB_accepted_name[i] <- pbdb$accepted_name[wh_genus][1]
        heim$PBDB_accepted_rank[i] <- pbdb$accepted_rank[wh_genus][1]
      } else stop()
    }
  }
}
beepr::beep()


# 1. Convert subgenera to genera (only using accepted names here)
name_col <- "taxon_name"
if (length(grep("[()]", heim[i, name_col])) == 1) {
  heim[i, name_col] <- sub_to_genus(heim[i, ], name_col = "accepted_name")
}
beepr::beep()

# 3. When get strat ranges, use the subgenus ranges instead of aggregate genus




## COMPARE COVERAGE OF GENERA IN DIFFERENT COMPILATIONS

# First need to replace their original names with the accepted names in PBDB (if
# replaced), and then replace subgenera as genera.

# Heim vs. PBDB
not.pbdb <- heim[which(!heim$taxon_name %in% pbdb$taxon_name), ]
head(not.pbdb)
nrow(not.pbdb) / nrow(heim)  # 19.9%
# But note many are junior synonyms, subgenera, or other names that are
# technically in the PDB

# Sepkoski vs. PBDB
not.pbdb <-
  sepkoski_pbdb[which(!sepkoski_pbdb$accepted_name %in% pbdb$taxon_name), ]
head(not.pbdb)
nrow(not.pbdb) / nrow(sepkoski_pbdb)  # 0.6%
# But note many are junior synonyms, subgenera, or other names that are
# technically in the PDB

# fossilbrush::Sepkoski vs. PBDB
not.pbdb <-
  sepkoski_clean[which(!sepkoski_clean$GENUS %in% pbdb$taxon_name), ]
head(not.pbdb)
nrow(not.pbdb) / nrow(sepkoski_pbdb)  # 25.1 %




## USE FOSSILBRUSH TO UPDATE PBDB STRAT AGES

# Note that fossilbrush uses the Gradstein, et al. (2020) timescale, which is
# slightly different from the dates on the latest ICS timescale
# (https://stratigraphy.org/chart). Joe Flannery-Sutherland tells me he chose
# this one because it includes the regional boundaries (i.e., Mohawkian,
# Edenian).

# View the timescale
head(fossilbrush::GTS2020)
# View intervals included
table(fossilbrush::GTS2020$Type)

# Note this is different than the version of the same timescale used in
# palaeoverse, which adds ICS colors, but includes less regional time intervals.

fossilbrush::GTS2020$Interval[-which(fossilbrush::GTS2020$Interval 
                                    %in% palaeoverse::GTS2020$interval_name)]
palaeoverse::GTS2020$Interval[-which(palaeoverse::GTS2020$Interval 
                                    %in% fossilbrush::GTS2020$interval_name)]
# NONE (so better to use fossilbrush)




