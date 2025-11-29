## Update age ranges from Paleobiology Database (and Sepkoski's Compendium) and
## create diversity curves. Check for missing ID numbers. Check that
## extant/extincts are correct (but note false flag if a subgenus is extinct
## within a still-extant genus).

## ISSUES ######################################################################
##
## 1. Consider adding a tag to the TaxonomyReference field to document when 
##    Sepkoski's Compendium and WoRMS were used to expand stratigraphic ranges. (Would require adding this field to occs.csv.)
##
################################################################################

## SEE CODE IN ConfirmExtantInWoRMS.R for easy way to add in WoRMS extant status
## (Just make sure to tag homonyms that would need manual confirmation and
## remove non-animals to reduce false-match homonyms). Then use the deprecated
## code there for how to pull out non-marines for FormatPBDB!


## IMPORT AND PROCESS FILES ####################################################

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")

library(beepr)

## Download data directly from PaleobioDB and save to working directory (will be
## > 25 MB)

## Easier if paste link into URL and save manually
# pbdb.all <- read.csv("https://www.paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&interval=Phanerozoic&show=app&show=acconly&vocab=pbdb")
# If want forams too, use base_name=Metazoa,Retaria
# https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app
pbdb.all <- read.csv("pbdb_data_AllMetazoaTaxa.csv")
head(pbdb.all)

# Extract only genera (to ensure only searching for sub/genus ranges; sometimes
# a genus, like Nuculoidea, has same name as a higher taxon)
pbdb <- pbdb.all[which(pbdb.all$taxon_rank == "genus" | 
                         pbdb.all$taxon_rank == "subgenus"), ]
nrow(pbdb)

## Identify potential homonym genera (used only if trouble-shooting for below)
which(table(pbdb$taxon_name) > 1L)
# Note many of these are misspellings, replacements, etc. so false positives.



## Run relevant code in SelectCols.R for UpdateAges&DivCurve.R to obtain
## following output.


## Prepare the database occurrences to return updated strat ranges:

## Use the following columns: IDNumber, Phylum, Class, Order, Superfamily,
## Family, Genus, Subgenus, Species, max_age, max_ma, min_age, t_age
occs <- read.csv("occs.csv", header = TRUE)
# occs <- read.csv("occs_Mode_PBDB.csv", header = TRUE)
head(occs)
str(occs)

ph.tbl <- table(occs$Phylum)
sort(ph.tbl, decreasing = FALSE)
length(ph.tbl)
cl.tbl <- table(occs$Class)
sort(cl.tbl, decreasing = FALSE)
length(cl.tbl)
or.tbl <- table(occs$Order)
sort(or.tbl, decreasing = FALSE)
length(or.tbl)
sf.tbl <- table(occs$Superfamily)
sort(sf.tbl, decreasing = FALSE)
length(sf.tbl)
fam.tbl <- table(occs$Family)
sort(fam.tbl, decreasing = FALSE)
length(fam.tbl)

# Confirm that max_age and min_age are factors
is.factor(occs$max_age)
is.factor(occs$min_age)

# If not, run following:
# occs$max_age <- as.factor(as.character(occs$max_age))
# occs$min_age <- as.factor(as.character(occs$min_age))

# Confirm that IDNumber and PBDB_GSG_Number are numerics
is.numeric(occs$IDNumber)
is.numeric(occs$PBDB_GSG_Number)
# If not, usually means there was a hidden tab on some data field. Check source
# file and rebuild, if necessary.


## Any duplicated ID numbers?
which(table(occs$IDNumber) > 1)

## Any missing ID numbers?
num <- seq(nrow(occs))
all(num %in% sort(occs$IDNumber)) # TRUE if nothing missing
which(num %in% sort(occs$IDNumber) == FALSE)

# Any duplicated genus entries?
if (length(table(table(occs$Genus))) > 1L) {
  foo <- which(table(occs$Genus) > 1L)
  print(foo)
  cat("There are", length(foo), "entries above.\n")
  stop("The above genus entries are entered twice. Delete the outdated entry/entries?")
}
# Ignore the cases where multiple subgenera are included in same genus, where
# there are ecologically quite different species in same genus, and homonyms.




## PREP ICS 2024 TIME SCALE FROM PBDB ##########################################

# Note substantial changes occurred in the stratigraphic naming system used in
# the PBDB starting ca. 2023. The new system uses new database field names and
# incorporates multiple geochronological systems. We only use the one listed as
# scale_no = 1 (ICS 2024).

## Get updated PBDB intervals and ages
strat_names <-
  read.csv("https://www.paleobiodb.org/data1.2/intervals/list.csv?all_records&vocab=pbdb")
# strat_names <- read.csv("strat_names.csv")
head(strat_names)

## Restrict standard intervals to the default geochronological scale (ICS 2024)
## epochs. Other options includes "eons", "eras", "periods", "epochs", and
## "ages", etc. But use all intervals when incorporating in Sepkoski's ages.
epochs <- strat_names[which(strat_names$type == "epoch" &
                              strat_names$scale_no == 1), ]
## Add in Ediacaran, too:
edia <- strat_names[which(strat_names$interval_name == "Ediacaran"), ]
epochs <- rbind(epochs, edia)
epochs[, 1:5]


## IMPORT AND PREP SEPKOSKI COMPENDIUM #########################################

## Import Peter Wagner's digitized version of Sepkoski's Compendium. DO NOT
## SHARE THIS FILE!
load(file = "Sepkoski_Genus_Compendium.Rdata")
head(sepkoski_compendium)
tail(sepkoski_compendium)

# Some explanation: The items tagged "_98" are the assignments from Sepkoski's
# 1998 version of the Compendium. The other ones are what the PBDB now assigns
# it as. For the list of genera, "Genus" is how it was spelled by Sepkoski,
# "Genus_23.1" is identical to "Genus" (WHY???) and "Genus_23" is how the PBDB
# currently spells it, or synonymizes it.


## Import a geochronological look-up table. Not using the ages in Peter's
## version because based on Gradstein (2020), which is different from the PBDB
## ages. The following provides interval name conversions between the ages in
## (Peter's digitized version of) Sepkoski's Compendium and those used in the
## PBDB. Age designations are based on the interval names specified in pages 7-9
## of Sepkoski's Compendium (BAP 2002). For intervals that are not specified,
## approximate intervals are chosen that currently subdivide those in the
## Compendium. In some cases, this involved using calcareous nannoplankton or
## geomagnetic chrons. In nearly all of these cases, the use of alternative
## interval choices will have negligible impact (given the coarse resolution of
## my database).
Sepkoski.converts <- read.csv("SepkoskiAgeConversion.csv")
head(Sepkoski.converts)



## Convert named intervals in Sepkoski Compendium to current ICS 2024 dates

# Example using middle Callovian (Jurassic)
"J (Call-m)" %in% Sepkoski.converts$Sepkoski_age
Sepkoski.converts[which(Sepkoski.converts$Sepkoski_age == "J (Call-m)"), ]

all.names <- sort(unique(c(sepkoski_compendium$FA, sepkoski_compendium$LA)))
sq <- seq.int(all.names)
matches <- sapply(sq, function(sq) all.names[sq] %in% Sepkoski.converts$Sepkoski_age)
table(matches)
all.names[!matches]

# OK, so only "empty" (= "") is not in this look-up table. That makes things
# easy!
which(sepkoski_compendium$FA == "")
which(sepkoski_compendium$LA == "")

# Let's take a look at the first two:
sepkoski_compendium[c(4474, 23222), ]

# Now we need to update (=override) the dates in the Compendium. Because the
# first is the blank one (""), we'll only loop through the others. "" can remain
# the default NA because missing.
sepkoski_compendium$max_ma <- as.numeric(NA)
sepkoski_compendium$min_ma <- as.numeric(NA)

for (int in 2:length(all.names)) {
  
  # Extract genera sharing interval as FA or LA
  wh.Sepkoski.FA <-
    which(sepkoski_compendium$FA == all.names[int])
  wh.Sepkoski.LA <-
    which(sepkoski_compendium$LA == all.names[int])
  
  # Get base and top ages for interval from converted PBDB (using raw
  # strat_names because includes all intervals)
  PBDB_FA <- Sepkoski.converts$PBDB_b_age[which(Sepkoski.converts == all.names[int])]
  PBDB_LA <- Sepkoski.converts$PBDB_t_age[which(Sepkoski.converts == all.names[int])]
  PBDB_max_ma <- strat_names$b_age[which(strat_names$interval_name == PBDB_FA)[1]]
  PBDB_min_ma <- strat_names$t_age[which(strat_names$interval_name == PBDB_LA)[1]]
  
  # Manual override for Recent taxa, because strat_names does not include Recent
  if (all.names[int] == "R") {
    PBDB_min_ma <- 0
  }

  # Replace ages in Sepkoski's Compendium
  sepkoski_compendium$max_ma[wh.Sepkoski.FA] <- PBDB_max_ma
  sepkoski_compendium$min_ma[wh.Sepkoski.LA] <- PBDB_min_ma
  
}

head(sepkoski_compendium)

# Confirm "" handled correctly (now should be NAs):
sepkoski_compendium[c(4474, 23222), ]


## IMPORT WoRMS NAMESPACE ######################################################

# Download of all WoRMS taxa. Only includes taxa with extant species. DO NOT
# SHARE, per download terms with WoRMS. Use to confirm extant status of PBDB
# taxa. See ConfirmExtantInWoRMS.R for additional algorithms to confirm
# mis-matches between taxa in PBDB, Sepkoski's Compendium, and WoRMS, as well as
# code to specify marine vs non-marine taxa.
WoRMS <- read.delim("WoRMS_download_2025-11-01/taxon.txt")
head(WoRMS)

# To reduce instances of homonyms, remove non-animals and non-accepted names
WoRMS <- WoRMS[which(WoRMS$kingdom == "Animalia" &
                       WoRMS$taxonomicStatus == "accepted"), ]

# Further restrict to genera and subgenera. (Note WoRMS only lists
# genus/subgenus when uncertain status or lacking subspecies, so need to also
# extract species.)
WoRMS <- WoRMS[which(WoRMS$taxonRank == "Genus" | 
                       WoRMS$taxonRank == "Subgenus" |
                       WoRMS$taxonRank == "Species"), ]




## EXTRACT AGE AND INTERVAL RANGES #############################################


# Do you choose to combine in ranges from both PBDB (= requires occurrences and
# uses updated taxonomic synonymizations) and Sepkoski's Compendium ( = doesn't
# require occurrences in PBDB but might use outdated synonyms).
use.Sepkoski = TRUE

# In cases where the Sepkoski range is substantially different than that in the
# PBDB, what maximum range extension window are you willing to expand PBDB
# occurrences based on Sepkoski?

# Rationale: If within this window, use Sepkoski's Compendium to expand it. If
# greater than this value, then only uses the PBDB range. 10 million years is
# used as a default because the median epoch is 11 million years in duration,
# which means adding in Sepkoski's ranges will only expand a range by one
# interval in each direction, on average, and assume greater range extensions
# are the result of errors in taxonomic identification, synonymizations that
# were subsequently corrected in the PBDB, or potentially spurious occurrences.
# In other words, when there is a conflict between PBDB and Sepkoski, the
# default is to go with the PBDB's more updated taxonomic namespace. But to
# revert to the Compendium when there are no PBDB occurrences (which is quite
# often, depending on taxonomic group).
Sepkoski.offset = 10

# Do you choose to confirm extant status using WoRMS? Many taxa in the PBDB are
# not tagged as extinct/extant, or potentially mis-tagged. Because WoRMS is
# edited by taxonomic authorities and focuses on extant taxa, comparing with
# WoRMS can improve the LAs for extant taxa.
use.WoRMS = TRUE

# Only use WoRMS when also consulting Sepkoski
if (use.WoRMS & !use.Sepkoski)
  warning("If using WoRMS, you must set use.Sepkoski = TRUE")

# How far do you wish to expand the LAD to the Recent? 70 million years is
# reasonable, based on spot checking taxa in PBDB and WoRMS. But submit warning
# if more than 20 million years.
WoRMS.FAD.expand <- 70
WoRMS.warning <- 20



## Various tests:

# The following code uses PBDB genus (or subgenus) ID numbers instead of genus
# (subgenus) names. The following cases will be ignored below because not in the
# PBDB ID number namespace.
wh.no.ID <- which(is.na(occs$PBDB_GSG_Number))
# Note all extant-only, missing PBDB ID numbers, and only in the core life-habit
# database
occs[wh.no.ID, c(1:2, 4, 8, 11, 13)]
length(wh.no.ID)
summary(occs$IDNumber[wh.no.ID])

# Confirm actually missing from PBDB namespace. (If so, manually update and
# reprocess above again.) Note some may be homonyms (e.g., Janira, Leptocheirus,
# and Lafoea)
any(occs$Genus[wh.no.ID] %in% pbdb$taxon_name)
occs[wh.no.ID, ][which(occs$Genus[wh.no.ID] %in% pbdb$taxon_name), ]

# Confirm actually missing from Sepkoski's Compendium. (If so, manually update
# and reprocess above again.)
any(occs$Genus[wh.no.ID] %in% sepkoski_compendium$Genus)
any(occs$Genus[wh.no.ID] %in% sepkoski_compendium$Genus_23)




## Update stratigraphic ranges

index <- seq(0, 100000, by = 1000)
# If crashes, most likely because the genus is mis-spelled, or not in PBDB AND 
# there are no ranges available to use. Manually set as Recent (if extant and no
# fossil record, according to WoRMS) or enter manually (and start at next "i" in
# loop).

Gen <- sort(unique(occs$PBDB_GSG_Number))
for (i in 1:length(Gen)) {
#  for (i in 33418:length(Gen)) {
  gen.pbdb <- max.ma <- min.ma <- Early <- Late <- Sepkoski.max.ma <- 
    Sepkoski.min.ma <- wh.occs.G <- wh.pbdb.G <- wh.Sepkoski.G <- 
    wh.WoRMS.G <- NA
  override <- not.in.pbdb <- not.in.Sepkoski <- FALSE
  gen <- Gen[i]
  if (i %in% index) cat("processing genus ", i, "\n")
  
  # Extract the rows for this taxon in occs, pbdb, Sepkoski, and WoRMS:
  wh.occs.G <- which(occs$PBDB_GSG_Number == gen)
  wh.pbdb.G <- which(pbdb$taxon_no == gen)
  wh.Sepkoski.G <- which(sepkoski_compendium$pbdb_taxon_no == gen)
  
  # Extract original min and max ages
  max.ma <- occs$max_ma[wh.occs.G][1]
  min.ma <- occs$min_ma[wh.occs.G][1]
  
  # Sometimes two genera get entangled in Sepkoski's Compendium. If so, issue a
  # warning to double check
  if (length(wh.Sepkoski.G) > 1L)
    cat(paste0("- Check the duplicate entry of ", occs$Genus[wh.occs.G], " ", 
               occs$Subgenus[wh.occs.G], "\n"))
  
  # Check for duplicate ID numbers (which should not happen):
  if (length(wh.pbdb.G) > 1L)
    cat(paste0("- Check if there's a duplicate PBDB ID number for ", 
               occs$Genus[wh.occs.G], " ", occs$Subgenus[wh.occs.G], "\n"))
  
  # Manual override: Taxa not in PBDB (nor Sepkoski's Compendium, if using) but
  # manually given dates or ages. Typically, these manual ranges are added from
  # the Treatise. This code re-confirms the ages and interval names are correct
  # if more recently added to PBDB. Note that all genera in Sepkoski's
  # Compendium are in PBDB, so no need to query the Compendium. If not in PBDB,
  # then also no need to query WoRMS.
  if (length(wh.pbdb.G) == 0L) not.in.pbdb <- TRUE
  
  if (use.Sepkoski) {
    if (length(wh.Sepkoski.G) == 0L) not.in.Sepkoski <- TRUE
  }
  
  if (not.in.pbdb & not.in.Sepkoski) override <- TRUE
  
  # In following (and other) lines, adding [1] index because occasionally two
  # species in same genus are entered in life habit / size database.

  if (override) {
    # If only range is provided, give correct interval name :
    if (occs$max_age[wh.occs.G][1] == "" &
        occs$min_age[wh.occs.G][1] == "" &
        !is.na(max.ma) & !is.na(min.ma)) {
      occs$max_age[wh.occs.G] <-
        as.character(epochs$interval_name[length(which(epochs$b_age < occs$max_ma[wh.occs.G]))])
      occs$min_age[wh.occs.G] <-
        as.character(epochs$interval_name[length(which(epochs$b_age <= occs$min_ma[wh.occs.G]))])
    }
    
    # If only interval names are given, give correct range (be aware range could
    # be overextended):
    if (occs$max_age[wh.occs.G][1] != "" &
        occs$min_age[wh.occs.G][1] != "" &
        is.na(occs$max_ma[wh.occs.G][1]) &
        is.na(occs$min_ma[wh.occs.G][1])) {
      if (occs$max_age[wh.occs.G][1] == "Recent") {
        occs$max_ma[wh.occs.G] <- 0
      } else {
        occs$max_ma[wh.occs.G] <-
          epochs$b_age[which(epochs$interval_name == as.character(occs$max_age[wh.occs.G]))]
      }
      if (occs$min_age[wh.occs.G][1] == "Recent") { 
        occs$min_ma[wh.occs.G] <- 0
      } else {
        occs$min_ma[wh.occs.G] <-
          epochs$b_age[which(epochs$interval_name == as.character(occs$min_age[wh.occs.G]))]
      }
    }
    
    # If no stratigraphic information is available:
    if (occs$max_age[wh.occs.G][1] == "" &
        occs$min_age[wh.occs.G][1] == "" &
        is.na(max.ma) & is.na(min.ma)) {
      cat(paste0("  - check ", gen, " (", occs$Genus[wh.occs.G], "): no age known in PBDB or Sepkoski; check Treatise/WoRMS\n"))
    }
  }
  
  if (override) next

  # Update using ranges in PBDB
  if (!not.in.pbdb) {
    gen.pbdb <- pbdb[wh.pbdb.G, ]
    max.ma <- gen.pbdb$firstapp_max_ma
    min.ma <- gen.pbdb$lastapp_min_ma
    
    # In PBDB, single occurrences only fill in the 'firstapp' and not the
    # 'lastapp'. This corrects for that in those rare instances.
    
    if (is.na(min.ma)) min.ma <- gen.pbdb$firstapp_min_ma

    # Only proceed if ages exist:
    if (!is.na(occs$min_age[wh.occs.G])[1]) {
    
      # Flag any discrepancies in extinct / extant tags (if in PBDB)
      if (occs$min_age[wh.occs.G][1] == "Recent" & gen.pbdb$is_extant == "extinct")
        cat("+ Confirm extinct/extant status for", occs$Genus[wh.occs.G][1], ", PBDB # ", gen, "\n")
      
      if (occs$min_age[wh.occs.G][1] != "Recent" & gen.pbdb$is_extant == "extant") 
        cat("+ Confirm extinct/extant status for", occs$Genus[wh.occs.G][1], 
            ", PBDB # ", gen, ". Check that all synonyms and subtaxa are correctly tagged, too.\n")
      
      # Only proceed with following if NOT consulting WoRMS. (Because there are
      # too many falsely tagged "extants" in the PBDB: a single incorrect
      # species can corrupt its genus or other parent taxa.)
      
      if (!use.WoRMS) {

        # Implement pull-of-the-Recent (if extant in PBDB)
        if (occs$min_age[wh.occs.G][1] == "Recent" |
            gen.pbdb$is_extant == "extant") {
          Late <- "Recent"
          min.ma <- 0
          
        }

      }
      
    }
    
  }
  
  # Same, using Sepkoski's Compendium (and using offset window), if there
  if (use.Sepkoski & !not.in.Sepkoski) {
    
    gen.Sepkoski <- sepkoski_compendium[wh.Sepkoski.G, ]

    Sepkoski.max.ma <- gen.Sepkoski$max_ma[1]
    Sepkoski.min.ma <- gen.Sepkoski$min_ma[1]
    
    # Default to Sepkoski if not in PBDD
    if (is.na(max.ma))
      max.ma <- Sepkoski.max.ma
    if (is.na(min.ma))
      min.ma <- Sepkoski.min.ma
    
    # Only expand if there are values in Sepkoski to expand with:
    if ((!is.na(min.ma) & !is.na(max.ma))
        & (!is.na(Sepkoski.min.ma) & !is.na(Sepkoski.max.ma))) {
      # And expand if within offset window (but not if more than that, implying
      # the record when Sepkoski compiled his ranges are now potentialy suspect)
      if (Sepkoski.max.ma > max.ma &
          (Sepkoski.max.ma - max.ma <= Sepkoski.offset))
        max.ma <- Sepkoski.max.ma
      if (Sepkoski.min.ma < min.ma  &
          (min.ma - Sepkoski.min.ma <= Sepkoski.offset))
        min.ma <- Sepkoski.min.ma
      }
    }
  
  # Confirming extant status in WoRMS (if using)
  if (use.WoRMS) {
    
    # If subgenus rank, first search for matching subgenus, then search as
    # genus, in case up-ranked. Do not search for parent genus by default, as
    # many subgenera in extant genera are legitimately extinct. (The parent
    # genus will be treated separately here, regardless.)
    if (occs$Subgenus[wh.occs.G][1] != "") {
      wh.WoRMS.G <- which(WoRMS$subgenus %in% occs$Subgenus[wh.occs.G])
      if (length(wh.WoRMS.G) == 0L)
        wh.WoRMS.G <- which(WoRMS$genus %in% occs$Subgenus[wh.occs.G])
    } else {
      wh.WoRMS.G <- which(WoRMS$genus %in% occs$Genus[wh.occs.G])
    }
    
    # Proceed if genus present in WoRMS:
    if (length(wh.WoRMS.G) > 0L) {
      
      # Trigger warning if likely homonyms exist:
      sq <- 12:16 # Using phylum : genus columns
      if (sum(sapply(sq, function(sq) length(unique(WoRMS[wh.WoRMS.G, sq])))) > 5L)
        cat(paste0("- Check WoRMS homonym in ", occs$Genus[wh.occs.G], " ",
                   occs$Subgenus[wh.occs.G], "\n"))
      
      # To prevent false results due to homonyms, only update if in the same
      # class. (Although higher taxonomic assignments can be inconsistent with
      # long-extinct taxa, the higher taxonomic assignments for extant taxa tend
      # to be much more compatible across neontologists and paleontologists,
      # making this a reliable check.) Allowing %in% in case of homonyms, as any
      # matching genus in same class confirms the intended genus is extant.
      
      if (occs$Class[wh.occs.G][1] %in% WoRMS$class[wh.WoRMS.G]) {
        
        # Only extend LAD if range extension is less than 70 million years.
        # (Spot-checking seems this is a reasonable ghost extension.) But issue
        # warning if extension is greater than 20 million years. (Be aware many
        # genera listed as extant in WoRMS are actually extinct.)
        
        if (!is.na(min.ma) & min.ma - 0 > WoRMS.warning)
          cat(paste0("- Check possible unreasonable WoRMS extension in ",
                     occs$Genus[wh.occs.G], " ", occs$Subgenus[wh.occs.G], "\n"))
        
        if (!is.na(min.ma) & min.ma - 0 < WoRMS.FAD.expand) {
          min.ma <- 0
          occs$min_age[wh.occs.G] <- "Recent"
        }

        # Special rule to ignore expansion window if PBDB, Sepkoski, and WoRMS
        # all concur extant:
        if (gen.pbdb$is_extant == "extant" & gen.Sepkoski$min_ma == 0 &
            length(wh.WoRMS.G) > 0L) {
          min.ma <- 0
          occs$min_age[wh.occs.G] <- "Recent"
        }
      }
    }
    
  }

  # Assign to epochs (overriding if previously assigned), but only if a range
  # exists
  if (!is.na(max.ma) & !is.na(min.ma)) {
    Early <-
      as.character(epochs$interval_name[length(which(epochs$t_age < max.ma))])
    Late <-
      as.character(epochs$interval_name[length(which(epochs$t_age <= min.ma))])
  }
  
  occs$max_ma[wh.occs.G] <- max.ma
  occs$max_age[wh.occs.G] <- Early
  
  occs$min_ma[wh.occs.G] <- min.ma
  occs$min_age[wh.occs.G] <- Late
  
}
  
beepr::beep(3)



# Confirm no dates are out of order (e.g., FAD younger than LAD). This is
# usually because of manual entry errors by me in cases where a genus does not
# have occurrences in the PBDB, or where the conversions in
# SepkoskiAgeConversion.csv is . Thanks for Dave Bapst for pointing out error,
# which is triggered if try to build a time-tree.
any(occs$max_ma < occs$min_ma)
if (any(occs$max_ma < occs$min_ma)) {
  cat(occs$Genus[which(occs$max_ma < occs$min_ma)])
  stop("Prior genera have the FAD and LAD switched.")
}
# Error means no errors :)



## Fill in (or update) ages (using 11-m.y. PBDB epochs)
for (int in 1:nrow(epochs)) {
  wh.FAD <- which(occs$max_ma > epochs$t_age[int]
                  & occs$max_ma <= epochs$b_age[int])
  wh.LAD <- which(occs$min_ma >= epochs$t_age[int] 
                  & occs$min_ma < epochs$b_age[int])
  occs$max_age[wh.FAD] <- as.character(epochs$interval_name[int])
  occs$min_age[wh.LAD] <- as.character(epochs$interval_name[int])
}

# Special work-around for singletons (only problematic for those that occur on a
# boundary):
wh.singleton <- which(occs$max_ma == occs$min_ma)
occs$min_age[wh.singleton] <- occs$max_age[wh.singleton]

# Recent is included for extant taxa (although no PBDB taxa have FADs = 0). PBDB
# uses Holocene instead of Recent. Here, Holocene is used for FADs > 0 but
# Recent for FADs and LADs = 0, if extant.
wh.Recent.FAD <- which(occs$max_ma == 0)
wh.Recent.LAD <- which(occs$min_ma == 0)
occs$max_age[wh.Recent.FAD] <- "Recent"
occs$min_age[wh.Recent.LAD] <- "Recent"

# Note the choice to leave extant-only taxa as having Recent LADs means they
# will not be included in diversity curves or subsequent analyses (although can
# still be used for size and life-habit propagations).

head(occs)


# write.csv(occs, file = "PBDBDates_New.csv", row.names = FALSE)

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
 read.csv("PBDBDates.csv", header = TRUE, stringsAsFactors=FALSE, colClasses=colCl)
# occs <- read.delim("PostSizes_withPBDB.tab", sep = "\t", header = TRUE, stringsAsFactors=FALSE, colClasses=colCl)
head(occs)


# Get midpoint age for PBDB subperiods
mids <- apply(epochs[ ,9:10], 1, mean)
divs <- data.frame(interval = epochs$interval_name, base = epochs$b_age,
                   top = epochs$t_age, midpt = mids, div = NA)

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
  t.occs <- occs[0 , c(3:wh.col, 11:14)]
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
    t.occs[i, 1:(wh.col - 2)] <- occs[wh.occs.taxon[1], 3:wh.col]
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
    # Update with PBDB (if extends range, often caused by indeterminate
    # occurrences lacking a genus identification):
    wh.pbdb.taxon <- which(pbdb$taxon_name == taxon)
    # Note sometimes higher taxa have homonyms. Use most frequently used name as
    # the likely correct one. (Not a perfect guarantee.)
    n.matches <- length(wh.pbdb.taxon)
    if (n.matches == 0L)
      next
    best.match <- which.max(pbdb$n_occs[wh.pbdb.taxon])
    taxon.pbdb <- pbdb[wh.pbdb.taxon[best.match], ]
    if (is.na(taxon.pbdb$firstapp_max_ma) &
        is.na(taxon.pbdb$lastapp_min_ma)) next
    max.ma <- max(taxon.pbdb$firstapp_max_ma, na.rm = TRUE)
    min.ma <- min(taxon.pbdb$lastapp_min_ma, na.rm = TRUE)
    if (taxon.pbdb$is_extant == "extant") min.ma <- 0
    if (length(occs.max.ma) == 0L)
      t.occs$max_ma[i] <- max.ma else
      if (max.ma >= occs.max.ma) t.occs$max_ma[i] <- max.ma
    if (length(occs.min.ma) == 0L)
      t.occs$min_ma[i] <- min.ma else
        if (min.ma <= occs.min.ma) t.occs$min_ma[i] <- min.ma
    # Assign to epochs
    Early <-
      as.character(epochs$interval_name[length(which(epochs$t_age < t.occs$max_ma[i]))])
    Late <-
      as.character(epochs$interval_name[length(which(epochs$t_age <= t.occs$min_ma[i]))])
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
  mids <- apply(epochs[, 9:10], 1, mean)
  divs <- data.frame(interval = epochs$interval_name, base = epochs$b_age,
                     top = epochs$t_age, midpt = mids, div = NA)
  
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

