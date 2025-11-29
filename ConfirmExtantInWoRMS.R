## CHECK WoRMS TO CONFIRM EXTINCT/EXTANT STATUS IN PBDB ########################

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")

library(beepr)

## IMPORT OBJECTS ##############################################################

# Import full taxon namespace for WoRMS taxa. DO NOT SHARE, per download terms
# with WoRMS
WoRMS <- read.delim("WoRMS_download_2025-11-01/taxon.txt")
head(WoRMS)

# Import extant status for WoRMS taxa. Note does not specify extinct status. DO
# NOT SHARE, per download terms with WoRMS
extant.WoRMS <- read.delim("WoRMS_download_2025-11-01/speciesprofile.txt")
head(extant.WoRMS)

# Import extinct status for WoRMS taxa. DO NOT SHARE, per download terms with
# WoRMS
extinct.WoRMS <- read.csv("WoRMS_download_2025-11-01/Fossils_species_03112025.csv")
head(extinct.WoRMS)


## Import Peter Wagner's digitized version of Sepkoski's Compendium. DO NOT
## SHARE THIS FILE!
load(file = "Sepkoski_Genus_Compendium.Rdata")
head(sepkoski_compendium)

# Some explanation: The items tagged "_98" are the assignments from Sepkoski's
# 1998 version of the Compendium. The other ones are what the PBDB now assigns
# it as. For the list of genera, "Genus" is how it was spelled by Sepkoski,
# "Genus_23.1" is identical to "Genus" (WHY???) and "Genus_23" is how the PBDB
# currently spells it, or synonymizes it.

# Import raw PBDB parenting (b/c has is_extant)
# pbdb <- read.csv("https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app")
# If want forams too, use base_name=Metazoa,Retaria
pbdb <- read.csv("pbdb_data_AllMetazoaTaxa.csv")
head(pbdb)




## REDUCE OBJECT SIZES BY EXTRACTING OUT ONLY GENERA & SUBGENERA ###############

# To reduce instances of homonyms, remove non-animals
WoRMS <- WoRMS[which(WoRMS$kingdom == "Animalia"), ]

# WoRMS (WoRMS only lists genus/subgenus when uncertain status or lacking subspecies)
WoRMS <- WoRMS[which(WoRMS$taxonRank == "Genus" | 
                       WoRMS$taxonRank == "Subgenus" |
                       WoRMS$taxonRank == "Species"), ]
# Copy subgenus names to genus column so matches format used in PBDB
wh.subg <- which(WoRMS$taxonRank == "Subgenus")
WoRMS$genus[wh.subg] <- WoRMS$subgenus[wh.subg]

# Extract out only valid genera and subgenera from PBDB (including forms
# potentially mistagged).
gsgs <- pbdb[which(pbdb$accepted_rank == "genus" |
                     pbdb$accepted_rank == "subgenus"), ]
gsgs.ordered <- gsgs[order(gsgs$flags, gsgs$difference), ]
which.first.valid.gsg <- unique(gsgs.ordered$accepted_no)
valid.gsg.matches <- match(which.first.valid.gsg, gsgs.ordered$accepted_no)
valid.gsgs <- pbdb[row.names(gsgs.ordered[valid.gsg.matches, ]), ]
valid.gsgs <- valid.gsgs[which(valid.gsgs$flags == "" | valid.gsgs$flags == "F"), ]
# Note processing this way removes tagged trace fossils (but keeps some forms,
# just in case not tagged correctly)
table(valid.gsgs$flags)
# And there are no discrepancies between original name and accepted name:
identical(valid.gsgs$taxon_name, valid.gsgs$accepted_name)

# Clean up memory
rm("pbdb")
gc()


## GET EXTINCT/EXTANT STATUS OF DIFFERENT DATABASES ############################

index <- seq(0, 100000, by = 5000)

# To store mismatches: (Use NA so easy to na.omit rows not flagged)
extant.status <- data.frame(taxon = valid.gsgs$taxon_name,
                            rank = valid.gsgs$taxon_rank, 
                            PBDB.status = NA, Sepkoski.status = NA,
                            WoRMS.status = NA, flag = NA)

for (g in 1:nrow(extant.status)) {

  in.WoRMS <- in.Sepkoski <- genus <- subgenus <- NA
  
  if (g %in% index) cat("processing genus", g, "of", nrow(extant.status), "\n")
  taxon <- extant.status$taxon[g]
  rank <- extant.status$rank[g]
  
  # Check whether potential homonym (need to manually check, if so)
  if (taxon %in% extant.status$taxon[-g])
    extant.status$flag[g] <- "possible homonym: check manually"
  
  # Check whether in WoRMS
  in.WoRMS <- which(WoRMS$genus == taxon)
  in.WoRMS2 <- which(extinct.WoRMS$Genus == taxon)
  
  # Sepkoski elevated subgenera to genus rank. If subgenus, check both
  # combinations:
  if (rank == "subgenus") {
    split.subgenus <- strsplit(taxon, " ")[[1]]
    genus <- as.character(split.subgenus[1])
    subgenus <- as.character(gsub("[()]", "", split.subgenus[2]))
  }
  
  in.Sepkoski <- which(sepkoski_compendium$Genus == taxon |
                         sepkoski_compendium$Genus == genus |
                         sepkoski_compendium$Genus == subgenus)
  
  # Update extinct/extant status
  extant.status$PBDB.status[g] <- valid.gsgs$is_extant[g]
  
  if (any(in.Sepkoski > 0L)) {
    seps <- sepkoski_compendium[in.Sepkoski, ]
    extant.status$Sepkoski.status[g] <- ifelse(any(seps$LA == "R"), "extant", "extinct")
  }

  # CHECK EXTINCT STATUS FIRST! Note all entries in extinct.WoRMS are either
  # extinct or extant (but with a fossil record). So extinct genera lack tags of
  # "recent". However, because this list does NOT include extant-only species,
  # it will incorrectly tag genuinely extant genera as extinct (unless also in
  # the extant-only database). For example, the snail Mikadotrochus has an
  # extinct species listed. If run this check first, correctly tags extinct
  # genera lacking in the extant list below, then can override them secondarily
  # if in the extant list.
  if (any(in.WoRMS2 > 0L)) {
    WoRMS2.extant <-
      any(grep("recent", extinct.WoRMS$Recent.Fossil[in.WoRMS2]))
    extant.status$WoRMS.status[g] <-
      ifelse(WoRMS2.extant, "extant", "extinct")
  }
  
  # In WoRMS, extinct is tagged as 1 and extant as 0, but this list only
  # includes extant taxa. (Cases tagged as NA seem to be unaccepted names).
  if (any(in.WoRMS > 0L)) {
    extant.status$WoRMS.status[g] <- "extant"
    
    # the following is deprecated, but maintaining as an example solution if
    # later want to add automatic flagging of 'isMarine'
    
    # WoRMS.taxonID <- WoRMS$taxonID[in.WoRMS]
    # extant.status$WoRMS.status[g] <- ifelse(any(extant.WoRMS$isMarine[which(extant.WoRMS$taxonID == WoRMS.taxonID)] == 1), "marine", "non-marine")
  }
  
  
  

}

beepr::beep(3)

head(extant.status)
tail(extant.status)


## TEST FOR MISMATCHES #########################################################

# Need to test for three cases:

# 1. Taxa not tagged correctly as extant in PBDB (or not tagged at all) but
# listed as extant in WoRMS
case1s <- which(extant.status$PBDB.status != "extant" &
                  extant.status$WoRMS.status == "extant")
length(case1s)
# 1449 genera that need to be corrected in PBDB. (Be aware not all are correct;
# for example, the Cambrian fossil Microdictyon has a WoRMS homonym Microdictyon
# that is a green alga.)
extant.status[head(case1s, 10), ]

# 2. Taxa not tagged correctly as extinct in PBDB (or not tagged at all) but
# clearly extinct based on Sepkoski ranges
case2s <- which(extant.status$PBDB.status != "extinct" &
                  extant.status$Sepkoski.status == "extinct")
length(case2s)
# 319 genera that need to be corrected in PBDB (but some of these seem
# incorrectly showing as extant in PBDB when the database is showing them as
# extinct.)
extant.status[head(case2s, 10), ]

# 3. Taxa not tagged correctly as extinct in PBDB (or not tagged at all) but
# not listed in WoRMS (so possibly extinct)
case3s <- which(extant.status$PBDB.status != "extinct" &
                  is.na(extant.status$WoRMS.status))
length(case3s)
# 9,329 genera putatively extinct but not listed as extinct in PBDB. Note
# nearly all of these will be false positives (where correctly flagged as extant
# but not listed as such in WoRMS.)
extant.status[head(case3s, 10), ]

# Homonyms that need to be hand parsed
table(extant.status$flag)
homonyms <- 
  extant.status[which(extant.status$flag == "possible homonym: check manually"), ]
homonyms[order(homonyms$taxon), 1:5]


