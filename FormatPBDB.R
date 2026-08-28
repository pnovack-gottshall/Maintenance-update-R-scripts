## FORMAT PBDB GENUS/SUBGENUS OCCURRENCES INTO TAXONOMIC STRUCTURE OF MY DATA,
## USING A PARALLEL-COMPUTING ENVIRONMENT

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ISSUE TO RESOLVE LATER: Because of weird parenting, some tetrapods (e.g.,
## mammalian Sirenia and cynodonts and Eupelycosauria) are parented to subclass
## Sarcopterygii instead of Tetrapoda, and pinnipeds and desmostylians parented
## to Perissodactyla. Some of these are terrestrial but not removed by the code
## below. Not sure if the solution is (1) to fix the vertebrate taxonomy in the
## PBDB (unrealistic), (2) add additional steps in the marine/non-marine
## post-processing to pick and choose which to keep/remove, or (3) add new code
## to the prep.pbdb() function to ignore cases when a name is parented to a
## "higher" rank that is actually "lower" (e.g., in the case of order Sirenia
## when class Mammalia is parented to infraorder Eucynodontia). With the new
## beta fix for the "original name" issue, I'm hoping this issue soon becomes
## moot. (Preliminary testing on the training site shows it won't.) But
## something to be aware of until know how will behave. The code below uses
## solution #3 for the time being, which seems to produce the desired outcomes.

## Another issue is that Pterosauria is currently an unranked clade rather than
## a formal order, which means the code below can not include/exclude them. A
## wrap-around for now is to list the genera known to be marine, exclude those
## known to be non-marine, and include the dominant suborders Pterodactyloidea
## and Rhamphorhynchoidea individually. Aaargh, I hate cladistic-based taxonomy!

## Also, it seems Aves are also now classed in PBDB as Reptilia. The code below
## seems to scoop in those (like Hesperornithiformes, Pelecaniformes,
## Charadriiformes, and Suliformes), but may need to test other work-arounds.

## When there are (sug)genera that have subsequently been reranked to genus or
## subgenus rank, duplicate genera (with different PBDB taxon_no) can sometimes
## get added. The easiest way to fix is to remove duplicates, but would be
## better to confirm that only the current one is retained.

## SEE CODE IN ConfirmExtantInWoRMS.R for easy way to use WoRMS to ID extant
## non-marine genera!

## 1. Consider adding a tag to the TaxonomyReference field to document form taxa,
##    nomina dubia, etc. (Would require adding this field to 'output'.)

## 2. Might be worthwhile to compile a list of (likely non-fossiliferous) deep 
##    sea taxa, but that seems better to hold off until final analyses, as many
##    currently deep sea taxa lived in shallower habitats earlier in their 
##    history.

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# The serial version of the main parenting algorithm below takes ~4.5 hours to
# process PBDB into a formatted taxonomic dataframe structure. This code embeds
# the algorithm as a function that can be implemented in parallel and is much
# faster.

## Download data directly from PaleobioDB and save to working directory (will be
## > 20 MB)

setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")

rm(list = ls())

# Libraries
library(data.table)  # v. 1.18.4
library(snowfall)    # v. 1.84-6.3
library(beepr)       # v. 2.0
library(parallel)    # v. 4.6.0


## RATIONALE -------------------------------------------------------------------

# It is better to propagate the life habit of every current genus (subgenus) in
# PBDB now and then remove invalid ones from analyses downstream than to decide
# later they should have been added.

# Include "all" taxonomic ranks at and above subgenus so that can properly
# parent taxa.

# Do not restrict to Phanerozoic. Restricting only includes taxa with fossil
# occurrences. We also want to include genera without current PBDB occurrences
# (such as those found in Sepkoski's Compendium).

# Do not restrict to marine environments. Likely non-marine (terrestrial and
# freshwater) taxa are manually removed downstream.

# Because taxa may be incorrectly tagged, download "all" preservation categories
# (body, trace, form), including "obsolete name variants"; then secondarily
# (= manually, via code) remove the ichnofossils (tagged with flags = "I" or
# "IF"). Include form taxa in life-habit database for now, as many are actually
# "regular" taxa improperly flagged by data enterers as "form" taxa.

# Make sure to include "all" names (including synonyms, misspellings, and other
# invalid names), as these can be found in Sepkoski's Compendium and other
# databases. To avoid "parenting" issues (such as when a legitimate genus is
# parented in a family tagged as an ichnofamily or in an obsolete naming
# variant, or when a subgenus is parented into a nomen nudum genus), also
# include all taxa in the parenting algorithm. Exclude "obsolete name variants"
# because they are almost certainly improper. If some (anticipated to be quite
# few) become deemed acceptable in the future, it is sufficiently easy to add
# them in, given their likely limited study. (Prior to July 2025, "subjective
# synonyms" and "nomen dubium" were propagated but stored separately, in case
# they were subsequently deemed valid. See GitHub
# https://github.com/pnovack-gottshall/Maintenance-update-R-scripts to view 
# now-obsolete code.)

# Manually exclude (sub)genera that are currently coded in "difference"
# categories: "corrected to", "invalid subgroup of", "misspelling of", "nomen
# dubium", "nomen nudum", "nomen oblitum", "nomen vanum", "objective synonym
# of", "obsolete variant of", "reassigned as", "replaced by", and "subjective
# synonym of". These are nearly certainly genuine errors or properly invalid
# (sub)genera that are unlikely to be reversed by future opinions. However,
# these names should be included in downstream analyses (such as adding
# stratigraphic ranges from Sepkoski's Compendium), as other databases may
# include such obsolete names. "obsolete variant of" and "reassigned as" are
# excluded because they are usually (always?) only used when a subgenus is
# re-ranked as a genus or vice versa; these variants should already be included
# above, and so including them here would simply add duplicate entries.





## DOWNLOAD FROM PBDB ----------------------------------------------------------

# Easier if paste URL link into browser and save manually 

# https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app
# pbdb <- read.csv("https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app")
# If want forams too, use base_name=Metazoa,Retaria
pbdb <- read.csv("pbdb_data_AllMetazoaTaxa.csv")
head(pbdb)
nrow(pbdb)

# See https://paleobiodb.org/data1.2/taxa/list_doc.html for description of API
# fields.

# Confirm flags (note "valid" names are not listed here, except for the base
# name)
table(pbdb$flags)
# B = base taxon downloaded (here, Metazoa, which redirects to Animalia)
# V = invalid taxon variants (replacements, misspellings, synonyms, etc)
# I = ichnotaxa
# F = form taxa (ignore here given many user errors in classifying these, and
#     represent a small number of taxa; known ones are manually removed below)


## Extract out trace fossils and invalid names, for (sub)genera only (keeping
## all taxon types for higher taxa to allow proper parenting).

# valid.gsgs <- which((pbdb$accepted_rank == "genus" | pbdb$accepted_rank == "subgenus") & pbdb$difference == "")
# traces <- grep("I", pbdb$flags)

# Not worth indexing on "V" because not all taxa with a "difference" are tagged
# with "V". The following treatment is more efficient.

# which.gsg <- setdiff(valid.gsgs, traces)

# Note that sometime since June 2024 and July 2025, the 'difference' flags have
# been changed. Until I receive confirmation on the new behavior, using the
# following instead of the code above.

# This code seems to work most effectively in identifying "them"valid" genera
# (while allowing some form taxa that may have been incorrectly tagged as
# forms.)
gsgs <- pbdb[which((pbdb$accepted_rank == "genus" |
                      pbdb$accepted_rank == "subgenus") &
                     (pbdb$taxon_name == pbdb$accepted_name)), ]
# Note that including ' & (pbdb$taxon_no == pbdb$accepted_no)' within this
# subset will exclude ~ 700 subgenera that were reranked as genera in Sepkoski's
# Compendium. Excluding it means both versions will be included below, as a
# duplicate (using the accepted_name and accepted_no), but the duplicates are
# secondarily removed below (in the 'unique' line).
gsgs.ordered <- gsgs[order(gsgs$flags, gsgs$difference), ]
which.first.valid.gsg <- unique(gsgs.ordered$accepted_no)
valid.gsg.matches <- match(which.first.valid.gsg, gsgs.ordered$accepted_no)
valid.gsgs <- pbdb[row.names(gsgs.ordered[valid.gsg.matches, ]), ]
valid.gsgs <- valid.gsgs[which(valid.gsgs$flags == "" | valid.gsgs$flags == "F"), ]
which.gsg <- as.integer(rownames(valid.gsgs))
# This seems to catch the 5 known genera the older versions missed, so in right
# direction. Examples of those with issues across different propagations:
# Alanisicystis, Mandalacystis, Quinquecaudex, and Neocrinus, but still two
# versions of Bolboporites (which now redirect to each other, as Sepkoski listed
# the genus twice, as an uncertain cnidarian). A common ingredient in each is
# that there exist objective synonyms, mis-spellings, or other alternatives of
# the same name.


# Valid (sub)genera now are all form taxa or regular taxa, with no 'differences'
table(pbdb[which.gsg, c("accepted_rank", "flags", "difference")])

# Invalid (sub)genera are traces or differenced, but other taxa include everything
table(pbdb[-which.gsg, c("accepted_rank", "flags", "difference")])[c(3, 12, 2, 8, 1), , 1]
table(pbdb[-which.gsg, c("accepted_rank", "flags", "difference")])[c(3, 12, 2, 8, 1), , -1]




## FUNCTIONS -------------------------------------------------------------------

## Function to add higher taxonomic names (phylum, class, order, etc.) for PBDB
## genus (and subgenus) names.
# g = Vector (sequence) of number of genus names to process.
# gen.order = Vector of ordered PBDB genus (and subgenus) names.
# which.gsg = Vector of indices for PBDB entries tagged as accepted genus or 
#   subgenus names.
# skip.unranked = Vector of unranked (non-marine) names to skip. (Note exception 
#   to still include Aves within Theropoda/Saurischia/etc.)
# pbdb = data frame of all PBDB occurrences.
#
# Output is a list, with each item the taxonomy for a single genus. Extends LAD
# to 'Recent' if genus is extant and splits subgenus names into genus and
# subgenus components.
prep.PBDB <- function(g = 1, gen.order = NULL, which.gsg = NULL, 
                      skip.unranked = NULL, pbdb = NULL) {
  scales <- c("phylum", "subphylum", "superclass", "class", "subclass", 
              "infraclass", "superorder", "order", "suborder", "infraorder", 
              "section", "subsection", "superfamily", "family", "subfamily", 
              "tribe", "genus", "subgenus")
  out <- data.frame(Phylum = character(1), Subphylum = character(1), 
                    Superclass = character(1), Class = character(1), 
                    Subclass = character(1), Infraclass = character(1), 
                    Superorder = character(1), Order = character(1), 
                    Suborder = character(1), Infraorder = character(1), 
                    Section = character(1), Subsection = character(1), 
                    Superfamily = character(1),Family = character(1), 
                    Subfamily = character(1), Tribe =  character(1), 
                    Genus = character(1), Subgenus = character(1), 
                    Species = "sp.", PBDBNumber = integer(1), 
                    stringsAsFactors = FALSE)
  out$Genus <- as.character(pbdb$accepted_name[which.gsg][gen.order[g]])
  wh <- which.gsg[gen.order[g]]
  out$PBDBNumber <- pbdb$accepted_no[wh]
  out$max_ma <- as.numeric(pbdb$firstapp_max_ma[wh])
  out$min_ma <- as.numeric(pbdb$lastapp_min_ma[wh])

  # Implement 'Pull-of-the-Recent' extension:
  if (any(pbdb$is_extant[wh] == "extant"))
    out$min_ma <- 0
  
  # Properly assign subgenera and genera:
  if (pbdb$accepted_rank[wh] == "subgenus") {
    split.subgenus <- strsplit(out$Genus, " ")[[1]]
    out$Genus <- as.character(split.subgenus[1])
    out$Subgenus <- as.character(gsub("[()]", "", split.subgenus[2]))
  }
  parent <- pbdb[which(pbdb$accepted_no == pbdb$parent_no[wh]), ][1, ]
  child_rank <- pbdb$accepted_rank[wh]

  # In rare cases (e.g., Devonocoryphe (Devonocoryphe)), a subgenus is parented
  # to an invalid "nomen" genus. This creates a blank parent (all NAs). Here, we
  # identify these cases and treat them separately. (Using taxon_no instead of
  # accepted_no because taxon_no includes such invalid taxa)
  alt.parent <- pbdb[which(pbdb$taxon_no == pbdb$parent_no[wh]), ][1, ]
  ignore_this <- FALSE
  if (all(is.na(parent)) &
      (alt.parent$difference == "nomen dubium" |
       alt.parent$difference == "nomen nudum" |
       alt.parent$difference == "nomen oblitum" |
       alt.parent$difference == "nomen vanum"))
    ignore_this <- TRUE
  
  # In cases where the priority opinion involves BOTH a correction AND elevation
  # of a subgenus to genus rank, the genus is orphaned (placed within a higher
  # taxon but not assigned a genus). Examples include Erioptera (Hoplolabis) and
  # Otolithus (Chrysophris). The following corrects for these rare cases. It is
  # placed within the repeat loop to allow for the rare instance where the
  # reassignment of a subgenus in another genus results in its being parented in
  # a subgenus (which can sometimes happen when genera are later reranked as a
  # subgenus; E.g., Amphistrophiella (Amphistrophiella) parented as
  # Amphistrophia (Amphistrophiella)).
  
  if (!ignore_this) {
    # The all is.na occurs in rare cases (e.g., Devonocoryphe (Devonocoryphe)),
    # when a subgenus is parented to a nomen nudum genus. In this case, we skip
    # the repeat loop below and remove the subgenus from the list of genera.
    
    repeat {
      
    # i.e., if parent is above genus and child is below genus (and there is no
    # genus), elevate the subgenus name to genus rank:
      if ((parent$accepted_rank != "genus" & parent$accepted_rank != "subgenus") &
          child_rank == "subgenus") {
        out$Genus <- out$Subgenus
        out$Subgenus <- ""
      }

      # If parent is a known unranked clade (excluding parents of birds), skip
      if (parent$accepted_name %in% skip.unranked &
          (all(!(c(out$Class == "Aves", out$Subclass == "Enantiornithes", 
                 out$Subclass == "Ornithurae")))))
        ignore_this <- TRUE

      # Assign parent name if a named rank
      if (parent$accepted_rank %in% scales) {
        out[1, which(scales == parent$accepted_rank)] <-
          as.character(parent$accepted_name)
        
        # For taxa with large numbers of cladistics-based unranked taxa, it is
        # common for a taxon to be parented to another name also deemed at that
        # rank (e.g., class Trilobita parented to class Artiopoda and class
        # Mammalia parented to class Osteichthyes). Redefining 'scales' forces
        # this to that only use the first correctly "ranked" parent as moves up
        # the parenting structure.
        scales <- scales[1:(which(scales == parent$accepted_rank) - 1)]
      }
      
      # Override subgenus treatment if parent's rank is also subgenus
      if (parent$accepted_rank == "subgenus") {
        split.subgenus <- strsplit(parent$accepted_name, " ")[[1]]
        out$Genus <- as.character(split.subgenus[1])
        out$Subgenus <- as.character(gsub("[()]", "", split.subgenus[2]))
      }
      
      # update for new parenting
      parent <-
        pbdb[which(pbdb$accepted_no == parent$parent_no), ][1, ]
      child_rank <- parent$accepted_rank
      if (all(is.na(parent)))
        break
    }
  }
  
  # Manually delete if parent is "nomum" or assigned unranked to skip
  if (ignore_this)
    out[1, ] <- NA
  
  return(out)
}



# Identify possibly problematic homonym genera
sort(table(pbdb$accepted_name[which.gsg]), decreasing = TRUE)[1:30]

# Note different parents (one is a brachiopod and one is a decapod)
pbdb[which(pbdb$accepted_name == "Varuna"), ]



## Format the PBDB data using a parallel-computing environment ---------------

# Version using parallel computing:
require(data.table) # Required below for merging parallel lists into dataframe
require(snowfall)
(t.start0 <- Sys.time())

# Initialize
gsgs <- pbdb[which((pbdb$accepted_rank == "genus" |
                      pbdb$accepted_rank == "subgenus") &
                     (pbdb$taxon_name == pbdb$accepted_name)), ]
# Note that including ' & (pbdb$taxon_no == pbdb$accepted_no)' within this
# subset will exclude ~ 700 subgenera that were reranked as genera in Sepkoski's
# Compendium. Excluding it means both versions will be included below, as a
# duplicate (using the accepted_name and accepted_no), but the duplicates are
# secondarily removed below (in the 'unique' line).
gsgs.ordered <- gsgs[order(gsgs$flags, gsgs$difference), ]
which.first.valid.gsg <- unique(gsgs.ordered$accepted_no)
valid.gsg.matches <- match(which.first.valid.gsg, gsgs.ordered$accepted_no)
valid.gsgs <- pbdb[row.names(gsgs.ordered[valid.gsg.matches, ]), ]
valid.gsgs <- valid.gsgs[which(valid.gsgs$flags == "" | valid.gsgs$flags == "F"), ]
which.gsg <- as.integer(rownames(valid.gsgs))
cat("Processing", length(which.gsg), "(sub)genera\n")
gen.order <- order(pbdb$accepted_name[which.gsg])
gen.seq <- seq_along(gen.order)

# Some non-marine taxa (especially tetrapods like dinosaurs) in the PBDB are
# assigned unranked names. These can not easily be removed once built. When a
# parent includes these names, skip processing them and move on to other genera.
skip.unranked <- c("Dinosauromorpha", "Dinosauriformes", "Dinosauria", 
                   "Eudinosauria", "Saurischia", "Ornithischia", "Theropoda", 
                   "Coelurosauria")
# Fast test batch:
# gen.seq <- 1:10000
  
# Set up computer cluster
require(parallel)
cpus <- parallel::detectCores() # Number of CPUs to cluster together
# sfSetMaxCPUs(cpus)			      # Use if plan more than 32 CPUs
sfInit(parallel = TRUE, cpus = cpus, slaveOutfile = "initfile") # Initialize cluster
stopifnot(sfCpus() == cpus)		    # Confirm set up CPUs properly
stopifnot(sfParallel() == TRUE)		# Confirm now running in parallel
sfExportAll()				            # Export all libraries, files, & objects

# Execute the function
prep <- NA
prep <- sfLapply(x = gen.seq, fun = prep.PBDB, gen.order = gen.order, 
                 which.gsg = which.gsg, skip.unranked = skip.unranked, 
                 pbdb = pbdb) # Version without load-balancing
sfStop()
output <- data.table::rbindlist(prep)
(Sys.time() - t.start0)  # ~ 13 minutes with 8 CPUs, ~5 minutes w/ 20 cores, 3 minutes w/ 112 cores
head(output)
beepr::beep(3)

# Remove subgenera parented to "nomen" or non-marine unranked genus parents:
nrow(output)
gen.seq2 <- 1:nrow(output)
wh.remove <- 
  sapply(gen.seq2, function(gen.seq2) all(is.na(output[gen.seq2, 1:18])))
output <- output[!wh.remove, ]
nrow(output)
beepr::beep()


## Add named geological intervals to stratigraphic ranges ----------------------

# This is just a temporary algorithm. Run UpdateAges&DivCurve.R for more
# comprehensive code, which additionally (1) adds ranges for taxa in Sepkoski
# Compendium, (2) interfaces with WoRMS to confirm extinct/extant status (and
# setting min_ma to 0, if extant), and (3) updates the Compendium dates from
# Gradstein, et al. (2020) to the 2024 ICS Geologic Time Scale.

# strat_names <- read.csv("https://www.paleobiodb.org/data1.2/intervals/list.csv?all_records&vocab=pbdb")
strat_names <- read.csv("strat_names.csv")
head(strat_names)
## Restrict to the default geochronological scale (ICS 2024) epochs.
## Other options includes "eons", "eras", "periods", "epochs", and "ages", etc.
epochs <- strat_names[which(strat_names$type == "epoch" &
                              strat_names$scale_no == 1), ]
## Add in Ediacaran, too:
edia <- strat_names[which(strat_names$interval_name == "Ediacaran"), ]
epochs <- rbind(epochs, edia)
epochs[, 1:5]
output$max_age <- character(1)
output$min_age <- character(1)

for(int in 1:nrow(epochs)) {
  wh.FAD <- which(output$max_ma > epochs$t_age[int] 
                  & output$max_ma <= epochs$b_age[int])
  wh.LAD <- which(output$min_ma >= epochs$t_age[int] 
                  & output$min_ma < epochs$b_age[int])
  output$max_age[wh.FAD] <- as.character(epochs$interval_name[int])
  output$min_age[wh.LAD] <- as.character(epochs$interval_name[int])
}

# Special work-around for singletons (only problematic for those that occur on a
# boundary):
wh.singleton <- which(output$max_ma == output$min_ma)
output$min_age[wh.singleton] <- output$max_age[wh.singleton]

# Recent is included for extant taxa (although no PBDB taxa have FADs = 0):
wh.Recent.FAD <- which(output$max_ma == 0)
wh.Recent.LAD <- which(output$min_ma == 0)
output$max_age[wh.Recent.FAD] <- "Recent"
output$min_age[wh.Recent.LAD] <- "Recent"

head(output)

## Save output
# write.csv(output, file = "PBDBformatted.csv", row.names = FALSE)



## Check for homonyms and possibly duplicate names -----------------------------

# Most genera with multiple entries are legitimate, caused by listing the genus
# as a whole, plus each subgenus separately. Saves the list to file specified
# below.

# Do you want to return the list of genera with subgenera? (DEFAULT = FALSE)
return.subgenera <- FALSE

mults <- sort(table(output$Genus), decreasing = TRUE)
mults <- mults[mults >= 2]
head(mults, 20)
file.name <- "multiGenera.txt"
sq <- 1:17     # Higher taxonomy columns (Phylum <---> Genus)
cat("The presence of subgenera, homonyms, and possible duplicates equals", 
    round(100 * length(mults) / nrow(output), 1), "% of the database\n", 
    file = file.name)
output[which(output$Genus == "Acanthopyge"), ] # Example of multiple subgenera

for(d in 1:length(mults)) {
  sus.gen <- names(mults[d])
  suspicious <- output[which(output$Genus == sus.gen), ]
  classes <- unique(suspicious$Class)
  
  # Identify likely subgenera:
  if (length(classes) == 1L)
    if (return.subgenera & all(sapply(sq, function(sq)
      nrow(unique(suspicious[, Phylum:Genus])) == 1)) &
      suspicious$Subgenus[1] == "" & all(suspicious$Subgenus[-1] != ""))
      cat("OK: Genus", names(mults[d]), "has", nrow(suspicious) - 1, 
          "subgenera.\n", file = file.name, append = TRUE)

  # Identify likely problematic duplicated entries:
  if (any(sapply(sq, function(sq) 
    nrow(unique(suspicious[, Phylum:Genus])) != 1)) & length(classes) == 1L)
    cat("WARNING: Genus", names(mults[d]), 
        "may be a duplicate genus entry. Investigate and override in PBDB if true.\n", 
        file = file.name, append = TRUE)
  
  # Identify likely legitimate homonyms:
  if (length(classes) == 2L)
    cat("OK: Genus", names(mults[d]), 
        "is a homonym for genera in difference classes:", classes, "\n", 
        file = file.name, append = TRUE)
}
beepr::beep()

# For any genera tagged as "WARNING" that represent duplicate entries of the
# same name, the best-practice is to add a new taxon to the PBDB that overrides
# the duplicate (and to re-classify their occurrences). But confirm first that
# the genus isn't listed twice in Sepkoski's Compendium, in which case we should
# keep both entries in the PBDB for legacy purposes. In these cases, change one
# of Sepkoski's opinions to "objective synonym of" and add a note in the
# Comments field to let future users understand why the opinion was changed.
# (Ideally, make the entry with zero occurrences and no other parenting opinions
# be the one to point to the more completely fleshed out genus.)

homonyms <- sort(table(pbdb$accepted_name[which.gsg]), decreasing = TRUE)
homonyms <- homonyms[which(as.integer(homonyms) > 1)]
write.table(names(homonyms), file = "known_homonyms.csv", row.names = FALSE, col.names = FALSE)
# All likely duplicates have been corrected (as of Nov. 21, 2022). The following
# are confirmed (or very likely) homonyms (occurring within the same class):
# Bicarinella, Billingsites, Coronopsis, Curculionites, Desmoceras, Didymoceras,
# Domiporta, Eleutherodon, Eolampra, Eumysops, Glypta, Heterosoma, Hoffeinsia,
# Hysteroceras, Liocaris, Longhuaia, Mesocorixa, Mesodiadema, Mesorthophlebia,
# Nectosaurus, Okruhliak, Onychoceras, Pamirophyllum, Parachorista, Pternodus,
# Pterocephalops, Saurornithoides, Sharovia, Sinoperla, Strophomena, Tinosaurus,
# Treptoceras. Many others exist in different classes; see "known_homonyms.csv"
# for fuller list.


## Post-processing marine-only standardizations --------------------------------

## Post-process to focus on marine taxa and to standardize taxonomy with mine
# x <- read.csv(file = "PBDBformatted.csv", header = TRUE, stringsAsFactors = FALSE)
x <- output
head(x)

# Remove terrestrial and non-marine taxa, but include marine tetrapods. List
# modified from Bush and Bambach (2015) to explicitly include three cetacean
# suborders (because Cetacea sometimes listed within Order Artiodactyla in PBDB)
# and known marine xiphosurans and eurypterids, and to exclude Myriapoda,
# Kannemeyeriiformes, Pelycosauria, Theriodontia, Therocephalia, freshwater
# Branchiopoda (= conchostrans, notostracans, cladocerans, etc.), all known
# arachnid taxa (because many arachnids are getting listed in the xiphosuran
# download), and a variety of fishes, tetrapods, and others. WoRMS relied on
# heavily for extant taxa. "Fish" genera (in paraphyletic sense) compiled from
# several publications: Schnetz, et al. (2024 Paleobiology, Paleozoic
# chondrichthyans), Sallan and Coates (2010 PNAS, Devonian fishes), Sallan et
# al. (2018 Science, mid Paleozoic "fishes"), and Romano, et al. (2016
# Biological Reviews, Permo-Triassic bony fishes). Note only genera restricted
# to freshwater environments are listed herein; euryhaline genera thought to
# have lived in both freshwater, brackish, and unambiguously marine settings are
# included as "marine." (Some early tetrapod genera may also be listed herein,
# innoculously because removed later, and a few early tetrapods that were
# genuinely marine will be added in last.) Thanks to Lauren Sallen for advice on
# identifying these.
non.marine <- read.csv(file = "non_marines.csv", header = FALSE)[, 1]
length(non.marine)
non.marine[1:40]

# Most tetrapods are terrestrial, so remove by default:
tetrapods <- c("Mammalia", "Reptilia", "Amphibia", "Aves", "Synapsida")

# Then add back in the known marine tetrapods, the sole marine amphibian
# (Trematosauridae), and some known marine xiphosuran, pterosaur, decapod, bird,
# early tetrapod, etc. taxa that are otherwise typically non-marine (and may
# have been removed in taxa above). WoRMS relied on heavily for extant taxa.
marine.exceptions <- read.csv(file = "marine_exceptions.csv", header = FALSE)[, 1]
length(marine.exceptions)
marine.exceptions[1:40]

# Confirm no conflicts. (Marine.exceptions lists what gets added and non.marine
# removes them)
if (any(non.marine %in% marine.exceptions) == TRUE)
  stop("Reconcile the conflict between the taxa in 'marine.exceptions' and 'non.marine'\n")
# non.marine[which(non.marine %in% marine.exceptions)]

# Pterosaur genus list from Dean, Mannion, and Butler (2016, Palaeontology,
# Appendix S1) and Longrich, et al. (2018, PLOS Biology) and family list from
# Bestwick, Unwin, Butler, Henderson, and Purnell (2018, Biological Reviews).
# Birds provided from Alex Clark (Field Museum). Early panarthropods
# ('lopopods', onychophorans, etc.) from Aria and Caron (2024) and  Smith and
# Ortega-Hernandez (2014).

# Extract the known marine taxa (in lineages that are typically non-marine):
sq <- 1:nrow(x)
marine.vert.exceptions <- 
  x[sapply(sq, function(sq) any(marine.exceptions %in% x[sq, ])), ]
beepr::beep()

# Remove the non-marine taxa, and all tetrapods, including marine tetrapods (in
# case of tetrapods that were not coded as members of Tetrapoda in the PBDB):
marine.typical <- 
  x[!sapply(sq, function(sq) any(c(non.marine, tetrapods) %in% x[sq, ])), ]
# Note this will cause some duplicates (from adding in exceptions that weren't
# tetrapods). This is acceptable to ensure they are not missed, as they will be
# removed below.
beepr::beep()

# Combine the typical marine taxa plus add back in the known marine tetrapods,
# etc.:
marine.taxa <- rbind(marine.typical, marine.vert.exceptions)
sort(table(marine.taxa$Class), decreasing = FALSE)
nrow(x)
nrow(marine.taxa)

# Remove confirmed form taxa (e.g., ammonoid aptychi and dissociated crinoid
# columnals, holdfasts, and anal sacs). Including these "genera" would
# artificially inflate standing genus richness. (Also including some non-marine
# pterosaurs, birds, and other tetrapods that the code above does not remove.
# Birds provided from Alex Clark at Field Museum.) Because Sepkoski treated
# "Problematica" (= incertae sedis) in his list of animal higher taxa, the PBDB
# maintains that "all" Problematica (including plants, algae, and Archean
# prokaryotes) are sometimes included as Animalia. This list also adds in the
# obvious incertae sedis non-animals (like acritarchs, Gunflintia,
# Sinosabellidites, plus "plant" genera listed in Andrews (1970, Index of
# Generic Names of Fossil Plants).
known.forms <- read.csv(file = "known_forms.csv", header = FALSE)[, 1]
length(known.forms)
known.forms[1:40]

# Confirm no conflicts. (Marine.exceptions lists what gets added and known.forms
# removes them.) Note it's OK for some duplicates in known.forms and non.marine
# because ensures not included in the marine list.
if (any(known.forms %in% marine.exceptions) == TRUE)
  stop("Reconcile the conflict between the taxa in 'marine.exceptions' and 'known.forms'\n")
# known.forms[which(known.forms %in% marine.exceptions)]

wh.forms <- which(marine.taxa$Order %in% known.forms | 
                    marine.taxa$Family %in% known.forms | 
                    marine.taxa$Genus %in% known.forms | 
                    marine.taxa$Subgenus %in% known.forms)
marine.taxa <- marine.taxa[-wh.forms, ]

# Special rule for Anaptychus, which is a junior subjective synonym of Sidetes
# and a homonym of a decapod (which is j.s.s. of Ala, so neither should be
# present)
wh.anaptychus <- which(marine.taxa$Genus == "Anaptychus" &
                         marine.taxa$Class == "Cephalopoda")
if (length(wh.anaptychus) > 0L)
  marine.taxa <- marine.taxa[-wh.anaptychus, ]

# Special rule for non-animal "Problematica" genera listed in Andrews (1970)
# that are homonyms of valid marine animal genera.
wh.problematica <- which((marine.taxa$Genus == "Hexagonaria" &
                         marine.taxa$Phylum == "Problematica") |
                           (marine.taxa$Genus == "Itieria" &
                              marine.taxa$Phylum == "Problematica") |
                           (marine.taxa$Genus == "Lenaella" &
                              marine.taxa$Phylum == "Problematica"))
if (length(wh.problematica) > 0L)
  marine.taxa <- marine.taxa[-wh.problematica, ]
nrow(x)
nrow(marine.taxa)

## Remove duplicates

# Note ~ 300 duplicated PBDBNumbers (typically caused by the 'cbind' above)
PBDBNumber.duplicates <- duplicated(marine.taxa$PBDBNumber)
table(table(marine.taxa$PBDBNumber))
# To observe them
PBDBNumber.dups <- marine.taxa[PBDBNumber.duplicates, ]
PBDBNumber.dups <- PBDBNumber.dups[order(PBDBNumber.dups$Genus), ]
head(PBDBNumber.dups[, c(1, 4, 8, 17:18, 20)])
# Note they are true duplicates!
marine.taxa[which(marine.taxa$PBDBNumber == "7374"), ]
marine.taxa[which(marine.taxa$PBDBNumber == "510947"), ]
# And typically added at the end (consistent with the processing above)
summary(which(PBDBNumber.duplicates == TRUE))
# So let's delete them:
marine.taxa.no.dups <- marine.taxa[-which(PBDBNumber.duplicates == TRUE), ]
nrow(marine.taxa.no.dups)


# Use work-around to reduce chances of genuine homonyms (matching only when ALL
# higher taxonomy is identical)
duplicates <- duplicated(marine.taxa.no.dups[, 1:18])

# There are still 51 likely duplicates (but with different PBDB Numbers)
length(which(duplicates == TRUE))

# Extract out duplicates and view them in sorted manner
gen.dups <- marine.taxa.no.dups[which(duplicates == TRUE), ]
gen.dups <- gen.dups[order(gen.dups$Phylum, gen.dups$Class, gen.dups$Order), ]
head(gen.dups[, c(1, 4, 8, 17:18, 20)])

# Note they are true duplicates (and not homonyms)!
for(g in 1:nrow(gen.dups)) {
  print(marine.taxa[which(marine.taxa$Genus == gen.dups$Genus[g] &
                            marine.taxa$Subgenus == gen.dups$Subgenus[g]), c(1, 4, 8, 17:18, 20)])
  cat("\n\n")
}
# Most occur from duplicates in Sepkoski's Compendium that were subsequently
# merged as "objective synonyms" and in nearly (but not all) cases, the
# currently used PBDB Number is the first one listed. So we can delete the
# subsequent duplicates. Other cases include rerankings (e.g., a Sepkoski genus
# now deemed a subgenus) or obsolete recombinations, and in these cases there is
# less predictability in which row to use. But using the first row ought to be
# acceptable (and better than including duplicates).

# So let's delete them:
marine.taxa.no.dups <- marine.taxa.no.dups[-which(duplicates == TRUE), ]

nrow(x)
nrow(marine.taxa)
nrow(marine.taxa.no.dups)

# Save object
# write.csv(marine.taxa.no.dups, file = "PBDBformatted_NoTerr.csv", row.names = FALSE)
# x <- read.csv(file = "PBDBformatted_NoTerr.csv", header = TRUE, stringsAsFactors = FALSE)


## Run following to manually combine my database and the PBDB databases

# (1) In Excel, open the "postSizes.tab" or "postLH.tab" file (may need to
#     re-export from FMP and use SelectCols.R to align proper columns) and
#     re-save as "PreSizes_withPBDB.tab" or "PreLH_withPBDB.tab" (MANUALLY ADD
#     THE ".TAB" TO FILE NAME TO FORCE AS TAB-DELIMITED INSTEAD OF TEXT FILE
#     FORMAT.) Open "PBDBformatted_NoTerr.csv" and copy this data into the
#     combined database. Manually delete any "NA"s in early and late ages. If
#     using both a "mode" and "constant" LH data treatment, only need to
#     propogate sizes using one of these data sets, as the size propogations are
#     the same for both.

# (2) Open here and run following code to remove duplicated genus entries. 

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Documents/GSA (& NAPC)/2024NAPC/Higher taxa eco diversity")
# pre <- read.delim(file = "PreSizes_Constant_withPBDB.tab", stringsAsFactors = FALSE)
# pre <- read.delim(file = "PreSizes_Mode_withPBDB.tab", stringsAsFactors = FALSE)
head(pre)
tail(pre)
str(pre)
dim(pre)
duplicate.G <- duplicated(pre$PBDB_GSG_Number)
table(duplicate.G)
# Note many of these are false positives, for genera not in PBDB (and therefore
# lacking PBDBNumbers)
pre$PBDB_GSG_Number[which(duplicate.G == TRUE)[1:100]]
# Only remove duplicates in the new additions (not inside the 'core' database,
# which occasionally have intentional duplicates of the same genus)
nrow.core <- 4985
new.Gs <- duplicate.G[(nrow.core + 1):nrow(pre)]
post <- pre[c(seq.int(nrow.core), (nrow.core + which(new.Gs == FALSE))), ]
dim(post)
# write.table(post, file = "PreSizes_Constant_withPBDB.tab", quote = FALSE, sep = "\t", row.names = FALSE)
# write.table(post, file = "PreSizes_Mode_withPBDB.tab", quote = FALSE, sep = "\t", row.names = FALSE)

# (3) Copy new (AND ONLY THE NEW!) entries by Phylum > Subphylum > Class >
#     Subclass > Order.

# (4) Add new IDNumbers (that pick up after those in the existing database), and
#     re-save.

# (5) Run entire PBDB-propogated data set (including core) using
#     IDBadHigherTaxa.R to identify any other possible taxonomic mis-alignments
#     or inconsistencies in my taxonomy and that of the PBDB. Common changes
#     include the following:

#  a. Changing suffixes (e.g., -acea to -oidea), adding superfamily and suborder
#     names when left empty in PBDB. See code in IDBadHigherTaxa.R for a
#     function to automate.

#  b. Some names have different ranks in my database and the PBDB. See code in 
#     IDBadHigherTaxa.R for a function to automate. Known instances include:

#   - ELEVATE the following subclass ranks to class rank:
#       (i)   arthropods Malacostraca (and set in Subphylum Crustacea)
#       (ii)  echinoderms Blastoidea and Parablastoidea

#   - LOWER class Opisthobranchia (using as senior synonym of Opisthobranchiata, 
#     per WoRMS) to order rank (and placing them in subclass Heterobranchia), 
#     such that Opisthobranchia is an order and their orders are suborders.

#   - Change order Cephalodiscida to Cephalodiscoidea (in class Cephalodiscida), 
#     per WoRMS.

#   - Place fish taxon Gasterosteiformes as a suborder within order 
#     Perciformes (not order Gasterosteiformes), per modification of 
#     Betancur-R, et al., 2013.

#   - Place fish taxon Scorpaeniformes as suborder Scorpaenoidei within order 
#     Perciformes (not order Scorpaeniformes), per modification of 
#     Betancur-R, et al., 2013.

#   - Treat order Notomyotida as an asteroid order, leaving the suborder 
#     unnamed. (Note taxon is currently empty, so disused in PBDB.)

#   - Treat suborder Orchocladina as a sponge order, leaving the suborder 
#     unnamed, per Rigby, et al., 2004.

#   - Maintain suborder Phymosomatoida as an echinoid suborder, leaving the 
#     order UNCERTAIN.

#   - Elevate suborder Pygocephalomorpha to order rank (so order rank is not 
#     left blank).

#   - Elevate infraclass Hoplocarida to subclass rank (in accordance to how 
#     treated in WoRMS). Orders within Hoplocarida include Stomatopoda, 
#     Aeschronectida, and Palaeostomatopoda.

#   - Treat unranked, informal priapulid group Archaeopriapulida as an order,
#     with subtaxa families Ancalagonidae, Fieldiidae, Miskoidae, and Ottoidae 
#     and uncertain genera Lecythioscopa, Scolecofurca, and Singuuriqia. Note 
#     that because the PBDB treats (superphylum?) Scalidophora as a parent 
#     phylum for phyla Priapulida, Loricifera, and Kinorhyncha, most PBDB
#     "scalidophorans" are actually priapulids. (Until larger ranks are included 
#     in my database, maintain Scalidophora as a separate phylum, for now.)
#     Exceptions include phylum Scalidophora (genera Eokinorhynchus, Eolympia, 
#     and Eopriapulites), phylum Loricifera (genus Eolorica), and phylum 
#     Kinorhyncha (no subtaxa currently in PBDB).

#   - Treat unranked Vetulocystida as a class (in phylum UNCERTAIN), including 
#     members of Vetulocystidae.

#   - Current consensus (Smith and Reich, 2013; Rahman, et al., 2019) considers 
#     the ophiocistioids as a branch of very early stem holothuroids. To 
#     maintain them as a distinct group, reduce class Ophiocistioidea to order 
#     rank (as ophiocistioids currently lack order names), and place in class 
#     Holothuroidea.

#   - Following WoRMS (and more recently PBDB), elevating Aplacophora to 
#     superclass rank and Caudofoveata and Solenogastres to class rank.

#  c. CHANGE rank names for order Rhombifera (Subphylum Pelmatozoa, Class
#     Cystoidea, Subclass Hydrophoridea) to Class 'rhombifera' (Subphylum 
#     Blastozoa, Orders Caryocystitida, Glyptocystitida, Hemicosmitida, and 
#     UNCERTAIN), and CHANGE Subphylum Pelmatozoa to Blastozoa.

#  d. ADD new taxonomic names for following:

#       (i)    Class Hyolitha (in Phylum Hyolitha) for orders Hyolithida, 
#              Orthothecida, Sulcavitida, Toxeumorphorida. Class Hyolithomorpha 
#              is treated as an unused synonym of class Hyolitha. Orders 
#              Circothecida and Exilithecida (in class Orthothecimorpha) are 
#              tentatively downranked as suborders Circothecina and Exilithecina 
#              within order Orthothecida, and outdated class Orthothecimorpha 
#              downranked as order Orthothecida (within class Hyolitha).

#       (ii)   Class Dipnomorpha for infraclasses Dipnoi and subclass
#              Dipnotetrapodomorpha. (And duplicate superorder Porolepidimorpha
#              as infraclass, in subclass Dipnotetrapodomorpha.)

#       (iii)  Class Tentaculita / phylum Tentaculita for order Tentaculitida 
#              (and replace class Tentaculitoidea with Tentaculita).

#       (iv)   Phylum Priapulida and (reranking unranked clade 
#              Palaeoscolecidomorpha as a superclass) for class Palaeoscolecida.

#       (v)    Class Agmata for order Volborthellida and family Salterellidae 
#                (now in phylum Cnidaria, cf., Vayda, et al., 2025).

#       (vi)   Add name UNCERTAIN for any phylum, class, order, or family that  
#                is blank.

#       (vii)  Change phylum Problematica to UNCERTAIN.

#       (viii) Assign order Sachitida to class Diplacophora (following Vinther 
#                and Nielsen 2005 and Parkhaev and Demidenko, 2010), although
#                their current status is in some unknown molluscan class. The
#                order also includes Halkieriidae and Wiwaxiidae.

#  e. CHANGE the following names (typically alternative spellings or 
#     archaic synonyms):
#       (1)    cephalochordate order Amphioxi to Amphioxiformes (and place in 
#                class Leptocardii)
#       (2)    vertebrate class Actinopterygii to Actinopteri (with 
#                Actinopterygii as superclass)  
#       (3)    sponge class Demospongea to Demospongiae 
#       (4)    sponge class Archeocyatha to Archaeocyatha
#       (5)    fish order Birkeniida to Birkeniiformes
#       (6)    annelid subclass Aciculata to Errantia
#       (7)    set starfish infraclass Neoasteroidea within subclass 
#                Ambuloasteroidea
#       (8)    bryozoan order Cryptostomata to Cryptostomida 
#       (9)    bryozoan order Ctenostomata to Ctenostomatida 
#       (10)   bryozoan order Cyclostomata to Cyclostomatida
#       (11)   bryozoan order Cystoporata to Cystoporida  
#       (12)   bryozoan order Fenestrata to Fenestrida 
#       (13)   bryozoan order Trepostomida and Trepostomata to Trepostomatida
#       (14)   diploporitan "superfamily" Glyptosphaeritida to 
#                Glyptosphaeritidoidea (in order Glyptosphaeritida) 
#                (See below for details on other diploporitans)
#       (15)   superfamily Mosasauria to superfamily Mosasauroidea
#       (16)   brachiopod family Disciniidae to Discinidae (and treat order
#                Discinida as j.s.s. of order Lingulida)
#       (17)   use subclass Hexacorallia instead of synonym Zoantharia
#       (18)   per WoRMS, use order Octocorallia instead of Gorgonacea

#  f. Bivalve classification primarily follows that of Carter, et al. (2011),
#     the basis of the forthcoming Treatise). For outdated names (i.e., bivalves
#     listed in PBDB without a family assignment and placed in outdated orders
#     used by Sepkoski but not mentioned in Carter, et al., 2011), use the
#     following rules: orders Anomalodesmacea, Myoida, Thraciida, and Trigoniida
#     are in infraclass Heteroconchia. Members of superfamily Grammysioidea
#     (Families Grammysiidae and Sanguinolitidae) and superfamily
#     Lyrodesmatoidea (Family Lyrodesmatidae) are in Order UNCERTAIN and
#     infraclass Heteroconchia (albeit in different superorders). Order
#     Pterioida is in Subclass Pteromorphia. Treat "cohorts" Mytilomorphi,
#     Ostreomorphi, Palaeoheterodonta (= senior synonym of Uniomorphi), and
#     Heterodonta (= senior synonym of Cardiomorphi) as rank superorder. Within
#     suborder Anomiidina, rank unranked clades Anomioidei, Aviculopectinoidei,
#     and Monotoidei as sections (with unranked subclades Anomioitei and
#     Dimyoitei as subsections). (For Ostreomorphi and others above, this has
#     the effect of making my propogation algorithm more conservative because
#     smaller ranks are treated as larger, more inclusive ones.) Because of lack
#     of available ranks, superorders Cardiiformii, Imparidentia and
#     Pholadiformii and subcohorts Carditioni and Cardioni and infrasubcohorts
#     Cardiidia and Lucinidia (and underlying megaorders) are all treated as
#     functionally equivalent to superorder Heterodonta. (This would be a great
#     opportunity to artifically downrank the subclades into sections, etc. to
#     allow these intermediately ranked clades!)

#  g. For opisthobranch and pulmonate gastropods, use a modified version of
#     Bouchet and Rocroi (2005), which is also consistent with how WoRMS treats
#     their clade names. Treat Heterobranchia as the subclass that includes the
#     orders Allogastropoda (now paraphyletic, so replacing Allogastropoda with
#     order UNCERTAIN and assigning to infraclass 'lower Heterobranchia'), plus
#     now-rearranged opisthobranch and pulmonate subgroups (now divided into
#     subclades because the traditional names are non-monophyletic). Replace
#     Sepkoski's order Heterostrophia with Heterostropha and his order
#     Cephalaspida as Cephalaspidea. With the division of the opisthobranchs,
#     otherwise-unclassified Heterostropha are classified herein as order
#     UNCERTAIN and in subclass Heterobranchia. Unranked clades Acteonimorpha
#     and Ringipleura ranked as superorder. See "Gastropod_taxononomy.docx" for standardized
#     taxonomic structure used, which merges (as best as can be done) the
#     taxonomic structure of the PBDB, Bouchet and Rocroi 2005, Bouchet, et al.
#     2017, and WoRMS. For now-defunct order Neotaenioglossa, replacing with
#     UNCERTAIN Caenogastropoda because likely polyphletic and split among
#     multiple superorders.

#  h. Following WoRMS (and modified from Bouchet and Rocroi, 2005), downrank
#     order Neogastropoda as a suborder in downranked superorder-to-order
#     Latrogastropoda < subclass Caenogastropoda.  Place suborder
#     Bellerophontina in order Bellerophontida. Treat unranked clades
#     Architaenioglossa, Sorbeoconcha, and Hypsogastropoda as caenogastropod
#     orders, and Campanilimorpha and Cerithiimorpha as sections of
#     Sorbeoconcha. Rerank infraorder Littorinimorpha and unranked clade
#     Ptenoglossa as hypsogastropod suborders. Downrank (disused in PBDB)
#     ptenoglossan suborder Gymnoglossa to infraorder (but place Muricoidea
#     within Neogastropoda). See other details of gastropod taxonomy above (and
#     in "Gastropod_taxonomy.docx"). Treat Annulariinae within Annulariidae 
#     (per WoRMS), not Pomatiidae.

#  i. For certain vertebrate groups whose taxonomy is often cladistically based
#     on unranked taxa, use the following ranks. Treat order Ichthyosauria as
#     suborder in Order Ichthyopterygia (with 'stem Ichthyopterygia' for
#     paraphyletic stem group), and large inclusive clades as ranked clades
#     (e.g., Hueneosauria as infraorder [which is allowed to be polyphyletic
#     because includes both stem and crown ichthyopterygians], Grippidia [=
#     Grippiidae], Merriamosauria, Longipinnati as sections, and Baracromia,
#     Mixosauria, and Parvipelvia as subsections, and Neoichthyosauria as
#     superfamily). Treat order Nothosauria as suborder (and superfamily
#     Pachypleurosauroidea within suborder Pachypleurosauria) within order
#     Eosauropterygia; treat them and orders Placodontia and Plesiosauria within
#     superorder Sauropterygia. (For placodonts, also place suborders
#     Cyamodontoidea and Placodontoidea as superfamilies. For nothosaurs, treat
#     order Nothosauria as a superfamily.) Maintain unranked clade Pistosauria
#     as an eosauropterygian suborder.

#  j. Use (only) the following subphylum names for (primarily marine) taxa:

#       (i)  Arthropods: Arachnomorpha, Chelicerata, Crustacea, and 
#            Artiopoda (= Trilobita + Nektaspidida + class[!] Vicissicaudata +
#            Agnostida)

#       (ii) Echinoderms: Subphyla for echinoderms are largely informal, often 
#            paraphyletic, and an area of much debate. For consistency, using 
#            following until better resolved, which elevates unranked clades 
#            Asterozoa, Echinozoa, Blastozoa (including Crinozoa) to subphylum 
#            status, and elevates Eleutherozoa and Pelmatozoa to unranked clades

#            (1) Asterozoa (classes Asteroidea, Ophiuroidea, Somasteroidea, and 
#                Stenuroidea)
            
#            (2) Echinozoa (classes Echinoidea, Holothuroidea [including 
#                Ophiocistioidea])

#            (3) Blastozoa  (classes Blastoidea, Parablastoidea, Paracrinoidea, 
#                Soluta, and Crinoidea plus paraphyletic-to-polyphyletic
#                informal classes 'diploporita', 'eocrinoidea' (downranking
#                Lepidocystoidea as suborder within Imbricata), 'rhombifera' and
#                treating class Coronoidea as subclass within blastoids, with
#                order Coronata). Eocrinoids come in elliptical and flattened
#                thecal forms. To propagate sizes (shapes) properly, creating
#                "orders" UNCERTAIN-Flat and UNCERTAIN-Round, when the shape is
#                known. Following Waters and Macurda (2026), treating
#                polyphyletic Spiraculata as informal infraclass 'spiriculates'
#                with superorders Stomatoblastida and Tubuloblastida and
#                fissiculates elevated to superorder Fissiculata.

#            (4) Remaining classes (non-radials Stylophora, Cincta, 
#                Ctenocystoidea and radials Helicoplacoidea, Helicocystoids
#                [Helicocystis], Edrioasteroidea, and stem echinoderm
#                Ctenoimbricata) are placed in informal subphylum 'stem
#                echinoderms'. Cyclocystoidea are placed in subphylum UNCERTAIN

#  k. For tetrapods, do not use the larger fish-inclusive Dipnotetrapodomorpha
#     as subclass (and Dipnomorpha for class). Instead, use subclass Theria
#     (etc.) for mammals (whales, sirenians, etc.); and place whales in order
#     Cetacea (and NOT Artiodactyla). Use subclass Eureptilia for reptiles
#     (plesiosaurs, ichthyosaurs, squamate mosasaurs, thalattosaurs, turtles,
#     etc.). Treat unranked clade Testudinata as a superorder and unranked clade
#     Thalassochelydia as section. For Aves, treat unranked clades Aequornithes
#     (for superorders Pelecanimorphae and Procellariimorphae and order
#     Gaviiformes) and Eurypygimorphae (order Phaethontiformes) as subclasses
#     and 'stem birds' for paraphyletic grouping of Hesperornithiformes and
#     Yanornithiformes. Treat unranked clades Procellariiformes and 
#     Sphenisciformes as orders.

#  l. Treat Xiphosura as a class that is a synonym with prior Class Merostomata,
#     despite how WoRMS ranks them. (Be aware that many PBDB "xiphosurans" are
#     terrestrial arachnids and not marine horseshoe crabs and the like.)
#     Because PBDB eurypterids are currently classified in PBDB in order
#     Eurypterida within order Xiphosurida, their eurypterid affiliation will be
#     over-ridden with the algorithm above. Assign members of suborders
#     Eurypterina and Stylonurina to order Eurypterida (in blank subclass) in
#     class UNCERTAIN. Following Lamsdell (2013), place order Eurypterida and
#     order Chasmataspidida (although a polyphyletic basal member closely
#     related to eurypterids), (and class Arachnida, for three early marine
#     genera) in unranked taxon Dekatriata, elevated to superclass status for
#     now, until a replacement class name is proposed.

#  m. Given ongoing difficulty in classification of chaetetids (most are 
#     demosponge form taxa with a few putative tabulates, c.f., West 2015
#     Treatise pp. 105-125 for discussion), assign all apparently legitimate
#     chaetetid families (e.g., Chaetetidae, Cryptolichenariidae but not those
#     likely not sponges) to subfamily rank (e.g., Chaetetinae,
#     Cryptolichenariinae, and maintaining other subfamily names if provided)
#     within Family Suberitidae (the assignment for genus Chaetetes), Order
#     Hadromerida (instead of Chaetetida), Subclass UNCERTAIN, Class
#     Demospongiae.

#  n. Move sponges placed by PBDB in order Calcaronea into subclass Calcaronea 
#     (sensu Rigby, et al. 2004) and place in order UNCERTAIN (unless the family
#     indicates a known order). Based on concensus in West, et al. (2015) that
#     polyphyletic, replace subclasses Tetractinomorpha and Ceractinomorpha with
#     UNCERTAIN. Use order Lychniscosa instead of Lychniscosida.

#  o. For converting Sepkoski's archaic polychaete orders, use Amphinomida for
#     Amphinomorpha, Eunicida for Eunicemorpha, Phyllodocida for
#     Phyllodocemorpha, Spionida for Spiomorpha, Scolecida (Family Arenicolidae)
#     for Drilomorpha, Terebellida for Terebellomorpha, Terebellida (Family
#     Flabelligeridae) for Flabelligerimorpha, and Sabellida (Family Serpulidae)
#     for Serpulimorpha (but allowing still-used order Sabelliditida, but
#     placing within Polychaeta instead of Annelida incertae sedis). Downrank 
#     infraclass Scolecida to order.

#  p. Based on recent work by Skovsted and Holmer and their group (mostly in
#     2008-2009), treating order Hyolithelminthida (including Hyolithellidae and
#     Torellellidae) and other members of Tommotiida (including Lapworthellidae,
#     Tannuolinidae, and Tommotiidae, assigned to order Tommotiida) as stem
#     brachiopods (Phylum Brachiopoda, UNCERTAIN subphylum). Allow Tommotiida to
#     serve as both a class and an order, given their taxonomic ambiguities.

#  q. Based on discoveries in Moysiuk, et al. (2017), retaining hyolithids in
#     their own phylum Hyolitha, as lophophorates that are possibly stem
#     brachiopods (like tommotiids). Allow Hyolitha to serve as both a phylum
#     and a class, given their taxonomic ambiguities.

#  r. For the branchiopods, primarily use WoRMS instead of PBDB, with class
#     Branchiopoda, subclasses Calmanostraca (extant all freshwater with orders
#     Acercostraca, Kazacharthra, and Notostraca), Sarsostraca (extant all
#     freshwater with order Anostraca), and Diplostraca (= Conchostraca, with
#     orders Laevicaudata and Spinicaudata and all cladoceran orders). Like PBDB
#     (and unlike WoRMS which subsumes the cladoceran taxa as individual
#     orders), treating Cladocera as a valid order and using WoRMS cladoceran
#     orders (Anomopoda, Ctenopoda, Haplopoda, and Onychopoda) as suborders.
#     Genera in outdated Subclass Phyllopoda with UNCERTAIN orders are placed in
#     Subclass UNCERTAIN because unclear whether diplostracans or notostracans
#     (although all appear to have been reassigned).

#  s. Follow Maletz (2014, basis of forthcoming Treatise revision) in treating
#     Graptolithina as a subclass in Class Pterobranchia. Assign subclass 
#     Cephalodiscida (order Cephalodiscoidea) as other pterobranch subclass
#     (unlike WoRMS, which treats as graptolith).

#  t. Do not override the following higher taxonomic homonyms! They are
#     distinct:
#     (i)   Families Ctenodontidae (bivalve and dipnoi fish)
#     (ii)  Bdelloidea are a rotifer class and an arachnid superfamily
#     (iii) Stolonifera are bryozoan suborder and octocoral order.
#     (iv)  Tentaculata is a ctenophore class and Tentaculita is a tentaculite
#           class.

#  u. Because the most recent crinoid classification (Wright, et al. 2017)
#     contains only two subclasses (camerates and pentacrinoids) for all 
#     crinoids (and infraclass Inadunata is essentially equivalent to subclass
#     Pentacrinoidea), elevate the parvclasses Disparida and Cladida to 
#     infraclass rank, but maintaining the order names. Allowed pentacrinoid 
#     subclasses include the stem inadunates, Disparida, Porocrinoidea, 
#     Flexibilia, Cyathoformes, Ampelocrinida (possibly paraphyletic), and 
#     Articulata. Subclass Eucamerata orders include the Diplobathrida, 
#     Monobathrida, and "stem eucamarates". (This has the effect of making my
#     propogation algorithm more conservative because smaller ranks are treated 
#     as larger, more inclusive ones.) See "Crinoid_taxonomy.docx" for summary.

#  v. Following Sheffield and Sumrall (2019), place the diploporitans in
#     "superfamilies" Glyptosphaeritidacea (= Glyptosphaeritidoidea) and
#     Asteroblastida in class 'diploporitan' because they are now polyphyletic.
#     Use class 'diploporita' only for members of order Sphaeronitida and any
#     traditional diploporitan NOT explicitly noted in their paper as not
#     monophyletic members of the Sphaeronitida clade. (In other words, the
#     default, for now, is to assume all diploporitans are within 'diploporita'
#     unless explicitly known not to be.)

#  w. Following Kroh and Smith (2010) for taxonomy of echinoids (opinions also
#     entered into PBDB), but adding superfamily and/or suborder rankings (which
#     they often avoided) in cases where an order is assigned and infraorder
#     subclades are assigned. Maintaining order Camarodonta infraorders:
#     Temnopleuridea (with families Temnopleuridae, Trigonocidaridae,
#     Zeuglopleuridae, and Glyphocyphidae) and Echinidea (with downranked
#     "superfamily" Odontophora [with families Toxopneustidae,
#     Strongylocentrotidae, and Echinometridae], and superfamily UNCERTAIN [with
#     families Echinidae and Parechinidae]) and suborder/superfamily UNCERTAIN
#     (with family Parasaleniidae). For suborder Scutellina, maintain
#     infraorders Laganiformes (including Fibulariidae = j.s.s. Echinocyamidae
#     and Laganidae including Laganinae and Neolaganinae plus any other genera
#     listed in now reranked suborder Laganina) and Scutelliformes (including
#     Taiwanasteridae, Protoscutellidae [in stem group], Echinarachniidae,
#     Dendrasteridae, Rotulidae, Scutellidae, Eoscutellidae, Scutasteridae,
#     Abertellidae, Astriclypeidae, Monophorasteridae, and Mellitidae) and
#     assigning to superfamily Scutelloidea except for family Protoscutellidae
#     in superfamily UNCERTAIN because stem Scutellina. Treat unranked clade
#     Meridosternata as a suborder of Holasteroida with infraorders
#     Cardiasterina (= Stegasterina (including Stegasteridae and Cardiasteridae
#     = Cardiotaxinae) and Urechinina (including echinoid homonym Corystidae
#     replaced by Corystusidae, Calymnidae, Carnarechinidae, Garumnasteridae,
#     Urechinidae, Plexechinidae, and Pourtalesiidae) [note that the corystusids
#     and calymnids are not listed this way on page 173 but clearly an error
#     based on cladogram in fig. 2]; families Echinocorythidae and Holasteridae
#     are then in infraorder UNCERTAIN as stem Meridosternata and families
#     Stenonasteridae, Hemipneustidae, and Pseudholasteridae are in infraorder
#     and suborder UNCERTAIN as stem holasteroids.

#  x. Following Blake (2018) for (primarily Paleozoic) asteroids (and other
#     asterozoans: somasteroids, stenuroids, and ophiuroids). (This set of
#     opinions has already been entered into PBDB.) Because Blake (2018) disused
#     previous suborder names (e.g., Diplozonina, Eugnathina, Hemizonina,
#     Platyasterida, Pustulosina, and Uractinina) for Paleozoic echinoids, they
#     are retained when restricted within his new orders. Need to write out
#     these exceptions when propagated later. Following Gale (2012) for
#     (primarily post-Paleozoic) asteroids, but treating following unranked
#     clades as orders: Forcipulatida (with orders/unranked clades Brisingida as
#     suborder Brisingina [for Brisingidae] and Asteriadina (for Asteriidae));
#     Hemizonida (including Taeniactinidae and now-disused Lepyriactinidae and
#     Palaeostellidae); Paxillosida (with suborders Diplozonina
#     [Astropectinidae, Luidiidae], Cribellina [Ctenodiscidae, Goniopectinidae,
#     Porcellanasteridae]), and Notomyotina [Benthopectinidae]); Spinulosida
#     (with suborders Eugnathina [Plesiosolasteridae], Leptognathina
#     [Echinasteridae and Valvasteridae], and reranking Chevronida as suborder
#     [Tremasteridae in superfamily UNCERTAIN plus reranking as superfamiy
#     Valetida [Korethrasteridae, Pterasteridae, Solasteridae, and
#     Tropidasteridae]]); and Valvatida (with suborders Granulosina
#     [Archasteridae, Chaetasteridae, Goniasteridae, Odontasteridae,
#     Ophidiasteridae, Oreasteridae, Pycinasteridae, and Radiasteridae] and
#     Tumulosina [Sphaerasteridae, Stauranderasteridae]. Treating unranked
#     Neoasteroidea as an infraclass of subclass Ambuloasteroidea (although
#     nearly a synonym). Following WoRMS (and partially PBDB), treat 
#     Chilophiurina as infraorder (suborder blank) in order Ophiurida.

#  y. Following Parry, et al. (2019) that demonstrates machaeridians are
#     polychaetes in order Phyllodocida and suborder Aphroditiformia, downgrade
#     machaeridian orders Lepidocoleomorpha, Hercolepadida, and
#     Turrilepadomorpha as new superfamilies, and unranked clade
#     Cuniculepadida [Lepidocoleidae and Turrilepadidae]	also as a new superfamily.

#  z. Based on the affirmation of Lindberg and Ponder (2020) regarding
#     conclusions of Dzik (2010), treating problematic tergomyan
#     ("monoplacophoran") families Kirengellidae, Pygmaeoconidae =
#     Pygmaeoconinae, Romaniellidae, and Shelbyoceridae (and other kirengellids
#     unassigned to families) as synonyms of Hypseloconidae in order
#     Kirengellida (= senior to junior synonyms Hypseloconida and Romaniellida),
#     and treating all as uncertain (possibly stem-group Craniiformea) members
#     of Brachiopoda (class UNCERTAIN and subphylum blank). Also assign
#     Scenellidae (order and class UNCERTAIN) to stem Brachiopoda (UNCERTAIN
#     subphylum), despite PBDB considering a tergomyan.

# aa. Based on results in Lerosey-Aubril, et al. (2017), replace subphylum 
#     Trilobitomorpha with near-equivalent but better-defined subphylum
#     Artiopoda, including classes Trilobita, Nektaspidida, and Vicissicaudata.
#     Modify class Nektaspidida to include monotypic order Nektaspida (spelled
#     differently than Nektaspidida). Treat Vicissicaudata as a class instead of
#     superclass, including subclasses Aglaspidida and Cheloniellida (and
#     Merostomoidea) and remaining unclassified genera in subclass UNCERTAIN.
#     For those genera in class Merostomoidea, downgrade the name to subclass
#     status, although likely not monophyletic. Treat class Aglaspidida as a
#     subclass with orders Aglaspidida (of same name) and Strabopida. Assign 
#     following orders to subclass Libristoma (= Librostoma): Asaphida,
#     Harpetida, Proetida, Ptychopariida, and Trinucleida.

# ab. The affinity of radiocyaths is uncertain (Treatise: Kruse, et al., 2015),
#     with most considering them either allied to archaeocyath or heteractine
#     sponges or receptaculacean (dasyclad) algae. Although most recent research
#     supports an algal affinity, parenting class Radiocyatha as distinct 
#     subclass of class Archaeocyatha so that life habit propagates as a sponge 
#     model, and maintaining Hill's 1972 order names for now. (Easier to assume 
#     an animal now and secondarily remove, than to ignore and add in later, if 
#     future consensus emerges.)

# ac. Treat order Bradoriida as a non-ostracod member of the stem Crustacean,
#     following consensus in Álvarez, et al. (2008) and Siveter, et al., (2014).

# ad. Treat Ostracoda as a class. Following WoRMS, treat the following ostracod 
#     superorders as equivalent to subclasses/orders: use subclass Myodocopa for
#     Myodocopamorphes, order Platycopida for Platycopamorphes, order
#     Palaeocopida for Palaeocopamorphes, and order Podocopida for
#     Podocopamorphes. Replace the following suborders: use suborder
#     Cypridocopina for suborder Cypriformes, Halocypridina for Halocypriformes,
#     Platycopina for Cytherelliformes and use family Polycopidae for suborder
#     Polycopiformes. Following Liebau (2005), and apparently WoRMS, treating
#     order Beyrichicopida as a j.s.s. of Beyrichicopina. Despite PBDB (and some
#     primary literature articles) claiming the order Metacopida is valid, most
#     consider them in suborder Metacopina in order Podocopida, the taxonomy
#     used herein. Assign Dolborellidae to superfamily Limbatuloidea. Generally,
#     the higher taxonomy of ostracodes (especially Paleozoic ones) is highly
#     unresolved and often contradictory. See "Ostracod_taxonomy.docx" for the
#     complete taxonomic structure used, which represents a consensus between
#     WoRMS and PBDBD. It is imperfect, but represents a balance between being
#     locally highly resolved (based on cladistic analyses of small clades)
#     while being relatively standardized at higher levels. We need more
#     ostracod workers!

# ae. Search the "Subfamilies" field for reranked families ending in -idae and 
#     use PBDB and WoRMS to decide whether to treat as a subfamily or family
#     rank. Examples, "subfamilies" Aplustridae, Cetotheriidae, Rissoinidae,
#     Tomogeridae, and Truncatellinidae.

# af. For phosphatocopine crustaceans, follow Zhang, et al. (2010) and Siveter, 
#     et al. (2003) in treating Euphosphatocopida (= original Phosphatocopina) 
#     as an order within class Phosphatocopida.

# ag. Confirm that subphylum Urochordata is replaced with synonym Tunicata (per 
#     WoRMS and most recent usages).

# ah. Use class Cincta instead of outdated class Homostelea. Note cinctans 
#     currently lack names for orders.

# ai. Vertebrate zoologists and paleontologists have relied much more on 
#     phylogenetic analyses to classify monophyletic clades than other 
#     disciplines, using a much more varied lexicon of formal and unranked 
#     clade names. In general, taxonomy at and below the rank of order are 
#     stable; ranks above are much less consistent. For fishes, relying on 
#     modification of Nelson, et al. (2016, Fishes of the World) and van der 
#     Laan (2019, Family-group names of fossil fishes) combined with WoRMS. 
#     Because PBDB lacks infraphylum, parvphylum, and similar intermediate 
#     ranks, using the following scheme to merge (often distinct) 
#     classifications in WoRMS and PBDB (defaulting to terms in PBDB when 
#     different versions of same name (e.g., Batomorphii vs Batoidea).

#     (1) Using modification of Nelson, et al. (2016, Fishes of the World), 
#         placing class Conodonta in superclass Conodontophorida (within
#         Vertebrata) instead of separate subphylum. Treat order
#         Conodontophorida as j.s.s. of class Conodonta, order Prioniodinida as
#         j.s.s of Prioniodontida .

#     (2) For presumably paraphyletic jaw-less vertebrates ("Agnatha"), 
#         ignoring infraphylum Agnatha and maintaining PBDB superclasses
#         Anaspidomorphi (= class Anaspida), Cyclostomata (= classes Myxini,
#         Petromyzontida), Osteostracomorphi (= classes Galeaspidomorphi [=
#         Galeaspida] and Osteostraci [= Cephalaspidomorphi]), Pteraspidomorphi
#         (= class Pteraspidomorpha), and Thelodontomorphi (= class Thelodonti).
#         Use order Benneviaspidiformes and Thyestiformes instead of 
#         Benneviaspidida and Thyestiida.

#         (a) Treat suborder Cyathaspida as j.s.s. of Cyathaspidoidei, with
#             family Cyathaspididae (= j.s.s. of Dinaspidae, Diplaspidae, and
#             Palaeaspidae). Replace family Ctenaspidae with Ctenaspididae,
#             Tolypaspidae with Tolypelepididae, and Tolypaspidae and
#             Tolylepidae with Tolypelepididae (the latter in order
#             Tolypelepidiformes, and treating subfamily Tolypelepidinae as
#             Tolypelepididae). Treat order Psammosteiformes as suborder
#             Psammosteoidei, in order Pteraspidiformes (= reranked class
#             Pituriaspida).

#     (3) Ignoring superclass (PBDB) / infraphylum (WoRMS) Gnathostomata, 
#         and re-ranking class (PBDB) / parvphylum (WoRMS) Chondrichthyes as 
#         superclass.  The superclass includes (after elevating subclasses to 
#         classes, as in WORMS and, in part, van der Laan, 2019): Acanthodii, 
#         Elasmobranchii (= s.s.s. of Neoselachii, the subclass used in WoRMS), 
#         and Holocephali (redundant with subclass Holocephali). Class 
#         Elasmobranchii is nearly redundant with subclass Neoselachii. 
#         Infraclass Euselachii is used instead of infraclass Selachii in WoRMS.
#         Superorder Batomorphii is used instead of infraclass Batoidea in
#         WoRMS. Infraclass Hybodonta is down-graded to rank superorder so it
#         can be parented to infraclass Euselachii. Use order Iniopterygia
#         instead of Iniopterygiformes and Mongolepidida instead of
#         Mongolepidiformes. Use order Odontaspidida as j.s.s. of Lamniformes.

#     (4) Traditional group Osteichthyes ("class" in PBDB and parvphylum in 
#         PBDB) is essentially ignored herein, with bony fishes assigned to
#         superclass Actinopterygii and lobe-finned fishes to superclass
#         Sarcopterygii. Superclass Actinopterygii (a superclass in PBDB and
#         gigaclass in WoRMS) is used to include classes Cladistia (= infraclass
#         Cladistia) and Actinopteri (= subclasses Chondrostei [redundant with
#         infraclass Chondrostei] and Neopterygii). Many non-traditional ranks
#         in van der Laan (2019) have been entered in the PBDB as unranked
#         clades. To best accommodate clades, intermediate ranks (suborders to
#         subclasses) have been "stretched," such that traditional orders are
#         often downranked as suborders or sections (in some cases as
#         superfamilies). This has the effect of making ichthyological ranks
#         more consistent with how they are used by invertebrate zoologists. In
#         other words, ichthyological orders are more akin to invert
#         superfamilies (or families), and these rerankings both better
#         accommodate established clades while making rankings more consistent
#         across animals. See "Body_fish_taxonomy.docx" for conventions used.
#         Treating unranked clade Bramiformes as a superfamily of Scombriformes.
#         Assign Salmonoidea and order Isospondyli to order Salmoniformes. Use
#         superorder Blenniimorphae instead of Blenniomorphae and suborder
#         Pleuronectoidei instead of Pleuronectoidea. Assign order
#         Acropomatiformes to Blenniimorphae. Treat superorder Clupeomorpha as
#         j.s.s. of superorder [now downranked suborder] Clupei. Treat unranked
#         clade Otocephala as j.s.s. subcohort Otomorpha [now downranked to
#         order]. Treat orders Percomorphi and Percomorpha as j.s.s. of
#         Percomorphaceae, Euteleostei as j.s.s. of Neoteleostei, unranked clade
#         Percomorphacea as j.s.s. of division Percomorphaceae (downranked to
#         superorder), unranked clades Osteoglossocephala and
#         Osteoglossocephalai as j.s.s. of cohort Osteoglossomorpha (downranked
#         to superorder), unranked clade Elopocephalai as j.s.s. of cohort
#         Elopomorpha [downranked to superorder], suborder Alepisauroidei
#         [downranked to superfamily] as equivalent to superfamily
#         Alepisauroidea, which it very nearly is. Superorder Pycnodontomorpha
#         is treated as j.s.s. of order Pycnodontiformes, order Gyrodontiformes
#         as j.s.s. of suborder Gyrodontoidei, unranked clade Gobiida as j.s.s.
#         of Gobiaria, Ophidiida as j.s.s. of Ophidiaria, Pelagia as j.s.s. of
#         Pelagiaria.

#     (5) Modifying Nelson, et al. (2016), assigning class Placodermi to 
#         superclass Placodermiomorphi (who considers this an unranked clade). 
#         See Placoderm_taxonomy.docx for conventions in handling unranked 
#         clades Antiarchi and Arthrodira, where assigned rank of subclass, with
#         other orders assigned class UNCERTAIN. Archaic "family" Acanthaspida
#         is treated as a j.s.s. of Macropetalichthyidae.

#     (6) Superclass Sarcopterygii (a gigaclass in WoRMS and subclass in PBDB)
#         is used to include classes Coelacanthimorpha (= subclass Actinistia)
#         and Dipnomorpha (= subclass Dipnotetrapodomorpha) and infraclass 
#         Onychodontida (in unnamed subclass, in duplicate class Onychodontida).
#         Unranked clades Choanata, Eutetrapoda, and Stegocephali are ignored.
#         Osteolepidae is treated as j.s.s. of Osteolepididae and use
#         Osteolepidiformes instead of j.s.s. Osteolepiformes. Infraclasses
#         Osteolepidida and Rhizodontida are placed as UNCERTAIN sarcopterygian
#         classes.

#     (7) Assigning all tetrapods to superclass Tetrapoda (= classes Amphibia,
#         Aves, Mammalia, and Reptilia). Treat unranked Squamata as order within 
#         subclass Eureptilia.

# aj. Based on convention in WoRMS, treating downgrade class Tergomya to 
#     subclass rank and placing within class Monoplacophora.

# ak. Problematic Ediacarans: Following Erwin, et al. (2011), treating unranked 
#     Dickinsoniomorpha as a class (redundant with order Dickinsoniida); placing
#     as stem member of phylum Placozoa following Sperling and Vinther (2010),
#     which is not entirely refuted by Gold, et al. (2015). Treating unranked
#     clade Rangeomorpha as order within class Petalonamae and Erniettomorpha as
#     order (and redundant class), both in phylum Petalonamae, as implied by
#     Hoyal Cuthill and Han (2018).

# al. Bryozoan taxonomy in the PBDB is a mess, but improving slowly. For 
#     cheilostomes, rely on combination of Gordon (2012, interim Treatise
#     classification) and WoRMS. Follow convention in WoRMS (but not
#     consistently in PBDB) to treat suborders Neocheilostomina,
#     Neocheilostomatina, Anasca, Ascophora, and Ascophorina all as junior
#     synonyms of suborder Flustrina (and ignore infraorder Ascophorina). Treat
#     suborder Malacostegina as j.s.s. of Membraniporina.

# am. Based on convention in WoRMS (much more current than PBDB), use corrected 
#     name Cyclostomatida for order of cyclostomes (synonym of Tubuliporata),
#     with suborders Articulina (replaced name for Articulata, with families
#     Crisiidae and Crisuliporidae), Cancellata, Cerioporina, Fasciculina (with
#     families Actinoporidae, Frondiporidae, Hastingsiidae, Semiceidae,
#     Siphoniotyphlidae, and Theonoidae), Hederellida (with families
#     Hederellidae and Reptariidae), Palaeostomata (= Paleotubuliporina in part,
#     with families Corynotrypidae, Crownoporidae, Flabellotrypidae, and
#     Sagenellidae), Rectangulata (with families Alyonushkidae [no fossils],
#     Anyutidae [no fossils], and Lichenoporidae), and Tubuliporina (most other
#     families). Treat order Rhabdomesida as suborder Rhabdomesina in order
#     Cryptostomida. Assign suborder Esthonioporina to order Esthonioporata.

# an. Treat problematic cambroernids (= unranked Cambroernida of Yang, et al., 
#     2020) as class Cambroernida in phylum UNCERTAIN. The "class" includes
#     order Eldonioidea (sensu Schroeder et al. 2018, reranked from
#     class)--which includes Eldoniidae, Paropsonemidae, and Rotadiscidae--plus
#     genera Cheungkongella and Herpetogaster, plus Phlogites and Yanjiahella,
#     which Nanglu, et al. (2023) align with the cambroernids.

# ao. Treat yunnanozoans (Yunnanozoon: Yunnanozoonidae < class Yunnanozoa), 
#     banffozoans (Banffia: Banffiidae < class Banffozoa), and vetulicolians
#     (Nesonektris, Skeemella, Didazoonidae, and Vetulicolidae in order
#     Vetulicolata < class Vetulicolida) as stem chordates, maintaining
#     traditional intermediate ranks (family, order, class) until more clearly
#     studied. Downrank "phylum" Vetulicolia as a subphylum until better
#     resolved.

# ap. Following Ramirez-Guerrero, et al. (2026), assign classes Cubozoa, 
#     Scyphozoa, and Staurozoa (= order Stauromedusae, which apparently has no
#     fossils) to superclass Acraspeda (within subphylum Medusozoa), but not
#     including Hydrozoa. (Note this includes the Conulariida.)

# aq. Place order Radiodonta within arthropod class Dinocarida (= Sepkoski's 
#     Dinocardia and others Dinocaridida). Also include Opabiniidae here, until 
#     better resolved.

# ar. Assign Coleolidae to order Coleoloida (in class and phylum UNCERTAIN). 
#     Assign Odontogriphidae to Mollusca, until better resolved.

# as. Given consensus that Lobopodia are paraphyletic relatives of 
#     arthropods, onychophorans, and tardigrades, treating "phylum" Lobopodia
#     (order Luolishaniida + Onychodictyon) as phylum Onychophora, until better
#     resolved. Also include Aysheaiidae (despite recent claims a possible stem
#     tardigrade),  informal "Hallucishaniids" (ranked as class
#     Hallucishaniida), and class Xenusia here.

# at. Treat unranked (monogeneric) Saccorhytida as a phylum.

# au. For cephalopods, treat unranked clade Neocoleoidea as a coleoid 
#     infraclass. Treat superorder Decembrachiata as j.s.s. of Decabrachia.
#     Treat suborders Teuthomorpha and Teuthina as j.s.s. of superorder
#     Decabrachia. Following King and Evans (2019), replacing subclass
#     Nautiloidea with Nautilia, subclass Orthoceratoidea with Orthoceratia, and
#     superorder Multiceratoidea with subclass Multiceratia. Treat Aulacocerida 
#     as j.s.s. of Aulacoceratida

# av. For anthozoans, defer to WoRMS for extant corals. Downrank subphylum 
#     Anthozoa to class rank. Subclass Hexacorallia (= Zoantharia) includes
#     orders Actiniaria, Antipatharia [= Ameripathidae, Antipathidae,
#     Aphanipathidae, Cladopathidae, Leiopathidae, Myriopathidae,
#     Pteridopathidae, Schizopathidae, Sinopathidae, Sterictopathidae, and
#     Stylopathidae], Ceriantharia, Corallimorpharia (= families
#     Corallimorphidae, Discosomidae, Ricordeidae, and Sideractinidae but no
#     occurrences in PBDB), Hexanthiniaria (= Hispaniastraeidae and
#     Pachythecalina [= Pachythecaliina]), Kilbuchophyllida, Scleractinia,
#     Tabulaconida, and Zoantharia (= j.s.s. Zoanthidea) (= suborders
#     Brachycnemina [= Neozoanthidae, Sphenopidae, Zoanthidae] and Macrocnemina
#     [= Epizoanthidae, Hydrozoanthidae, Microzoanthidae, Nanozoanthidae, and
#     Parazoanthidae but no occurrences in PBDB])

# aw. For octocorals, defer to WoRMS where better informed by molecular 
#     phylogenetics. Downrank class Octocorallia to subclass rank, and
#     equivalent to now-defunct Alcyonacea and Gorgonacea. Only two orders are
#     currently recognized: Malacalcyonacea (= j.s.s. Alcyoniina, Holaxonia,
#     Protoalcyonaria, Scleraxonia, and Stolonifera) and Scleralcyonacea (=
#     j.s.s. Calcaxonia). Treat order Helioporacea as j.s.s. of family
#     Helioporidae. Superfamily Pennatuloidea is equivalent to j.s.s. order
#     Pennatulacea and suborders Sessiliflorae and Subselliflorae. Maintaining 
#     most recent concensus that order Heterocorallia are octocorals.

# ax. For extant holothuroids, defer to WoRMS for higher taxonomy.

# ay. Following WoRMS, assign order (and redundant subclass) Paleoloricata to 
#     Polyplacophora, whereas PBDB considers a stem aculiferan.

# az. Treat unranked Euprotobranchia as bivalve subclass (although solely 
#     contains Fordillida).

# ba. Treat hydroid suborders / orders Anthomedusae, Athecata, Gymnoblastea, 
#     Gymnoblastina, and Stylasterina all as synonyms of order Anthoathecata.
#     Treat Calyptoblastea, Leptomedusae, Thecata, and Thecaphora as synonyms of
#     order Leptothecata. Use WoRMS for extant and higher taxonomic structure.
#     Suborder Chondrophorina and Velellidae are treated as j.s.s. of family
#     Porpitidae. Replace Olindiadidae with Olindiidae. Suborder Milliporina is
#     treated as family Milleporina. Heterastrididae treated as Milleporidae.
#     Siphonophorida is treated as order Siphonophorae. Order Trachylinida is
#     treated as j.s.s. of subclass Trachylinae. Suborder Suborder Narcomedusina
#     is treated as j.s.s. of order Narcomedusae. Order Hydroida is treated as
#     j.s.s. of subclass Hydroidolina.

# bb. Use traditional order Stromatocystitida for Stromatocystitidae (instead 
#     of UNCERTAIN).

# bc. For extant tunicates (and higher taxonomy for extinct ones), use taxonomy 
#     from WoRMS.


# (6) Run code in "IDBadHigherTaxa.R" to identify taxa that are not consistently
#     parented, such as a family in my database parented to a superfamily that
#     is absent from PBDB import. See that file for list of taxa that are
#     allowed to be duplicated. For example, redundant ranks (Cambridioidea used
#     as both superfamily and order of stenothecoids) or homonyms (Palaeostomata
#     as both superorder [of many bryozoan orders] and suborder [of
#     cyclostomes]).

# (7) Run code as usual in "PropogateSizes.R" or "PropogateLifeHabits.R", but
#     resaving as postX_withPBDB" file name. Make sure to add new IDNumbers to
#     the new PBDB entries!

# (8) Import into copy of FileMakerPro life habit database, adding the new
#     entries. Use this one for running next analyses.

# (9) Before running disparity and tiering analyses, open here and remove the
#     non-terrestrials (and non-fossils with Recent-only occurrences?) once
#     again, in case any got included during the taxonomy standardization.