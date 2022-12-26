## FORMAT PBDB GENUS/SUBGENUS OCCURRENCES INTO TAXONOMIC STRUCTURE OF MY DATA,
## USING A PARALLEL-COMPUTING ENVIRONMENT

# The serial version of the main parenting algorithm below takes ~4.5 hours to
# process PBDB into a formatted taxonomic dataframe structure. This code embeds
# the algorithm as a function that can be implemented in parallel and is much
# faster.

## Download data directly from PaleobioDB and save to working directory (will be
## > 20 MB)

setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")

rm(list = ls())

# Libraries
library(data.table)  # v. 1.14.4
library(snowfall)    # v. 1.84-6.2
library(beepr)       # v. 1.3
library(parallel)    # v. 4.2.2


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
# (=manually, via code) remove the ichnofossils (tagged with flags = "I" or
# "IF"). Include form taxa in life-habit database for now, as many are actually
# "regular" taxa improperly flagged by data enterers as "form" taxa.

# Make sure to include "all" names (including synonyms, misspellings, and other
# invalid names), as these can be found in Sepkoski Compendium and other
# databases. Include "obsolete name variants" but only propagate (sub)genera
# that are tagged in the following categories: "nomen dubium" (because status
# deserves further study rather than confirmed lack of validity) and "subjective
# synonym of" because these opinions are subjective and may change in the
# future.

# Manually exclude (sub)genera that are currently coded in the other
# "difference" categories: "corrected to", "invalid subgroup of", "misspelling
# of", "nomen nudum", "nomen oblitum", "nomen vanum", "objective synonym of",
# "obsolete variant of", "reassigned as", and "replaced by". These are genuine
# errors or objectively invalid (sub)genera that are unlikely to be reversed by
# future opinions. However, these names should be included in downstream
# analyses (such as adding stratigraphic ranges from Sepkoski's Compendium), as
# other data bases may include such obsolete names. "obsolete variant of" and
# "reassigned as" are excluded because they are usually (always?) only used when
# a subgenus is re-ranked as a genus or vice versa; these variants should
# already be included above, and so including them here would simply add
# duplicate entries.

# Because the 'parent' of subjective synonyms and nomen dubia is their
# accepted_name (and not a higher taxon), need to hold these genera until
# everything else is propagated. To maintain consistency in the PBDB
# "namespace," these names are set aside here. They will be added back in (1)
# after updating the stratigraphic ranges (including with WoRMS for adding
# extant ranges and with the Sepkoski Compendium), (2) after propagating the
# body sizes (which rely on the stratigraphic ranges), and (3) after propagating
# the life habits. Only then will we (4) copy the taxonomic classification of
# their accepted parent, and (5) add a tag to the TaxonomyReference field that
# copies the invalid opinion (i.e., "Possibly invalid (sub)genus.
# Paratoernquistia may be a subjective synonym of Toernquistia.").

# However, to avoid "parenting" issues (such as when a legitimate genus is
# parented in a family tagged as an ichnofamily or in an obsolete naming
# varient), include all taxa in the parenting algorithm.




## DOWNLOAD FROM PBDB ----------------------------------------------------------

# Easier if paste link into URL and save manually 

# https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app
# pbdb <- read.csv("https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app")
# If want forams too, use base_name=Metazoa,Retaria
pbdb <- read.csv("pbdb_data_AllMetazoaTaxa.csv")
head(pbdb)
nrow(pbdb)

# See https://paleobiodb.org/data1.2/taxa/list_doc.html for description of API
# fields.

# Confirm flags
table(pbdb$flags)
# B = base taxon downloaded (here, Metazoa, which redirects to Animalia)
# V = invalid taxon variants (replacements, misspellings, synonyms, etc)
# I = ichnotaxa
# F = form taxa (ignore here given many user errors in classifying these, and
#     they represent a small number of taxa)


## Extract out trace fossils and invalid names, for (sub)genera only (keeping
## all taxon types for higher taxa to allow proper parenting).
valid.gsgs <- 
  which((pbdb$accepted_rank == "genus" | pbdb$accepted_rank == "subgenus")
        & pbdb$difference == "")
traces <- grep("I", pbdb$flags)
# Not worth indexing on "V" because not all taxa with a "difference" is tagged
# with "V". Following is more efficient.
which.gsg <- setdiff(valid.gsgs, traces)

# Valid (sub)genera now all forms or regular taxa, with no 'differences'
table(pbdb[which.gsg, c("accepted_rank", "flags", "difference")])

# Invalid (sub)genera are traces or differenced, but other taxa include everything
table(pbdb[-which.gsg, c("accepted_rank", "flags", "difference")])[c(3, 12, 2, 8, 1), , ]

# Save the allowed invalids (subjective synonyms and nomen dubia) for later use
# (to maintain a consistent namespace during propagations).
allowed_invalids <-
  which((pbdb$accepted_rank == "genus" |
           pbdb$accepted_rank == "subgenus")
        & (pbdb$difference == "nomen dubium" |
            pbdb$difference == "subjective synonym of"))
pbdb_add_synonyms <- pbdb[setdiff(allowed_invalids, traces), ]
nrow(pbdb_add_synonyms)
table(pbdb_add_synonyms$flags) # form, variants, and form-variants

# Confirm no duplicates with 'pbdb'
table(duplicated(rownames(pbdb), rownames(pbdb_add_synonyms)))

# Save for later
# write.csv(pbdb_add_synonyms, file = "PBDB_synonyms_and_dubia.csv", row.names = FALSE)



## FUNCTIONS -------------------------------------------------------------------

## Function to add higher taxonomic names (phylum, class, order, etc.) for PBDB
## genus (and subgenus) names.
# g = Vector (sequence) of number of genus names to process.
# gen.order = Vector of ordered PBDB genus (and subgenus) names.
# which.gsg = Vector of indices for PBDB entries tagged as accepted genus or 
#   subgenus names.
# pbdb = data frame of all PBDB occurrences.
#
# Output is a list, with each item the taxonomy for a single genus. Extends LAD
# to 'Recent' if genus is extant and splits subgenus names into genus and
# subgenus components.
prep.PBDB <- function(g = 1, gen.order, which.gsg, pbdb) {
  scales <- c("phylum", "subphylum", "superclass", "class", "subclass", 
              "infraclass", "superorder", "order", "suborder", "infraorder", 
              "superfamily", "family", "subfamily", "genus", "subgenus")
  out <- data.frame(Phylum = character(1), Subphylum = character(1), 
                    Superclass = character(1), Class = character(1), 
                    Subclass = character(1), Infraclass = character(1), 
                    Superorder = character(1), Order = character(1), 
                    Suborder = character(1), Infraorder = character(1), 
                    Superfamily = character(1), Family = character(1), 
                    Subfamily = character(1), Genus = character(1), 
                    Subgenus = character(1), Species = "sp.", 
                    stringsAsFactors = FALSE)
  out$Genus <- as.character(pbdb$accepted_name[which.gsg][gen.order[g]])
  wh <- which.gsg[gen.order[g]]
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
  # In cases where the priority opinion involves BOTH a correction AND elevation
  # of a subgenus to genus rank, the genus is orphaned (placed within a higher
  # taxon but not assigned a genus.) Examples include Erioptera (Hoplolabis) and
  # Otolithus (Chrysophris). The following corrects for these rare cases. It is
  # placed within the repeat loop to allow for the rare instance where the
  # reassignment of a subgenus in another genus results in its being parented in
  # a subgenus (which can sometimes happen when genera are later reranked as a
  # subgenus; E.g., Amphistrophiella (Amphistrophiella) parented as
  # Amphistrophia (Amphistrophiella)).
  repeat {
    # i.e., if parent is above genus and child is below genus (and there is no
    # genus), elevate the subgenus name to genus rank:
    if ((parent$accepted_rank != "genus" & parent$accepted_rank != "subgenus") &
        child_rank == "subgenus") {
      out$Genus <- out$Subgenus
      out$Subgenus <- ""
      }
    if (parent$accepted_rank %in% scales)
      out[1, which(scales == parent$accepted_rank)] <-
        as.character(parent$accepted_name)
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
  return(out)
}



# Identify possibly problematic homonym genera
sort(table(pbdb$accepted_name[which.gsg]), decreasing = TRUE)[1:30]

# Note different parents (one is a brachiopod and one is a gastropod)
pbdb[which(pbdb$accepted_name == "Lowenstamia"), ]



## Format the PBDB data using a parallel-computing environment ---------------

# Version using parallel computing:
require(data.table) # Required below for merging parallel lists into dataframe
require(snowfall)
(t.start0 <- Sys.time())

# Initialize
valid.gsgs <- 
  which((pbdb$accepted_rank == "genus" | pbdb$accepted_rank == "subgenus")
        & pbdb$difference == "")
traces <- grep("I", pbdb$flags)
which.gsg <- setdiff(valid.gsgs, traces)
cat("Processing", length(which.gsg), "(sub)genera\n")
gen.order <- order(pbdb$accepted_name[which.gsg])
gen.seq <- seq_along(gen.order)
# Fast test batch:
# gen.seq <- 1:1000

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
                 which.gsg = which.gsg, pbdb = pbdb) # Version without load-balancing
sfStop()
output <- data.table::rbindlist(prep)
(Sys.time() - t.start0)  # ~ 13 minutes with 8 CPUs
head(output)
beepr::beep(3)


                

## Add named geological intervals to stratigraphic ranges ----------------------

# This is just a temporary algorithm. Run UpdateAges&DivCurve.R for more
# comprehensive code, which additionally (1) adds ranges for taxa in Sepkoski
# Compendium, (2) interfaces with WoRMS to confirm extinct/extant status (and
# setting min_ma to 0), and (3) updates the dates to the Gradstein (2020)
# Geologic Time Scale (using lookup table of Peter Wagner).
strat_names <-
  read.csv("https://www.paleobiodb.org/data1.2/intervals/list.csv?all_records&vocab=pbdb")
# 1 = eons, 2 = eras, 3 = periods, 4 (the default) = subperiods, and 5 = epochs.
scale_level <- 4
ages <- strat_names[which(strat_names$scale_level == scale_level),]
edia <- strat_names[which(strat_names$interval_name == "Ediacaran"), ]
ages <- rbind(ages, edia)
# Replace outdated Series 3 with Miaolingian (if using epochs)
ages$interval_name <-
  replace(ages$interval_name, which(ages$interval_name == "Series 3"), 
          "Miaolingian")
tail(ages[, 1:5])
output$max_age <- character(1)
output$min_age <- character(1)

for(int in 1:nrow(ages)) {
  wh.FAD <- which(output$max_ma > ages$min_ma[int] 
                  & output$max_ma <= ages$max_ma[int])
  wh.LAD <- which(output$min_ma >= ages$min_ma[int] 
                  & output$min_ma < ages$max_ma[int])
  output$max_age[wh.FAD] <- as.character(ages$interval_name[int])
  output$min_age[wh.LAD] <- as.character(ages$interval_name[int])
}

# Special work-around for singletons (only problematic for those that occur on a
# boundary):
wh.singleton <- which(output$max_ma == output$min_ma)
output$min_age[wh.singleton] <- output$max_age[wh.singleton]

# Recent is included for extant taxa (although no PBDB taxa have FADs = 0):
wh.Recent.FAD <- which(output$max_ma == 0)
wh.Recent.LAD <- which(output$min_ma == 0)
output$max_age[wh.Recent.FAD] <- "Recent"
output$min_age[wh.Recent.FAD] <- "Recent"

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
sq <- 1:14     # Higher taxonomy columns (Phylum <---> Genus)
cat("The presence of subgenera, homonyms, and possible duplicates equals", 
    round(100 * length(mults) / nrow(output), 1), "% of the database\n", 
    file = file.name)
output[which(output$Genus == "Acanthopyge"), ] # Example of multiple subgenera

for(d in 1:length(mults)) {
  sus.gen <- names(mults[d])
  suspicious <- output[which(output$Genus == sus.gen), ]
  classes <- unique(suspicious$Class)
  if (length(classes) == 1L)
    # Identify likely subgenera:
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
# of Sepkoski's opinions to "objective synonym" of and add a note in the
# Comments field to let future users understand why the opinion was changed.
# (Ideally, make the entry with zero occurrences and no other parenting opinions
# be the one to point to the more completely fleshed out genus.)

# All likely duplicates have been corrected (as of Nov. 21, 2022). The following
# are confirmed (or very likely) homonyms (occurring within the same class):
# Strophomena, Desmoceras, Aspidocrinus, Bicarinella, Billingsites,
# Brachyrhizodus, Coronopsis, Curculionites, Didymoceras, EolampraStrophomena,
# Desmoceras, Aspidocrinus, Bicarinella, Billingsites, Brachyrhizodus,
# Coronopsis, Curculionites, Didymoceras, Eolampra, Geometra, Glypta,
# Hoffeinsia, Hysteroceras, Longhuaia, Mesocorixa, Mesodiadema, Mesorthophlebia,
# Microcorystes, Noctua, Onychoceras, Pamirophyllum, Parachorista, Pternodus,
# Pyralis, Rhectomyax, Saurornithoides, Sharovia, Sinoperla, Solenochilus,
# Sutherlandia, Tinea, Tinosaurus, Tortrix, Treptoceras, Geometra, Glypta,
# Hoffeinsia, Hysteroceras, Longhuaia, Mesocorixa, Mesodiadema, Mesorthophlebia,
# Microcorystes, Noctua, Onychoceras, Pamirophyllum, Parachorista, Pternodus,
# Pyralis, Rhectomyax, Saurornithoides, Sharovia, Sinoperla, Solenochilus,
# Sutherlandia, Tinea, Tinosaurus, Tortrix, and Treptoceras.



## Post-processing marine-only standardizations --------------------------------

## Post-process to focus on marine taxa and to standardize taxonomy with mine
# x <- read.csv(file = "PBDBformatted.csv", header = TRUE, stringsAsFactors = FALSE)
head(x)

# Remove terrestrial and non-marine taxa, but include marine tetrapods. List
# modified from Bush and Bambach (2015) to explicitly include three cetacean
# suborders (because Cetacea now listed within Order Artiodactyla in PBDB) and
# known marine xiphosurans and eurypterids, and to exclude Myriapoda, 
# Kannemeyeriiformes, Pelycosauria, Theriodontia, Therocephalia, 
# freshwater Branchiopoda (= conchostrans, notostracans, cladocerans, etc.), and
# all known arachnid taxa (because many arachnids are getting listed in the 
# xiphosuran download).
non.marine <- c("Arachnida", "Insecta", "Collembola", "Palaeophreatoicidae",
    "Limnocytheridae", "Darwinuloidea", "Cypridoidea", "Cytherideidae", "Assimineidae",
    "Stenothyridae", "Hydrobiidae", "Ampullariidae", "Cyclophoridae", "Diplommatinidae",
    "Maizaniidae", "Viviparidae", "Melanopsidae", "Pachychilidae", "Thiaridae",
    "Amnicolidae", "Bithyniidae", "Lithoglyphidae", "Pomatiopsidae", "Valvatidae", 
    "Physidae", "Ancylidae", "Lymnaeidae", "Endodontidae", "Melampidae", 
    "Oreohelicidae", "Pleurodontidae", "Pupillidae", "Sagdidae", "Limacidae", 
    "Vitrinidae", "Milacidae", "Strobilopsidae", "Valloniidae", "Subulinidae", 
    "Succineidae", "Zonitidae", "Planorbidae", "Plicatusidae", "Pragoserpulinidae",
    "Unionida", "Corbiculidae", "Sphaeriidae", "Dreissenidae", "Amiinae", "Sinamiidae",
    "Cypriniformes", "Lepisosteiformes", "Osmeriformes", "Osteoglossiformes",
    "Percidae", "Esocidae", "Siluriformes", "Lepidosirenidae", "Polypteridae",
    "Cichlidae", "Gonorynchiformes", "Characiformes", "Gymnotiformes", "Myriapoda",
    "Kannemeyeriiformes", "Pelycosauria", "Theriodontia", "Therocephalia",
    "Branchiopoda", "Notostraca", "Calmanostraca", "Diplostraca", "Cladocera", 
    "Laevicaudata", "Spinicaudata", "Ammotrechidae", "Anthracomartidae", 
    "Anthracosironidae", "Anthracotarbidae", "Aphantomartidae", 
    "Archaeomartidae", "Archaeometidae", "Architarbidae", "Ceromidae", 
    "Chaerilobuthidae", "Daesiidae", "Devonotarbidae", "Dracochelidae", 
    "Eophrynidae", "Eukoeneniidae", "Garypidae", "Heterotarbidae",
    "Kreischeriidae", "Lissomartidae", "Opiliotarbidae", "Palaeocharinidae", 
    "Palaeotrilineatidae", "Proscorpiidae", "Trigonotarbidae", "Cheiridioidea",
    "Cheliferoidea", "Chthonioidea", "Feaelloidea", "Garypoidea", "Neobisioidea",
    "Sternophoroidea", "Bellinuridae", "Bilobosternina", "Cyphophthalmi", 
    "Dyspnoi", "Euproopidae", "Eupnoi", "Holosternina", "Ixodida", "Laniatores", 
    "Lobosternina", "Meristosternina", "Mesostigmata", "Opilioacarida", 
    "Orthosternina", "Posteriorricinulei", "Primoricinulei", "Sarcoptiformes", 
    "Tetrophthalmi", "Trombidiformes", "Tetrapulmonata", "Diplura", 
    "Phylactolaemata", "Spongillidae")
# Most tetrapods are terrestrial, so remove by default:
tetrapods <- c("Mammalia", "Reptilia")
# Then add back in the known marine tetrapods (and some known marine xiphosurans,
# etc.):
marine.exceptions <- c("Chelonioidea", "Ophidiomorpha", "Mosasauroidea", "Mosasauria",
    "Thalattosauria", "Sauropterygia", "Ichthyopterygia", "Mesoeucrocodylia",
    "Pterosauria", "Hesperornithiformes", "Ichthyornithiformes", "Sphenisciformes",
    "Procellariiformes", "Pelecaniformes", "Pelagornithidae", "Plotopteridae",
    "Charadriiformes", "Cetacea", "Sirenia", "Pinnipedia", "Desmostylia", "Ariidae", 
    "Plotosidae", "Archaeoceti", "Mysticeti", "Odontoceti", "Diploaspididae",
    "Mycteropidae", "Pterygotidae", "Woodwardopteridae", "Waeringopteroidea",
    "Eurypterina", "Limulina", "Stylonurina")

# Extract the known marine taxa (in lineages that are typically non-marine):
sq <- 1:nrow(x)
marine.vert.exceptions <- x[sapply(sq, function(sq) any(marine.exceptions %in% x[sq, ])), ]

# Remove the non-marine taxa, and all tetrapopds, including marine tetrapods (in
# case of tetrapods that were not coded as members of Tetrapoda in the PBDB):
marine.typical <- x[!sapply(sq, function(sq) any(c(non.marine, tetrapods, 
                                                   marine.exceptions) %in% x[sq, ])), ]

# Combine the typical marine taxa plus the known marine tetrapods, etc.:
marine.taxa <- rbind(marine.typical, marine.vert.exceptions)
table(marine.taxa$Class)
nrow(x)
nrow(marine.taxa)
beepr::beep()

# Remove confirmed form taxa (ammonoid aptychi and dissociated crinoid
# columnals, holdfasts, and anal sacs). Including these "genera" would
# artificially inflate standing genus richness.
known.forms <- c("Aptychus", "Cornaptychus", "Crassaptychus", "Granulaptychus", 
                 "Laevaptychus", "Laevicornaptychus", "Laevilamellaptychus", 
                 "Lamellaptychus", "Lissaptychus", "Praestriaptychus", 
                 "Pseudostriaptychus", "Pteraptychus", "Punctaptychus", 
                 "Rugaptychus", "Sidetes", "Spinaptychus", "Striaptychus", 
                 "Aspidocrinus", "Babanicrinus", "Calleocrinus", "Cyclocaudex", 
                 "Cyclocaudiculus", "Cyclocharax", "Cyclocrista", 
                 "Cyclocyclicus", "Cycloscapus", "Dulanocrinus", 
                 "Dwortsowaecrinus", "Exaesiodiscus", "Flexicrinus", 
                 "Floripila", "Flucticharax", "Glyphidocrinus", 
                 "Heterostelechus", "Jonkerocrinus", "Kstutocrinus", 
                 "Lamprosterigma", "Laudonomphalus", "Lichenocrinus", 
                 "Malovicrinus", "Mooreanteris", "Pandocrinus", 
                 "Pentagonocyclicus", "Pentaridica", "Preptopremnum", 
                 "Salairocrinus", "Schyschcatocrinus", "Stenocrinus", 
                 "Tetragonocyclicus", "Tetralobocrinus", "Tjeecrinus", 
                 "Zeravschanocrinus")
wh.forms <- which(marine.taxa$Genus %in% known.forms | 
                    marine.taxa$Subgenus %in% known.forms)
marine.taxa <- marine.taxa[-wh.forms, ]

# Special rule for Anaptychus, which is a junior subjective synonym of Sidetes
# and a homonym of a decapod (which is j.s.s. of Ala, so neither should be
# present)
wh.anaptychus <- which(marine.taxa$Genus == "Anaptychus" &
                         marine.taxa$Class == "Cephalopoda")
if (length(wh.anaptychus) > 0L)
  marine.taxa <- marine.taxa[-wh.anaptychus,]

# Save object
# write.csv(marine.taxa, file = "PBDBformatted_NoTerr.csv", row.names = FALSE)
# x <- read.csv(file = "PBDBformatted_NoTerr.csv", header = TRUE, stringsAsFactors = FALSE)


## Run following to manually combine my database and the PBDB databases

# (1) In Excel, open the "postSizes.tab" or "postLH.tab" file (may need to
# re-export from FMP and use SelectCols.R to align proper columns) and re-save
# as "PreSizes_withPBDB.tab" or "PreLH_withPBDB.tab" (MANUALLY ADD THE ".TAB" TO
# FILE NAME TO FORCE AS TAB-DELIMITED INSTEAD OF TEXT FILE FORMAT. Open
# "PBDBformatted_NoTerr.csv" and copy this data into the combined database.
# Manually delete any "NA"s in early and late ages. If using both a "mode" and
# "constant" LH data treatment, only need to propogate sizes using one of these
# data sets, as the size propogations are the same for both.

# (2) Open here and run following code to remove duplicated genus entries.

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
# pre <- read.delim(file = "PreSizes_Constant_withPBDB.tab", stringsAsFactors = FALSE)
head(pre)
tail(pre)
duplicate.G <- duplicated(pre$Genus)
post <- pre[!duplicate.G, ]
write.table(post, file = "PreSizes_Constant_withPBDB.tab", quote = FALSE, 
            sep = "\t", row.names = FALSE)

# (3) Copy new entries by Phylum > Subphylum > Class > Subclass > Order.

# (4) Add new IDNumbers (that pick up after those in the existing database), and
# re-save.

# (5) Run entire PBDB-propogated data set (including core) using
# IDBadHigherTaxa.R to identify any other possible taxonomic mis-alignments or
# inconsistencies in my taxonomy and that of the PBDB. Common changes include
# the following:

#  a. Changing suffixes (e.g., -acea to -oidea), adding superfamily and suborder
#  names when left empty in PBDB. See code in IDBadHigherTaxa.R for a function
#  to automate.

#  b. Some names have different ranks in my database and the PBDB. See code in 
#     IDBadHigherTaxa.R for a function to automate. Known instances include:

#   - CHANGE the following subclass ranks to rank class:
#       (i) arthropods Malacostraca and Cirripedia (and set in Subphylum Crustacea), 
#       (ii) echinoderms Blastoidea and Parablastoidea
#       (iii) vertebrates Placodermi and Thelodonti.

#   - CHANGE Class Opisthobranchs down a rank to subclass (and placing them in
#     Class Heterobranchia) such that Opisthobranchia is a Subclass and their 
#     orders are suborders.

#   - Change order Cephalodiscida to Cephalodiscoidea (in class Cephalodiscida), 
#     per WoRMS.

#   - Place fish taxon Gasterosteiformes as a suborder within order 
#     Perciformes (not order Gasterosteiformes), per modification of 
#     Betancur-R, et al., 2013.

#   - Place fish taxon Scorpaeniformes as a suborder within order 
#     Perciformes (not order Scorpaeniformes), per modification of 
#     Betancur-R, et al., 2013.

#   - Treat Notomyotida as an asteroid order (not also as a suborder), leaving 
#     the suborder unnamed.

#   - Treat Orchocladina as a sponge order (not also as a suborder), leaving 
#     the suborder unnamed, per Rigby, et al., 2004.

#   - Treat Phymosomatoida as an echinoid suborder (not also as an order), 
#     leaving the order UNCERTAIN.

#  c. CHANGE rank names for Order Rhombifera (Subphylum Pelmatozoa, Class
#     Cystoidea, Subclass Hydrophoridea) to Class Rhombifera (Subphylum 
#     Blastozoa, Order UNCERTAIN), and CHANGE Subphylum Pelmatozoa to Blastozoa.

#  d. ADD new taxonomic names for following:

#       (i)   Class Hyolitha (in Phylum Hyolitha) for Orders Hyolithida and
#               Orthothecida.
#       (ii)  Class Dipnomorpha for orders Dipnoi and Dipnotetrapodomorpha.
#       (iii) Class Tentaculitita / Phylum Mollusca for Order Tentaculitida.
#       (iv)  Phylum Annelida for Class Palaeoscolecida.
#       (v)   Phylum Agmata for Order Volborthellida.
#       (vi)  Add name UNCERTAIN for any phylum, class, order, or family that is 
#               blank.
#       (vii) Change phylum Problematica to UNCERTAIN.

#  e. CHANGE the following names (typically alternative spellings or 
#     archaic synonyms):
#       (i)    cephalochordate order Amphioxi to Amphioxiformes
#       (ii)   vertebrate class Actinopteri to Actinopterygii
#       (iii)  sponge class Demospongea to Demospongiae 
#       (iv)   sponge class Archeocyatha to Archaeocyatha
#       (v)    fish order Birkeniida to Birkeniiformes
#       (vi)   annelid subclass Aciculata to Errantia
#       (vii)  starfish class (or infraclass) Neoasteroidea to Ambuloasteroidea
#       (viii) bryozoan order Trepostomida to Trepostomata 
#       (vix)  bryozoan order Cystoporida to Cystoporata 
#       (x)    barnacle order Thoracica to Sessilia
#       (xi)   diploporitan superfamily Glyptosphaeritida to 
#              Glyptosphaeritidacea (in order UNCERTAIN). 
#              (See below for details on other diploporitans.)
#       (xii)  Superfamily Mosasauria to superfamily Mosasauroidea

#  f. Because the most recent bivalve classification (Carter, et al. 2011, to be
#     used in the forthcoming Treatise) contains only two subclasses for all
#     bivalves, elevate their Infraclass names to subclass rank for their 
#     Subclass Autobranchia (which is similar to how WoRMS names the bivalves).
#     For outdated names (i.e., bivalves listed in PBDB without a family assignment 
#     and placed in outdated orders used by Sepkoski but not mentioned in 
#     Carter, et al., 2011), use the following rules: orders Anomalodesmacea, Myoida, 
#     Thraciida, and Trigoniida are in Subclass Heteroconchia. Members of 
#     Superfamily Grammysioidea (Families Grammysiidae and Sanguinolitidae) and 
#     Superfamily Lyrodesmatoidea (Family Lyrodesmatidae) are in Order UNCERTAIN 
#     and Subclass Heteroconchia. Order Pterioida is in Subclass Pteromorphia.
#     (This has the effect of making my propogation algorithm more conservative
#     because smaller ranks are treated as larger, more inclusive ones.)

#  g. For opisthobranch and pulmonate gastropods, use a modified version of 
#     Bouchet and Rocroi (2005), that is also consistent with how WoRMS treats 
#     their clade names. Treat Heterobranchia as the subclass that includes the 
#     orders Allogastropoda (= lower Heterobranchia, including Architectibranchia 
#     as a suborder), Opisthobranchia, and Pulmonata, and treat their major 
#     subgroups as suborders. Treat Sepkoski's Order Heterostrophia as 
#     Opisthobranchia incertae sedis and his Order Cephalaspida as opisthobranch 
#     Suborder Cephalaspidea. Treat Suborder Bellerophontina as Order Bellerophontida

#  h. For certain vertebrate groups whose taxonomy is often cladistically based
#     on unranked taxa, use the following ranks. Treat Order Ichthyosauria as
#     suborder in Order Ichthyopterygia, and large inclusive clades (e.g.,
#     Merriamosauria) as superfamilies. Treat traditional orders Nothosauroidea,
#     Placodontia, and Plesiosauria as suborders within Order Sauropterygia. (For
#     placodonts, also place suborders Cyamodontoidea and Placodontoidea as
#     superfamilies. For nothosaurs, treat orders Nothosauria and
#     Pachypleurosauroidea the major nothosaur subgroups used in PBDB as
#     superfamilies.) But maintain superfamily Pistosauroidea as a valid
#     superfamily in unnamed suborder.

#  i. Use (only) the following subphylum names for (primarily marine)
#     arthropods: Arachnomorpha, Chelicerata, Crustacea, and Trilobitomorpha. Use
#     (only) the following subphylum names for echinoderms: Asterozoa (including
#     classes Asteroidea, Ophiuroidea, Somasteroidea, and Stenuroidea), Blastozoa
#     (including classes Blastoidea, Coronoidea, Ctenocystoidea, Diploporita,
#     polyphyletic 'diploporitans', Eocrinoidea, Soluta, Homostelea [and treating
#     Cincta as a near synonym, as sole homostelean order], Parablastoidea,
#     Paracrinoidea [thus not recognizing Subphylum Paracrinozoa], and Rhombifera),
#     Crinozoa (including classes Crinoidea, Edrioasteroidea,  Stylophora), and
#     Echinozoa (including classes Ctenoimbricata, Cyclocystoidea, Echinoidea,
#     Helicoplacoidea, Holothuroidea, and Ophiocistioidea).

#  j. For tetrapods, do not use the larger fish-inclusive Dipnotetrapodomorpha
#     as subclass (and Dipnomorpha for class). Instead, use Subclass Eutheria
#     (etc.) for mammals (whales, sirenians, etc.); and place whales in Order 
#     Artiodactyla instead of Cetacea. Use Subclass Eureptilia for reptiles 
#     (plesiosaurs, ichthyosaurs, squamate mosasaurs, thalattosaurs, etc.). 
#     Use Subclass Testudinata for turtles. Use Subclass Aequorlitornithes 
#     for most birds (within Class Aves).

#  k. Treat Xiphosura as a class that is a synonym with prior Class Merostomata,
#     despite how WoRMS ranks them. (Be aware that many PBDB "xiphosurans" are
#     terrestrial arachnids and not marine horseshoe crabs and the like.) Because
#     PBDB eurypterids are currently classified in PBDB in Order Eurypterida within
#     Order Xiphosurida, their eurypterid affiliation will be over-ridden with the
#     algorithm above. Assign members of suborders Eurypterina and Stylonurina to
#     Order Eurypterida (in blank subclass). Following Lamsdell (2013), place 
#     Order Eurypterida and Order Chasmataspidida (although a polyphyletic basal 
#     member closely related to eurypterids), (and Class Arachnida) in unranked 
#     taxon Dekatriata, elevated to class status for now, until a replacement 
#     class name is proposed.

#  l. Given ongoing difficulty in classification of chaetetids (most are 
#     demosponge form taxa with a few putative tabulates, c.f., Webby, et al. 
#     2015 Treatise for discussion: ), assign all chaetetid families (e.g., 
#     Chaetetidae, Cryptolichenariidae) to subfamily rank (e.g., Chaetetinae, 
#     Cryptolichenariinae, and maintaining subfamily names if provided) within 
#     Family Suberitidae (the assignment for genus Chaetetes), Order Hadromerida 
#     (instead of Chaetetida), Subclass UNCERTAIN, Class Demospongiae.

#  m. Move sponges placed by PBDB in order Calcaronea into subclass Calcaronea 
#     (sensu Rigby, et al. 2004) and place in order UNCERTAIN (unless the family 
#     indicates a known order).

#  n. Treat the Elasmobranchii and Holocephali as subclasses within Class 
#     Chondrichthyes. (And following Coates, et al. 2017 treat Symmoriiformes 
#     as holocephalans.) And treat the Batoidea as a superorder (and thus 
#     unranked here) of elasmobranch.

#  o. Follow Maletz (2014, basis of forthcoming Treatse revision) in treating
#     Graptolithina as a subclass in Class Pterobranchia (with Cephalodiscida 
#     as other subclass).

#  p. For converting Sepkoski's archaic polychaete orders, use Amphinomida for
#     Amphinomorpha, Eunicida for Eunicemorpha, Phyllodocida for Phyllodocemorpha,
#     Spionida for Spiomorpha, Scolecida (Family Arenicolidae) for Drilomorpha,
#     Terebellida for Terebellomorpha, Terebellida (Family Flabelligeridae) for
#     Flabelligerimorpha, and Sabellida (Family Serpulidae) for Serpulimorpha.

#  q. Based on recent work by Skovsted and Holmer and their group (mostly in
#     2008-2009), treating Hyolithelminthida (including Hyolithellidae and
#     Torellellidae) and other Tommotiida (including Lapworthellidae,
#     Tannuolinidae, and Tommotiidae) as stem brachiopods (Phylum Brachiopoda).
#     Based on discoveries in Moysiuk, et al. (2017), retaining hyolithids in their
#     own phylum Hyolitha, as lophophorates that are possibly stem brachiopods like
#     tommotiids. Allow Tommotiida to serve as both a class and an order and
#     Hyolitha as both a phylum and class, given their taxonomic ambiguities.

#  r. For the branchiopods, primarily use WoRMS instead of PBDB, with Class
#     Branchiopoda, subclasses Calmanostraca (extant all freshwater with order
#     Notostraca), Sarsostraca (extant all freshwater with order Anostraca), and
#     Diplostraca (= Conchostraca, with orders Laevicaudata and Spinicaudata and all
#     cladoceran orders). Like PBDB (and unlike WoRMS which subsumes the cladoceran
#     taxa as individual orders), treating Cladocera as a valid order and using
#     WoRMS cladoceran orders (Anomopoda, Ctenopoda, Haplopoda, and Onychopoda) as
#     suborders. Genera in outdated Subclass Phyllopoda with UNCERTAIN orders are
#     placed in Subclass UNCERTAIN because unclear whether diplostracans or notostracans.

#  s. Use PBDB to change suffix for superfamilies to -oidea or -acea based on 
#     consensus.

#  t. Despite PBDB (and some primary literature articles) claiming the order
#     Metacopida is valid, most consider them in suborder Metacopina and order
#     Podocopida, the taxonomy used herein.

#  u. Following WoRMS (and modified from Bouchet and Rocroi, 2005), treat
#     Neogastropoda as a suborder in order Hypsogastropoda / subclass 
#     Caenogastropoda.

#  v. Do not override the following higher taxonomic homonyms! There are two
#     families Ctenodontidae, one a bivalve and the other a dipnoi fish. The
#     Bdelloidea are both a rotifer class and a xiphosuran superfamily.

#  w. Because the most recent crinoid classification (Wright, et al. 2017)
#     contains only two subclasses (camerates and pentacrinoids) for all
#     crinoids, elevate the parvclass or superorder names to subclass rank for
#     Subclass Pentacrinoidea, but maintaining the order names. Allowed
#     pentacrinoid subclasses include the stem inadunates, Disparida,
#     Porocrinoidea, Flexibilia, Cyathoformes, Ampelocrinida (possibly
#     paraphyletic), and Articulata. Subclass Eucamerata orders include the
#     Diplobathrida, Monobathrida, and "stem eucamarates". (This has the effect
#     of making my propogation algorithm more conservative because smaller ranks
#     are treated as larger, more inclusive ones.)

#  x. Following Sheffield and Sumrall (2019), place the diploporitans in
#     Glyptosphaeritidacea and Asteroblastida in Class 'diploporitans' because they
#     are now polyphyletic. Use Order Diplporita only for members of Sphaeronitida
#     and any traditional diploporitan NOT explicitly noted in their paper as not
#     monophyletic members of the Sphaeronitida clade. (In other words, the
#     default, for now, is to assume all diploporitans are within Diploporita
#     unless explicitly known not to be.)

#  y. Following Kroh and Smith (2010) for taxonomy of echinoids (opinions also
#     entered into PBDB), but adding superfamily and/or suborder rankings (which
#     they often avoided) in cases where an order is assigned and infraorder
#     subclades are assigned. Reducing following order Camarodonta infraorders to
#     suborders: Temnopleuridea (with families Temnopleuridae, Trigonocidaridae,
#     Zeoglopleuridae, and Glyphocyphidae) and Echinidea (with superfamily
#     Odontophora with families Toxopneustidae, Strongylocentrotidae, and
#     Echinometridae, and superfamily UNCERTAIN with families Echinidae and
#     Parechinidae) and suborder/superfamily UNCERTAIN (with family Parasaleniidae).
#     For suborder Scutellina, reduce infraorders Laganiformes (including
#     Fibulariidae = Echinocyamidae and Laganidae = Laganinae and Neolaganinae) and
#     Scutelliformes (including Taiwanasteridae, Protoscutellidae [in stem group],
#     Echinarachniidae, Dendrasteridae, Rotulidae, Scutellidae, Eoscutellidae,
#     Scutasteridae, Abertellidae, Astriclypeidae, Monophorasteridae, and
#     Mellitidae) to superfamily rank; family Scutellinidae is then in superfamily
#     UNCERTAIN as stem Scutellina. Treat unranked clade Meridosternata as a
#     suborder of Holasteroida and reduce infraorders Cardiasterina (= Stegasterina
#     (including Stegasteridae and Cardiasteridae = Cardiotaxinae) and Urechinina
#     (including echinoid homonym Corystidae replaced by Corystusidae, Calymnidae,
#     Carnarechinidae, Urechinidae, Plexechinidae, and Pourtelasiidae) to
#     superfamily rank [note that the corystusids and calymnids are not listed this
#     way on page 173 but clearly an error based on cladogram in fig. 2]; families
#     Echinocorythidae and Holasteridae are then in superfamily UNCERTAIN as stem
#     Meridosternata and families Stenonasteridae, Hemipneustidae, and
#     Pseudholasteridae are in superfamily and suborder UNCERTAIN as stem
#     holasteroids.

#  z. Following Blake (2018) for (primarily Paleozoic) asteroids (and other
#     asterozoans: somasteroids, stenuroids, and ophiuroids). (This set of 
#     opinions has already been entered into PBDB.) Because Blake (2018) disused 
#     previous suborder names (e.g., Diplozonina, Eugnathina, Hemizonina, 
#     Platyasterida, Pustulosina, and Uractinina) for Paleozoic echinoids, they 
#     are retained when restricted to his new orders. Need to write out these 
#     exceptions when propagated later. Following Gale (2012) for (primarily 
#     post-Paleozoic) asteroids, but treating unranked clades Forcipulatida,
#     Hemizonida, Paxillosida, Spinulosida, and Valvatida as orders, and clades
#     within them as suborders (either when monophyletic from this paper or as 
#     applied from earlier Treatise usage). For example, for Asteriadina, 
#     Brisingina, Cribellina, Diplozonina, Leptognathina, and Tumulosina, 
#     although these are most likely irrelevant because they are now essentially 
#     mono-familial. Treating unranked Neoasteroidea as a synonym of subclass 
#     Ambuloasteroidea (which it very nearly is).

# aa. Following Parry, et al. (2019) that demonstrates machaeridians are
#     polychaetes in order Phyllodocida and suborder Aphroditiformia, downgrade
#     machaeridian orders Lepidocoleomorpha, Hercolepadida, and Turrilepadomorpha
#     as new superfamilies.

# ab. Based on the affirmation of Lindberg and Ponder (2020) regarding
#     conclusions of Dzik (2010), treating problematic tergomyan
#     ("monoplacophoran") families Kirengellidae, Pygmaeoconidae = Pygmaeoconinae,
#     Romaniellidae, and Scenellidae as synonyms of Hypseloconidae in Order
#     Kirengellida (= senior to junior synonyms Hypseloconida and Romaniellida),
#     and treating all as uncertain (possibly stem-group Craniiformea) members of
#     Brachiopoda (class UNCERTAIN and subphylum blank).

# ac. Based on results in Lerosey-Aubril, et al. (2017), replace subphylum 
#     Trilobitomorpha with near-equivalent but better-defined subphylum Artiopoda, 
#     including classes Trilobita, Nektaspidida, and Vicissicaudata. Modify class 
#     Nektaspidida to include monotypic order Nektaspida. Treat Vicissicaudata 
#     as a class instead of superclass, including subclasses Aglaspidida and 
#     Cheloniellida (and Merostomoidea) and remaining unclassified genera in 
#     subclass UNCERTAIN. For those genera in class Merostomoidea, downgrade the
#     name to subclass status, although likely not monophyletic. Treat class 
#     Aglaspidida as a subclass with orders Aglaspidida (of same name) and
#     Strabopida.

# ad. Treat Order Bradoriida as a non-ostracod member of the stem Crustacean,
#     following consensus in Álvarez, et al. (2008) and Siveter, et al., (2014).

# ae. The affinity of radiocyaths is uncertain (Treatise: Kruse, et al., 2015),
#     with most considering them either allied to archaeocyath or heteractine
#     sponges or receptaculacean (dasyclad) algae. Although most recent research
#     supports an algal affinity, re-ranking class Radiocyatha as distinct 
#     subclass of class Archaeocyatha so that life habit propagates as a sponge 
#     model. (Easier to assume an animal now and secondarily remove than to 
#     ignore and add in later, if future consensus emerges.) subclass

# (6) Run code as usual in "PropogateSizes.R" or "PropogateLifeHabits.R", but
# resaving as postX_withPBDB" file name. Make sure to add new IDNumbers to the
# new PBDB entries!

# (7) Import into copy of FileMakerPro life habit database, adding the new
# entries. Use this one for running next analyses.

# (8) Before running disparity and tiering analyses, open here and remove the
# non-terrestrials (and non-fossils with Recent-only occurrences?) once again,
# in case any got included during the taxonomy standardization.

