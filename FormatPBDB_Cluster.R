## Format PBDB genus/subgenus occurrences into taxonomic structure of my data,
## using a parallel computing environment

# The serial version takes ~4.5 hours to process PBDB into a formatted taxonomic
# dataframe structure. This code rewrites the algorithm as a function that can
# be implemented "in parallel."

## Download data directly from PaleobioDB and save to working directory (will be
## > 40 MB) (DO NOT RESTRICT TO PHANEROZOIC! Restricting only includes taxa with
## fossil occurrences. Here we want ALL taxa!)

setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

rm(list=ls())
## Easier if paste link into URL and save manually 
# pbdb <- read.csv("www.paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&show=app&vocab=pbdb")
# If want forams too, use base_name=Metazoa,Retaria
pbdb <- read.csv("pbdb_data.csv")
head(pbdb)


## Function to add higher taxonomic names (phylum, class, order, etc.) for PBDB
## genus (and subgenus) names
# gen.seq = Vector (sequence) of number of genus names to process
# gen.names = Vector of PBDB genus (and subgenus) names
# pbdb = data frame of all PBDB occurrences
#
# output is a list, with each item the taxonomy for a single genus
prep.PBDB <- function(g = 1, gen.names, pbdb) {
  scales <- c("phylum", "subphylum", "class", "subclass", "order", "suborder", 
              "superfamily", "family", "subfamily", "genus", "subgenus")
  out <- data.frame(Phylum=character(1), Subphylum=character(1), 
                  Class=character(1), Subclass=character(1), 
                  Order=character(1), Suborder=character(1),
                  Superfamily=character(1), Family=character(1), 
                  Subfamily=character(1), Genus=character(1), 
                  Subgenus=character(1), Species="sp.", 
                  stringsAsFactors=FALSE)
  out$Genus <- as.character(gen.names[g])
  wh <- which(pbdb$accepted_name == out$Genus & (pbdb$taxon_rank == "genus" | 
                                                   pbdb$taxon_rank == "subgenus"))[1]
  out$early_age <- as.numeric(pbdb$firstapp_max_ma[wh])
  out$late_age <- as.numeric(pbdb$lastapp_min_ma[wh])
  if (pbdb$accepted_rank[wh] == "subgenus")
    out$Subgenus <- as.character(out$Genus)
  parent <- pbdb[which(pbdb$accepted_no == pbdb$parent_no[wh]), ][1, ]
  repeat {
    if (parent$accepted_rank %in% scales)
      out[1, which(scales == parent$accepted_rank)] <-
        as.character(parent$accepted_name)
    parent <-
      pbdb[which(pbdb$accepted_no == parent$parent_no),][1,]
    if (all(is.na(parent)))
      break
  }
  return(out)
}

## Function to unpack the list into a compact data frame
# Note that with a large PBDB object that this still takes some time to
# complete. Consider tweaking to speed up.
unpack.PBDB <- function(prep) {
  Seq <- seq(length(prep))
  out <- prep[[1]]
  for (i in 2:length(prep)) {
    out[i, ] <- prep[[i]]
  }
  return(out)
}



# Serial version (running through 1000 genera)
(start.t <- Sys.time())
# Use genera and subgenera
which.gsg <- which(pbdb$accepted_rank=="genus" | pbdb$accepted_rank=="subgenus")
n.gen <- length(unique(pbdb$accepted_name[which.gsg]))
x <- data.frame(Phylum=character(n.gen), Subphylum=character(n.gen), 
                Class=character(n.gen), Subclass=character(n.gen), Order=character(n.gen), 
                Suborder=character(n.gen), Superfamily=character(n.gen), Family=character(n.gen), 
                Subfamily=character(n.gen), Genus=sort(unique(pbdb$accepted_name[which.gsg])), 
                Subgenus=character(n.gen), Species=rep("sp.", n.gen), early_age=numeric(n.gen),
                late_age=numeric(n.gen), stringsAsFactors=FALSE)
scales <- c("phylum", "subphylum", "class", "subclass", "order", "suborder", 
            "superfamily", "family", "subfamily", "genus", "subgenus")
for(g in 1:1000) {
  wh <- which(pbdb$accepted_name == x$Genus[g] & (pbdb$taxon_rank == "genus" | 
                                                    pbdb$taxon_rank == "subgenus"))[1]
  x$early_age[g] <- pbdb$firstapp_max_ma[wh]
  x$late_age[g] <- pbdb$lastapp_min_ma[wh]
  if(pbdb$accepted_rank[wh] == "subgenus") x$Subgenus[g] <- as.character(x$Genus[g])
  parent <- pbdb[which(pbdb$accepted_no == pbdb$parent_no[wh]), ][1, ]
  repeat {
    if(parent$accepted_rank %in% scales) 
      x[g, which(scales == parent$accepted_rank)] <- as.character(parent$accepted_name)
    parent <- pbdb[which(pbdb$accepted_no == parent$parent_no), ][1, ]
    if(all(is.na(parent))) break
  }
}
(Sys.time() - start.t)    

# Serial version takes 232.3 seconds for 1000 (or 4.65 hours for all)



# Re-do using lapply (only 1 CPU)
(start.t <- Sys.time())
which.gsg <- which(pbdb$accepted_rank=="genus" | pbdb$accepted_rank=="subgenus")
gen.names <- sort(unique(pbdb$accepted_name[which.gsg]))
# gen.seq <- seq_along(gen.names)
gen.seq <- 1:1000
prep <- lapply(X = gen.seq, FUN = prep.PBDB, gen.names = gen.names, pbdb = pbdb)
output <- unpack.PBDB(prep)
(Sys.time() - start.t)    
# Serial lapply version takes 41.2 seconds for 1000 (or 49.4 mins for all)


# Now compare using parallel computing:
library(snowfall)
(t.start0 <- Sys.time())
# Initialize
which.gsg <- which(pbdb$accepted_rank=="genus" | pbdb$accepted_rank=="subgenus")
gen.names <- sort(unique(pbdb$accepted_name[which.gsg]))
gen.seq <- seq_along(gen.names)
# gen.seq <- 1:1000
# Set up computer cluster
cpus <- 8					# Number of CPUs to cluster together
# sfSetMaxCPUs(cpus)			# Use if plan more than 32 CPUs
sfInit(parallel=T, cpus=cpus, slaveOutfile="initfile") # Initialize cluster
stopifnot(sfCpus()==cpus)		# Confirm set up CPUs properly
stopifnot(sfParallel()==TRUE)		# Confirm now running in parallel
sfExportAll()				# Export all libraries, files, & objects
# Execute the function
(t.start1 <- Sys.time())
prep <- NA
prep <- sfLapply(x = gen.seq, fun = prep.PBDB, gen.names = gen.names, pbdb = pbdb) # Version without load-balancing
# prep <- sfClusterApplyLB(x = gen.seq, fun = prep.PBDB, gen.names = gen.names, pbdb = pbdb) 	# Version using load-balancing
sfStop()
(Sys.time() - t.start1)
output2 <- unpack.PBDB(prep)
(Sys.time() - t.start0)
(Sys.time() - t.start1)
head(output2)
write.csv(output2, file="PBDBformatted.csv", row.names=FALSE)

# Parallel lapply version takes 60.2 secs for 1000 (21 s for the sfLapply) (or 27.8 min for all)
# Parallel lapply version (w/o export time) takes 19.7 secs for 1000 (or 23.7 mins for all)
# Parallel lapplyLB version takes 642 secs for 1000 (or 12.8 hrs for all)
# Parallel lapplyLB version (w/o export time) takes 602 secs for 1000 (or 12.0 hrs for all)


# Compare output
rbind(x[1000, ], output[1000, ], output2[1000, ])



# x <- read.csv(file="PBDBformatted.csv", header=TRUE, stringsAsFactors=FALSE)
head(x)

# Remove terrestrial and non-marine taxa, but include marine tetrapods. List
# courtesy of Bush and Bambach, 2015, but modified to explicitly include three
# cetacean suborders because Cetacea now listed within Order Artiodactyla in
# PBDB and known marine xiphosurans, and to exclude Myriapoda,
# Kannemeyeriiformes, Pelycosauria, Theriodontia, Therocephalia, freshwater
# conchostrans, and all known arachnid taxa [because many arachnids are getting
# listed in the xiphosuran download])
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
    "Notostraca", "Ammotrechidae", "Anthracomartidae", "Anthracosironidae",
    "Anthracotarbidae", "Aphantomartidae", "Archaeomartidae", "Archaeometidae",
    "Architarbidae", "Ceromidae", "Chaerilobuthidae", "Daesiidae", "Devonotarbidae",
    "Dracochelidae", "Eophrynidae", "Eukoeneniidae", "Garypidae", "Heterotarbidae",
    "Kreischeriidae", "Lissomartidae", "Opiliotarbidae", "Palaeocharinidae", 
    "Palaeotrilineatidae", "Proscorpiidae", "Trigonotarbidae", "Cheiridioidea",
    "Cheliferoidea", "Chthonioidea", "Feaelloidea", "Garypoidea", "Neobisioidea",
    "Sternophoroidea", "Bilobosternina", "Cyphophthalmi", "Dyspnoi", "Eupnoi",
    "Holosternina", "Ixodida", "Laniatores", "Lobosternina", "Meristosternina",
    "Mesostigmata", "Opilioacarida", "Orthosternina", "Posteriorricinulei",
    "Primoricinulei", "Sarcoptiformes", "Tetrophthalmi", "Trombidiformes",
    "Tetrapulmonata")
tetrapods <- c("Mammalia", "Reptilia")
marine.exceptions <- c("Chelonioidea", "Ophidiomorpha", "Mosasauroidea",
    "Thalattosauria", "Sauropterygia", "Ichthyopterygia", "Mesoeucrocodylia",
    "Pterosauria", "Hesperornithiformes", "Ichthyornithiformes", "Sphenisciformes",
    "Procellariiformes", "Pelecaniformes", "Pelagornithidae", "Plotopteridae",
    "Charadriiformes", "Cetacea", "Sirenia", "Pinnipedia", "Desmostylia", "Ariidae", 
    "Plotosidae", "Archaeoceti", "Mysticeti", "Odontoceti", "Diploaspididae",
    "Mycteropidae", "Pterygotidae", "Woodwardopteridae", "Waeringopteroidea",
    "Bellinurina", "Eurypterina", "Limulina", "Stylonurina")
    
sq <- 1:nrow(x)
marine.vert.exceptions <- x[sapply(sq, function(sq) any(marine.exceptions %in% x[sq, ])), ]
# Remove the exceptions here, and add back in (in case of non-tetrapod duplicates)
marine.typical <- x[!sapply(sq, function(sq) any(c(non.marine, tetrapods, 
                                                   marine.exceptions) %in% x[sq, ])), ]
marine.taxa <- rbind(marine.typical, marine.vert.exceptions)
table(marine.taxa$Class)
nrow(x)
nrow(marine.taxa)

write.csv(marine.taxa, file="PBDBformatted_NoTerr.csv", row.names=FALSE)
# x <- read.csv(file="PBDBformatted_NoTerr.csv", header=TRUE, stringsAsFactors=FALSE)


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

rm(list=ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
# pre <- read.delim(file="PreSizes_Constant_withPBDB.tab", stringsAsFactors=FALSE)
head(pre)
tail(pre)
duplicate.G <- duplicated(pre$Genus)
post <- pre[!duplicate.G, ]
write.table(post, file="PreSizes_Constant_withPBDB.tab", quote=FALSE, sep="\t", row.names=FALSE)

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
#       (i)   Class Hyolitha for Orders Hyolithida and Orthothecida.
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
#       (vii)  starfish subclass Neoasteroidea to Ambuloasteroidea
#       (viii) bryozoan order Trepostomida to Trepostomata 
#       (vix)  bryozoan order Cystoporida to Cystoporata 
#       (x)    barnacle order Thoracica to Sessilia
#       (xi)   diploporitan superfamily Glyptosphaeritida to 
#              Glyptosphaeritidacea to (in order UNCERTAIN)

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

#  g. For opisthobranch and pulmonate gastropods, use a modified version of 
#     Bouchet and Rocroi (2005), that is also consistent with how WoRMS treats 
#     their clade names. Treat Heterobranchia as the subclass that includes the 
#     orders Allogastropoda (=lower Heterobranchia, including Architectibranchia 
#     as a suborder), Opisthobranchia, and Pulmonata, and treat their major 
#     subgroups as suborders. Treat Sepkoski's Order Heterostrophia as 
#     Opisthobranchia incertae sedis and his Order Cephalaspida as opisthobranch 
#     Suborder Cephalaspidea. Treat Suborder Bellerophontina as Order Bellerophontida

#  h. For certain vertebrate groups whose taxonomy is cladistically based on often 
#     unranked, use the following ranks. Treat Order Ichthyosauria as suborder in 
#     Order Ichthyopterygia, and large inclusive clades (e.g., Merriamosauria) 
#     as superfamilies. Treat traditional orders Nothosauroidea, Placodontia, 
#     and Plesiosauria as suborders within Order Sauropterygia. (For placodonts, 
#     also place suborders Cyamodontoidea and Placodontoidea as superfamilies. 
#     For nothosaurs, treat orders Nothosauria and Pachypleurosauroidea the 
#     major nothosaur subgroups used in PBDB as superfamilies.) But maintain 
#     superfamily Pistosauroidea as a valid superfamily in unnamed suborder.

#  i. Use (only) the following subphylum names for (primarily marine)
#     arthropods: Arachnomorpha, Chelicerata, Crustacea, and Trilobitomorpha. 
#     Use (only) the following subphylum names for echinoderms: Asterozoa 
#     (including classes Asteroidea, Ophiuroidea, Somasteroidea, and Stenuroidea), 
#     Blastozoa  (including classes Blastoidea, Coronoidea, Ctenocystoidea, 
#     Diploporita, Eocrinoidea, Homoiostelea [including sole Order Soluta], 
#     Homostelea [and treating Cincta as a near synonym, as sole homostelean 
#     order], Parablastoidea, Paracrinoidea [thus not recognizing Subphylum 
#     Paracrinozoa], and Rhombifera, Crinozoa (including classes Crinoidea, 
#     Edrioasteroidea,  Stylophora), and Echinozoa (including classes 
#     Ctenoimbricata, Cyclocystoidea, Echinoidea, Helicoplacoidea, Holothuroidea, 
#     and Ophiocistioidea).

#  j. For tetrapods, do not use the larger fish-inclusive Dipnotetrapodomorpha
#     as subclass (and Dipnomorpha for class). Instead, use Subclass Eutheria
#     (etc.) for mammals (whales, sirenians, etc.); and place whales in Order 
#     Artiodactyla instead of Cetacea. Use Subclass Eureptilia for reptiles 
#     (plesiosaurs, ichthyosaurs, squamate mosasaurs, thalattosaurs, etc.). 
#     Use Subclass Testudinata for turtles. Use Subclass Aequorlitornithes 
#     for most birds (within Class Aves).

#  k. Treat Xiphosura as a subclass within Class Merostomata, similar to how 
#     WoRMS ranks them. (And be aware that many PBDB xiphosurans are terrestrial 
#     arachnids and not marine horseshoe crabs and the like.)

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
#    Branchiopoda, subclasses Calmanostraca (extant all freshwater with order
#    Notostraca), Sarsostraca (extant all freshwater with order Anostraca), and
#    Diplostraca (=Conchostraca, with orders Laevicaudata and Spinicaudata and all
#    cladoceran orders). Like PBDB (and unlike WoRMS which subsumes the cladoceran
#    taxa as individual orders), treating Cladocera as a valid order and using
#    WoRMS cladoceran orders (Anomopoda, Ctenopoda, Haplopoda, and Onychopoda) as
#    suborders. Genera in outdated Subclass Phyllopopda with UNCERTAIN orders are
#    placed in Subclass UNCERTAIN because unclear whether diplostracans or notostracans.

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

# (6) Run code as usual in "PropogateSizes.R" or "PropogateLifeHabits.R", but
# resaving as postX_withPBDB" file name. Make sure to add new IDNumbers to the
# new PBDB entries!

# (7) Import into copy of FileMakerPro life habit database, adding the new
# entries. Use this one for running next analyses.

# (8) Before running disparity and tiering analyses, open here and remove the
# non-terrestrials (and non-fossils with Recent-only occurrences?) once again,
# in case any got included during the taxonomy standardization.

