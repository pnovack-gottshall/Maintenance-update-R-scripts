## RECREATE DIGITAL SEPKOSKI COMPENDIUM FROM FOSSILBRUSH WITH INTERVAL NAMES ###
## INSTEAD OF FOSSILBRUSH AGES #################################################


## Peter Wagner's version (shared by email Dec. 6, 2023)

setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Documents/SepkoskiCompendium")
load("Sepkoski_Genus_Compendium.RData")
head(sepkoski_compendium)

# This version sets Sepkoski's original taxonomy in columns marked "98" and 2023
# PBDB reassignments and taxonomy for other columns.

# Note Skwia was corrected to Sekwia
sepkoski_compendium[which(sepkoski_compendium$Class_98=="Cyclozoa"), ]

# But Cályxhydra was treated differently:
sepkoski_compendium[grep("xhydra", sepkoski_compendium$Genus), ]

# Let's see if Peter's version pre-dates Shanan's website version (or whatever
# was the source for Lewis Jones' digital version)
sepkoski_compendium[which(sepkoski_compendium$Order_98 == "Foraminiferida"), ][168:169, ]

# Yes! Shanan's version has Auroria (Sepkoski mis-spelling of Auroriina)
# followed by weird Cályxhydra instead of Austrocolomiá (Sepkoski typo for
# Austrocolomia). But seems the non-Latin letters have been replaced with · in
# Peter's version. Most (but not all) of these are the known errors in my
# version.

# Peter thinks that "he" originated the non-Latin errors when converting Jack's
# DOS files to Mac Excel format in early/mid 1990s.



sepkoski_compendium[grep("á", sepkoski_compendium$Genus), ]
sepkoski_compendium[grep("·", sepkoski_compendium$Genus), ]




op <- par()
rm(list = ls())

setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

## LIBRARIES
library(fossilbrush)      # v. 1.0.3
library(beepr)            # v. 1.3

## IMPORT DIGITAL COMPENDIUM ###################################################

# fossilbrush includes a digitized Compendium (from chronosphere)
data("sepkoski")
head(sepkoski)


## PROCESS TIME INTERVAL DATA TABLE ############################################

# The FADs and LADs are dates instead of Sepkoski's original use of intervals.
# These can be resurrected from the interval look up table from fossilbrush.
head(sep_code)

# Explore this object
sq <- seq.int(nrow(sep_code))
pairs <- sapply(sq, function(sq) paste(sep_code$FAD[sq], sep_code$LAD[sq]))
nrow(sep_code)
length(unique(pairs))
# 8 intervals share the same FAD/LAD pairs

# Which?
sort(table(pairs))
# Guadalupian = Middle Permian
# Miocene = Upper Pliocene ??? Note says ICS Piacenzian
# Pennsylvanian = Upper Carboniferous
# Mississippian = Lower Carboniferous
# australis-kockelianus zone spans upper Givetian and upper Eifelian. Note says conodont zone not in Givetian
# costatus zone spans lower Givetian and lower Eifelian. Note says conodont zone not in Givetian
# Wenlockian = Middle Silurian (Sheinwoodian)
# Llandoverian = Lower Silurian (Rhuddanian)

# To ease the algorithm below, we'll drop the duplicates and reverse the order
# (so that stages and biozones are at the top of their respective periods).
# We'll specifically drop the Upper Pliocene, Middle Permian, Upper/Lower
# Carboniferous, Lower/Middle Silurian, and duplicated Givetian biozones, to use
# the more modern equivalents above instead (Miocene, Pennsylvanian, Missippian,
# Guadalupian, Wenlockian, and Llandoverian).

# The reason to reverse order is so the algorithm matches to narrower zones
# instead of broader zones (in the manner in how Sepkoski did in his
# Compendium). This also minimizes the risk of non-sensible intervals (such as
# FAD = Caradoc and LAD = Ordovician, which would imply the FAD was earlier than
# reported.)

# We'll also drop the Recent to the last position, so that genera with LAD = 0
# are preferentially placed in the Quaternary. Downstream, we'll instead use
# WoRMS to determine which genera are extant.

sort.order <- 
  c(306:238, 236:234, 232:213, 210:191, 189:176, 174:161, 159:10, 8:3, 1, 2)
sep_code <- sep_code[sort.order, ]
dim(sep_code)
head(sep_code)
tail(sep_code)



## ADD INTERVAL NAMES FOR RANGES ###############################################

# Given the size of the Compendium, better to loop through unique FAD/LADs
# rather than genera. This also ensures consistency among genera sharing the
# same ranges.

bases <- sort(unique(sepkoski$RANGE_BASE))
tops <- sort(unique(sepkoski$RANGE_TOP))

# Add new columns to record stratigraphic ranges (with names to match those in
# PBDB)
sepkoski$early_interval <- ""
sepkoski$late_interval <- ""

# First pass: Add intervals first whose FAD and LAD match those in the (now) 298
# 'sep_code' intervals. Unlike Sepkoski's Compendium, duplicating the first
# interval column for (mostly singleton) genera restricted to a single interval.
# Doing it in two passes to more likely maintain the original ranges used by
# Sepkoski.
ages <- nrow(sep_code)
for (t in 1:ages) {
  wh.bin <- which(sepkoski$RANGE_BASE == sep_code$FAD[t] &
                    sepkoski$RANGE_TOP == sep_code$LAD[t])
  sepkoski$early_interval[wh.bin] <- sep_code$sepkoski_code[t]
  sepkoski$late_interval[wh.bin] <- sep_code$sepkoski_code[t]
}

# How many added? ~ 55% (19826)
sum(sepkoski$early_interval != "") / nrow(sepkoski)
sum(sepkoski$early_interval != "")

# Compare with Sepkoski's Compendium (Word file or Bull of Amer Paleo Memoir).
head(sepkoski)
# Seems correct. The only major difference is that period abbreviations are left
# off. (E.g., "Aren" instead of O (Aren).)

# Second pass: Work separately through the FADs and LADs (using the first match
# to prioritize smaller stratigraphic intervals). We can simplify the index
# because the length of unique bases is the same as the length of unique tops.

length(bases)
length(tops)

# Ignore those whose intervals are already entered
entered <- which(sepkoski$early_interval != "")

for (t in 1:length(bases)) {
  
  # Process FADs first
  wh.FAD <- which(sepkoski$RANGE_BASE == bases[t])
  
  # Skip those already entered
  wh.entered <- which(wh.FAD %in% entered)
  if (length(wh.entered) > 0L)
    wh.FAD <- wh.FAD[-wh.entered]
  
  if (length(wh.FAD) > 0L) {
    wh.interval <- which(sep_code$FAD == bases[t])
    # In case of multiple matches, using first returns the smallest interval
    sepkoski$early_interval[wh.FAD] <- sep_code$sepkoski_code[wh.interval[1]]
  }
  
  # Process LADs second
  wh.LAD <- which(sepkoski$RANGE_TOP == tops[t])
  wh.entered <- which(wh.LAD %in% entered)
  if (length(wh.entered) > 0L) wh.LAD <- wh.LAD[-wh.entered]
  if (length(wh.LAD) > 0L) {
    wh.interval <- which(sep_code$LAD == tops[t])
    sepkoski$late_interval[wh.LAD] <- sep_code$sepkoski_code[wh.interval[1]]
  }

}
beepr::beep()

    
# How many added? Now all of them (35700)
sum(sepkoski$early_interval != "") / nrow(sepkoski)
sum(sepkoski$late_interval != "") / nrow(sepkoski)

# Confirm matches Compendium
head(sepkoski, 10)
# Good - the major difference (aside from lacking period prefix) is the
# intervals are often made narrower (e.g., Eo-l-l vs T (Eo-l)). But given the
# intervals share the same FAD/LAD boundaries, this is fine.

## SAVE FILE TO ARCHIVE ########################################################

## Include rownames so can return to original order
# write.csv(sepkoski, file = "Sepkoski_Compendium.csv", row.names = TRUE)

## Non-Latin letters in the Compendium include the following:
## á, ã, å, é, í, ï, ó, ò, ô, Š, and ù


## Manually change the following genera with non-Latin letters:
# PHYLUM        CLASS	         ORDER              COMPEND. SPELLING PBDB ENTERED AS
# Rhizopodea    Rhizopodea     Foraminiferida     Austrocolomiá     Austrocolomia
# Porifera	    Regulares	     Capsulocyathida    Còyptoporocyathus	Cyptoporocyathus
#                                                     =misspelling of Cryptoporocyathus
# Cnidaria	    Hydrozoa	     Hydroida           Cályxhydra	      Calyxhydra
# Cnidaria	    Hydrozoa	     Hydroida           Drevoôella	      Drevoella
#                                                     =misspelling of Drevotella
# Cnidaria	    Hydrozoa	     Hydroida           Veluíbrella	      Velumbrella
# Cnidaria	    Anthozoa	     Scleractina        Hindeastraeá      Hindeastraea
# Cnidaria	    Anthozoa	     Scleractina        Heliocïenia       Heliocenia
#                                                     =misspelling of Heliocoenia
# Cnidaria	    Anthozoa	     Scleractina        Hexaheliocïenia   Hexaheliocenia
#                                                     =misspelling of Hexaheliocoenia
# Cnidaria	    Anthozoa	     Scleractina        Holocùsôió        Holocsi
#                                                     =misspelling of Holocoenia
# Cnidaria	    Anthozoa	     Scleractina        Hydnophorareá     Hydnophorarea
#                                                     =misspelling of Hydnophorarea
# Cnidaria	    Anthozoa	     Scleractina        Isástraea         Isastraea
# Cnidaria	    Anthozoa	     Scleractina        Isïphyllastrea    Isphyllastrea
#                                                   =misspelling of Isophyllastrea
# Cnidaria	    Anthozoa	     Scleractina        Kángilacùathus	  Kangiliacyathus
# Cnidaria	    Anthozoa	     Scleractina        Káratchasôraea	  Karatchastraea
# Cnidaria	    Anthozoa	     Scleractina        Mussisíilia       Mussisiilia
#                                                   =misspelling of Mussismilia
# Cnidaria	    Anthozoa	     Scleractina        Oppelisíilia	      Oppelisilia
#                                                   =misspelling of Oppelismilia
# Cnidaria	    Anthozoa	     Scleractina        Paãhythecalis     Pachythecalis
# Cnidaria	    Anthozoa	     Scleractina        Parapleurosmélia  Parapleurosmilia
# Cnidaria	    Anthozoa	     Scleractina        Parastraeoíorpha	Parastraeoorpha
#                                                   =misspelling of Parastraeomorpha
# Cnidaria	    Anthozoa	     Scleractina        Parasôraeopora	  Parastraeopora
# Cnidaria	    Anthozoa	     Scleractina        Sïlenocoenia      Slenocoenia
#                                                   =misspelling of Solenocoenia
# Cnidaria	    Anthozoa	     Scleractina        Smiloôrochus      Smilorochus
#                                                   =misspelling of Smilotrochus
# Cnidaria	    Anthozoa	     Scleractina        Stephanasôrea     Stephanasrea
#                                                   =misspelling of Stephanastrea
# Cnidaria	    Anthozoa	     Scleractina        Stenocùathus      Stenocathus
#                                                   =misspelling of Stenocyathus
# Cnidaria	    Cyclozoa	     UNCERTAIN          Såkwia	          Sekwia
# Cnidaria	    Medusae	       Incertae sedis     Jyxiålla	        Jyxialla
# Brachiopoda	  Inarticulata	 UNCERTAIN          Ernogiá	          Ernogia
#                                        note prior was corrected in 'sepkoski'
# Brachiopoda	  Inarticulata Lingulata	          Dysoristuó	      Dysoristus
#                                        note prior was corrected in 'sepkoski'
# Brachiopoda	  Inarticulata Lingulata	     Prototretá	        Prototreta
#                                        note prior was corrected in 'sepkoski'
# Mollusca      Incertae sedis Incertae sedis     Truncátoconus     Truncatoconus                         
# Echinodermata	Echinoidea	   Cidaroida          Mikrocidarió	    Mikrocidari
#                                                   =misspelling of Mikrocidaris

# Also note that because of a concatenation error, the following genus was not added to the 'sepkoski' database: 
# Brachiopoda	  Articulata     Terebratulida     Aneuthelasma     P (Guad-u) (ref 7)

# And some brachiopods (others, too?) seem to be missing, such as order
# Kutorginida, Obolellida, Chileida, Naukatoidea, and uncertains following them.
# Worth checking for others that may have been missed?



# And also make write-protected to archive the file.

## Confirm open as expected
# sepkoski_cleaned <- read.csv("Sepkoski_Compendium.csv", row.names = 1)
head(sepkoski_cleaned)

trouble <- c(16148, 16157, 16160, 16181, 16229, 17997, 18018, 18019, 18139, 
             18174, 18176, 18177, 18323, 19870, 21413, 24368, 24433, 24518,
             24911, 29148)
sepkoski$GENUS[trouble]
sepkoski_cleaned$GENUS[trouble]

