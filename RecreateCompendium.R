## RECREATE DIGITAL SEPKOSKI COMPENDIUM FROM FOSSILBRUSH WITH INTERVAL NAMES ###
## INSTEAD OF FOSSILBRUSH AGES #################################################

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

## Manually change the following genera with non-Latin letters:
# PHYLUM        CLASS	         GENUS	            CORRECT
# Arthropoda	  Thylacocephala Atropicarió	      Atropicaris
# Brachiopoda	  Inarticulata	 Ernogiá	          Ernogia
# Brachiopoda	  Lingulata	     Dysoristuó	        Dysoristus
# Brachiopoda	  Lingulata	     Prototretá	        Prototreta
# Cnidaria	    Anthozoa	     Holocùsôió	        Holocoenia
# Cnidaria	    Anthozoa	     Kángilacùathus	    Kangiliacyathus
# Cnidaria	    Anthozoa	     Káratchasôraea	    Karatchastraea
# Cnidaria	    Anthozoa	     Oppelisíilia	      Oppelismilia
# Cnidaria	    Anthozoa	     Parapleurosmélia	  Parapleurosmilia
# Cnidaria	    Anthozoa	     Parasôraeopora	    Parastraeopora
# Cnidaria	    Anthozoa	     Parastraeoíorpha	  Parastraeomorpha
# Cnidaria	    Anthozoa	     Sïlenocoenia	      Solenocoenia
# Cnidaria	    Hydrozoa	     Cályxhydra	        Calyxhydra
# Cnidaria	    Hydrozoa	     Drevoôella	        Drevotella
# Cnidaria	    Hydrozoa	     Veluíbrella	      Velumbrella
# Cnidaria	    Medusae	       Jyxiålla	          Jyxialla
# Cnidaria	    NA	           Såkwia	            Sekwia
# Echinodermata	Echinoidea	   Mikrocidarió	      Mikrocidaris
# Mollusca	    Monoplacophora Truncátoconus	    Truncatoconus
# Porifera	    Regulares	     Còyptoporocyathus	Cryptoporocyathus

# And also made write-protected to archive the file.

## Confirm open as expected
# sepkoski_cleaned <- read.csv("Sepkoski_Compendium.csv", row.names = 1)
head(sepkoski_cleaned)

trouble <- c(16148, 16157, 16160, 16181, 16229, 17997, 18018, 18019, 18139, 
             18174, 18176, 18177, 18323, 19870, 21413, 24368, 24433, 24518,
             24911, 29148)
sepkoski$GENUS[trouble]
sepkoski_cleaned$GENUS[trouble]

