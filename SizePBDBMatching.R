## Add accepted PBDB names, ranks, and ID numbers for size imports from Heim, et
## al. and other SI body size data sets before ingestion into the core life
## habit database.

rm(list = ls())

# Get PBDB data (use unformatted so includes improper names, too!)

# https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app
# pbdb <- read.csv("https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&rank=min_subgenus&variant=all&show=app")
# If want forams too, use base_name=Metazoa,Retaria
pbdb <- read.csv("pbdb_data_AllMetazoaTaxa.csv")
head(pbdb)
nrow(pbdb)

# Remove taxa that are not at genus or subgenus rank (in case of "homonyms" like
# the brachiopod genus Cincta).
pbdb <- pbdb[which(pbdb$taxon_rank == "genus" | pbdb$taxon_rank == "subgenus"), ]


# Import SizesToImport.tab
sizes <- read.delim("TempSizes.txt")
# Add columns for accepted no, name, and rank
sizes$accepted_no <- NA
sizes$accepted_rank <- NA
sizes$accepted_name <- NA
head(sizes)
(orig.nrow <- nrow(sizes))

# Choose columns for viewing summaries
sum.cols <- which(colnames(sizes) %in% c("Row", "Genus", "Subgenus", "accepted_no", "accepted_rank", "accepted_name"))


## 1. Match subgenera in SI data that are still subgenera in PBDB. Note PBDB
## uses "Genus (Subgenus)" for format.
subgenera <- paste0(sizes$Genus, " (", sizes$Subgenus, ")")
wh.subg <- which(subgenera %in% pbdb$taxon_name)
subgenera[wh.subg[1:10]]
subg.matches <- match(subgenera[wh.subg], pbdb$taxon_name)
pbdb[subg.matches[1:10], c(2, 5:6, 8:10)]
# Add in accepted no, name, and rank
sizes[wh.subg, c("accepted_no", "accepted_rank", "accepted_name")] <-
  pbdb[subg.matches, c("accepted_no", "accepted_rank", "accepted_name")]
sizes[wh.subg[1:20], sum.cols]
# Set these aside and remove them from 'sizes'
valid.1 <- sizes[wh.subg, ]
head(valid.1)
tail(valid.1)
table(valid.1$accepted_rank)

sizes <- sizes[-wh.subg, ]
nrow(sizes)
orig.nrow
nrow(valid.1)
identical(orig.nrow, nrow(valid.1) + nrow(sizes))


## 2. Match genera in SI data that are still genera in PBDB (restricting to
## those lacking subgenus names so only matches genera)
index <- which(sizes$Subgenus == "")
genera <- sizes$Genus[index]
wh.gen <- which(genera %in% pbdb$taxon_name)
genera[wh.gen[1:10]]
gen.matches <- match(genera[wh.gen], pbdb$taxon_name)
pbdb[gen.matches[1:10], c(2, 5:6, 8:10)]
# Add in accepted no, name, and rank
sizes[index[wh.gen], c("accepted_no", "accepted_rank", "accepted_name")] <-
  pbdb[gen.matches, c("accepted_no", "accepted_rank", "accepted_name")]
sizes[index[wh.gen[1:20]], sum.cols]
# Set these aside and remove them from 'sizes'
valid.2 <- sizes[index[wh.gen], ]
head(valid.2)
tail(valid.2)
table(valid.2$accepted_rank)
# Most non-genus/subgenus ranks are currently considered nomen nuda. Manually
# check in case of homonyms: Polystylus (brach), Cincta (brach), Ophidion
# (fish), Chonopectoides (productid), Petaloconchus (vermetid snail).

# Following are allowed nomen nuda: Atracauda, Fubarichthys, Yogoniscus,
# Floydus, Asterodermus, Ophidion (Serpentes), Sphenodus, Halitherium,
# Polyptychodon, Helicancylus, Aristocaris, Silesicaris, Aluta, Lebediorthis,
# Malinella, Turarella, Chuiella, Polystylus (Collembola), Chaoina,
# Chonopectella, Paucicostella, Lotocrinus, Gryphellina, and Petaloconchus
# (cheliostome)
valid.2[-which(valid.2$accepted_rank == "genus" |
                valid.2$accepted_rank == "subgenus"), sum.cols]

sizes <- sizes[-index[wh.gen], ]
nrow(sizes)
orig.nrow
nrow(valid.2)
identical(orig.nrow, nrow(valid.1) + nrow(valid.2) + nrow(sizes))


## 3. Match subgenera in SI data now elevated to genus rank in PBDB. (Make sure
## to exclude nominate/type subgenera parented to the same genus. Eg., subgenus
## Atrypa (Atrypa) is a different taxon than genus Atrypa.)
index <- which(sizes$Subgenus != "")
remove.nominates <- which(sizes$Genus == sizes$Subgenus)
index <- setdiff(index, remove.nominates)
subgenera <- sizes$Subgenus[index]
pbdb.genera <- pbdb[which(pbdb$taxon_rank == "genus"), ]
wh.subg.now.gen <- which(subgenera %in% pbdb.genera$taxon_name)
subgenera[wh.subg.now.gen[1:10]]
subg.matches <- match(subgenera[wh.subg.now.gen], pbdb$taxon_name)
pbdb[subg.matches[1:10], c(2, 5:6, 8:10)]
# Add in accepted no, name, and rank
sizes[index[wh.subg.now.gen], c("accepted_no", "accepted_rank", "accepted_name")] <-
  pbdb[subg.matches, c("accepted_no", "accepted_rank", "accepted_name")]
sizes[index[wh.subg.now.gen][1:20], sum.cols]
# Set these aside and remove them from 'sizes'
valid.3 <- sizes[index[wh.subg.now.gen], ]
head(valid.3)
tail(valid.3)
table(valid.3$accepted_rank)
# If any non-genus/subgenus occur, most are currently considered nomen nuda. but
# manually check in case of homonyms.
valid.3[-which(valid.3$accepted_rank == "genus" |
                 valid.3$accepted_rank == "subgenus"), sum.cols]

sizes <- sizes[-index[wh.subg.now.gen], ]
nrow(sizes)
orig.nrow
nrow(valid.3)
identical(orig.nrow, nrow(valid.1) + nrow(valid.2) + nrow(valid.3) + nrow(sizes))


## For last two checks, need to separate subgenus names from concatenated
## version in PBDB
pbdb.subg <- pbdb[which(pbdb$taxon_rank == "subgenus" | 
                          pbdb$accepted_rank == "subgenus"), ]
pbdb.subg$subgenus <- NA
for (i in 1:nrow(pbdb.subg)) {
  subgenus <- pbdb.subg$taxon_name[i]
  if (grepl(" \\(", subgenus)) {
    pbdb.subg$subgenus[i] <- unlist(regmatches(subgenus,
                                               gregexpr("(?<=\\()[^()]+(?=\\))",
                                                        subgenus, perl = TRUE)))
  } else {
    subgenus <- pbdb.subg$accepted_name[i]
    if (grepl(" \\(", subgenus))
      pbdb.subg$subgenus[i] <- unlist(regmatches(subgenus,
                                                 gregexpr("(?<=\\()[^()]+(?=\\))",
                                                          subgenus, perl = TRUE)))
  }
}
pbdb.subg[1:20, c(5:6, 10, 21)]

# Efficient way to identify alternative recombinations:
names <- names(sort(table(pbdb.subg$subgenus), decreasing = TRUE))
num <- 0
for (i in 1:length(names)) {
  if (length(table(pbdb.subg$accepted_name[which(pbdb.subg$subgenus == names[i])])) > 1L) {
    # Added to ignore case where a valid genus and nominate subgenus are only ones:
    if (length(table(pbdb.subg$accepted_rank[which(pbdb.subg$subgenus == names[i])])) != 2L) {
      print(pbdb.subg[which(pbdb.subg$subgenus == names[i]), c(5:7, 8:10, 21)])
      print(cat("\n\n"))
      num <- num + 1
    }
  }
}
num

# There are ~ 60 potential mismatches (relatively few!). Conveniently, the first
# listed are (generally) the best matches, so using 'match' below (which only
# matches the first one) is a reasonable choice.

# Ignore following (because terrestrial mammals, insects, birds, or dinosaurs):
#      - Grallator
#      - Protohippus (Calippus) & Calippus (Calippus)
#      - Cervus (Deperetia)
#      - Pleurarthropterus (Balticarthropterus) & Pleuarthropterus (Balticarthropterus)
#      - Parabrenthorrhinus
#      - Trigona (Liotrigona), Liotrigona (Liotrigona) & Liotrigona
#      - Electrapis (Melikertes), Melikertes (Melikertes) & Melikertes
#      - Olor & Cygnus (Olor)
#      - Polydrusus (Palaeodrosus) & Polydrusus (Palaeodrusus)
#      - Apalone (Platypeltis) and Platypeltis
#      - Mastodon (Tetralophodon) and Tetralophodon (Tetralophodon)

# Manually check Amara (Curtonotus) (a beetle) and Curtonotus (= Cancer
# (Curtonotus), now Carcinoplax) (a decapod).

# Manually check Trautscholdia (now Alphachoristites, a brachiopod) and
# Trautscholdia, an Astartidae bivalve.

# Manually check Anasibirites (Goniodiscus) Waagen 1895 (j.s.s. of
# Hemiprionites, an ammonoid) and Goniaster (Goniodiscus) Müller and Troschel
# 1842 (j.s.s. of Culcita, an asteroid).

# Manually check Eocardia (Procardia) Ameghino 1891 (a caviomorph mammal, since
# replaced with Eoprocardia) and Pholadomya (Procardia), now genus Procardia, a
# pholadomyid bivalve.

# Manually check Smittina (Reussia) (a cheilostome), Odontochile (Reussia) (a
# trilobite, now replaced with Reussiana), and Reussia.

# Manually check Lonsdaleia (Waagenella) (a rugose coral) and Waagenella (a
# bellerophontid).

# Euryptychites (Euryptychites) should be recombined as Polyptychites
# (Euryptychites).

# Amphistrophiella (Amphistrophiella) should be Amphistrophia (Amphistrophiella)

# Strophodonta (Strophodonta) should be Strophomena (Strophodonta)

# Manually correct the entanglements for Cardiopsis and Cordiopsis, which I
# entangled. Cardiopsis Meek & Worthen, 1861 is in Praecardiidae and restricted
# to Carboniferous. Cordiopsis Cossmann, 1910 is in Veneriidae (and currently
# ranked as subgenus of Pelecyora in PBDB).

# Also check entanglement for Kellia Turton, 1822 (a bivalve) and Kelletia
# Fischer, 1884 (a gastropod).

# Hemifusus (Hemifusus) should be Volema (Hemifusus)

# Siphonochelus (Laevityphis), Typhis (Laevityphis), and Laevityphis should all
# point to Laevityphis, allowing subgenus Laevityphis (Laevityphis)

# Ammonites (Oecotraustes), Oekotraustes, and Oppelia (Oecotraustes) should all
# point to Oecotraustes, allowing Oecotraustes (Oecotraustes)

# Volutilithes (Volutocorbis), Volutocorbis (Volutocorbis), and Volutocorbis
# should all point to Athleta (Volutocorbis)

# Antiguamya and Mya (Antiguamya) should point to Tugonia (Antiguamya)

# Brevicardium, Protocardium (Brevicardium), and Nemocardium (Brevicardium)
# should point to Brevicardium

# Chlamys (Camptochlamys), Camptochlamys, and Camptochlamys (Camptochlamys)
# should point to Camptochlamys

# Gryphaea (Catinula), Ostrea (Catinula), and Catinula should point to Catinula

# Kheraiceras (Kheraiceras) and Kheraiceras should point to Bullatimorphites
# (Kheraiceras)

# Tropidophora (Ligatella) [a Pomatiidae snail] and Notothyris (Ligatella) [a
# Notothyrididae brachiopod] are distinct, so allow both.

# Musashia, Fulgoraria (Musashia), and Musashia (Musashia) should point to
# Fulgoraria (Musashia)

# Zeradina (Naridista) should point to Micreschara (Naridista).

# Orthogarantiana (Orthogarantiana) should point to Garantiana
# (Orthogarantiana).

# Cardium (Pachycardium), Protocardia (Pachycardium), and Pachycardium should
# point to Protocardia (Pachycardium)

# Ptychosalpinx (Paranassa), Paranassa, and Ptychosalpinx (Paranassa) should
# point to Ptychosalpinx (Paranassa)

# Asaphus (Platypeltis) should be replaced with Platypeltoides (and ignore
# Apalone (Platypeltis) and Platypeltis, a reptile)

# Strombina (Sinuina) should point to Sincola (Sinuina).

# Crassispira (Tripia), Drillia (Tripia), and Pleurotoma (Tripia) should all
# point to Crassispira.

# Agasoma (Trophosycon) and Ficus (Trophosycon) should point to Trophosycon

# Calliostoma (Ziziphinus) and Trochus (Ziziphinus) should be synonyms of
# Calliostoma

# Micmacca (Acanthomicmacca) should be recombined as Acanthomicmacca
# (Acanthomicmacca)

# Lopha (Arctostrea) and Rastellum (Arctostrea) should be synonyms of Rastellum

# Wentzelloides (Armeniaphyllum) is a misspelling (so should not exist) of
# Wentzellophyllum (Armeniaphyllum)

# Perisphinctes (Ataxioceras) should be recombined as Ataxioceras (Ataxioceras)

# Caryocorbula (Caryocorbula) and Corbula (Caryocorbula) should be reranked as
# Caryocorbula (with no subgenera)

# Myophorella (Clavitrigonia) and Trigonia (Clavitrigonia) should be synonyms of
# Myophorella (Myophorella)

# Opis (Coelopis) should be reranked as Coelopis, with Coelopis (Coelopis) as
# subgenus

# Cheirurus (Crotalocephalina) should point to Crotalocephalina, with subgenus
# (I think?) Crotalocephalina (Crotalocephalina)

# Pecten (Deperetia) is a syononym of Pecten

# Isocrinus (Diplocrinus) and Diplocrinus should be recombined as Endoxocrinus
# (Diplocrinus)

# Lucina (Divaricella) should be recombined as Divaricella (Divaricella)

# Megastrophia (Eomegastrophia) and Brachyprion (Eomegastrophia) should be
# recombined as Eomegastrophia (Eomegastrophia)

# Kossmaticeras (Jacobites) should be recombined as Jacobites, with subgenus
# Jacobites (Jacobites)

# Rissoina (Leaella), Zebina (Leaella) and Leaella should all be synonyms of
# Rissoina

# Lloydia (Leiostegium) should be reranked as Leiostegium, with subgenus
# Leiostegium (Leiostegium)

# Lepidopleurus (Leptochiton) should be recombined as Leptochiton (Leptochiton)

# Pecten (Neithea) should be reranked as Neithea, with subgenus Neithea
# (Neithea)

# Ilyocypris (Neuquenocypris) should be reranked as Neuquenocypris, with
# subgenus Neuquenocypris (Neuquenocypris)

# Cypraea (Notocypraea) should be reranked as Notocypraea, with subgenus
# Notocypraea (Notocypraea)

# Trinacria (Pachecoa) should be recombined as Pachecoa (Pachecoa)

# Eucalycoceras (Proeucalycoceras) should be recombined as Calycoceras
# (Proeucalycoceras)

# I'm not sure if Typhloproetus (Silesiops) should be parented to Typhloproetus
# or redirect to Silesiops. I don't have access to the relevant literature
# (which is sparse) and both have been used within the past decade.

# Proetus (Tropidocoryphe) should be reranked as Tropidocoryphe

# Waldheimia (Zeilleria) should redirect to Zeilleria



## 4. Match subgenera in SI data now recombined in different genera in PBDB (or
## possibly elevated to genus rank)
index <- which(sizes$Subgenus != "")
subgenera <- sizes$Subgenus[index]
wh.subg.now.recombined <- which(subgenera %in% pbdb.subg$subgenus)
subgenera[wh.subg.now.recombined[1:10]]
subg.matches <- match(subgenera[wh.subg.now.recombined], pbdb.subg$subgenus)
pbdb.subg[subg.matches[1:10], c(2, 5:6, 8:10)]
# Add in accepted no, name, and rank
sizes[index[wh.subg.now.recombined], c("accepted_no", "accepted_rank", "accepted_name")] <-
  pbdb.subg[subg.matches, c("accepted_no", "accepted_rank", "accepted_name")]
sizes[index[wh.subg.now.recombined][1:20], sum.cols]
# Set these aside and remove them from 'sizes'
valid.4 <- sizes[index[wh.subg.now.recombined], ]
head(valid.4)
tail(valid.4)
table(valid.4$accepted_rank)
# Note many are nominate subgenera for genera in the PBDB that lack nominate
# subgenera (but presumably have other subgenera). These may be worth
# maintaining (if other subgenera are recognized), even if not recognized in the
# PBDB (under principle that a nominate subgenus must exist if other subgenera
# are recognized). Such "recognized" parent genera will be swept in during the
# later propagation, and the nominate will likely be the size that gets
# accordingly propagated (in the same way the Heim, et al.'s SI data did).
valid.4[-which(valid.3$accepted_rank == "genus" |
                 valid.3$accepted_rank == "subgenus"), sum.cols]

sizes <- sizes[-index[wh.subg.now.recombined], ]
nrow(sizes)
orig.nrow
nrow(valid.4)
identical(orig.nrow, nrow(valid.1) + nrow(valid.2) + nrow(valid.3) + 
            nrow(valid.4) + nrow(sizes))


## 5. Match genera (lacking subgenera, as want to maintain subgenera, even if
## not in PBDB!) in SI data now reranked as a subgenus in PBDB.
index <- which(sizes$Subgenus == "")
genera <- sizes$Genus[index]
wh.g.now.demoted <- which(genera %in% pbdb.subg$subgenus)
genera[wh.g.now.demoted]
gen.matches <- match(genera[wh.g.now.demoted], pbdb.subg$subgenus)
pbdb.subg[gen.matches, c(2, 5:6, 8:10)]
# Add in accepted no, name, and rank
sizes[index[wh.g.now.demoted], c("accepted_no", "accepted_rank", "accepted_name")] <-
  pbdb.subg[gen.matches, c("accepted_no", "accepted_rank", "accepted_name")]
sizes[index[wh.g.now.demoted], sum.cols]
# Set these aside and remove them from 'sizes'
valid.5 <- sizes[index[wh.g.now.demoted], ]
head(valid.5)
tail(valid.5)
table(valid.5$accepted_rank)
# If any non-genus/genenus occur, most are currently considered nomen nuda. but
# manually check in case of homonyms.
valid.5[-which(valid.3$accepted_rank == "genus" |
                 valid.3$accepted_rank == "subgenus"), sum.cols]

sizes <- sizes[-index[wh.g.now.demoted], ]
nrow(sizes)
orig.nrow
nrow(valid.5)
identical(orig.nrow, nrow(valid.1) + nrow(valid.2) + nrow(valid.3) + 
            nrow(valid.4) + nrow(valid.5) + nrow(sizes))

