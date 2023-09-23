suckers <- "Suckers are large, bottom-dwelling fishes that occupy small 
streams to large rivers. They feed on detritus and small animals that
live on the bottom. Most species can approach 60 cm (2 feet) some grow
much larger. There are about 75 species of Catostomidae in North America."

sculpins <- "Sculpins are mostly small bottom-dwelling species that live
in cold streams and lakes. Some sculpins can be found in nearshore marine
habitats. Many more species not included in this data set are strictly 
marine. There are at least 20 species of Cottidae in North American 
freshwaters but molecular evidence suggests the actual number is much
higher."

catfishes <- "Catfishes are a diverse group of bottom-oriented fishes
that occupy almost every freshwater habitat from cold streams to warm
lakes. Popular as food fishes but the greatest diversity in North
America are the small madtoms. There are about 50 species of Ictaluridae
in North America."

cyprinids <- "Minnows and shiners is the largest group of freshwater fishes
North America, with well over 200 species in North America. They occupy 
almost every freshwater habitat but are found mostly in small streams to 
large rivers. The greatest richness of North American Cyprinidae is 
concentrated in the southeastern U.S. (cyprinid1) but others are more
widespread (cyprinid2)."

cyprinodonts <- "This group contains the families Fundulidae (topminnows) and Cyprinodontidae (pupfishes). Topminnows live and feed just below
the surface of the water. Many pupfishes are found in coastal waters but some
live in small hot springs of the desert southwest. Species of both families can
tolerate relatively high salinity for at least short durations. Combined, there
are more than 80 species in North America."

perches <- "Perches are the second largest family of fishes with more than
150 species in North America. Walleye and Yellow Perch are popular game 
fish but most species are small, bottom-dwelling fishes called darters that
live in streams and small rivers, usually with flowing water."

salmon <- "Salmon, trout, and a few other species are popular sport fishes. 
Salmon spawn in fresh water but live in saltwater as adults. Trout remain in
freshwater their entire live. Most trout species have been widely introduced 
outside their native range and often reared in aquaculture facilities. The 
map shows native ranges as best as possible. There are more than 20 species
in North America."

get_species_info <- function(spp) {
  switch(spp,
         "Catostomidae" = suckers,
         "Cottidae" = sculpins,
         "Cyprinidae" = cyprinids,
         "Cyprinodontidae" = cyprinodonts,
         "Ictaluridae" = catfishes,
         "Percidae" = perches,
         "Salmonidae" = salmon,
         validate("Oops, something went wrong.")
  )
}

get_species_image <- function(spp) {
  switch(spp,
         "Catostomidae" = "sucker.jpg",
         "Cottidae" = "sculpin.jpg",
         "Cyprinidae" = "cyprinid.jpg",
         "Cyprinodontidae" = "cyprinodont.jpg",
         "Ictaluridae" = "catfish.jpg",
         "Percidae" = "darter.jpg",
         "Salmonidae" = "trout.jpg",
         validate("")
  )
}
