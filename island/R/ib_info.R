herps_info <- tagList(
  p("This is a classic data set used to develop the concept of species-area
    relationships for island biogeography equilibrium. Species richness is
    based on the number of reptiles and amphibians found on seven islands in
    the northern Antilles"),
  p("The plot and results should look familiar. These are the example data I
    used on the previous tab."),
  p("Data from Macarthur and Wilson 1967. The Theory of Island Biogeography. 
    Monographs in Population Biology, No. 1. Princeton University Press, NJ."
  ),
  img(src = "caribbean.png", width = "100%")
)

rajas_info <- tagList(
  p("The", tags$a(href = "https://en.wikipedia.org/wiki/Raja_Ampat_Islands", 
    "Raja Ampat archipelago", target = "_blank"), "is a group of more than 
    1000 small islands in Gam Bay, Indonesia. This data set contains tree
    species richness for 60 of those islands. The islands area ranged between
    3-11,806 m². The islands were chosen because they did not show signs of
    human disturbance. Distance was measured from the large island of Gam."),
  p("Data from Schrader et al. 2020. A new dataset on plant occurrences on
    small islands, including species abundances and functional traits across
    different spatial scales. Biodiversity Data Journal 8: e55275, 
    doi: 10.3897/BDJ.8.e55275"),
  img(src = "raja_ampat.jpeg", width = "100%")
)

aleuts_info <- tagList(
  p("The Aleutian Islands is a group of more than 150 islands that extend from
    the Alaskan Peninsula west towards the Kamchatka Peninsula in Siberia. The
    Aleutians form a volcanic arc where the northern edge of the Pacific Plate
    subducts beneath the North American plate. As the islands extend from
    Alaska to Siberia, distance from a mainland source can be considered from
    two Alaska and also from Siberia."),
  p("This data set includes counts of all known plant species from 13 islands
    considered “well-surveyed” for flora. The plants are primarily grasses,
    shrubs, and small flowering plants. Trees do not grow on the islands. 
    Distance was calculated from both the Alaskan Peninsula in Alaska and
    the Kamchatka Peninsula in Siberia."),
  p("Data from Garroutte, M., F. Huettmann, C.O. Webb, and S.M. Ickert-Bond.
    2018. Biogeographic and anthropogenic correlates of Aleutian Islands plant
    diversity: A machine-learning approach. Journal of Systematics and
    Evolution 56: 476-497."),
  img(src = "aleutians.png", width = "100%")
)

beetles_info <- tagList(
  p("The beetles of this study are long-horned beetles (family Cerambycidae),
    named for their very long antennae. At the time of this study, 91 species
    of long-horned beetles were known from south Florida. The authors wanted
    to know whether the distribution of the beetles in the Florida Keys fit
    the species-area and species-distance relationship of island biogeography.
    South Florida would be the primary source of species for the Florida Keys.
    They sampled beetles from 11 islands. Distance was measured from a fixed
    location in south Florida."
  ),
  p("Data from Browne and Peck 1996. The long-horned beetles of south Florida
    (Cerambycidae: Coleoptera): biogeography and relationships with the Bahama
    Islands and Cuba. Canadian Journal of Zoology. 74: 2154-2169."
  ),
  img(src = "florida_keys.png", width = "100%")
)

mtn_info <- tagList(
  p("Montaine mammals in this study were 15 mammal species found on mountain
    tops above 7500 m in the southwestern deserts of the United States. The
    mammals were boreal mammals and thus adapted to cooler temperatures. The
    mammals could not tolerate the warm temperatures in the valleys between
    the mountains. Thus, the mountain tops function as “islands” in a valley
    “sea.”"
  ),
  p("This study measured area, distance between mountain tops and also distance
    from mainlands (larger mountain ranges east and west of the study area). 
    View species richness for all three variables. Do all three fit the island
    biogeography model? If not, which do? Why do you think the others do not?
    Use the significance (p value) from the regression table to guide your
    decisions." 
  ),
  p("Data from Brown 1971. Mammals on mountaintops: nonequilibrium insular
    biogeography. The American Naturalist 105: 467-478."
  ),
  img(src = "sky_islands.jpg", width = "100%")
)

arthro_info <- tagList(
  p("Arboreal arthropods are insects, spiders, millipeds, isopods and related
    species that live in trees. The islands studied were 8 very small mangrove
    islands off Sugarloaf Key in Florida. The mangroves were Red Mangrove trees.
    The goal was to determine how quickly islands returned to dynamic
    equilibrium between colonization and extinction. They censused the islands
    in 1969, reduced the size of the island by about 30-50%, censused again
    in 1970, reduced the size again by about 50%, and did a final census in
    1971. Some islands could only be censused for two years."
  ),
  p("View the results with an eye towards island size. How does richness change
    as island size decreases? After viewing the data by island, you can pick out
    the trends for each island when you view the entire data set at once."
  ),
  p("Data from Simberloff 1976. Experimental zoogeography of islands: effects
    of island size. Ecology 57: 629-648."
  ),
  img(src = "sugarloaf.jpg", width = "100%")
)
