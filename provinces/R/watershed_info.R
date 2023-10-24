# Watershed info for cluster and nmds tabs
#alabama_watersheds

alabama_watersheds <- tagList(
  p("All watersheds flow to Gulf of Mexico"),
  p(tags$b("Tennessee River")),
  p(tags$b("Mobile Bay")),
  tags$ul(
   tags$li("Mobile Bay-Tensas"),
   tags$li("Tombigbee River"),
   tags$ul(
     tags$li("Black Warrior"),
   ),
   tags$li("Alabama River"),
    tags$ul(
      tags$li("Cahaba River"),
      tags$li("Coosa River"),
      tags$li("Tallapoosa River")
    )
  ),
  tags$b("Coastal Rivers (flow directly into Gulf)"),
  tags$ul(
    tags$li("Escatawpa"),
    tags$li("Perdido"),
    tags$li("Conecuh"),
    tags$li("Yellow"),
    tags$li("Choctawhatchee"),
    tags$li("Chattahoochee River")
  )
)

georgia_watersheds <- tagList(
  p(tags$b("Atlantic")),
  tags$ul(
    style = "padding-left: 15px;",
    tags$li("Savannah"),
    tags$li("Ogeechee"),
    tags$li("Altamaha"),
    tags$li("Oconee"),
    tags$li("Ocmulgee"),
    tags$li("Satilla"),
  ),
  p(tags$b("Gulf of Mexico")),
  tags$ul(
    style = "padding-left: 15px;",
    tags$li("Saint Marys"),
    tags$li("Suwannee"),
    tags$li("Ochlocknonee"),
    tags$li("Chattachoochee"),
    tags$ul(
      style = "padding-left: 15px;",
      tags$li("Flint")
    ),
    tags$li("Alabama"),
    tags$ul(
      style = "padding-left: 15px;",
      tags$li("Tallapoosa"),
      tags$li("Coosa")
    ),
    tags$li("Tennessee")
  )
)

mississippi_watersheds <- tagList(
  p(tags$b("Mississippi River")),
  tags$ul(
    tags$li("Mississippi North"),
    tags$li("Mississippi South"),
    tags$li("Yazoo"),
    tags$li("Big Black")
  ),
  p(tags$b("Gulf of Mexico")),
  tags$ul(
    tags$li("Lake Pontchartrain"),
    tags$li("Pearl"),
    tags$li("Coastal"),
    tags$li("Pascagoula"),
  ),
  p(tags$b("Mobile Bay")),
  tags$ul(
    tags$li("Tombigbee")
  ),
  p(tags$b("Tennessee River (TN)"))
)

missouri_watersheds <- tagList(
  p(tags$b("Mississippi River")),
  tags$ul(
    tags$li("U. Mississippi"),
    tags$li("Salt"),
    tags$li("Meramec"),
    tags$li("L. Mississippi"),
  ),
  p(tags$b("Missouri River (flows into Mississippi R.)")),
  tags$ul(
    tags$li("Missouri"),
    tags$li("Platte"),
    tags$li("Grand"),
    tags$li("Chariton"),
    tags$li("Osage"),
    tags$li("Gasconade")
  ),
  p(tags$b("White River")),
  tags$ul(
    tags$li("White"),
    tags$li("Black"),
    tags$li("St. Francis")
  ),
  p(tags$b("Arkansas River")),
  tags$ul(
    tags$li("Neosho")
  )
)

northcarolina_watersheds <- tagList(
  p(tags$b("Atlantic Slope")),
  tags$ul(
    tags$li("Chowan - Pasquotank"),
    tags$li("Roanoke"),
    tags$li("Tar-Pamlico"),
    tags$li("Neuse"),
    tags$li("White Oak"),
    tags$li("Cape Fear"),
    tags$li("Pee Dee"),
    tags$ul(
      tags$li("Lumber"),
      tags$li("Yadkin-Pee Dee")
    ),
    tags$li("Santee (in South Carolina)"),
    tags$ul(
      tags$li("Catawba"),
      tags$li("Broad")
    )
  ),
  p(tags$b("Gulf of Mexico")),
  tags$ul(
    tags$li("Savannah River")
  ),
  p(tags$b("Tennessee River")),
  tags$ul(
    tags$li("Hiwasee"),
    tags$li("Little Tennessee"),
    tags$li("French Broad"),
    tags$li("Wautauga")
  ),
  p(tags$b("Ohio River")),
  tags$ul(
    tags$li("New")
  )
)

southcarolina_watersheds <- tagList(
  p(tags$b("Atlantic Slope")),
  tags$ul(
    tags$li("Pee Dee"),
    tags$li("Santee"),
    tags$li("Edisto"),
    tags$li("Ashley-Cooper"),
    tags$li("Salketachie"),
    tags$li("Savannah")
  )
)

virginia_watersheds <- tagList(
  p(tags$b("Atlantic Slope")),
  tags$ul(
    tags$li("Potomac"),
    tags$li("Rappahannock"),
    tags$li("York"),
    tags$li("James"),
    tags$li("Chowan"),
    tags$li("Roanoke")
  ),
  p(tags$b("Ohio River")),
  tags$ul(
    tags$li("New"),
    tags$li("Big Sandy")
  ),
  p(tags$b("Tennessee River")),
  tags$ul(
    tags$li("Clinch/Powell"),
    tags$li("Holston")
  )
)
