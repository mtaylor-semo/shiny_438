fix_missouri <- function(df) {
  
  motibble <- as_tibble(df, rownames = "watershed")
  
  merge_groups <- list(
    Black = c("Black_Up", "Black_Low", "Current", "Eleven_Pt", "Spring_White", "L_Black"),
    #Black_Up = c("Black_Up"),
    #Black_Low = c("Black_Low"),
    Chariton = c("Chariton"),
    #Current = c("Current", "Jacks_Fk", "L_Black"),
    Gasconade = c("Gasconade", "Big_Piney"),
    Grand = c("Grand"),
    #Lamine = c("Lamine", "Blackwater"), # Consider adding to Missouri
    Meramec = c("Meramec", "Big", "Bourbeuse"),
    Miss_Low = c("Miss_Low", "Head_Div"),
    Miss_Up = c("Miss_Up", "Cuivre", "North", "Fabius", "Wyaconda", "Fox", "Low_Des_Moines"),
    Missouri = c("Missouri", "Moreau", "Tarkio", "Nodaway", "Lamine", "Blackwater"), # Consider moving Lamine here
    Neosho = c("Spring_Neosho", "Elk"),
    Osage_East = c("Osage_East", "Niangua"),
    Osage_West = c("Osage_West", "Sac", "Pomme_De_Terre", "South_Grand"),
    Platte = c("Platte"),
    Salt = c("Salt"),
    StFrancis_Low = c("StFrancis_Low"),
    StFrancis_Up = c("StFrancis_Up"),
    White = c("White", "James", "NFk_White")
  )
  
  
  # modified code based on original (see below)
  # from https://stackoverflow.com/a/77225302/3832941
  enframe(merge_groups, name = "main", value = "watershed") %>%
    unnest(watershed) %>%
    left_join(motibble, by = "watershed") %>%
    summarise(across(-watershed, ~ as.numeric(sum(.x) > 0)), .by = main) %>%
    rename(watershed = main) %>%
    column_to_rownames("watershed")
}


fix_alabama <- function(df) {
  altibble <- as_tibble(df, rownames = "watershed")
  
  merge_groups <- list(
    Tennessee = c("Tennesse Above Fall", "Tennesse Below Fall"),
    Tombigbee = c("Tombigbee Upland", "Tombigbee Lowland"),
    Black_Warrier_Up = "Black Warrior Above Fall",
    Black_Warrier_Low = "Black Warrior Below Fall",
    Cahaba_Up = "Cahaba Above Fall",
    Cahaba_Low = "Cahaba Below Fall",
    Coosa_Up = "Coosa Above Fall",
    Coosa_Low = "Coosa Below Fall",
    Tallapoosa_Up = "Tallapoosa Above Fall",
    Tallapoosa_Low = "Tallapoosa Below Fall",
    Chattahoochee_Up = "Chattahoochee Above Fall",
    Chattahoochee_Low = c("Chattahoochee Below Fall", "Chipola River"),
    Alabama = "Alabama River",
    Conecuh = "Conecuh River",
    Yellow = "Yellow River",
    Choctawhatchee = c("Chactawhatchee River", "Pea River"),
    Mobile_Tensas = c("Mississippi Sound", "Mobile-Tensas River"),
    Perdido = "Perdido River",
    Escatawpa = "Escatawpa River"
  )
  enframe(merge_groups, name = "main", value = "watershed") %>% 
    unnest(watershed) %>% 
    left_join(altibble, by = "watershed") %>% 
    summarise(across(-watershed, ~ as.numeric(sum(.x) > 0)), .by = main) %>% 
    rename(watershed = main) %>% 
    column_to_rownames("watershed")
}

fix_georgia <- function(df) {
  gatibble <- as_tibble(df, rownames = "watershed")

  merge_groups <- list(
    Tennessee = c("Tennessee", "Little_Tennessee", "Hiwassee"),
    Coosa = "Coosa",
    Tallapoosa = "Tallapoosa",
    Up_Chattahoochee = "Up_Chattahoochee",
    Low_Chattachoochee = "Low_Chattachoochee",
    Up_Flint = "Up_Flint",
    Low_Flint = "Low_Flint",
    Ochlockonee = "Ochlockonee",
    Suwannee = "Suwannee",
    Saint_Marys = "Saint_Marys",
    Satilla = "Satilla",
    Up_Ocmulgee = "Up_Ocmulgee",
    Low_Ocmulgee = "Low_Ocmulgee",
    Up_Oconee = "Up_Oconee",
    Low_Oconee = "Low_Oconee",
    Altamaha = "Altamaha",
    Up_Ogeechee = "Up_Ogeechee",
    Low_Ogeechee = "Low_Ogeechee",
    Up_Savannah = "Up_Savannah",
    Low_Savannah = "Low_Savannah"
  )
    enframe(merge_groups, name = "main", value = "watershed") %>%
      unnest(watershed) %>%
      left_join(gatibble, by = "watershed") %>%
      summarise(across(-watershed, ~ as.numeric(sum(.x) > 0)), .by = main) %>%
      rename(watershed = main) %>%
      column_to_rownames("watershed")

}

state_fishes$Alabama <-  fix_alabama(state_fishes$Alabama)
state_fishes$Missouri <- fix_missouri(state_fishes$Missouri)
state_fishes$Georgia <- fix_georgia(state_fishes$Georgia)
