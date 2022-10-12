library(RODBC) 


db <- "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/FishTraits.accdb"
con2 <- odbcConnectAccess2007(db)

#get a list of the table names
sqlTables(con2, tableType = "TABLE")$TABLE_NAME

#bring in 3 of the tables into R as dataframes
gillnet_data <- sqlFetch(con2, "Copy Of Fish_Traits")
seine_data <- sqlFetch(con2, "seine_data")
water_chem <- sqlFetch(con2, "water_chemistry")


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/FishTraits.accdb")
flowmet_endofcen <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/FishTraits.accdb", layer = "Copy Of Fish_Traits")
