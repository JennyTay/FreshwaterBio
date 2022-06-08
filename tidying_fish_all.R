

#install libraries


library(tidyverse)
library(lubridate)
library(sf)
library(readxl)



#load in datasets
path <-  "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata"
files <- list.files(path = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata")

for (i in 1:length(files)){
  
  load(paste(path, files[i], sep = "/"))  #load files
  
}



#combine event data

str(ct_event)
str(dec_event)
str(nh_event)
str(ma_event)





###################################

#### combine event dataframes ####

###################################

event <- bind_rows(ct_event, dec_event, nh_event, ma_event, ri_event, dfw_event)
event$date <- substr(event$date, start=1, stop=10)
event$date <- ymd(event$date)
event$year <- year(event$date)
event$month <- month(event$date)
event <- event %>% 
  select(UID, state, date, year, month, waterbody, latitude, longitude, project, source)

str(event)
table(event$state)
table(event$year)

save(event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_event.RData")









###################################

##### combine fish dataframes #####

###################################








fish <- bind_rows(ma_fish, ct_fish, nh_fish, dec_fish, ri_fish, dfw_fish)
fish$scientific_name <- tolower(fish$scientific_name)
fish$scientific_name <- trimws(fish$scientific_name)
fish$scientific_name[fish$scientific_name == 'catostomus commersoni'] <- "catostomus commersonii"
fish$scientific_name[fish$scientific_name == 'luxillus cornutus'] <- "luxilus cornutus"
fish$scientific_name[fish$scientific_name == 'micropterus dolomieui'] <- "micropterus dolomieu"
fish$scientific_name[fish$scientific_name == 'rhinicthys cataractae'] <- "rhinichthys cataractae"
fish$scientific_name[fish$scientific_name == 'alosa pseudoharangus'] <- "alosa pseudoharengus"
fish$scientific_name[fish$scientific_name == 'apeltes quadracas'] <- "apeltes quadracus"
fish$scientific_name[fish$scientific_name == 'unknown 1'] <- "unknown"
fish$scientific_name[fish$scientific_name == 'unknown 2'] <- "unknown"
fish$scientific_name[fish$scientific_name == 'sander vitreus'] <- "stizostedion vitreum" 
fish$scientific_name[fish$scientific_name == 'lampetra appendix'] <- "lethenteron appendix" 
fish$scientific_name[fish$scientific_name == 'phoxinus eos'] <- "chrosomus eos" #per rebeccas correction
fish$scientific_name[fish$scientific_name == 'esox americanus'] <- "esox americanus americanus"
fish$scientific_name[fish$scientific_name == 'lepomis spp.'] <- "lepomis sp"

#need to add in common names
fish$common_name[fish$scientific_name =="acipenser brevirostrum"] <- "shortnose sturgeon"
fish$common_name[fish$scientific_name =="alosa aestivalis"] <- "blueback herring"
fish$common_name[fish$scientific_name =="alosa pseudoharengus"] <- "alewife"
fish$common_name[fish$scientific_name =="alosa sapidissima"] <- "American shad"
fish$common_name[fish$scientific_name =="ambloplites rupestris"] <- "rock bass"
fish$common_name[fish$scientific_name =="ameiurus catus"] <- "white catfish"
fish$common_name[fish$scientific_name =="ameiurus natalis"] <- "yellow bullhead"
fish$common_name[fish$scientific_name =="ameiurus nebulosus"] <- "brown bullhead"
fish$common_name[fish$scientific_name =="amia calva"] <- "bowfin"
fish$common_name[fish$scientific_name =="anguilla rostrata"] <- "American eel"
fish$common_name[fish$scientific_name =="apeltes quadracus"] <- "fourspine stickleback"
fish$common_name[fish$scientific_name =="carassius auratus"] <- "goldfish"
fish$common_name[fish$scientific_name =="catostomus catostomus"] <- "longnose sucker"
fish$common_name[fish$scientific_name =="catostomus commersonii"] <- "white sucker"
fish$common_name[fish$scientific_name =="centrarchidae"] <- "sunfish family"
fish$common_name[fish$scientific_name =="channa sp."] <- "snakehead genus"
fish$common_name[fish$scientific_name =="chrosomus eos"] <- "redbelly dace"
fish$common_name[fish$scientific_name =="clinostomus funduloides"] <- "rosyside dace"
fish$common_name[fish$scientific_name =="coregonus clupeaformis"] <- "lake whitefish"
fish$common_name[fish$scientific_name =="cottus cognatus"] <- "slimy sculpin"
fish$common_name[fish$scientific_name =="couesius plumbeus"] <- "lake chub"
fish$common_name[fish$scientific_name =="cyprinidae"] <- "minnow family"
fish$common_name[fish$scientific_name =="cyprinus carpio"] <- "common carp"
fish$common_name[fish$scientific_name =="enneacanthus obesus"] <- "banded sunfish"
fish$common_name[fish$scientific_name =="erimyzon oblongus"] <- "creek chubsucker"
fish$common_name[fish$scientific_name =="esox americanus americanus"] <- "redfin pickerel"
fish$common_name[fish$scientific_name =="esox americanus americanus x esox niger"] <- "redfin pickerel x chain pickerel hybrid"
fish$common_name[fish$scientific_name =="esox lucius"] <- "northern pike"
fish$common_name[fish$scientific_name =="esox lucius x esox masquinongy"] <- "northern pike x muskellunge hybrid"
fish$common_name[fish$scientific_name =="esox niger"] <- "chain pickerel"
fish$common_name[fish$scientific_name =="etheostoma fusiforme"] <- "swamp darter"
fish$common_name[fish$scientific_name =="etheostoma olmstedi"] <- "tessellated darter"
fish$common_name[fish$scientific_name =="exoglossum maxillingua"] <- "cutlips minnow"
fish$common_name[fish$scientific_name =="fundulus diaphanus"] <- "banded killifish"
fish$common_name[fish$scientific_name =="fundulus heteroclitus"] <- "mummichog"
fish$common_name[fish$scientific_name =="fundulus luciae"] <- "spotfin killifish"
fish$common_name[fish$scientific_name =="fundulus majalis"] <- "striped killifish"
fish$common_name[fish$scientific_name =="gambusia affinis"] <- "western Mosquitofish"
fish$common_name[fish$scientific_name =="gasterosteus aculeatus"] <- "three-spined stickleback"
fish$common_name[fish$scientific_name =="gobiosoma bosc"] <- "naked goby"
fish$common_name[fish$scientific_name =="hybognathus regius"] <- "eastern silvery minnow"
fish$common_name[fish$scientific_name =="ictalurus punctatus"] <- "channel catfish"
fish$common_name[fish$scientific_name =="lepomis auritus"] <- "redbreast sunfish"
fish$common_name[fish$scientific_name =="lepomis cyanellus"] <- "green sunfish"
fish$common_name[fish$scientific_name =="lepomis gibbosus"] <- "pumpkinseed"
fish$common_name[fish$scientific_name =="lepomis macrochirus"] <- "bluegill"
fish$common_name[fish$scientific_name =="lepomis macrochirus x lepomis gibbosus"] <- "bluegill x pumpkinseed hybrid"
fish$common_name[fish$scientific_name =="lepomis sp"] <- "sunfish genus"
fish$common_name[fish$scientific_name =="lethenteron appendix"] <- "American brook lamprey"
fish$common_name[fish$scientific_name =="lota lota"] <- "burbot"
fish$common_name[fish$scientific_name =="luxilus cornutus"] <- "common shiner"
fish$common_name[fish$scientific_name =="menidia menidia"] <- "Atlantic silverside"
fish$common_name[fish$scientific_name =="micropterus dolomieu"] <- "smallmouth bass"
fish$common_name[fish$scientific_name =="micropterus salmoides"] <- "largemouth bass"
fish$common_name[fish$scientific_name =="morone americana"] <- "white perch"
fish$common_name[fish$scientific_name =="morone saxatilis"] <- "striped bass"
fish$common_name[fish$scientific_name =="no fish"] <- "no fish"
fish$common_name[fish$scientific_name =="notemigonus crysoleucas"] <- "golden shiner"
fish$common_name[fish$scientific_name =="notropis atherinoides"] <- "emerald shiner"
fish$common_name[fish$scientific_name =="notropis bifrenatus"] <- "bridle shiner"
fish$common_name[fish$scientific_name =="notropis heterolepis"] <- "blacknose shiner"
fish$common_name[fish$scientific_name =="notropis hudsonius"] <- "spottail shiner"
fish$common_name[fish$scientific_name =="notropis rubellus"] <- "rosyface shiner"
fish$common_name[fish$scientific_name =="notropis volucellus"] <- "mimic shiner"
fish$common_name[fish$scientific_name =="noturus gyrinus"] <- "tadpole madtom"
fish$common_name[fish$scientific_name =="noturus insignis"] <- "margined madtom"
fish$common_name[fish$scientific_name =="oncorhynchus mykiss"] <- "rainbow trout"
fish$common_name[fish$scientific_name =="osmerus mordax"] <- "rainbow smelt"
fish$common_name[fish$scientific_name =="perca flavescens"] <- "yellow perch"
fish$common_name[fish$scientific_name =="petromyzon marinus"] <- "sea lamprey"
fish$common_name[fish$scientific_name =="phoxinus neogaeus"] <- "finescale dace"
fish$common_name[fish$scientific_name =="pimephales notatus"] <- "bluntnose minnow"
fish$common_name[fish$scientific_name =="pimephales promelas"] <- "fathead minnow"
fish$common_name[fish$scientific_name =="pomoxis annularis"] <- "white crappie"
fish$common_name[fish$scientific_name =="pomoxis nigromaculatus"] <- "black crappie"
fish$common_name[fish$scientific_name =="prionotus evolans"] <- "striped searobin"
fish$common_name[fish$scientific_name =="prosopium cylindraceum"] <- "round whitefish"
fish$common_name[fish$scientific_name =="pungitius pungitius"] <- "ninespine stickleback"
fish$common_name[fish$scientific_name =="rhinichthys atratulus"] <- "Eastern blacknose dace"
fish$common_name[fish$scientific_name =="rhinichthys cataractae"] <- "longnose dace"
fish$common_name[fish$scientific_name =="salmo"] <- "salmon and trout genus"
fish$common_name[fish$scientific_name =="salmo salar"] <- "Atlantic salmon"
fish$common_name[fish$scientific_name =="salmo trutta"] <- "brown trout"
fish$common_name[fish$scientific_name =="salvelinus fontinalis"] <- "brook trout"
fish$common_name[fish$scientific_name =="salvelinus fontinalis x salmo trutta"] <- "brook trout x brown trout hybrid"
fish$common_name[fish$scientific_name =="salvelinus namaycush"] <- "lake trout"
fish$common_name[fish$scientific_name =="semotilus atromaculatus"] <- "creek chub"
fish$common_name[fish$scientific_name =="semotilus corporalis"] <- "fallfish "
fish$common_name[fish$scientific_name =="stizostedion vitreum"] <- "walleye"
fish$common_name[fish$scientific_name =="strongylura marina"] <- "Atlantic needlefish"
fish$common_name[fish$scientific_name =="syngnathus fuscus"] <- "northern pipefish"
fish$common_name[fish$scientific_name =="trinectes maculatus"] <- "hogchoker"
fish$common_name[fish$scientific_name =="umbra limi"] <- "central mudminnow"
fish$common_name[fish$scientific_name =="unknown"] <- "unknown"
fish$common_name[fish$scientific_name =="salmo trutta_hatchery"] <- "brown trout_hatchery"
fish$common_name[fish$scientific_name =="salvelinus fontinalis_hatchery"] <- "brook trout_hatchery"
fish$common_name[fish$scientific_name =="oncorhynchus mykiss_hatchery"] <- "rainbow trout_hatchery"
fish$common_name[fish$scientific_name =="alosa spp."] <- "river herring"
fish$common_name[fish$scientific_name =="ictaluridae"] <- "catfish family"
fish$common_name[fish$scientific_name =="menidia beryllina"] <- "inland silverside"
fish$common_name[fish$scientific_name =="salmonidae"] <- "salmonid family"
fish$common_name[fish$scientific_name =="dorosoma cepedianum"] <- "gizzard shad"
fish$common_name[fish$scientific_name =="anchoa mitchilli"] <- "bay anchovy"
fish$common_name[fish$scientific_name =="brevoortia tyrannus"] <- "Atlantic menhaden"
fish$common_name[fish$scientific_name =="caranx hippos"] <- "crevalle jack"
fish$common_name[fish$scientific_name =="cynoscion regalis"] <- "weakfish"
fish$common_name[fish$scientific_name =="leiostomus xanthurus"] <- "spot"
fish$common_name[fish$scientific_name =="paralichthys dentatus"] <- "summer flounder"
fish$common_name[fish$scientific_name =="peprilus triacanthus"] <- "butterfish"
fish$common_name[fish$scientific_name =="pomatomus saltatrix"] <- "bluefish"
fish$common_name[fish$scientific_name =="prionotus carolinus"] <- "northern searobin"
fish$common_name[fish$scientific_name =="pseudopleuronectes americanus"] <- "winter flounder"
fish$common_name[fish$scientific_name =="scomberomorus cavalla"] <- "king mackerel"
fish$common_name[fish$scientific_name =="eucinostomus argentous"] <- "spotfin mojarra"
fish$common_name[fish$scientific_name =="menticirrhus saxatilis"] <- "northern kingfish"
fish$common_name[fish$scientific_name =="alosa mediocris"] <- "hickory shad"
fish$common_name[fish$scientific_name =="microgadus tomcod"] <- "Atlantic tomcod"
fish$common_name[fish$scientific_name =="cyprinodon variegatus"] <- "sheepshead minnow"
fish$common_name[fish$scientific_name =="lucania parva"] <- "rainwater killifish"
fish$common_name[fish$scientific_name =="poecilia reticulata"] <- "guppy"


#The VT DEC and the VT DFW data did not have scientific names, just common names, so I want to make a separatetable to join
fish$common_name <- tolower(fish$common_name)
fish$common_name <- trimws(fish$common_name)
join <- fish %>% 
  select(common_name, scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  unique()

fish <- fish %>% 
  select(-scientific_name) %>% 
  left_join(join, by = "common_name")

unique(fish$common_name[is.na(fish$scientific_name)]) #39 common names differ from the ones above
fish$common_name[fish$common_name =="blacknose dace"] <- "eastern blacknose dace"
fish$common_name[fish$common_name =="unidentified cyprinid"] <- "minnow family"
fish$common_name[fish$common_name =="no fish!"] <- "no fish"
fish$common_name[fish$common_name =="log perch"] <- "log perch genus"
fish$common_name[fish$common_name =="rockbass"] <- "rock bass"
fish$common_name[fish$common_name =="mudminnow"] <- "central mudminnow"
fish$common_name[fish$common_name =="silvery minnow"] <- "eastern silvery minnow"
fish$common_name[fish$common_name =="carp"] <- "common carp"
fish$common_name[fish$common_name =="moxostoma sp."] <- "redhorse genus"
fish$common_name[fish$common_name =="lamprey"] <- "lamprey order"
fish$common_name[fish$common_name =="northern redbelly dace"] <- "redbelly dace"
fish$common_name[fish$common_name =="pearl dace"] <- "allegheny pearl dace" #need to confirm with courtney
fish$common_name[fish$common_name =="atlantic salmon (anadromous)"] <- "atlantic salmon"
fish$common_name[fish$common_name =="atlantic salmon (landlocked)"] <- "atlantic salmon"

fish <- fish %>% 
  select(-scientific_name) %>% 
  left_join(join, by = "common_name")

fish$scientific_name[fish$common_name =="log perch genus"] <- "percina spp."
fish$scientific_name[fish$common_name =="redbelly dace"] <- "chrosomus eos"
fish$scientific_name[fish$common_name =="allegheny pearl dace"] <- "margariscus margarita"
fish$scientific_name[fish$common_name =="mottled sculpin"] <- "cottus bairdii"
fish$scientific_name[fish$common_name =="brook stickleback"] <- "culaea inconstans"
fish$scientific_name[fish$common_name =="silver lamprey"] <- "ichthyomyzon unicuspis"
fish$scientific_name[fish$common_name =="sand shiner"] <- "notropis stramineus"
fish$scientific_name[fish$common_name =="silver lamprey"] <- "ichthyomyzon unicuspis"
fish$scientific_name[fish$common_name =="trout perch"] <- "percopsis omiscomaycus"
fish$scientific_name[fish$common_name =="chrosomus hybrid"] <- "chrosomus hybrid"
fish$scientific_name[fish$common_name =="blackchin shiner"] <- "notropis heterodon"
fish$scientific_name[fish$common_name =="spotfin shiner"] <- "cyprinella spiloptera"
fish$scientific_name[fish$common_name =="longnose gar"] <- "lepisosteus osseus"
fish$scientific_name[fish$common_name =="redhorse genus"] <- "moxostoma spp."
fish$scientific_name[fish$common_name =="fantail darter"] <- "etheostoma flabellare"
fish$scientific_name[fish$common_name =="brassy minnow"] <- "hybognathus hankinsoni"
fish$scientific_name[fish$common_name =="lamprey order"] <- "petromyzontiformes order"
fish$scientific_name[fish$common_name =="shorthead redhorse"] <- "moxostoma macrolepidotum"
fish$scientific_name[fish$common_name =="redear sunfish"] <- "lepomis microlophus"
fish$scientific_name[fish$common_name =="brook silverside"] <- "labidesthes sicculus"
fish$scientific_name[fish$common_name =="northern pearl dace"] <- "margariscus nachtriebi"
fish$scientific_name[fish$common_name =="greater redhorse"] <- "moxostoma valenciennesi"
fish$scientific_name[fish$common_name =="tench"] <- "tinca tinca"
fish$scientific_name[fish$common_name =="rudd"] <- "scardinius spp."
fish$scientific_name[fish$common_name =="eastern sand darter"] <- "Ammocrypta pellucida"
fish$scientific_name[fish$common_name =="channel darter"] <- "Percina copelandi"
fish$scientific_name[fish$common_name =="stonecat"] <- "Noturus flavus"
fish$scientific_name[fish$common_name =="northern brook lamprey"] <- "Ichthyomyzon fossor"
fish$scientific_name[fish$common_name =="kokanee salmon"] <- "Oncorhynchus nerka"

test <- fish %>% 
  filter(is.na(common_name))
#66 observations from MA do not have a common name or a scientific name.. go back to ma data and figure it out

#remove observations at the family or higher taxonomic level
fish <- fish %>% 
  filter(!grepl("order|family|hybrid|no fish|unknown", common_name))


#add columns for genus and family and remove the genus and family names in the spp column
tmp <- fish %>% 
  select(scientific_name) %>% 
  separate(scientific_name, into = c("genus", "remove"), sep = " ") %>% 
  select(-remove)
fish <- cbind(fish, tmp)


#Remove the genus level that is recorded in spp level
fish$common_name[fish$common_name == "snakehead genus"] <- NA
fish$common_name[fish$common_name == "sunfish genus"] <- NA
fish$common_name[fish$common_name == "river herring"] <- NA
fish$common_name[fish$common_name == "log perch genus"] <- NA
fish$common_name[fish$common_name == "redhorse genus"] <- NA
fish$common_name[fish$common_name == "rudd"] <- NA
fish$common_name[fish$common_name == "salmon and trout genus"] <- NA

fish$scientific_name[fish$scientific_name == "channa sp."] <- NA
fish$scientific_name[fish$scientific_name == "lepomis sp"] <- NA
fish$scientific_name[fish$scientific_name == "alosa spp."] <- NA
fish$scientific_name[fish$scientific_name == "percina spp."] <- NA
fish$scientific_name[fish$scientific_name == "moxostoma spp."] <- NA
fish$scientific_name[fish$scientific_name == "scardinius spp."] <- NA
fish$scientific_name[fish$scientific_name == "salmo"] <- NA

head(fish)

#want three fish datasets: a spp count by run and survey; and one with spp lengths; and a spp presence absence


fish_count <- fish %>% 
  filter(!is.na(count)) %>% 
  select(UID, run_num, count, common_name, scientific_name, genus) %>% 
  unique()  #thi shoudl just be unique

fish_size <- fish  %>% 
  filter(!is.na(length_mm)) %>% 
  select(-count) #dont want to do unique() here because there may be the same spp with the same lengths in the same survey
  
fish_presence <- fish %>% 
  select(UID, common_name, scientific_name, genus) %>% 
  unique()

#save
save(fish_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_count.RData")
save(fish_size, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_size.RData")
save(fish_presence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_presence.RData")



#fish look up tables
fishspp <- fish %>% 
  filter(!is.na(count)) %>% 
  group_by(scientific_name, common_name) %>% 
  summarize(count = n() ) %>% 
  arrange(scientific_name)
write.csv(fishspp, "fishspp_count.csv")




fishgenus <- fish %>% 
  filter(!is.na(count)) %>% 
  group_by(genus) %>% 
  summarise(count = n()) %>% 
  arrange(genus)
write.csv(fishgenus, "fishgenus_count.csv")










###################################

#### combine method dataframes ####

###################################









method <- bind_rows(ma_method, ct_method, nh_method, dec_method, dfw_method, ri_method)
method$gear <- tolower(method$gear)
unique(method$gear)

#the gears that are unclear - need to confirm with each agency

# t <- method$UID[method$gear == "electroshock (other)"] #CT #Chris confirmed these are efish_barge
# 
# t <- method$UID[method$gear == "efish"]  #Matt confirmed backpack electrofish
# t <- substr(t, start = 1, stop = 3)
# t <- unique(t) #FG
# 
# t <- method$UID[method$gear == "barge and backpack shocking"] 
# t <- substr(t, start = 1, stop = 3)
# t <- unique(t) #MA
# 
# 
# t <- method$UID[method$gear == "barge"] 
# t <- substr(t, start = 1, stop = 3)
# t <- unique(t) #MA
# 
# t <- method$UID[method$gear == "boat"] #Andy confirmed electroshock boat 
# t <- substr(t, start = 1, stop = 3)
# t <- unique(t) #des

t <- method$UID[method$gear == "electroshock"] 
t <- substr(t, start = 1, stop = 3)
t <- unique(t) #VT

method$gear[method$gear == "backpack shocking"] <- "efish_backpack"
method$gear[method$gear == "boat shocking"] <- "efish_boat"
method$gear[method$gear == "electroshock (other)"] <- "efish_barge"
method$gear[method$gear == "efish"] <- "efish_backpack"
method$gear[method$gear == "barge and backpack shocking"] <- "efish_misc"
method$gear[method$gear == "barge shocking"] <- "efish_barge"
method$gear[method$gear == "barge"] <- "efish_barge"
method$gear[method$gear == "boat"] <- "efish_boat" #confirmed by Andy
method$gear[method$gear == "eboat"] <- "efish_boat"
method$gear[method$gear == "backpack"] <- "efish_backpack"
method$gear[method$gear == "electroshock"] <- "efish_backpack" #ned to confirm with Jim

method$gear[method$gear == "gillnet"] <- "gill net"
method$gear[method$gear == "gill"] <- "gill net"

method$gear[method$gear == "dipnet"] <- "dip net"
method$gear[method$gear == "fyke"] <- "fyke net"
method$gear[method$gear == "minnowtrap"] <- "minnow trap"
method$gear[method$gear == "not stated"] <- NA

unique(method$goal)
method$goal <- tolower(method$goal)
method$goal[method$goal == "selective pick up"] <- "selective pick-up"
method$goal[method$goal == "total pick up"] <- "total pick-up"

unique(method$target)


#save
save(method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_method.RData")






#################

#make fish data spatial



#load the merged HUC boundary files
dat8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NDH/mergedfiles/huc8.shp") #this is the crs that we want


#load fish data
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_count.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_event.RData")

dat <- left_join(fish_count, event, by = "UID")

unique(dat$UID[is.na(dat$latitude)])
#99 unique fish survey UID do not have corresponding event data: 8 from MA, 15 from NH DFG, 76 from RI DEM  
#remove the surveys with no location information
dat <- dat %>% 
  filter(!is.na(latitude))

#make the fish data frame an sf object so it can be plotted spatially
shp <- st_as_sf(x = dat,                         
                coords = c("longitude", "latitude"),
                crs = st_crs(dat8))

st_write(shp, dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/sppdata/all_fish_count.shp")






#make the event data spatial
event <- event %>% 
  filter(!is.na(latitude))
shp <- st_as_sf(x = event,                         
                coords = c("longitude", "latitude"),
                crs = st_crs(dat8))

st_write(shp, dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/sppdata/all_fish_event.shp")
