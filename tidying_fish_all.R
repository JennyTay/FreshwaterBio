

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





#### combine event dataframes ####
event <- bind_rows(ct_event, dec_event, nh_event, ma_event)







#### combine fish dataframes ####


fish <- bind_rows(ma_fish, ct_fish, nh_fish, dec_fish)
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

#need to add in common names
fish$common_name[fish$scientific_name =="acipenser brevirostrum"] <- "shortnose sturgeon"
fish$common_name[fish$scientific_name =="alosa aestivalis"] <- "blueback herring"
fish$common_name[fish$scientific_name =="alosa pseudoharengus"] <- "alewife"
fish$common_name[fish$scientific_name =="alosa sapidissima"] <- "American shad"
fish$common_name[fish$scientific_name =="ambloplites rupestris"] <- "rock bass"
fish$common_name[fish$scientific_name =="ameiurus catus"] <- "white bullhead"
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
fish$common_name[fish$scientific_name =="clinostomus funduloides"] <- "rosyside dace"
fish$common_name[fish$scientific_name =="coregonus clupeaformis"] <- "lake whitefish"
fish$common_name[fish$scientific_name =="cottus cognatus"] <- "slimy sculpin"
fish$common_name[fish$scientific_name =="couesius plumbeus"] <- "lake chub"
fish$common_name[fish$scientific_name =="cyprinidae"] <- "minnow family"
fish$common_name[fish$scientific_name =="cyprinus carpio"] <- "common carp"
fish$common_name[fish$scientific_name =="enneacanthus obesus"] <- "banded sunfish"
fish$common_name[fish$scientific_name =="erimyzon oblongus"] <- "creek chubsucker"
fish$common_name[fish$scientific_name =="esox americanus"] <- "American pickerel"
fish$common_name[fish$scientific_name =="esox americanus americanus"] <- "redfin pickerel"
fish$common_name[fish$scientific_name =="esox americanus americanus x esox niger"] <- "redfin pickerel x chain pickerel"
fish$common_name[fish$scientific_name =="esox lucius"] <- "northern pike"
fish$common_name[fish$scientific_name =="esox lucius x esox masquinongy"] <- "northern pike x muskellunge"
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
fish$common_name[fish$scientific_name =="lampetra appendix"] <- "American brook lamprey"
fish$common_name[fish$scientific_name =="lepomis auritus"] <- "redbreast sunfish"
fish$common_name[fish$scientific_name =="lepomis cyanellus"] <- "green sunfish"
fish$common_name[fish$scientific_name =="lepomis gibbosus"] <- "pumpkinseed"
fish$common_name[fish$scientific_name =="lepomis macrochirus"] <- "bluegill"
fish$common_name[fish$scientific_name =="lepomis macrochirus x lepomis gibbosus"] <- "bluegill x pumpkinseed"
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
fish$common_name[fish$scientific_name =="phoxinus eos"] <- "northern redbelly dace"
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
fish$common_name[fish$scientific_name =="salvelinus fontinalis x salmo trutta"] <- "brook trout x brown trout"
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

#The vermont DEC data did not have scientific names, just common names, so I want to make a separatetable to join
fish$common_name <- tolower(fish$common_name)
fish$common_name <- trimws(fish$common_name)
join <- fish %>% 
  select(common_name, scientific_name) %>% 
  filter(!is.na(scientific_name)) %>% 
  unique()

fish <- fish %>% 
  select(-scientific_name) %>% 
  left_join(join, by = "common_name")

unique(fish$common_name[is.na(fish$scientific_name)]) #34 common names differ from the ones above
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

test <- fish %>% 
  filter(is.na(common_name))
#66 observations from MA do not have a common name or a scientific name.. go back to ma data and figure it out

fishspp <- fish %>% group_by(common_name, scientific_name) %>% summarize(count = n() ) %>% arrange(scientific_name)
write.csv(fishspp, "fishspp_lookup.csv")





#### combine method dataframes ####

method <- bind_rows(ma_method, ct_method, nh_method, dec_method)
method$gear <- tolower(method$gear)
unique(method$gear)
method$gear[method$gear == "backpack shocking"] <- "electroshock"
method$gear[method$gear == "boat shocking"] <- "electroshock"
method$gear[method$gear == "electroshock (other)"] <- "electroshock"
method$gear[method$gear == "backpack shocking"] <- "electroshock"
method$gear[method$gear == "efish"] <- "electroshock"
method$gear[method$gear == "barge and backpack shocking"] <- "electroshock"
method$gear[method$gear == "barge shocking"] <- "electroshock"
method$gear[method$gear == "barge"] <- "electroshock"
method$gear[method$gear == "boat"] <- "electroshock"
method$gear[method$gear == "eboat"] <- "electroshock"
method$gear[method$gear == "backpack"] <- "electroshock"

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



#### describing methods move to a new script after I decide where to save the dataframes


test <- method %>% 
  group_by(goal) %>% 
  summarise(count = n())

test <- method %>%  #how many surveys were targeted by spp
  filter(goal == "selective pick-up") %>% 
  group_by(target) %>% 
  summarise(count = n())
write.csv(test, "tmpfigures/table.csv")

test2 <- fish %>% #total surveys for each spp that also had the targeted surveys so we can see what percentage the targeted surveys were of the total
  filter(common_name %in% test$target) %>% 
  group_by(common_name) %>% 
  summarize(count = n())

test <- method %>% #gear by state
  left_join(event, by = "UID") %>% 
  group_by(state, gear) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = state, values_from = count)
write.csv(test, "tmpfigures/table.csv")

test <- fish %>% #fish missed by only including efish
  left_join(method, by = "UID") %>% 
  group_by(gear, common_name) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = gear, values_from = count) %>% 
  filter(is.na(electroshock) | electroshock <100) %>% 
  arrange(electroshock)
write.csv(test, "tmpfigures/table1.csv")

fishspp <- fish %>% group_by(common_name, scientific_name) %>% summarize(count = n() ) %>% arrange(scientific_name)
write.csv(fishspp, "tmpfigures/fishspp.csv")

