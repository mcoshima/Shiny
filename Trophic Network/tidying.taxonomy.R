
library(rfishbase)
library(dplyr)
library(stringr)
library(purrr)
library(stringi)

`%not_in%` <- purrr::negate(`%in%`)

diet <- read.csv("Diet.sub.csv", 
                 stringsAsFactors = F,
                 header = T)
head(diet)
colnames(diet)

pred_names <- unique(diet$Predator_Scientific_Name)
pred_names <- pred_names %>% str_trim(side = "right") %>% str_replace("  ", " ") %>% stri_remove_empty()
pred_names_val <- c()

for(i in 1:length(pred_names)){
  
  nm <- validate_names(pred_names[i])
  if(length(nm) == 0){
    pred_names_val[i] <- "not_validated"
  }else{
    pred_names_val[i] <- nm 
  }
  
}


for(i in 1:length(pred_names)){
  
  ind <- agrep(pred_names[i], pred_names_val, max.distance = .2, ignore.case = T)
  if(length(ind) == 0){
    next
  }
  pred_names[i] <- pred_names_val[ind]
  
}

pred_names <- unique(pred_names)
pred_names_val <- c()
#get which names haven't be validated then search species list by genus for name  

for(i in 1:length(pred_names)){
  
  nm <- validate_names(pred_names[i])
  if(length(nm) == 0){
    pred_names_val[i] <- "not_validated"
  }else{
    pred_names_val[i] <- nm 
  }
  
}

needs_val <- pred_names[which(pred_names_val == "not_validated")]
needs_val <- needs_val %>% str_subset("spp", negate = T) %>% str_subset(" ") %>% str_subset("Subfamily", negate = T)

sci.com <- read.csv("sci_to_com_names.csv", stringsAsFactors = F)

gen.vec <- strsplit(needs_val, " ") %>% map(pluck(1)) %>% unique()
sp.vec <- strsplit(needs_val, " ") %>% map(pluck(2)) %>% unique()

potential_spp <- list()
for(i in 1:length(gen.vec)){
  
  potential_spp[[i]] <- species_list(Genus = gen.vec[[i]])
  
}
names(potential_spp) <- gen.vec

not_val <- list()
val <- c()
for(i in 1:length(needs_val)){
  gen <- strsplit(needs_val[i], split = " ")
  ind <- which(names(potential_spp) %in% gen[[1]][1])
  ind. <- agrep(needs_val[i], potential_spp[[ind]], max.distance = .1, ignore.case = T)
    if(length(ind.)==0){
      not_val[[i]] <- gen[[1]]
      next
    }
    val[i] <- potential_spp[[ind]][ind.]
}

val[6] <- "Fundulus grandis"
val <- c(val, rep(NA, 3))

not_val <- compact(not_val) %>% map(paste, collapse = " ") %>% unlist()

which(not_val[1] %in% sci.com$Sci_Name)

#get the common names for the non validated ones to use common_to_sci function to get actual scientific name
nv_sci_com <- diet[which( diet$Predator_Scientific_Name %in% not_val),c(4:5)]

nv_sci_com_uin <- nv_sci_com %>% 
  distinct() %>% 
  filter(str_detect(Predator_Common_Name, ""))

comm_nv <- nv_sci_com_uin %>% 
  select(Predator_Common_Name)

pot_sci_nms <- list()
for(i in 1:nrow(comm_nv)){
  pot_sci_nms[[i]] <- common_to_sci(comm_nv[i,])
  
}

for(i in 1:nrow(nv_sci_com_uin)){
  
  if(length(unique(pot_sci_nms[[i]]$Species)) > 1){
    ind <- agrep(nv_sci_com_uin[i,1], unique(pot_sci_nms[[i]]$Species), max.distance = 0.2, ignore.case = T)
    if(length(ind) == 0){
      print(nv_sci_com_uin[i,1])
      print(i)
      next
    }else{
      nv_sci_com_uin[i,1] <- unique(pot_sci_nms[[i]]$Species)[ind]  
    }
  }
  if(length(unique(pot_sci_nms[[i]]$Species)) == 1){
    nv_sci_com_uin[i,1] <- unique(pot_sci_nms[[i]]$Species) 
  }
  if(length(unique(pot_sci_nms[[i]]$Species)) == 0){
    next
  }
}

nv_sci_com_uin[5,1] <- "Elops saurus"
nv_sci_com_uin[9,1] <- "Pagrus pagrus"
nv_sci_com_uin[10,1] <- "Pogonias cromis"

sci_com_uin_val <- c()
for(i in 1:nrow(nv_sci_com_uin)){
  
  nm <- validate_names(nv_sci_com_uin[i,1])
  if(length(nm) == 0){
    sci_com_uin_val[i] <- "not_validated"
  }else{
    sci_com_uin_val[i] <- nm 
  }
  
}

#looked these up bc they weren't in Fishbase
sci_com_uin_val[13] <- "Stomias affinis"
sci_com_uin_val[7] <- "Holacanthus bermudensis"

needs_val_ind <- list()
for(i in 1:length(needs_val)){
  
  needs_val_ind[[i]] <- which(diet$Predator_Scientific_Name == needs_val[i])
  
}

org_pred_names <- unique(diet$Predator_Scientific_Name)
val.narm <- val[!is.na(val)]
#make dataframe with new and old names
pred_names_val_sub <- pred_names_val[-which(pred_names_val == "not_validated")]
length(c(pred_names_val_sub, val.narm, sci_com_uin_val))

fixed_names <- data.frame("Original_name" = c(not_val, needs_val, pred_names), "Validated_name" = c(sci_com_uin_val, val, pred_names_val))

write.csv(fixed_names, "fixed_names.csv", row.names = F)

#checked in excel and got rid of NA and not_validated rows

checked_nms <- read.csv("fixed_names.csv", stringsAsFactors = F);head(checked_nms)

diet.2 <- matrix(NA, nrow = nrow(diet), ncol = ncol(diet))
for(i in 1:nrow(checked_nms)){
 
  ind <- which(checked_nms[i,1] == diet$Predator_Scientific_Name)
  diet.2[ind,] <- as.matrix(diet[ind,])
  diet.2[ind, 4] <- checked_nms[i,2]
  
}

diet.3 <- diet.2 %>% 
  as.data.frame() %>% 
  set_names(colnames(diet)) %>% 
  drop_na(Title)


diet.3 <- diet.3 %>% 
  filter_at(vars(Frequency_of_Occurrence, 
                        Dry_Weight, 
                        Weight, 
                        IRI, 
                        Number, 
                        ICI, 
                        Volume), 
                   any_vars(!is.na(.))) %>% 
  separate(Predator_Scientific_Name, into = c("Genus_Predator", "Species_Predator"), sep = " ", remove = F)


diet.3 <- diet.3 %>% 
  mutate(
    Species_Prey = replace(Species_Prey, str_detect(Species_Prey, "spp"), ""),
    Lowest_Taxonomic_Identification_Prey = paste(Genus_Prey, Species_Prey),
    Predator_Scientific_Name = as.character(Predator_Scientific_Name),
    Predator_Common_Name = as.character(Predator_Common_Name))
  
class(diet.3$Predator_Common_Name)

fixed_names$Validated_name <- as.character(fixed_names$Validated_name)

cmn.nm <- list()

for(i in 1:nrow(fixed_names)){
  cmn.nm[[i]] <- common_names(fixed_names$Validated_name[i]) %>% filter(Language == "English") %>% unique()
}

fixed_cmnm <- data.frame(Sci_name = fixed_names$Validated_name, Common_name = rep(NA, nrow(fixed_names)))
fixed_cmnm <- fixed_cmnm %>% mutate(Sci_name = as.character(Sci_name)) %>% drop_na(Sci_name)

for(i in 1:nrow(fixed_cmnm)){
  cmn.nm[[i]] <- common_names(fixed_cmnm$Sci_name[i]) %>% filter(Language == "English") %>% unique()
}


for(i in 1:nrow(fixed_cmnm)){
  
  com_name <- diet.3 %>% 
    filter(str_detect(Predator_Scientific_Name, paste(fixed_cmnm[i,1]))) %>% 
    select(Predator_Common_Name) %>% distinct() %>% pull() %>% as.character() 
 
  com_name <- com_name[com_name != ""]
  
  if(length(com_name) > 1){
  fixed_cmnm$Common_name[i] <- "multiple_org_nms"
  }
  if(length(com_name) == 0){
    next
  }
  
  cand.names <- as.character(unlist(cmn.nm[[i]][,3]))
  row <- agrep(com_name, cand.names, max.distance = 0.1, ignore.case = T)
  row <- row[1]
  if(length(row)== 0){
    fixed_cmnm$Common_name[i] <- "go_back"
    next
  }
  fixed_cmnm$Common_name[i] <- cand.names[row]
}

fixed_cmnm$Common_name <- unlist(fixed_cmnm$Common_name)

write.csv(fixed_cmnm, file = "fixed_common_names.csv")

##fixed the NAs by looking the up and in excel

fixed_cmnm <- read.csv("fixed_common_names.csv", stringsAsFactors = F)
head(fixed_cmnm)

final.names <- read.csv("names_final.csv", stringsAsFactors = F)
head(final.names)

for(i in 1:nrow(final.names)){
  
  ind <- which(final.names[i,1] == diet.3$Predator_Scientific_Name)
  diet.3$Predator_Common_Name[ind] <- final.names[i,3]
  
}

diet.3$Species_Prey[which(diet.3$Species_Prey == "")] <- NA
diet.3 <- diet.3 %>% filter_at(vars(Kingdom_Prey, 
                          Class_Prey, 
                          Order_Prey, 
                          Family_Prey, 
                          Genus_Prey, 
                          Species_Prey), 
                     any_vars(!is.na(.))) 
write.csv(diet.3, "diet.tidy.csv", row.names = F)

prey.tax <- unique(Diet$Lowest_Taxonomic_Identification_Prey)
prey.tax <- sort(prey.tax)

prey.val <- c()
for(i in 1:length(prey.tax)){
  
  val.name <- validate_names(prey.tax[i])
  if(length(val.name) == 0){
    
    val.name <- validate_names(prey.tax[i], "sealifebase")
  }
  if(length(val.name) == 0){
    val.name <- "not_validated"
  }
  prey.val[i] <- val.name
}

diet.3 <- read.csv("diet.tidy.csv", stringsAsFactors = F)


diet.3 %>% group_by(Predator_Scientific_Name) %>% map()

diet.3 %>% filter(str_detect(Predator_Scientific_Name, "Acanthocybium solandri")) %>% select(Frequency_of_Occurrence, Order_Prey, Family_Prey, Genus_Prey, Species_Prey) %>% discard(~all(is.na(.x))) %>% names()  


diet.3 %>% filter(str_detect(Predator_Scientific_Name, "Acanthocybium solandri")) %>% select(Class_Prey, Dry_Weight, Weight, IRI, Number, ICI, Volume) %>% discard(~all(is.na(.x))) %>% names()  

diet.3[c(1:4),c(15:20)]


for(i in 1:nrow(diet.3)){
  
  temp <- subset(diet.3[i,c(14:19)])
  col <- which(!is.na(temp))
  
  diet.3$Lowest_Tax_Level[i] <- colnames(temp)[tail(col, n =1)]
  
}

diet.3$Lowest_Tax_Level <- gsub("_Prey", "", diet.3$Lowest_Tax_Level)
diet.3$Lowest_Tax_Level <- factor(diet.3$Lowest_Tax_Level, levels = c("Kingdom", "Class", "Order", "Family", "Genus", "Species"))

prey.diet <- diet.3 %>% select(-c(1:13))
prey.diet$Prey_name <- NA


for (i in 1:nrow(prey.diet)) {
  col.ind <- which(str_detect(colnames(prey.diet), prey.diet$Lowest_Tax_Level[i]))
  if(col.ind == 6){
    prey.diet$Prey_name[i] <- paste(prey.diet$Genus_Prey[i], prey.diet[i, col.ind])
  }else{
    prey.diet$Prey_name[i]  <- prey.diet[i, col.ind]
  }
  
}
diet.3$Lowest_Taxonomic_Identification_Prey <- prey.diet$Prey_name
colnames(diet.3)[6] <- "Species_Predator"
diet.3$Prey_name <- NULL

write.csv(diet.3, "diet.tidy.csv", row.names = F)