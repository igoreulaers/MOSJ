# Housekeeping ####
library(jsonlite)
library(openxlsx)
library(dplyr)

# NPDC data retrieval ####
fieldwork_json = fromJSON("https://v2-api.npolar.no/biology/fielddata/?page=..&includeData=true")
lab_json = fromJSON("https://v2-api.npolar.no/biology/fielddata/_all_/ecotox/?page=..&includeData=true")
fieldwork_df = fieldwork_json$items$data
field_df_flat = flatten(fieldwork_df, recursive = TRUE)
lab_df = lab_json$items$data
lab_df_flat = flatten(lab_df, recursive = TRUE)
lab_df_flat <- lab_df_flat %>% rename(fieldNumberLab = fieldNumber)
lab_df_flat <- lab_df_flat %>% rename(rightsholderLab = rightsholder)
lab_df_flat <- lab_df_flat %>% rename(scientificNameLab = scientificName)
lab_df_flat <- lab_df_flat %>% rename(lifestageLab = lifestage)
lab_df_flat <- lab_df_flat %>% rename(sexLab = sex)
lab_df_flat <- lab_df_flat %>% rename(dynamicProperties.matrixLab = dynamicProperties.matrix)
lab_df_flat <- lab_df_flat %>% rename(dynamicProperties.responsibleLab = dynamicProperties.responsible)
fieldLab_df <- merge(field_df_flat, lab_df_flat, by = "eventID")

QUERY <- fieldLab_df[fieldLab_df$scientificName == "Larus hyperboreus",]
QUERY <- QUERY[complete.cases(QUERY[ , "eventID"]), ]

oldCOLname_extFL <- c("fieldNumber", "scientificName",
                      "eventDate", "locality", "decimalLatitude", "decimalLongitude",
                      "lifestage", "dynamicProperties.age", "sex", "dynamicProperties.weightInGrams",
                      "rightsholder", "dynamicProperties.responsible", "samplingProtocol",
                      "dynamicProperties.matrix", "measurementUnit", "dynamicProperties.fatPercentage",
                      "dynamicProperties.measurementCategory", "measurementType", "measurementValue",
                      "rightsholderLab", "measurementDeterminedBy", "dynamicProperties.responsibleLab", "measurementDeterminedDate",
                      "dynamicProperties.detectionLimit", "dynamicProperties.levelOfQuantification", "dynamicProperties.percentRecovery")
newCOLname_extFL <- c("ID.field", "species",
                      "date_field", "locality", "latitude", "longitude",
                      "maturity", "age", "sex", "mass.gram",
                      "rightsholder_field", "responsible_field", "protocol_field",
                      "matrix", "unit", "EOM.%",
                      "group", "compound", "concentration",
                      "rightsholder_lab", "lab", "responsible_lab", "date_lab",
                      "LOD", "LOQ", "recovery")
QUERY_long_extFL <- QUERY %>% select(8, 13,
                                     6, 10, 4, 5, 
                                     15, 18, 17, 27,
                                     12, 21, 16,
                                     19, 46, 54,
                                     56, 45, 47,
                                     49, 41, 58, 42,
                                     53, 61, 60) %>% rename_with(~ newCOLname_extFL, all_of(oldCOLname_extFL))

# NPDC data - cleaning #####
egg <- QUERY_long_extFL[QUERY_long_extFL$matrix == "egg",]
egg$year <- substr(egg$date_field, 1, 4)

egg <- egg[egg$compound %in% c("Hg",
                               "PCB-153",
                               "pp-DDE",
                               "HCB",
                               "oxy-CHL",
                               "b-HCH",
                               "PFOS"), ]

str(egg)
summary(egg)
egg$concentration <- as.numeric(egg$concentration)
names(egg)

any(is.na(egg$year))
egg <- egg[!is.na(egg$year), ]

any(is.na(egg$locality))
egg <- egg[!is.na(egg$locality), ]
unique(egg$locality)
egg$locality[egg$locality != "Kongsfjorden"] <- "Bjørnøya"

any(is.na(egg$ID.field))
length(egg$ID.field)
unique(egg$year[is.na(egg$ID.field)])
unique(egg$year[!is.na(egg$ID.field)])
egg <- egg[!is.na(egg$ID.field), ]

any(is.na(egg$unit))
unique(egg$unit)
unique(egg$year[egg$unit == "ng/g"])
unique(egg$year[egg$unit == "ng/g lw"])

# 2002 ng/g lw concentration conversion ####
eggLW <- egg[egg$unit == "ng/g lw",]
eggLW$concentration2 <- eggLW$concentration*(eggLW$`EOM.%`/100)
summary_data <- eggLW %>%
  group_by(locality, compound, year) %>%
  summarise(
    value = mean(concentration2, na.rm = TRUE),
    min = min(concentration2, na.rm = TRUE),
    max = max(concentration2, na.rm = TRUE)
  )

# Reporting NPDC summary data in MOSJ format ####
egg <- egg %>% rename("when" = "year")
summary_data <- egg %>%
  group_by(locality, compound, when) %>%
  summarise(
    value = mean(concentration, na.rm = TRUE),
    min = min(concentration, na.rm = TRUE),
    max = max(concentration, na.rm = TRUE)
  )
write.xlsx(summary_data, file = "MOSJ data - glaucous gull egg.xlsx")


