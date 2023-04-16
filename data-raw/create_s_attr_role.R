library(polmineR)
library(data.table)
library(cwbtools)

regdir <- "~/Lab/github/polmineR/inst/extdata/cwb/registry"
datadir <- "~/Lab/github/polmineR/inst/extdata/cwb/indexed_corpora/germaparlmini"

use("polmineR")

date_gparl_mini <- s_attributes("GERMAPARLMINI", "date")

s_attrs_mini <- corpus("GERMAPARLMINI") %>%
  s_attributes(c("date", "speaker", "party"), unique = FALSE)
s_attrs_mini[, "struc" := 0:(nrow(s_attrs_mini) - 1L)]
regions <- RcppCWB::get_region_matrix(
  corpus = "GERMAPARLMINI",
  registry = registry(),
  struc = s_attrs_mini[["struc"]],
  s_attribute = "speaker"
)
s_attrs_mini[, "cpos_left" := regions[,1]]
s_attrs_mini[, "cpos_right" := regions[,2]]
setcolorder(s_attrs_mini, c("struc", "cpos_left", "cpos_right"))

s_attrs_full <- corpus("GERMAPARL") %>%
  subset(date %in% date_gparl_mini) %>%
  s_attributes(c("date", "speaker", "party", "role"), unique = TRUE)

# parties have been consolidated differently with GERMAPARL
s_attrs_full[, "party" := ifelse(party == "CSU", "CDU_CSU", party)]
s_attrs_full[, "party" := ifelse(party == "CDU", "CDU_CSU", party)]
s_attrs_full[, "party" := ifelse(party == "LINKE", "DIE_LINKE", party)]
s_attrs_full[, "party" := ifelse(party == "GRUENE", "B90_DIE_GRUENEN", party)]

# GERMAPARLMINI retains middle names
s_attrs_mini2 <- s_attrs_mini[, "speaker" := gsub("\\s+", " ", speaker)]
s_attrs_mini2 <- s_attrs_mini2[, "speaker" := ifelse(speaker == "Angela Dorothea Merkel", "Angela Merkel", speaker)]
s_attrs_mini2 <- s_attrs_mini2[, "speaker" := ifelse(speaker == "Uwe Karl Beckmeyer", "Uwe Beckmeyer", speaker)]
s_attrs_mini2 <- s_attrs_mini2[, "speaker" := ifelse(speaker == "Michael Franz Wilhelm Fuchs", "Michael Fuchs", speaker)]
s_attrs_mini2 <- s_attrs_mini2[, "party" := ifelse(speaker == "Kerstin Andreae", "B90_DIE_GRUENEN", party)]

# now merge
comb <- s_attrs_full[s_attrs_mini2, on = c("date", "speaker", "party")]

# fill missing (known) information
comb2 <- comb[, "role" := ifelse(speaker %in% c("Elke Ferner", "Daniela Raab", "Peter Götz", "Ulla Lötzer"), "mp", role)]

# we now have missing values only for members of the presidency
unique(comb2[is.na(role)]$speaker)
# "Heinz Riesenhuber"     "Norbert Lammert"       "Wolfgang Thierse"      "Gerda Hasselfeldt"     "Petra Pau"            
#  "Katrin Göring-Eckardt" "Hermann Otto Solms"

comb2 <- comb2[, "role" := ifelse(is.na(role), "presidency", role)] 

s_attribute_encode(
  values = comb2$role,
  data_dir = datadir,
  s_attribute = "role",
  corpus = "GERMAPARLMINI",
  region_matrix = as.matrix(comb2[, c("cpos_left", "cpos_right")]),
  registry_dir = regdir,
  encoding = "latin1",
  method = "R"
)


