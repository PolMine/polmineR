# Copy the registry file of GERMAPARLMINI to a temporary registry, with corpus
# properties declaring named-entity annotation. GERMAPARLMINI has no
# named-entity annotation, so existing s-attributes stand in for it. A corpus
# id of its own avoids that GERMAPARLMINI is loaded from two registries.
ne_test_corpus <- function(id, properties){
  registry <- tempfile()
  dir.create(registry)
  reglines <- readLines(file.path(registry(), "germaparlmini"))
  reglines <- sub("^ID\\s+germaparlmini$", sprintf("ID   %s", id), reglines)
  # Corpus properties have to precede attribute declarations.
  writeLines(
    append(reglines, properties, after = grep("^##:: build_date", reglines)),
    file.path(registry, id)
  )
  RcppCWB::cl_load_corpus(corpus = toupper(id), registry = registry)
  RcppCWB::cqp_load_corpus(corpus = toupper(id), registry = registry)
  registry
}

# Encode a small corpus with named-entity annotation, declared by corpus
# properties. Attributes of the 'ne' element become s-attributes 'ne_type',
# 'ne_uri' and 'ne_wikidata_type'. If an attribute is missing on an element,
# cwb-encode writes an empty string, as in GermaParl2.
ne_vrt_corpus <- function(id = "netestvrt"){
  tmp <- tempfile()
  vrt_dir <- file.path(tmp, "vrt")
  data_dir <- file.path(tmp, "data")
  registry <- file.path(tmp, "registry")
  for (d in c(vrt_dir, data_dir, registry)) dir.create(d, recursive = TRUE)
  writeLines(
    c(
      '<doc id="d1">',
      '<ne type="PERSON" uri="Q567" wikidata_type="|Q5|">',
      "Angela", "Merkel", "</ne>",
      "spricht", "in",
      '<ne type="LOCATION" uri="Q64" wikidata_type="|Q515|Q1549591|">',
      "Berlin", "</ne>",
      "und", "in",
      '<ne type="LOCATION" uri="Q64" wikidata_type="|Q515|Q1549591|">',
      "Berlin", "</ne>",
      "mit",
      '<ne type="PERSON">', "Müller", "</ne>",
      "über",
      "<ne>", "Ostpolitik", "</ne>",
      ".",
      "</doc>"
    ),
    file.path(vrt_dir, paste0(id, ".vrt"))
  )
  RcppCWB::cwb_encode(
    corpus = toupper(id), registry = registry, data_dir = data_dir,
    vrt_dir = vrt_dir, p_attributes = "word",
    s_attributes = list(doc = "id", ne = c("type", "uri", "wikidata_type")),
    quietly = TRUE
  )
  regfile <- file.path(registry, id)
  reglines <- readLines(regfile)
  properties <- c(
    '##:: ne_s_attribute = "ne"',
    '##:: ne_type_s_attribute = "ne_type"',
    '##:: ne_uri_s_attribute = "ne_uri"'
  )
  # Corpus properties have to precede attribute declarations.
  first_attribute <- grep("^(ATTRIBUTE|STRUCTURE)", reglines)[1]
  writeLines(append(reglines, properties, after = first_attribute - 1L), regfile)
  RcppCWB::cl_load_corpus(corpus = toupper(id), registry = registry)
  RcppCWB::cqp_load_corpus(corpus = toupper(id), registry = registry)
  registry
}
