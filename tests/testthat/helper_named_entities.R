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
