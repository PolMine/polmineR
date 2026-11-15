#' @include S4classes.R
NULL

#' Get the attributes of named-entity annotation
#'
#' Corpora differ in how named entities are annotated. Corpus properties in
#' the registry file can declare which structural attributes hold the
#' annotation. `ne_attributes()` reads this declaration, checks it against the
#' s-attributes of the corpus and returns it in a standardized form.
#'
#' The following corpus properties are evaluated (lines starting with `##::`
#' in the registry file):
#'
#' - `ne_s_attribute` (required): The s-attribute with the regions of named
#'   entity mentions (e.g. "ne"). A mention is a region of one or more tokens.
#' - `ne_type_s_attribute` (optional): The s-attribute with the type of a
#'   mention (e.g. "ne_type").
#' - `ne_uri_s_attribute` (optional): The s-attribute with a link of the
#'   mention to an entry in a knowledge base (e.g. "dbpedia_uri"). Regions of
#'   this s-attribute are expected to coincide with the regions of mentions.
#' - `ne_types` (optional): The types of named entities, separated by "|"
#'   (e.g. "PERSON|ORGANIZATION|LOCATION|MISC").
#'
#' A registry file declaring the named-entity annotation of the GermaParl2
#' corpus would include these lines:
#'
#' ```
#' ##:: ne_s_attribute = "ne"
#' ##:: ne_type_s_attribute = "ne_type"
#' ##:: ne_uri_s_attribute = "dbpedia_uri"
#' ##:: ne_types = "PERSON|ORGANIZATION|LOCATION|MISC"
#' ```
#'
#' @param .Object A `corpus` object, an object inheriting from it (such as a
#'   `subcorpus`), or a length-one `character` vector with the ID of a corpus.
#' @param ... Further arguments (not used).
#' @return `NULL` if the corpus does not declare named-entity annotation
#'   (property `ne_s_attribute` is not defined). Otherwise a named `list`
#'   with the elements:
#'   - `s_attribute`: The s-attribute with the mentions.
#'   - `type`: The s-attribute with the type, or `NA` if not declared.
#'   - `uri`: The s-attribute with links to a knowledge base, or `NA` if not
#'     declared.
#'   - `types`: A `character` vector with the types of named entities. If
#'     property `ne_types` is not defined, these are the values of the
#'     s-attribute `type`. `NULL` if neither is available.
#'
#'   An error is raised if a declared s-attribute is not present in the corpus.
#' @exportMethod ne_attributes
#' @rdname ne_attributes
#' @name ne_attributes
#' @examples
#' use("polmineR")
#'
#' # GERMAPARLMINI does not declare named-entity annotation
#' ne_attributes("GERMAPARLMINI")
#'
#' \dontrun{
#' use("GermaParl2")
#' ne_attributes("GERMAPARL2")
#' corpus("GERMAPARL2") %>% ne_attributes()
#' }
setGeneric("ne_attributes", function(.Object, ...) standardGeneric("ne_attributes"))


#' @rdname ne_attributes
#' @importFrom RcppCWB corpus_properties corpus_property corpus_s_attributes
setMethod("ne_attributes", "corpus", function(.Object, ...){
  corpus_id <- slot(.Object, "corpus")
  registry_dir <- slot(.Object, "registry_dir")

  properties <- corpus_properties(corpus = corpus_id, registry = registry_dir)
  if (!"ne_s_attribute" %in% properties) return(NULL)

  get_property <- function(property){
    if (!property %in% properties) return(NA_character_)
    value <- trimws(
      corpus_property(
        corpus = corpus_id, registry = registry_dir, property = property
      )
    )
    if (nchar(value) == 0L) NA_character_ else value
  }

  y <- list(
    s_attribute = get_property("ne_s_attribute"),
    type = get_property("ne_type_s_attribute"),
    uri = get_property("ne_uri_s_attribute"),
    types = NULL
  )

  declared <- unlist(y[c("s_attribute", "type", "uri")])
  declared <- declared[!is.na(declared)]
  # The declaration applies to the corpus, so s-attributes are checked (and
  # types derived) for the corpus, also if .Object is a subcorpus.
  missing_s_attrs <- setdiff(
    declared,
    corpus_s_attributes(corpus = corpus_id, registry = registry_dir)
  )
  if (length(missing_s_attrs) > 0L){
    stop(sprintf(
      "s-attribute(s) declared for named-entity annotation not available in corpus '%s': %s",
      corpus_id, paste(missing_s_attrs, collapse = ", ")
    ))
  }

  types <- get_property("ne_types")
  if (!is.na(types)){
    y[["types"]] <- trimws(strsplit(types, "|", fixed = TRUE)[[1]])
  } else if (!is.na(y[["type"]])){
    y[["types"]] <- sort(s_attributes(
      corpus(corpus_id, registry_dir = registry_dir),
      s_attribute = y[["type"]], unique = TRUE
    ))
  }

  y
})


#' @rdname ne_attributes
setMethod("ne_attributes", "character", function(.Object, ...){
  if (length(.Object) != 1L){
    stop("'.Object' is expected to be a length-one character vector")
  }
  ne_attributes(corpus(.Object), ...)
})
