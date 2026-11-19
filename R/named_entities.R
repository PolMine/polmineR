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
#'   mention to an entry in a knowledge base (e.g. "ne_dbpedia_uri"). Regions
#'   of this s-attribute are expected to coincide with the regions of
#'   mentions. An empty string means that a mention is not linked.
#' - `ne_types` (optional): The types of named entities, separated by "|"
#'   (e.g. "PERSON|ORGANIZATION|LOCATION|MISC").
#'
#' A registry file declaring the named-entity annotation of the GermaParl2
#' corpus would include these lines:
#'
#' ```
#' ##:: ne_s_attribute = "ne"
#' ##:: ne_type_s_attribute = "ne_type"
#' ##:: ne_uri_s_attribute = "ne_dbpedia_uri"
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
    types <- s_attributes(
      corpus(corpus_id, registry_dir = registry_dir),
      s_attribute = y[["type"]], unique = TRUE
    )
    # an empty string means that the type is not available
    y[["types"]] <- sort(types[types != ""])
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



#' Named entities
#'
#' The mentions of named entities in a corpus or subcorpus, and the number of
#' mentions of entities. A mention is a region of the s-attribute declared for
#' named-entity annotation (see [ne_attributes()]), so a mention of several
#' tokens (e.g. "Helmut Schmidt") is one mention.
#'
#' The `named_entities` class inherits from the `subcorpus` class: Its regions
#' (slot `cpos`) are the mentions, slot `strucs` has the strucs of the
#' s-attribute with mentions. Methods for `subcorpus` objects can be used
#' (e.g. `size()` for the number of tokens of all mentions, or `kwic()`).
#' The type and the URI of mentions are not stored but derived from the
#' corpus when needed.
#'
#' The `named_entities()` constructor gets the mentions within a `corpus`
#' object or the regions of an object inheriting from the `regions` class
#' (such as a `subcorpus`). A mention is included if it is completely within
#' a region. As a `named_entities` object is a `subcorpus`, the constructor can
#' also be applied to it, e.g. to select mentions of a type.
#'
#' The `count()` method counts the mentions by surface form or by the URI
#' mentions are linked to. It returns a `count` object. Slot `p_attribute` of
#' this object has the name of the column with the key ("surface" or "uri"),
#' and slot `size` is the number of mentions. Thus, `features()` can be used to
#' compare the named entities in two `count` objects.
#'
#' The s-attribute with the type of mentions is expected to be an attribute of
#' the mention element (e.g. "ne_type" for the element "ne"), so that types
#' are found by the strucs of mentions. Regions of the URI s-attribute are
#' expected to coincide with mentions. Mentions without a URI region with the
#' same boundaries are not linked. An empty string means that a value is not
#' available: Mentions with an empty URI are not linked, and an empty type is
#' `NA`.
#'
#' @param .Object A `corpus` object, an object inheriting from the `regions`
#'   class (such as a `subcorpus`), or a length-one `character` vector with
#'   the ID of a corpus. For `count()`, a `named_entities` object.
#' @param type A `character` vector with the types of named entities to get
#'   (e.g. "PERSON"). If `NULL` (default), mentions of all types are included.
#' @param by Either "surface" (default), to count mentions by their surface
#'   form, or "uri", to count mentions by the URI they are linked to.
#' @param p_attribute The p-attribute surface forms are derived from (e.g.
#'   "word" or "lemma"). The tokens of a mention are separated by a space.
#' @param mention_attributes A `character` vector with further s-attributes of
#'   mentions (e.g. "ne_wikidata_id"), or `NULL` (default). These are
#'   attributes of the mention element: they have one region for each mention,
#'   with the same boundaries. For each key, the most frequent value is
#'   reported in a column named after the s-attribute. Counting by URI, values
#'   derived from the linked resource (such as its Wikidata ID or classes) are
#'   the same for all mentions of a key.
#' @param ... Further arguments (not used).
#' @return `named_entities()` returns a `named_entities` object, or `NULL`
#'   (with a warning) if there is no mention. An error is raised if the corpus
#'   does not declare named-entity annotation.
#'
#'   `count()` returns a `count` object. The table in slot `stat` is ordered
#'   by the number of mentions (column `count`). With `by = "surface"`, the
#'   columns are `surface`, `type` (the most frequent type of the surface
#'   form) and `count`. With `by = "uri"`, the columns are `uri`, `type`,
#'   `count`, `surface` (the most frequent surface form) and `n_surface` (the
#'   number of distinct surface forms). Mentions that are not linked are
#'   reported in a row with `uri` `NA`. Column `type` is `NA` if no type
#'   s-attribute is declared. Columns for `mention_attributes` follow.
#' @exportClass named_entities
#' @exportMethod named_entities
#' @rdname named_entities
#' @name named_entities
#' @aliases named_entities-class
#' @seealso [ne_attributes()]
#' @examples
#' \dontrun{
#' use("GermaParl2")
#'
#' # Requires that GERMAPARL2 declares named-entity annotation (see
#' # ?ne_attributes)
#' persons <- corpus("GERMAPARL2") %>%
#'   subset(p_type == "speech") %>%
#'   named_entities(type = "PERSON")
#'
#' count(persons, by = "uri")
#'
#' # Persons mentioned more often in speeches of the SPD than of the CDU
#' spd <- corpus("GERMAPARL2") %>%
#'   subset(p_type == "speech") %>%
#'   subset(speaker_party == "SPD") %>%
#'   named_entities(type = "PERSON") %>%
#'   count(by = "uri")
#' cdu <- corpus("GERMAPARL2") %>%
#'   subset(p_type == "speech") %>%
#'   subset(speaker_party == "CDU") %>%
#'   named_entities(type = "PERSON") %>%
#'   count(by = "uri")
#' features(spd, cdu, included = FALSE)
#' }
setClass("named_entities", contains = "subcorpus")


#' @rdname named_entities
setGeneric("named_entities", function(.Object, ...) standardGeneric("named_entities"))


#' @rdname named_entities
setMethod("named_entities", "corpus", function(.Object, type = NULL, ...){
  .named_entities(.Object, cpos = NULL, type = type)
})


#' @rdname named_entities
setMethod("named_entities", "regions", function(.Object, type = NULL, ...){
  .named_entities(.Object, cpos = slot(.Object, "cpos"), type = type)
})


#' @rdname named_entities
setMethod("named_entities", "character", function(.Object, type = NULL, ...){
  if (length(.Object) != 1L){
    stop("'.Object' is expected to be a length-one character vector")
  }
  named_entities(corpus(.Object), type = type)
})


# Get the mentions in a corpus, or within the regions of 'cpos' (a two-column
# matrix with the corpus positions of regions).
#' @importFrom RcppCWB cl_attribute_size get_region_matrix
.named_entities <- function(x, cpos = NULL, type = NULL){
  corpus_id <- slot(x, "corpus")
  registry_dir <- slot(x, "registry_dir")

  ne <- ne_attributes(x)
  if (is.null(ne)){
    stop(sprintf(
      "corpus '%s' does not declare named-entity annotation (see ?ne_attributes)",
      corpus_id
    ))
  }
  if (!is.null(type)){
    if (is.na(ne[["type"]])){
      stop("argument 'type' requires that the corpus declares a type s-attribute")
    }
    unknown <- setdiff(type, ne[["types"]])
    if (length(unknown) > 0L){
      stop(sprintf(
        "type(s) not available: %s - available types: %s",
        paste(unknown, collapse = ", "), paste(ne[["types"]], collapse = ", ")
      ))
    }
  }

  n <- cl_attribute_size(
    corpus = corpus_id, attribute = ne[["s_attribute"]],
    attribute_type = "s", registry = registry_dir
  )
  if (!is.na(ne[["type"]])){
    n_type <- cl_attribute_size(
      corpus = corpus_id, attribute = ne[["type"]],
      attribute_type = "s", registry = registry_dir
    )
    if (n_type != n){
      stop(sprintf(
        "s-attribute '%s' is expected to be an attribute of s-attribute '%s'",
        ne[["type"]], ne[["s_attribute"]]
      ))
    }
  }

  strucs <- 0L:(n - 1L)
  regions <- get_region_matrix(
    corpus = corpus_id, s_attribute = ne[["s_attribute"]],
    strucs = strucs, registry = registry_dir
  )

  # Keep mentions within regions. Regions do not overlap, so the region a
  # mention starts in is found by findInterval() on sorted left boundaries.
  if (!is.null(cpos)){
    cpos <- cpos[order(cpos[, 1]), , drop = FALSE]
    i <- findInterval(regions[, 1], cpos[, 1])
    keep <- i > 0L
    keep[keep] <- regions[keep, 2] <= cpos[i[keep], 2]
    strucs <- strucs[keep]
    regions <- regions[keep, , drop = FALSE]
  }

  if (!is.null(type) && length(strucs) > 0L){
    keep <- .ne_values(x, ne[["type"]], strucs) %in% type
    strucs <- strucs[keep]
    regions <- regions[keep, , drop = FALSE]
  }

  if (length(strucs) == 0L){
    warning("no named-entity mentions found (returning NULL)")
    return(NULL)
  }

  new(
    "named_entities",
    as(x, "corpus"),
    cpos = regions,
    strucs = strucs,
    s_attribute_strucs = ne[["s_attribute"]],
    s_attributes = if (is.null(type)) list() else setNames(list(type), ne[["type"]]),
    size = as.integer(sum(regions[, 2] - regions[, 1] + 1L))
  )
}


# Decode the values of an s-attribute for strucs, as UTF-8. An empty string
# means that a value is not available (cwb-encode writes an empty string if an
# XML attribute is missing), so it is turned into NA.
#' @importFrom RcppCWB cl_struc2str
.ne_values <- function(x, s_attribute, strucs){
  values <- cl_struc2str(
    corpus = slot(x, "corpus"), s_attribute = s_attribute,
    struc = strucs, registry = slot(x, "registry_dir")
  )
  values <- as.nativeEnc(values, from = slot(x, "encoding"))
  values[values == ""] <- NA_character_
  values
}


# Check that s-attributes are attributes of the mention element: they are
# s-attributes of the corpus with the same number of regions as mentions. An
# s-attribute with other regions would not have the strucs of mentions.
#' @importFrom RcppCWB cl_attribute_size
.ne_check_mention_attributes <- function(x, s_attrs, ne){
  if (is.null(s_attrs)) return(invisible(TRUE))
  stopifnot(is.character(s_attrs))
  missing_s_attrs <- s_attrs[!s_attrs %in% s_attributes(x)]
  if (length(missing_s_attrs) > 0L){
    stop(sprintf(
      "s-attribute(s) not available: %s", paste(missing_s_attrs, collapse = ", ")
    ))
  }
  n <- function(s_attr) cl_attribute_size(
    corpus = slot(x, "corpus"), attribute = s_attr, attribute_type = "s",
    registry = slot(x, "registry_dir")
  )
  n_mentions <- n(ne[["s_attribute"]])
  not_aligned <- s_attrs[vapply(s_attrs, n, integer(1L)) != n_mentions]
  if (length(not_aligned) > 0L){
    stop(sprintf(
      "s-attribute(s) without one region for each mention: %s",
      paste(not_aligned, collapse = ", ")
    ))
  }
  invisible(TRUE)
}


# The URI of each mention of a named_entities object, NA if a mention is not
# linked. Regions of the URI s-attribute coincide with mentions, but are a
# separate s-attribute: A mention is linked if it starts and ends a URI region
# with a value that is not an empty string. Both encodings of unlinked mentions
# occur: No URI region (sparse encoding), or a URI region with an empty value
# (an attribute of the mention element, as in GermaParl2).
#' @importFrom RcppCWB cl_cpos2struc get_region_matrix
.ne_uri <- function(x, s_attribute){
  cpos <- slot(x, "cpos")
  y <- rep(NA_character_, nrow(cpos))
  uri_strucs <- cl_cpos2struc(
    corpus = slot(x, "corpus"), s_attribute = s_attribute,
    cpos = cpos[, 1], registry = slot(x, "registry_dir")
  )
  linked <- which(uri_strucs >= 0L)
  if (length(linked) == 0L) return(y)
  uri_regions <- get_region_matrix(
    corpus = slot(x, "corpus"), s_attribute = s_attribute,
    strucs = uri_strucs[linked], registry = slot(x, "registry_dir")
  )
  same <- uri_regions[, 1] == cpos[linked, 1] & uri_regions[, 2] == cpos[linked, 2]
  linked <- linked[same]
  if (length(linked) > 0L){
    y[linked] <- .ne_values(x, s_attribute, uri_strucs[linked])
  }
  y
}


# The surface form of each mention of a named_entities object: The tokens of
# a mention, separated by a space. Tokens are added position by position
# (mentions are short), which is vectorized.
#' @importFrom RcppCWB cl_cpos2str
.ne_surface <- function(x, p_attribute){
  cpos <- slot(x, "cpos")
  n_tokens <- cpos[, 2] - cpos[, 1] + 1L
  get_tokens <- function(positions) as.nativeEnc(
    cl_cpos2str(
      corpus = slot(x, "corpus"), p_attribute = p_attribute,
      cpos = positions, registry = slot(x, "registry_dir")
    ),
    from = slot(x, "encoding")
  )
  y <- get_tokens(cpos[, 1])
  for (k in seq_len(max(n_tokens) - 1L)){
    i <- which(n_tokens > k)
    y[i] <- paste(y[i], get_tokens(cpos[i, 1] + k))
  }
  y
}


#' @rdname named_entities
#' @importFrom data.table data.table setorderv
setMethod("count", "named_entities", function(.Object, by = c("surface", "uri"), p_attribute = "word", mention_attributes = NULL, ...){
  N <- NULL # column of data.table objects, defined to avoid R CMD check notes
  by <- match.arg(by)
  ne <- ne_attributes(.Object)
  if (by == "uri" && is.na(ne[["uri"]])){
    stop("by = 'uri' requires that the corpus declares a URI s-attribute")
  }
  if (!p_attribute %in% p_attributes(.Object)){
    stop(sprintf("p-attribute '%s' not available", p_attribute))
  }
  .ne_check_mention_attributes(.Object, mention_attributes, ne)

  dt <- data.table(
    surface = .ne_surface(.Object, p_attribute = p_attribute),
    type = if (is.na(ne[["type"]])){
      NA_character_
    } else {
      .ne_values(.Object, ne[["type"]], slot(.Object, "strucs"))
    }
  )
  if (by == "uri") dt[["uri"]] <- .ne_uri(.Object, ne[["uri"]])

  # The most frequent type (and surface form) of a key is the first after
  # ordering by key and count.
  types <- dt[, .N, by = c(by, "type")]
  setorderv(types, cols = c(by, "N"), order = c(1L, -1L))
  y <- types[, list(type = type[1], count = sum(N)), by = by]

  if (by == "uri"){
    surfaces <- dt[, .N, by = c("uri", "surface")]
    setorderv(surfaces, cols = c("uri", "N"), order = c(1L, -1L))
    surfaces <- surfaces[, list(surface = surface[1], n_surface = .N), by = "uri"]
    y <- y[surfaces, on = "uri"]
  }

  # The most frequent value of further attributes of mentions for a key.
  # Values of these attributes have the strucs of mentions.
  for (s_attr in mention_attributes){
    dt[[s_attr]] <- .ne_values(.Object, s_attr, slot(.Object, "strucs"))
    values <- dt[, .N, by = c(by, s_attr)]
    setorderv(values, cols = c(by, "N"), order = c(1L, -1L))
    values <- values[!duplicated(values[[by]])]
    y[[s_attr]] <- values[[s_attr]][match(y[[by]], values[[by]])]
  }
  setorderv(y, cols = c("count", by), order = c(-1L, 1L), na.last = TRUE)

  new(
    "count",
    as(.Object, "corpus"),
    p_attribute = by,
    stat = y,
    name = character(),
    size = nrow(slot(.Object, "cpos"))
  )
})
