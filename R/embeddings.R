#' @include S4classes.R
NULL


#' Embeddings of corpus regions
#'
#' An `embeddings` object describes vector representations (embeddings) of the
#' regions of a structural attribute (such as paragraphs `p`) of a corpus, and
#' how to access them. It is a handle and does not necessarily hold the
#' vectors: For a large corpus, they are stored in Parquet files or a search
#' index (backends "parquet" and "faiss", not yet implemented), described by a
#' file `_embeddings.json`. For small corpora and tests, the vectors can be
#' kept in memory as a `matrix` (backend "matrix").
#'
#' The class inherits from the `corpus` class. Embeddings refer to regions of
#' one specific build of a corpus: Region numbers (strucs) change when a corpus
#' is rebuilt with a different segmentation. The validity method therefore
#' checks that the corpus version recorded in `_embeddings.json` matches the
#' version of the corpus.
#'
#' Search indexes are not stored in a slot, but kept in a cache within the
#' package (not yet implemented), so that `embeddings` objects can be
#' serialized.
#'
#' @slot s_attribute A length-one `character` vector, the s-attribute whose
#'   regions are embedded (e.g. "p").
#' @slot model A length-one `character` vector, the model (e.g. a Hugging Face
#'   model ID).
#' @slot model_revision A length-one `character` vector, the revision of the
#'   model. Model and revision define the vector space.
#' @slot dim A length-one `integer` vector, the number of dimensions.
#' @slot normalized A length-one `logical` vector, whether vectors are
#'   L2-normalized. If `TRUE`, the dot product is the cosine similarity.
#' @slot metric A length-one `character` vector, "cosine" or "dot".
#' @slot backend A length-one `character` vector, "matrix", "parquet" or
#'   "faiss".
#' @slot path The directory with the file `_embeddings.json`, or
#'   `NA` (backend "matrix").
#' @slot spec A `list`, the parsed content of `_embeddings.json`.
#' @slot vectors A `matrix` with one row per region (backend "matrix"
#'   only).
#' @slot vector_strucs An `integer` vector, the struc numbers of the rows of
#'   slot `vectors`.
#' @param .Object A `corpus` object, or a length-one `character` vector with
#'   the ID of a corpus.
#' @param path The directory with the file `_embeddings.json`. If `NULL`, the
#'   directory is taken from the corpus property "embeddings" (path relative to
#'   the data directory of the corpus).
#' @param vectors A `matrix` with vectors (one row per region) to create an
#'   `embeddings` object with the backend "matrix".
#' @param strucs An `integer` vector, the struc numbers of the rows of
#'   `vectors`. Defaults to all regions of `s_attribute` in corpus order.
#' @param s_attribute The s-attribute whose regions are embedded (backend
#'   "matrix").
#' @param model,model_revision Model and model revision (backend "matrix").
#' @param normalize A `logical` value, whether to L2-normalize `vectors`.
#' @param x,object An `embeddings` object.
#' @param ... Further arguments.
#' @exportClass embeddings
#' @rdname embeddings
#' @name embeddings
#' @aliases embeddings-class
#' @seealso [semantic_search()]
#' @family embeddings
#' @examples
#' use("polmineR")
#'
#' # Random vectors for the regions of s-attribute 'speaker' stand in for
#' # embeddings computed with a language model.
#' n <- s_attributes("GERMAPARLMINI", "speaker", unique = FALSE) |> length()
#' m <- matrix(rnorm(n * 16), nrow = n)
#' emb <- embeddings("GERMAPARLMINI", vectors = m, s_attribute = "speaker")
#' emb
setClass(
  "embeddings",
  slots = c(
    s_attribute = "character",
    model = "character",
    model_revision = "character",
    dim = "integer",
    normalized = "logical",
    metric = "character",
    backend = "character",
    path = "character",
    spec = "list",
    vectors = "matrix",
    vector_strucs = "integer"
  ),
  contains = "corpus",
  validity = function(object){
    msg <- character()

    if (length(slot(object, "s_attribute")) != 1L)
      msg <- c(msg, "slot 's_attribute' needs to be a length-one character vector")
    else if (!slot(object, "s_attribute") %in% corpus_s_attributes(corpus = slot(object, "corpus"), registry = slot(object, "registry_dir")))
      msg <- c(msg, sprintf("s-attribute '%s' not available", slot(object, "s_attribute")))

    if (!slot(object, "backend") %in% c("matrix", "parquet", "faiss"))
      msg <- c(msg, "slot 'backend' needs to be 'matrix', 'parquet' or 'faiss'")

    if (slot(object, "backend") == "matrix"){
      v <- slot(object, "vectors")
      if (ncol(v) != slot(object, "dim"))
        msg <- c(msg, "number of columns of slot 'vectors' differs from slot 'dim'")
      if (nrow(v) != length(slot(object, "vector_strucs")))
        msg <- c(msg, "number of rows of slot 'vectors' differs from length of slot 'vector_strucs'")
    }

    # Embeddings refer to one build of a corpus.
    spec_version <- slot(object, "spec")[["corpus_version"]]
    if (!is.null(spec_version)){
      version <- registry_property(object, "version")
      if (!is.null(version) && !identical(spec_version, version)){
        msg <- c(
          msg,
          sprintf(
            "embeddings computed for corpus version '%s', corpus version is '%s'",
            spec_version, version
          )
        )
      }
    }

    if (length(msg) == 0L) TRUE else msg
  }
)


#' @rdname embeddings
#' @exportMethod embeddings
setGeneric("embeddings", function(.Object, ...) standardGeneric("embeddings"))


#' @rdname embeddings
setMethod("embeddings", "corpus", function(
    .Object, path = NULL, vectors = NULL, strucs = NULL, s_attribute = NULL,
    model = NA_character_, model_revision = NA_character_, normalize = TRUE,
    ...
  ){
  # Embeddings refer to regions of the corpus, not of a subcorpus.
  x <- as(.Object, "corpus")
  if (!is.null(vectors)) {
    return(
      .embeddings_matrix(
        x, vectors = vectors, strucs = strucs, s_attribute = s_attribute,
        model = model, model_revision = model_revision, normalize = normalize
      )
    )
  }
  .embeddings_spec(x, path = path)
})


#' @rdname embeddings
setMethod("embeddings", "character", function(.Object, ...){
  embeddings(corpus(.Object), ...)
})


#' @rdname embeddings
setMethod("show", "embeddings", function(object){
  cli_text("{.cls embeddings} of corpus {.val {slot(object, 'corpus')}}")
  version <- registry_property(object, "version")
  cli_bullets(
    c(
      "*" = "Corpus version: {if (is.null(version)) 'not available' else version}",
      "*" = "Regions of s-attribute: {.val {slot(object, 's_attribute')}}",
      "*" = "Model: {slot(object, 'model')} (revision {slot(object, 'model_revision')})",
      "*" = "Dimensions: {slot(object, 'dim')}",
      "*" = "Backend: {slot(object, 'backend')}",
      "*" = "Number of regions: {length(object)}"
    )
  )
  invisible(object)
})


#' @rdname embeddings
setMethod("length", "embeddings", function(x){
  if (slot(x, "backend") == "matrix")
    return(nrow(slot(x, "vectors")))
  n <- slot(x, "spec")[["n"]]
  if (is.null(n)) NA_integer_ else as.integer(n)
})


#' @importFrom RcppCWB cl_attribute_size corpus_s_attributes
#' @importFrom cli cli_text cli_bullets
#' @noRd
.embeddings_matrix <- function(x, vectors, strucs, s_attribute, model,
                               model_revision, normalize){
  if (is.null(s_attribute)) stop("argument 's_attribute' is required")
  if (!is.matrix(vectors) || !is.numeric(vectors))
    stop("argument 'vectors' needs to be a numeric matrix")
  if (is.null(strucs)){
    n <- cl_attribute_size(
      corpus = slot(x, "corpus"), attribute = s_attribute,
      attribute_type = "s", registry = slot(x, "registry_dir")
    )
    if (nrow(vectors) != n)
      stop(
        sprintf(
          "'vectors' has %d rows, s-attribute '%s' has %d regions (provide 'strucs')",
          nrow(vectors), s_attribute, n
        )
      )
    strucs <- 0L:(n - 1L)
  }
  storage.mode(vectors) <- "double"
  if (normalize) vectors <- vectors / sqrt(rowSums(vectors^2))
  dimnames(vectors) <- NULL

  y <- new(
    "embeddings",
    x,
    s_attribute = s_attribute,
    model = as.character(model),
    model_revision = as.character(model_revision),
    dim = ncol(vectors),
    normalized = normalize,
    metric = "cosine",
    backend = "matrix",
    path = NA_character_,
    spec = list(),
    vectors = vectors,
    vector_strucs = as.integer(strucs)
  )
  y
}


#' @importFrom jsonlite read_json
#' @noRd
.embeddings_spec <- function(x, path){
  if (is.null(path)){
    property <- registry_property(x, "embeddings")
    if (is.null(property)){
      stop(
        sprintf(
          "corpus '%s' does not declare embeddings (corpus property 'embeddings')",
          slot(x, "corpus")
        )
      )
    }
    path <- file.path(slot(x, "data_dir"), property)
  }
  json <- file.path(path, "_embeddings.json")
  if (!file.exists(json)) stop(sprintf("file not found: %s", json))
  spec <- read_json(json)

  if (!identical(spec[["schema"]], "polmineR-embeddings"))
    stop("_embeddings.json: unknown schema")
  if (!identical(toupper(spec[["corpus"]]), slot(x, "corpus")))
    stop(
      sprintf(
        "_embeddings.json refers to corpus '%s', not to '%s'",
        spec[["corpus"]], slot(x, "corpus")
      )
    )

  backend <- if (!is.null(spec[["index"]])) "faiss" else "parquet"

  new(
    "embeddings",
    x,
    s_attribute = spec[["s_attribute"]],
    model = spec[["model"]],
    model_revision = if (is.null(spec[["model_revision"]])) NA_character_ else spec[["model_revision"]],
    dim = as.integer(spec[["dim"]]),
    normalized = isTRUE(spec[["normalized"]]),
    metric = if (is.null(spec[["metric"]])) "cosine" else spec[["metric"]],
    backend = backend,
    path = path,
    spec = spec,
    vectors = matrix(numeric(), ncol = as.integer(spec[["dim"]])),
    vector_strucs = integer()
  )
}


#' Results of a semantic search
#'
#' A `semantic_hits` object is a subcorpus with the regions (e.g. paragraphs)
#' found by [semantic_search()], and the similarity of each region with the
#' query. Since the class inherits from the `subcorpus` class, methods for
#' subcorpora (such as `count()`, `kwic()`, `split()` or `read()`) can be
#' applied to the hits.
#'
#' Regions are stored in corpus order, as expected by methods for subcorpora.
#' The rank of a hit is derived from slot `similarity`. The `subset()`-method
#' keeps slot `similarity` aligned with the regions that remain. If subsetting
#' yields regions that are not regions of the hits (e.g. sentences within
#' paragraphs), similarity values are meaningless and a `subcorpus` is
#' returned.
#'
#' @slot similarity A `numeric` vector, the similarity of the regions with the
#'   query, aligned with slot `strucs`.
#' @slot query The query: A `character` vector, a `numeric` vector, or a
#'   `subcorpus` ("more like this").
#' @slot model A `character` vector, model and revision of the embeddings.
#' @slot k A length-one `integer` vector, the number of hits requested.
#' @slot exact A length-one `logical` vector, whether similarity values are
#'   exact (`FALSE` for approximations from a compressed index).
#' @param x,object A `semantic_hits` object.
#' @param subset An expression to subset the hits, see [subset()].
#' @param ... Further arguments passed into the `subset()`-method for
#'   `subcorpus` objects.
#' @exportClass semantic_hits
#' @rdname semantic_hits
#' @name semantic_hits
#' @aliases semantic_hits-class
#' @family embeddings
setClass(
  "semantic_hits",
  slots = c(
    similarity = "numeric",
    query = "ANY",
    model = "character",
    k = "integer",
    exact = "logical"
  ),
  contains = "subcorpus",
  validity = function(object){
    n <- length(slot(object, "strucs"))
    if (length(slot(object, "similarity")) != n || nrow(slot(object, "cpos")) != n)
      return("slots 'similarity', 'strucs' and 'cpos' need to have the same length")
    TRUE
  }
)


#' @rdname semantic_hits
#' @param keep.rownames Required for consistency with the S3 generic of the
#'   data.table package. Unused.
#' @param sort A `logical` value, whether to order hits by similarity.
#' @importFrom data.table as.data.table data.table
#' @export
#' @method as.data.table semantic_hits
as.data.table.semantic_hits <- function(x, keep.rownames, sort = TRUE, ...){
  dt <- data.table(
    struc = slot(x, "strucs"),
    cpos_left = slot(x, "cpos")[, 1],
    cpos_right = slot(x, "cpos")[, 2],
    similarity = slot(x, "similarity")
  )
  if (sort) dt <- dt[order(-dt[["similarity"]])]
  dt[, "rank" := seq_len(nrow(dt))]
  dt
}


#' @rdname semantic_hits
setMethod("show", "semantic_hits", function(object){
  q <- slot(object, "query")
  query <- if (is.character(q)) sprintf('"%s"', q[1]) else
    if (is(q, "subcorpus")) "subcorpus (more like this)" else "vector"
  sim <- slot(object, "similarity")
  cli_text("{.cls semantic_hits} in corpus {.val {slot(object, 'corpus')}}")
  cli_bullets(
    c(
      "*" = "Query: {query}",
      "*" = "Model: {slot(object, 'model')}",
      "*" = "Hits: {length(sim)} region{?s} of s-attribute {.val {slot(object, 's_attribute_strucs')}}",
      "*" = "Similarity: {if (length(sim)) sprintf('%.3f to %.3f', min(sim), max(sim)) else '-'}{if (!slot(object, 'exact')) ' (approximate)' else ''}"
    )
  )
  invisible(object)
})


#' @rdname semantic_hits
setMethod("subset", "semantic_hits", function(x, subset, ...){
  s_attr <- slot(x, "s_attribute_strucs")
  # Within this method, the argument 'subset' masks the generic, so the call
  # is forwarded via a helper.
  y <- .subset_subcorpus(as(x, "subcorpus"), {{ subset }}, ...)
  if (is.null(y)) return(NULL)

  # The subset()-method for subcorpora may report the s-attribute used for
  # subsetting in slot 's_attribute_strucs', even if regions are unchanged.
  # So hits are matched by their regions: if every region of the result is a
  # region of the hits, similarity values remain valid.
  idx <- match(
    paste(slot(y, "cpos")[, 1], slot(y, "cpos")[, 2]),
    paste(slot(x, "cpos")[, 1], slot(x, "cpos")[, 2])
  )
  if (anyNA(idx)){
    cli_alert_info(
      "regions do not correspond to regions of s-attribute {.val {s_attr}} any more, returning subcorpus without similarity values"
    )
    return(y)
  }
  new(
    "semantic_hits",
    y,
    strucs = slot(x, "strucs")[idx],
    s_attribute_strucs = s_attr,
    similarity = slot(x, "similarity")[idx],
    query = slot(x, "query"),
    model = slot(x, "model"),
    k = slot(x, "k"),
    exact = slot(x, "exact")
  )
})


#' Call the subset() generic for a subcorpus, forwarding the expression.
#' @noRd
.subset_subcorpus <- function(x, ...) subset(x, ...)


#' Semantic search
#'
#' Find the regions of a corpus (e.g. paragraphs) that are most similar to a
#' query, using embeddings. Applied to a `subcorpus`, the search is restricted
#' to the regions fully contained in the subcorpus. Applied to a
#' `subcorpus_bundle`, `k` hits are returned for each subcorpus.
#'
#' The query can be:
#'
#' - a `character` vector, which is encoded using the function given by
#'   option `polmineR.encoder` (see [encoder_sentence_transformers()]). The
#'   encoder needs to use the model the embeddings were computed with.
#' - a `numeric` vector with the same number of dimensions as the embeddings
#'   (a query that has been encoded already).
#' - a `subcorpus` ("more like this"): The query is the mean of the vectors of
#'   the regions of the subcorpus. No model is required. These regions are
#'   excluded from the result if `exclude_query` is `TRUE`.
#'
#' @param .Object A `corpus`, `subcorpus` or `subcorpus_bundle` object, or a
#'   length-one `character` vector with a corpus ID.
#' @param query The query, see details.
#' @param k A length-one `integer` vector, the number of hits.
#' @param embeddings An `embeddings` object. Defaults to the embeddings
#'   declared for the corpus (see [embeddings()]).
#' @param exclude_query A `logical` value, whether to exclude the regions of a
#'   `subcorpus` used as query from the result.
#' @param ... Further arguments.
#' @return A `semantic_hits` object, or a `subcorpus_bundle` of
#'   `semantic_hits` objects for a `subcorpus_bundle`.
#' @exportMethod semantic_search
#' @rdname semantic_search
#' @name semantic_search
#' @family embeddings
#' @examples
#' use("polmineR")
#' n <- s_attributes("GERMAPARLMINI", "speaker", unique = FALSE) |> length()
#' m <- matrix(rnorm(n * 16), nrow = n)
#' emb <- embeddings("GERMAPARLMINI", vectors = m, s_attribute = "speaker")
#'
#' # More like this: utterances similar to the first utterance of a speaker
#' q <- corpus("GERMAPARLMINI") |> subset(speaker == "Volker Kauder")
#' hits <- semantic_search("GERMAPARLMINI", query = q, embeddings = emb, k = 5L)
#' hits
#' as.data.table(hits)
#'
#' # Restrict search to a subcorpus, and to each subcorpus of a bundle
#' corpus("GERMAPARLMINI") |>
#'   subset(party == "SPD") |>
#'   semantic_search(query = q, embeddings = emb, k = 3L)
#'
#' corpus("GERMAPARLMINI") |>
#'   split(s_attribute = "party") |>
#'   semantic_search(query = q, embeddings = emb, k = 3L)
setGeneric("semantic_search", function(.Object, ...) standardGeneric("semantic_search"))


#' @rdname semantic_search
setMethod("semantic_search", "corpus", function(.Object, query, k = 10L, embeddings = NULL, exclude_query = TRUE, ...){
  if (is.null(embeddings)) embeddings <- embeddings(.Object)
  .semantic_search(
    .Object, query = .query_vector(query, embeddings), strucs = NULL,
    k = k, embeddings = embeddings, exclude_query = exclude_query
  )
})


#' @rdname semantic_search
setMethod("semantic_search", "subcorpus", function(.Object, query, k = 10L, embeddings = NULL, exclude_query = TRUE, ...){
  if (is.null(embeddings)) embeddings <- embeddings(.Object)
  .semantic_search(
    .Object, query = .query_vector(query, embeddings),
    strucs = .contained_strucs(.Object, slot(embeddings, "s_attribute")),
    k = k, embeddings = embeddings, exclude_query = exclude_query
  )
})


#' @rdname semantic_search
setMethod("semantic_search", "subcorpus_bundle", function(.Object, query, k = 10L, embeddings = NULL, exclude_query = TRUE, ...){
  if (is.null(embeddings)) embeddings <- embeddings(as(.Object, "corpus"))
  # Encode the query once for all subcorpora.
  q <- .query_vector(query, embeddings)
  y <- as(as(.Object, "corpus"), "subcorpus_bundle")
  slot(y, "objects") <- lapply(
    slot(.Object, "objects"),
    function(sc){
      .semantic_search(
        sc, query = q,
        strucs = .contained_strucs(sc, slot(embeddings, "s_attribute")),
        k = k, embeddings = embeddings, exclude_query = exclude_query
      )
    }
  )
  names(slot(y, "objects")) <- names(.Object)
  y
})


#' @rdname semantic_search
setMethod("semantic_search", "character", function(.Object, ...){
  semantic_search(corpus(.Object), ...)
})


#' Turn a query into a vector. The original query is kept as attribute
#' "query", the regions of a subcorpus used as query as attribute "strucs".
#' @noRd
.query_vector <- function(query, embeddings){
  if (is(query, "subcorpus")){
    strucs <- .contained_strucs(query, slot(embeddings, "s_attribute"))
    if (length(strucs) == 0L)
      stop(
        sprintf(
          "query subcorpus does not contain a region of s-attribute '%s'",
          slot(embeddings, "s_attribute")
        )
      )
    v <- colMeans(.get_vectors(embeddings, strucs), na.rm = TRUE)
    attr(v, "strucs") <- strucs
  } else if (is.character(query)){
    encoder <- getOption("polmineR.encoder")
    if (is.null(encoder)){
      stop(
        "To encode a query, set option 'polmineR.encoder' to a function ",
        "(see ?encoder_sentence_transformers), or provide the query as a ",
        "numeric vector or a subcorpus."
      )
    }
    v <- encoder(
      query,
      model = slot(embeddings, "model"),
      revision = slot(embeddings, "model_revision")
    )
    v <- as.numeric(if (is.matrix(v)) v[1L, ] else v)
  } else if (is.numeric(query)){
    v <- as.numeric(query)
  } else {
    stop("query needs to be a character vector, a numeric vector or a subcorpus")
  }

  if (length(v) != slot(embeddings, "dim"))
    stop(sprintf("query has %d dimensions, embeddings have %d", length(v), slot(embeddings, "dim")))
  strucs <- attr(v, "strucs")
  if (slot(embeddings, "normalized")) v <- v / sqrt(sum(v^2))
  attr(v, "strucs") <- strucs
  attr(v, "query") <- query
  v
}


#' Get the strucs of the regions of an s-attribute fully contained in the
#' regions of a subcorpus.
#' @importFrom RcppCWB cl_attribute_size get_region_matrix
#' @importFrom data.table foverlaps setkeyv
#' @noRd
.contained_strucs <- function(x, s_attribute){
  if (identical(slot(x, "s_attribute_strucs"), s_attribute))
    return(slot(x, "strucs"))

  n <- cl_attribute_size(
    corpus = slot(x, "corpus"), attribute = s_attribute,
    attribute_type = "s", registry = slot(x, "registry_dir")
  )
  regions <- get_region_matrix(
    corpus = slot(x, "corpus"), s_attribute = s_attribute,
    strucs = 0L:(n - 1L), registry = slot(x, "registry_dir")
  )
  units <- data.table(struc = 0L:(n - 1L), start = regions[, 1], end = regions[, 2])
  sc <- data.table(start = slot(x, "cpos")[, 1], end = slot(x, "cpos")[, 2])
  setkeyv(sc, c("start", "end"))
  matched <- foverlaps(units, sc, type = "within", nomatch = NULL)
  sort(unique(matched[["struc"]]))
}


#' Get vectors for strucs (rows in the order of strucs, NA if not available).
#' @noRd
.get_vectors <- function(embeddings, strucs){
  switch(
    slot(embeddings, "backend"),
    matrix = slot(embeddings, "vectors")[
      match(strucs, slot(embeddings, "vector_strucs")), , drop = FALSE
    ],
    stop(sprintf("backend '%s' not yet implemented", slot(embeddings, "backend")))
  )
}


#' Search the k regions most similar to vector q. Returns a data.table with
#' columns struc and similarity.
#' @noRd
.search_backend <- function(embeddings, q, strucs, k){
  switch(
    slot(embeddings, "backend"),
    matrix = {
      rows <- if (is.null(strucs)){
        seq_len(nrow(slot(embeddings, "vectors")))
      } else {
        r <- match(strucs, slot(embeddings, "vector_strucs"))
        r[!is.na(r)]
      }
      sim <- as.vector(slot(embeddings, "vectors")[rows, , drop = FALSE] %*% q)
      top <- order(-sim)[seq_len(min(k, length(sim)))]
      data.table(
        struc = slot(embeddings, "vector_strucs")[rows][top],
        similarity = sim[top]
      )
    },
    # "parquet": exhaustive search over the vectors, partition by partition
    # "faiss": IVF-PQ index with ID filter for large subsets, exhaustive
    #   scoring for small subsets, exact rescoring of candidates
    stop(sprintf("backend '%s' not yet implemented", slot(embeddings, "backend")))
  )
}


#' @importFrom RcppCWB get_region_matrix
#' @noRd
.semantic_search <- function(x, query, strucs, k, embeddings, exclude_query){
  k <- as.integer(k)
  excl <- if (exclude_query) attr(query, "strucs") else NULL
  if (!is.null(excl)){
    if (is.null(strucs)) strucs <- slot(embeddings, "vector_strucs")
    strucs <- setdiff(strucs, excl)
  }

  hits <- .search_backend(embeddings, q = as.numeric(query), strucs = strucs, k = k)
  # Corpus order, as expected by methods for subcorpora
  hits <- hits[order(hits[["struc"]])]

  s_attr <- slot(embeddings, "s_attribute")
  cpos <- if (nrow(hits) > 0L){
    get_region_matrix(
      corpus = slot(x, "corpus"), s_attribute = s_attr,
      strucs = hits[["struc"]], registry = slot(x, "registry_dir")
    )
  } else {
    matrix(integer(), ncol = 2L)
  }
  dimnames(cpos) <- NULL

  sc <- as(as(x, "corpus"), "subcorpus")
  y <- new(
    "semantic_hits",
    sc,
    cpos = cpos,
    strucs = hits[["struc"]],
    s_attribute_strucs = s_attr,
    s_attributes = if (is(x, "subcorpus")) slot(x, "s_attributes") else list(),
    xml = slot(x, "xml"),
    similarity = hits[["similarity"]],
    query = attr(query, "query"),
    model = paste(slot(embeddings, "model"), slot(embeddings, "model_revision"), sep = "@"),
    k = k,
    exact = TRUE
  )
  slot(y, "size") <- if (nrow(cpos) > 0L) as.integer(sum(cpos[, 2] - cpos[, 1] + 1L)) else 0L
  y
}


#' Encode queries with sentence-transformers
#'
#' Create a function that encodes queries using the Python package
#' sentence-transformers via reticulate. Assign it to option
#' `polmineR.encoder` to use character queries with [semantic_search()]. The
#' model is loaded on first use and kept for later calls.
#'
#' @param model A Hugging Face model ID or local path. If `NULL`, the model
#'   recorded in the embeddings is used.
#' @param device The device ("cpu", "mps", "cuda"), or `NULL` to let
#'   sentence-transformers decide.
#' @return A function with arguments `x` (a `character` vector), `model` and
#'   `revision`, returning a matrix with one row per element of `x`.
#' @export encoder_sentence_transformers
#' @family embeddings
#' @examples
#' \dontrun{
#' options(polmineR.encoder = encoder_sentence_transformers())
#' semantic_search("GERMAPARL2", query = "Wir brauchen mehr Klimaschutz.")
#' }
encoder_sentence_transformers <- function(model = NULL, device = NULL){
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("package 'reticulate' required")
  model_override <- model
  cache <- new.env()
  function(x, model = NULL, revision = NULL, ...){
    m <- if (is.null(model_override)) model else model_override
    if (is.null(m) || is.na(m)) stop("model unknown")
    if (is.null(cache[[m]])){
      st <- reticulate::import("sentence_transformers")
      args <- list(m)
      if (!is.null(device)) args$device <- device
      if (!is.null(revision) && !is.na(revision) && is.null(model_override))
        args$revision <- revision
      cache[[m]] <- do.call(st$SentenceTransformer, args)
    }
    cache[[m]]$encode(as.list(x), normalize_embeddings = TRUE)
  }
}
