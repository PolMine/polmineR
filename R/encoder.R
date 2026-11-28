#' @include S4classes.R embeddings.R
NULL


#' Encoders for embeddings
#'
#' An `encoder` turns texts into vectors in the vector space of an
#' `embeddings` object, e.g. to encode a query for [semantic_search()]. The
#' encoder takes the model, its revision and the prefixes some models expect
#' (e.g. "query: ") from the embeddings, so that texts are encoded with
#' exactly the model the embeddings were computed with.
#'
#' An `encoder` object is a function: `enc(x)` encodes the `character` vector
#' `x` and returns a `matrix` with one row per element of `x`. Use `type =
#' "document"` to encode texts that are compared with queries (e.g. regions of
#' the corpus) if the model distinguishes queries and documents.
#'
#' So far, models of the Python package sentence-transformers are supported
#' (class `sentence_encoder`), used via reticulate. The model is loaded on
#' first use. Loaded models are kept for the R session and shared by encoders
#' using the same model.
#'
#' @section Where the model is loaded from:
#' 1. A local directory given by argument `path`.
#' 2. A directory named after the model (e.g. "gbert-large-paraphrase-cosine")
#'    in the directory given by option `polmineR.model_dir`.
#' 3. The cache of the Hugging Face hub, at the revision of the embeddings. The
#'    model is not downloaded unless `download` is `TRUE` (the model may have
#'    a size of several gigabytes).
#'
#' The revision of a local directory written by `hf download --local-dir` is
#' read from the metadata the command stores in `.cache/huggingface/download`
#' and checked against the revision of the embeddings. A mismatch is an
#' error. If the revision cannot be determined, a warning is issued.
#'
#' @section OpenMP on macOS:
#' On macOS, polmineR and PyTorch use different copies of the OpenMP runtime
#' (data.table links the copy distributed with R, PyTorch ships its own). When
#' both are loaded into the R process, the OpenMP runtime aborts with "OMP:
#' Error #15: Initializing libomp.dylib, but found libomp.dylib already
#' initialized". With `allow_duplicate_openmp = TRUE` (default on macOS), the
#' environment variable `KMP_DUPLICATE_LIB_OK` is set to "TRUE" before PyTorch
#' is loaded, which lets both copies coexist. The OpenMP documentation
#' describes this as an unsupported workaround that may cause crashes or
#' incorrect results. In a test, vectors computed this way were identical to
#' vectors computed in a separate Python process (up to float16 precision). To
#' avoid the workaround, compute query vectors outside the R process and pass
#' them as `numeric` vectors to [semantic_search()]. Note that
#' `reticulate::py_module_available("sentence_transformers")` imports the
#' module, and PyTorch with it.
#'
#' reticulate up to version 1.47.0 fails to restore the logging handlers of
#' torch >= 2.14 when it captures Python output, e.g. in knitr documents (fixed
#' in rstudio/reticulate#1953). In an ordinary R session, this does not
#' matter.
#'
#' @slot library A length-one `character` vector, the library of the model
#'   (e.g. "sentence-transformers").
#' @slot model A length-one `character` vector, the model ID.
#' @slot revision A length-one `character` vector, the revision of the model.
#' @slot revision_check A length-one `character` vector, the result of
#'   checking the revision of a local model: "verified", "unknown" or "not
#'   checked" (model from the Hugging Face hub, which is loaded at the
#'   revision).
#' @slot query_prefix,document_prefix Length-one `character` vectors, prefixes
#'   prepended to queries and documents, or `NA`.
#' @slot path A length-one `character` vector, the local directory of the
#'   model, or `NA` (Hugging Face hub).
#' @slot device A length-one `character` vector, the device ("cpu", "mps",
#'   "cuda"), or `NA` to let the library decide.
#' @slot fp16 A length-one `logical` vector, whether the model runs in float16.
#' @slot batch_size A length-one `integer` vector, the batch size.
#' @slot dimensions A length-one `integer` vector, the number of dimensions of the
#'   embeddings.
#' @slot corpus,corpus_version,s_attribute Length-one `character` vectors,
#'   corpus, corpus version and s-attribute of the embeddings the encoder
#'   belongs to.
#' @slot download A length-one `logical` vector, whether the model may be
#'   downloaded from the Hugging Face hub.
#' @slot allow_duplicate_openmp A length-one `logical` vector, see section
#'   on OpenMP.
#' @param .Object An `embeddings` object, a `corpus` object or an object
#'   inheriting from it (the embeddings declared for the corpus are used), or
#'   a length-one `character` vector with a corpus ID.
#' @param path A local directory with the model, or `NULL`.
#' @param device The device ("cpu", "mps", "cuda"), or `NULL` to let the
#'   library decide.
#' @param fp16 A `logical` value, whether to run the model in float16. Vectors
#'   differ slightly between float16 and float32 (cosine similarity about
#'   0.999).
#' @param batch_size An `integer` value, the batch size for encoding.
#' @param download A `logical` value, whether the model may be downloaded from
#'   the Hugging Face hub if it is not available locally.
#' @param allow_duplicate_openmp A `logical` value, whether to set the
#'   environment variable `KMP_DUPLICATE_LIB_OK` before loading PyTorch (see
#'   section on OpenMP). Defaults to `TRUE` on macOS.
#' @param object An `encoder` object.
#' @param ... Further arguments passed into the method for `embeddings`
#'   objects.
#' @return An object of class `sentence_encoder`, which inherits from class
#'   `encoder`.
#' @exportClass encoder
#' @exportClass sentence_encoder
#' @exportMethod encoder
#' @rdname encoder
#' @name encoder
#' @aliases encoder-class sentence_encoder-class
#' @family embeddings
#' @examples
#' \dontrun{
#' options(polmineR.model_dir = "~/Data/embeddings")
#' enc <- encoder("GERMAPARL2")
#' enc
#' v <- enc("Wir brauchen mehr Klimaschutz.")
#' dim(v)
#'
#' # semantic_search() creates the encoder itself
#' semantic_search("GERMAPARL2", query = "Wir brauchen mehr Klimaschutz.")
#' }
setClass(
  "encoder",
  slots = c(
    library = "character",
    model = "character",
    revision = "character",
    revision_check = "character",
    query_prefix = "character",
    document_prefix = "character",
    path = "character",
    device = "character",
    fp16 = "logical",
    batch_size = "integer",
    dimensions = "integer", # not "dim": a function cannot have a dim attribute
    corpus = "character",
    corpus_version = "character",
    s_attribute = "character",
    download = "logical",
    allow_duplicate_openmp = "logical"
  ),
  contains = "function"
)


#' @rdname encoder
setClass("sentence_encoder", contains = "encoder")


#' @rdname encoder
setGeneric("encoder", function(.Object, ...) standardGeneric("encoder"))


#' @rdname encoder
setMethod("encoder", "embeddings", function(
    .Object, path = NULL, device = NULL, fp16 = FALSE, batch_size = 32L,
    download = FALSE,
    allow_duplicate_openmp = Sys.info()[["sysname"]] == "Darwin",
    ...
  ){
  model <- slot(.Object, "model")
  if (length(model) != 1L || is.na(model)){
    stop(
      "The embeddings do not state a model, so an encoder cannot be created. ",
      "Provide the query as a numeric vector."
    )
  }
  spec <- slot(.Object, "spec")
  library <- if (is.null(spec[["library"]])) "sentence-transformers" else spec[["library"]]
  if (library != "sentence-transformers"){
    stop(sprintf("library '%s' not supported (supported: sentence-transformers)", library))
  }
  revision <- slot(.Object, "model_revision")
  if (length(revision) != 1L) revision <- NA_character_

  path <- .model_path(path = path, model = model)
  revision_check <- "not checked"
  if (!is.na(path) && !is.na(revision)){
    local_revision <- .local_model_revision(path)
    if (is.na(local_revision)){
      warning(
        sprintf("cannot determine the revision of the model in %s, expected %s", path, revision)
      )
      revision_check <- "unknown"
    } else if (local_revision != revision){
      stop(
        sprintf(
          "model in %s has revision %s, embeddings were computed with revision %s",
          path, local_revision, revision
        )
      )
    } else {
      revision_check <- "verified"
    }
  }

  prefix <- function(type){
    p <- spec[[type]][["prefix"]]
    if (is.null(p)) NA_character_ else p
  }
  version <- registry_property(.Object, "version")

  # The function finds the encoder object in its environment, assigned below.
  env <- new.env(parent = asNamespace("polmineR"))
  y <- new(
    "sentence_encoder",
    .sentence_encoder_function(env),
    library = library,
    model = model,
    revision = revision,
    revision_check = revision_check,
    query_prefix = prefix("query"),
    document_prefix = prefix("document"),
    path = path,
    device = if (is.null(device)) NA_character_ else device,
    fp16 = fp16,
    batch_size = as.integer(batch_size),
    dimensions = slot(.Object, "dim"),
    corpus = slot(.Object, "corpus"),
    corpus_version = if (is.null(version)) NA_character_ else version,
    s_attribute = slot(.Object, "s_attribute"),
    download = download,
    allow_duplicate_openmp = allow_duplicate_openmp
  )
  assign("enc", y, envir = env)
  y
})


#' @rdname encoder
setMethod("encoder", "corpus", function(.Object, ...){
  encoder(embeddings(as(.Object, "corpus")), ...)
})


#' @rdname encoder
setMethod("encoder", "character", function(.Object, ...){
  encoder(corpus(.Object), ...)
})


#' @rdname encoder
setMethod("show", "encoder", function(object){
  slot_or <- function(x, alt) if (length(x) != 1L || is.na(x)) alt else x
  origin <- if (is.na(slot(object, "path"))){
    sprintf(
      "Hugging Face hub (%s)",
      if (slot(object, "download")) "download if not cached" else "local cache only"
    )
  } else {
    sprintf("local directory %s", slot(object, "path"))
  }
  revision <- slot_or(slot(object, "revision"), "not stated")
  revision_check <- switch(
    slot(object, "revision_check"),
    "verified" = " (verified: local copy matches)",
    "unknown" = " (local copy: revision unknown)",
    ""
  )
  cli_text("{.cls {class(object)}} {slot(object, 'library')}")
  cli_bullets(
    c(
      "*" = "Model: {slot(object, 'model')}",
      "*" = "Revision: {revision}{revision_check}",
      "*" = "Origin: {origin}",
      "*" = "Embeddings: {slot(object, 'corpus')} {slot_or(slot(object, 'corpus_version'), '')}, s-attribute {.val {slot(object, 's_attribute')}}, {slot(object, 'dimensions')} dimensions",
      "*" = "Query prefix: {slot_or(slot(object, 'query_prefix'), 'none')}, document prefix: {slot_or(slot(object, 'document_prefix'), 'none')}",
      "*" = "Device: {slot_or(slot(object, 'device'), 'chosen by the library')}, float16: {if (slot(object, 'fp16')) 'yes' else 'no'}",
      "*" = "Loaded: {if (!is.null(.encoders[[.encoder_key(object)]])) 'yes' else 'no (loaded on first use)'}"
    )
  )
  invisible(object)
})


# Models loaded by encoders, kept for the R session.
.encoders <- new.env()


#' Directory of a local model: argument path, or a directory named after the
#' model in the directory of option polmineR.model_dir. NA if not available.
#' @noRd
.model_path <- function(path, model){
  if (!is.null(path)){
    path <- path.expand(path)
    if (!dir.exists(path)) stop(sprintf("directory not found: %s", path))
    return(normalizePath(path))
  }
  model_dir <- getOption("polmineR.model_dir")
  if (!is.null(model_dir)){
    candidate <- file.path(path.expand(model_dir), basename(model))
    if (dir.exists(candidate)) return(normalizePath(candidate))
  }
  NA_character_
}


#' Revision of a model in a directory written by `hf download --local-dir`:
#' the first line of the metadata files in .cache/huggingface/download is the
#' commit hash. NA if not available or not unique.
#' @noRd
.local_model_revision <- function(path){
  files <- list.files(
    file.path(path, ".cache", "huggingface", "download"),
    pattern = "\\.metadata$", recursive = TRUE, full.names = TRUE
  )
  if (length(files) == 0L) return(NA_character_)
  revisions <- unique(vapply(files, function(f) readLines(f, n = 1L, warn = FALSE), character(1)))
  if (length(revisions) != 1L) return(NA_character_)
  revisions
}


#' Key of a loaded model in the cache: a local model is identified by its
#' directory, a model from the hub by its ID and revision.
#' @noRd
.encoder_key <- function(x){
  paste(
    if (is.na(slot(x, "path"))) paste(slot(x, "model"), slot(x, "revision"), sep = "@") else slot(x, "path"),
    slot(x, "device"),
    slot(x, "fp16"),
    sep = "|"
  )
}


#' The function of a sentence_encoder object. It is evaluated in env, which
#' holds the encoder object as "enc".
#' @noRd
.sentence_encoder_function <- function(env){
  f <- function(x, type = c("query", "document"), normalize = TRUE){
    enc <- get("enc", envir = env)
    type <- match.arg(type)
    if (!is.character(x)) stop("x needs to be a character vector")
    m <- .load_sentence_transformer(enc)
    prefix <- slot(enc, paste(type, "prefix", sep = "_"))
    if (!is.na(prefix)) x <- paste0(prefix, x)
    y <- m$encode(
      as.list(x),
      batch_size = slot(enc, "batch_size"),
      normalize_embeddings = normalize,
      convert_to_numpy = TRUE,
      show_progress_bar = FALSE
    )
    y <- matrix(as.numeric(y), nrow = length(x))
    if (ncol(y) != slot(enc, "dimensions")){
      stop(
        sprintf(
          "model returns %d dimensions, embeddings have %d", ncol(y), slot(enc, "dimensions")
        )
      )
    }
    y
  }
  environment(f) <- list2env(list(env = env), parent = asNamespace("polmineR"))
  f
}


#' Load the model of a sentence_encoder, or get it from the cache.
#' @noRd
.load_sentence_transformer <- function(enc){
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("package 'reticulate' required")
  key <- .encoder_key(enc)
  if (!is.null(.encoders[[key]])) return(.encoders[[key]])

  # The OpenMP runtime reads the variable when PyTorch loads its copy, so it
  # needs to be set before the first import of torch in the R session.
  if (slot(enc, "allow_duplicate_openmp") && !isTRUE(.encoders[[".openmp_handled"]])){
    torch_loaded <- reticulate::py_available(initialize = FALSE) &&
      isTRUE(reticulate::py_eval('"torch" in __import__("sys").modules'))
    if (torch_loaded && !identical(Sys.getenv("KMP_DUPLICATE_LIB_OK"), "TRUE")){
      cli_alert_warning(
        "PyTorch has been loaded already, setting KMP_DUPLICATE_LIB_OK has no effect"
      )
    } else if (!identical(Sys.getenv("KMP_DUPLICATE_LIB_OK"), "TRUE")){
      cli_alert_info(
        "setting KMP_DUPLICATE_LIB_OK=TRUE (two OpenMP runtimes, see ?encoder)"
      )
    }
    Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
    .encoders[[".openmp_handled"]] <- TRUE
  }

  st <- reticulate::import("sentence_transformers")
  local <- !is.na(slot(enc, "path"))
  args <- list(if (local) slot(enc, "path") else slot(enc, "model"))
  if (!is.na(slot(enc, "device"))) args$device <- slot(enc, "device")
  if (!local){
    if (!is.na(slot(enc, "revision"))) args$revision <- slot(enc, "revision")
    if (!slot(enc, "download")) args$local_files_only <- TRUE
  }
  cli_alert_info("loading model {.val {args[[1]]}}")
  m <- tryCatch(
    do.call(st$SentenceTransformer, args),
    error = function(e){
      if (!local && !slot(enc, "download")){
        stop(
          sprintf(
            paste0(
              "model %s (revision %s) not found in the cache of the Hugging Face hub. ",
              "Download it (e.g. 'hf download %s --revision %s --local-dir <dir>') and ",
              "set option polmineR.model_dir or argument 'path', or use 'download = TRUE'. ",
              "Original error: %s"
            ),
            slot(enc, "model"), slot(enc, "revision"),
            slot(enc, "model"), slot(enc, "revision"), conditionMessage(e)
          ),
          call. = FALSE
        )
      }
      stop(e)
    }
  )
  if (slot(enc, "fp16")) m <- m$half()
  .encoders[[key]] <- m
  m
}
