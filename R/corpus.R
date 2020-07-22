#' @include partition.R bundle.R S4classes.R
NULL

#' @exportMethod corpus
setGeneric("corpus", function(.Object, ...) standardGeneric("corpus"))


#' @rdname corpus-class
#' @param .Object The upper-case ID of a CWB corpus stated by a
#'   length-one \code{character} vector.
#' @param server If \code{NULL} (default), the corpus is expected to be present
#'   locally. If provided, the name of an OpenCPU server (can be an IP address)
#'   that hosts a corpus, or several corpora. The \code{corpus}-method will then
#'   instantiate a \code{remote_corpus} object.
#' @param restricted A \code{logical} value, whether access to a remote corpus is
#'   restricted (\code{TRUE}) or not (\code{FALSE}).
#' @exportMethod corpus
#' @importFrom RcppCWB cqp_list_corpora
setMethod("corpus", "character", function(.Object, server = NULL, restricted){
  
  if (length(.Object) != 1L) stop("Cannot process more than one corpus at a time: Provide only one corpus ID as input.")
  
  if (is.null(server)){
    corpora <- cqp_list_corpora()
    
    # check that corpus is available 
    if (!.Object %in% corpora){
      uppered <- toupper(.Object)
      if (uppered %in% corpora){
        warning(
          sprintf(
            "Using corpus '%s', not '%s' - note that corpus ids are expected to be in upper case throughout.",
            uppered, .Object
            )
        )
        .Object <- uppered
      } else {
        proxy <- agrep(uppered, corpora, value = TRUE)
        if (length(proxy) == 0L){
          stop("Corpus '", .Object, "' is not available.")
        } else {
          stop("Corpus '", .Object, "' is not available. Maybe you meant '", proxy, "'?")
        }
      }
    }
    
    properties <- registry_get_properties(.Object)
    y <- new(
      "corpus",
      corpus = .Object,
      encoding = registry_get_encoding(.Object),
      data_dir = registry_get_home(.Object),
      type = if ("type" %in% names(properties)) properties[["type"]] else character(),
      size = cl_attribute_size(corpus = .Object, attribute = "word", attribute_type = "p", registry = registry())
    )
    return(y)
  } else {
    if (missing(restricted)) restricted <- FALSE
    if (isFALSE(is.logical(restricted))) stop("Argument 'restricted' is required to be a logical value.")
    y <- ocpu_exec(fn = "corpus", corpus = .Object, server = server, restricted = restricted, .Object = .Object)
    y <- as(y, "remote_corpus")
    # The object returned from the remote server will not include information on the server and
    # the accessibility status.
    y@server <- server
    y@restricted <- restricted
    return(y)
  }
})




#' @noRd
setGeneric("get_corpus", function(x) standardGeneric("get_corpus"))

#' @exportMethod get_corpus
#' @rdname textstat-class
setMethod("get_corpus", "textstat", function(x) x@corpus)


#' @exportMethod get_corpus
#' @rdname corpus_methods
#' @details Use \code{get_corpus}-method to get the corpus ID from the slot
#'   \code{corpus} of the \code{corpus} object.
setMethod("get_corpus", "corpus", function(x) x@corpus)

#' @exportMethod get_corpus
#' @describeIn subcorpus Get the corpus ID from the \code{subcorpus} object.
setMethod("get_corpus", "subcorpus", function(x) x@corpus)


#' @exportMethod get_corpus
#' @rdname kwic-class
setMethod("get_corpus", "kwic", function(x) x@corpus)

#' @exportMethod get_corpus
#' @rdname bundle
setMethod("get_corpus", "bundle", function(x) unique(sapply(x@objects, get_corpus)))


#' @rdname corpus-class 
setMethod("corpus", "missing", function(){
  if (nchar(Sys.getenv("CORPUS_REGISTRY")) > 1){
    corpora <- .list_corpora()
    y <- data.frame(
      corpus = corpora,
      size = unname(sapply(corpora,function(x) cl_attribute_size(corpus = x, attribute = registry_get_p_attributes(x)[1], attribute_type = "p", registry = registry()))),
      template = unname(sapply(corpora, function(x) if (is.null(get_template(x, warn = FALSE))) FALSE else TRUE )),
      stringsAsFactors = FALSE
    )
  } else {
    y <- data.frame(corpus = character(), size = integer())
  }
  return(y)
})



#' @noRd
.s_attributes_stop_if_nested <- function(corpus, s_attr){
  max_attr <- unique(
    sapply(
      s_attr,
      function(s) cl_attribute_size(corpus = corpus, attribute = s, attribute_type = "s", registry = registry())
    )
  )
  if (length(max_attr) != 1){
    stop(
      sprintf("Differing attribute size of s-attributes detected (%s), ", paste(s_attr, collapse = "/")),
      "but the method does not (yet) work for nested XML / nested structural attributes."
    )
  }
  max_attr
}

#' @title Subsetting corpora and subcorpora
#' @description The structural attributes of a corpus (s-attributes) can be used
#'   to generate subcorpora (i.e. a \code{subcorpus} class object) by applying
#'   the \code{subset}-method. To obtain a \code{subcorpus}, the
#'   \code{subset}-method can be applied on a corpus represented by a
#'   \code{corpus} object, a length-one \code{character} vector (as a shortcut),
#'   and on a \code{subcorpus} object.
#' @rdname subset
#' @name subset
#' @aliases subset,corpus-method
#' @seealso The methods applicable for the \code{subcorpus} object resulting
#'   from subsetting a corpus or subcorpus are described in the documentation of
#'   the \code{\link{subcorpus-class}}. Note that the \code{subset}-method can also be
#'   applied to \code{\link{textstat-class}} objects (and objects inheriting from
#'   this class).
#' @examples
#' use("polmineR")
#' 
#' # examples for standard and non-standard evaluation
#' a <- corpus("GERMAPARLMINI")
#' 
#' # subsetting a corpus object using non-standard evaluation 
#' sc <- subset(a, speaker == "Angela Dorothea Merkel")
#' sc <- subset(a, speaker == "Angela Dorothea Merkel" & date == "2009-10-28")
#' sc <- subset(a, grepl("Merkel", speaker))
#' sc <- subset(a, grepl("Merkel", speaker) & date == "2009-10-28")
#' 
#' # subsetting corpus specified by character vector 
#' sc <- subset("GERMAPARLMINI", grepl("Merkel", speaker))
#' sc <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel")
#' sc <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel" & date == "2009-10-28")
#' sc <- subset("GERMAPARLMINI", grepl("Merkel", speaker) & date == "2009-10-28")
#' 
#' # subsetting a corpus using the (old) logic of the partition-method
#' sc <- subset(a, speaker = "Angela Dorothea Merkel")
#' sc <- subset(a, speaker = "Angela Dorothea Merkel", date = "2009-10-28")
#' sc <- subset(a, speaker = "Merkel", regex = TRUE)
#' sc <- subset(a, speaker = c("Merkel", "Kauder"), regex = TRUE)
#' sc <- subset(a, speaker = "Merkel", date = "2009-10-28", regex = TRUE)
#' 
#' # providing the value for s-attribute as a variable
#' who <- "Volker Kauder"
#' sc <- subset(a, quote(speaker == who))
#' 
#' # use bquote for quasiquotation when using a variable for subsetting in a loop
#' for (who in c("Angela Dorothea Merkel", "Volker Kauder", "Ronald Pofalla")){
#'    sc <- subset(a, bquote(speaker == .(who)))
#'    if (interactive()) print(size(sc))
#' }
#' 
#' # equivalent procedure with lapply (DOES NOT WORK YET)
#' b <- lapply(
#'   c("Angela Dorothea Merkel", "Volker Kauder", "Ronald Pofalla"),
#'   function(who) subset(a, bquote(speaker == .(who)))
#' )
#' sapply(b, size)
#' @param x A \code{corpus} or \code{subcorpus} object. A corpus may also
#'   specified by a length-one \code{character} vector.
#' @param ... An expression that will be used to create a subcorpus from
#'   s-attributes.
#' @param subset A \code{logical} expression indicating elements or rows to
#'   keep. The expression may be unevaluated (using \code{quote} or
#'   \code{bquote}).
#' @importFrom data.table setindexv
#' @param regex A \code{logical} value. If \code{TRUE}, values for s-attributes
#'   defined using the three dots (...) are interpreted as regular expressions
#'   and passed into a \code{grep} call for subsetting a table with the regions
#'   and values of structural attributes. If \code{FALSE} (the default), values
#'   for s-attributes must match exactly.
setMethod("subset", "corpus", function(x, subset, regex = FALSE, ...){
  stopifnot(is.logical(regex))
  s_attr <- character()
  
  if (!missing(subset)){
    expr <- substitute(subset)
    # The expression may also have been passed in as an unevaluated expression. In
    # this case, it is "unwrapped". Note that parent.frames looks back two generations
    # because the S4 Method inserts an additional layer to the original calling
    # environment
    if (class(try(eval(expr, envir = parent.frame(n = c(1L, 2L))), silent = TRUE)) == "call"){
      expr <- eval(expr, envir = parent.frame(n = c(1L, 2L)))
    }
    # Adjust the encoding of the expression to the one of the corpus. Adjusting
    # encodings is expensive, so the (small) epression will be adjusted to the
    # encoding of the corpus, not vice versa
    if (localeToCharset()[1] != x@encoding)
      expr <- .recode_call(x = expr, from = localeToCharset()[1], to = x@encoding)
    s_attr_expr <- s_attributes(expr, corpus = x) # get s_attributes present in the expression
    s_attr <- c(s_attr, s_attr_expr)
  }

  dots <- list(...)
  if (length(dots) > 0L){
    if (!all(names(dots) %in% s_attributes(x))){
      stop("Aborting - at least one of the s-attributes provided as an argument is not available.")
    }
    s_attr_dots <- names(dots)
    s_attr <- c(s_attr, s_attr_dots)
    if (localeToCharset()[1] != x@encoding){
      s_attr_dots <- lapply(s_attr_dots, function(v) as.corpusEnc(v, corpusEnc = x@encoding))
    }
  }
  
  # Reading the binary file with the ranges for the whole corpus is faster than using
  # the RcppCWB functionality. The assumption here is that the XML is flat, i.e. no need
  # to read in seperate rng files.
  rng_file <- file.path(x@data_dir, paste(s_attr[1], "rng", sep = "."))
  rng_size <- file.info(rng_file)[["size"]]
  rng <- readBin(rng_file, what = integer(), size = 4L, n = rng_size / 4L, endian = "big")
  dt <- data.table(
    struc = 0L:((length(rng) / 2L) - 1L),
    cpos_left = rng[seq.int(from = 1L, to = length(rng), by = 2L)],
    cpos_right = rng[seq.int(from = 2L, to = length(rng), by = 2L)]
  )
  
  # Now we add the values of the s-attributes to the data.table with regions, one at
  # a time. Again, doing this from the binary files directly is faster than using RcppCWB.
  for (i in seq_along(s_attr)){
    files <- list(
      avs = file.path(x@data_dir, paste(s_attr[i], "avs", sep = ".")),
      avx = file.path(x@data_dir, paste(s_attr[i], "avx", sep = "."))
    )
    sizes <- lapply(files, function(file) file.info(file)[["size"]])
    
    avx <- readBin(files[["avx"]], what = integer(), size = 4L, n = sizes[["avx"]] / 4L, endian = "big")
    avx_matrix <- matrix(avx, ncol = 2, byrow = TRUE)
    
    avs <- readBin(con = files[["avs"]], what = character(), n = sizes[["avs"]])
    if (!is.null(encoding)) Encoding(avs) <- x@encoding
    
    dt[, (s_attr[i]) := avs[match(avx_matrix[, 2], unique(avx_matrix[, 2]))] ]
  }
  
  # Apply the expression.
  if (!missing(subset)){
    setindexv(dt, cols = s_attr)
    dt <- dt[eval(expr, envir = dt)]
  }
  
  if (length(dots) > 0L){
    for (s in s_attr_dots){
      if (regex){
        for (i in length(dots[[s]])){
          dt <- dt[grep(dots[[s]][i], dt[[s]])]
        }
      } else {
        if (length(dots[[s]]) == 1L){
          dt <- dt[dt[[s]] == dots[[s]]]
        } else {
          dt <- dt[dt[[s]] %in% dots[[s]]]
        }
      }
    }
  }
  
  
  # And assemble the subcorpus object that is returned.
  if (nrow(dt) == 0L){
    warning("No matching regions found for the s-attributes provided: Returning NULL object")
    return(NULL)
  }
  
  
  y <- new(
    if (length(x@type) > 0L) paste(x@type, "subcorpus", sep = "_") else "subcorpus",
    corpus = x@corpus,
    encoding = x@encoding,
    type = x@type,
    data_dir = x@data_dir,
    cpos = as.matrix(dt[, c("cpos_left", "cpos_right")]),
    strucs = dt[["struc"]],
    s_attribute_strucs = s_attr[length(s_attr)],
    s_attributes = lapply(setNames(s_attr, s_attr), function(s) unique(dt[[s]])),
    xml = "flat",
    name = ""
  )
  dimnames(y@cpos) <- NULL
  y@size <- size(y)
  y
})


#' @rdname subset
setMethod("subset", "character", function(x, ...){
  subset(x = corpus(x), ...)
})


#' @rdname subset
setMethod("subset", "subcorpus", function(x, subset, ...){
  expr <- substitute(subset)
  
  if (localeToCharset()[1] != x@encoding)
    expr <- .recode_call(x = expr, from = localeToCharset()[1], to = x@encoding)
  
  s_attr <- s_attributes(expr, corpus = x) # get s_attributes present in the expression
  max_attr <- .s_attributes_stop_if_nested(corpus = x@corpus, s_attr = s_attr)

  if (max_attr != cl_attribute_size(corpus = x@corpus, attribute = x@s_attribute_strucs, attribute_type = "s", registry = registry())){
    stop("New s-attributes are nested in existing s-attribute defining subcorpus. ",
         "The method does not (yet) work for nested XML / nested structural attributes.")
  }
  
  dt <- data.table(
    struc = x@strucs,
    cpos_left = x@cpos[,1],
    cpos_right = x@cpos[,2]
  )
  for (s in s_attr){
    str <- RcppCWB::cl_struc2str(corpus = x@corpus, s_attribute = s, struc = dt[["struc"]], registry = registry())
    Encoding(str) <- x@encoding
    dt[, (s) := str]
  }
  setindexv(dt, cols = s_attr)
  dt_min <- dt[eval(expr, envir = dt)]

  y <- new(
    "subcorpus",
    corpus = x@corpus,
    encoding = x@encoding,
    type = x@type,
    data_dir = x@data_dir,
    cpos = as.matrix(dt_min[, c("cpos_left", "cpos_right")]),
    strucs = dt_min[["struc"]],
    s_attributes = c(x@s_attributes, lapply(setNames(s_attr, s_attr), function(s) unique(dt_min[[s]]))),
    s_attribute_strucs = s_attr[length(s_attr)],
    xml = "flat",
    name = x@name
  )
  y@size <- size(y)
  y
})


#' @exportMethod show
#' @docType methods
#' @rdname corpus_methods
#' @details The \code{show}-method will show basic information on the
#'   \code{corpus} object.
setMethod("show", "corpus", function(object){
  message(sprintf("** '%s' object **", class(object)))
  message(sprintf("%-12s", "corpus:"), object@corpus)
  message(sprintf("%-12s", "encoding:"), object@encoding)
  message(sprintf("%-12s", "type:"), if (length(object@type) > 0) object@type else "[undefined]")
  message(sprintf("%-12s", "size:"), size(object))
})




#' @details Applying the `$`-method on a corpus will return the values for the
#'   s-attribute stated with argument \code{name}.
#' @examples
#' # show-method 
#' if (interactive()) corpus("REUTERS") %>% show()
#' if (interactive()) corpus("REUTERS") # show is called implicitly
#' 
#' # get corpus ID
#' corpus("REUTERS") %>% get_corpus()
#' 
#' # use $ to access s_attributes quickly
#' use("polmineR")
#' g <- corpus("GERMAPARLMINI")
#' g$date
#' corpus("GERMAPARLMINI")$date # 
#' corpus("GERMAPARLMINI") %>% s_attributes(s_attribute = "date") # equivalent
#' 
#' use("polmineR")
#' sc <- subset("GERMAPARLMINI", date == "2009-10-27")
#' sc$date
#' @exportMethod $
#' @rdname corpus_methods
#' @param x An object of class \code{corpus}, or inheriting from it.
#' @param name A (single) s-attribute.
setMethod("$", "corpus", function(x, name) s_attributes(x, s_attribute = name))

#' @param object An object of class \code{subcorpus_bundle}.
#' @rdname subcorpus_bundle
setMethod("show", "subcorpus_bundle", function (object) {
  message('** subcorpus_bundle object: **')
  message(sprintf('%-25s', 'Number of subcorpora:'), length(object@objects))
})


#' @rdname subset
setMethod("subset", "remote_corpus", function(x, subset){
  expr <- substitute(subset)
  sc <- ocpu_exec(fn = "subset", corpus = x@corpus, server = x@server, restricted = x@restricted, do.call = FALSE, x = as(x, "corpus"), subset = expr)
  y <- as(sc, "remote_subcorpus")
  # Capture information on accessibility status and the server which is not included
  # in the object that is returned.
  y@restricted <- x@restricted
  y@server <- x@server
  y
})

