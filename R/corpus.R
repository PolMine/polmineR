#' @include partition.R bundle.R S4classes.R
NULL

#' @exportMethod corpus
setGeneric("corpus", function(.Object, ...) standardGeneric("corpus"))


#' @rdname corpus-class
#' @param .Object The upper-case ID of a CWB corpus stated by a
#'   length-one `character` vector.
#' @param registry_dir The registry directory with the registry file describing
#'   the corpus (length-one `character` vector). If missing, the C
#'   representations of loaded corpora will be evaluated to get the registry
#'   directory with the registry file for the corpus.
#' @param server If `NULL` (default), the corpus is expected to be present
#'   locally. If provided, the name of an OpenCPU server (can be an IP address)
#'   that hosts a corpus, or several corpora. The `corpus`-method will then
#'   instantiate a `remote_corpus` object.
#' @param restricted A `logical` value, whether access to a remote corpus is
#'   restricted (`TRUE`) or not (`FALSE`).
#' @exportMethod corpus
#' @importFrom RcppCWB cqp_list_corpora corpus_data_dir corpus_registry_dir
#'   corpus_info_file corpus_full_name
#' @importFrom fs path path_expand
setMethod("corpus", "character", function(
    .Object, registry_dir,
    server = NULL, restricted
){

  if (length(.Object) > 1L){
    stop(
      "Cannot process more than one corpus at a time: ",
      "Provide only one corpus ID as input."
    )
  }

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
          stop(
            "Corpus '", .Object, "' is not available. Maybe you meant '", proxy, "'?"
          )
        }
      }
    }

    c_regdir <- path(corpus_registry_dir(.Object))
    if (missing(registry_dir)){
      if (is.na(c_regdir)){
        stop(
          "Cannot initialize corpus object - ",
          "cannot derive argument 'registry' from C representation of corpus."
        )
      } else if (length(c_regdir) > 1L){
        stop(
          "Cannot initialize corpus object - ",
          "corpus defined by two different registry files."
        )
      } else {
        registry_dir <- c_regdir
      }
    } else {
      registry_dir <- path(path_expand(registry_dir))
      if (is.na(c_regdir)){
        stop("Cannot locate corpus with registry provided.")
      } else if (length(c_regdir) > 1L){
        if (!registry_dir %in% c_regdir){
          stop("Cannot locate corpus with registry provided.")
        }
      } else if (registry_dir != c_regdir){
        stop("Cannot locate corpus with registry provided.")
      }
      
    }
    
    properties <- registry_get_properties(.Object)
    data_dir <- corpus_data_dir(.Object, registry = registry_dir)
    template <- path(data_dir, "template.json")
    info_file <- corpus_info_file(.Object, registry = registry_dir)
    
    y <- new(
      "corpus",
      corpus = .Object,
      encoding = cl_charset_name(corpus = .Object, registry = registry_dir),
      registry_dir = registry_dir,
      data_dir = data_dir,
      info_file = if (file.exists(info_file)) info_file else path(NA),
      template = if (file.exists(template)) template else path(NA),
      type = if ("type" %in% names(properties))
        properties[["type"]]
      else
        NA_character_,
      size = cl_attribute_size(
        corpus = .Object, registry = registry_dir,
        attribute = "word", attribute_type = "p"
      ),
      name = corpus_full_name(corpus = .Object, registry = registry_dir)
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
  
  dt <- data.table(corpus = cqp_list_corpora())
  regdirs <- sapply(dt[["corpus"]], corpus_registry_dir)
  charset <- mapply(
    RcppCWB::corpus_property,
    corpus = dt[["corpus"]], registry = regdirs, property = "charset"
  )
  dt[, "encoding" := charset]
  
  corpus_type <- mapply(
    RcppCWB::corpus_property,
    corpus = dt[["corpus"]], registry = regdirs, property = "type"
  )
  dt[, "type" := corpus_type]
  
  data_dirs <- mapply(
    RcppCWB::corpus_data_dir,
    corpus = dt[["corpus"]],
    registry = regdirs
  )
  template <- sapply(data_dirs, function(dir) file.exists(path(dir, "template.json")))
  dt[, "template" := template]
  
  size <- mapply(
    RcppCWB::cl_attribute_size,
    corpus = dt[["corpus"]],
    registry = regdirs,
    attribute = "word", 
    attribute_type = "p"
  )
  dt[, "size" := size]
  
  
  as.data.frame(dt)
})


#' @title Subsetting corpora and subcorpora
#' @description The structural attributes of a corpus (s-attributes) can be used
#'   to generate subcorpora (i.e. a `subcorpus` class object) by applying the
#'   `subset`-method. To obtain a `subcorpus`, the `subset`-method can be
#'   applied on a corpus represented by a `corpus` object, a length-one
#'   `character` vector (as a shortcut), and on a `subcorpus` object.
#' @return A `subcorpus` object. If the expression provided by argument `subset`
#'   includes undefined s-attributes, a warning is issued and the return value
#'   is `NULL`.
#' @rdname subset
#' @name subset
#' @aliases subset,corpus-method
#' @seealso The methods applicable for the `subcorpus` object resulting from
#'   subsetting a corpus or subcorpus are described in the documentation of the
#'   `\link{subcorpus-class}`. Note that the `subset`-method can also be applied
#'   to \code{\link{textstat-class}} objects (and objects inheriting from this
#'   class).
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
#' @importFrom data.table setindexv setDT
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
    # The expression may also have been passed in as an unevaluated expression.
    # In this case, it is "unwrapped". Note that parent.frames looks back two
    # generations because the S4 Method inserts an additional layer to the
    # original calling environment
    if (is.call(try(eval(expr, envir = parent.frame(n = c(1L, 2L))), silent = TRUE))){
      expr <- eval(expr, envir = parent.frame(n = c(1L, 2L)))
    }
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
    if (encoding() != x@encoding){
      s_attr_dots <- lapply(
        s_attr_dots,
        function(v) as.corpusEnc(v, corpusEnc = x@encoding)
      )
    }
  }
  
  if (all(is.na(s_attr))){
    warning("no valid s-attributes for subsetting the corpus (returning NULL)")
    return(NULL)
  }

  # Reading the binary file with the ranges for the whole corpus is faster than using
  # the RcppCWB functionality. The assumption here is that the XML is flat, i.e. no need
  # to read in seperate rng files.
  if (!s_attr[1] %in% s_attributes(x)){
    warning(sprintf("structural attribute '%s' not defined", s_attr[1]))
    return(NULL)
  }
    
  rng_file <- fs::path(x@data_dir, paste(s_attr[1], "rng", sep = "."))
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
    if (!s_attr[i] %in% s_attributes(x)){
      warning(sprintf("structural attribute '%s' not defined", s_attr[i]))
      return(NULL)
    }
    files <- list(
      avs = fs::path(x@data_dir, paste(s_attr[i], "avs", sep = ".")),
      avx = fs::path(x@data_dir, paste(s_attr[i], "avx", sep = "."))
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
    # Adjust the encoding of the expression to the one of the corpus. Adjusting
    # encodings is expensive, so the (small) epression will be adjusted to the
    # encoding of the corpus, not vice versa
    if (encoding() != x@encoding)
      expr <- .recode_call(x = expr, from = encoding(), to = x@encoding)
    setindexv(dt, cols = s_attr)
    success <- try({dt <- dt[eval(expr, envir = dt)]})
    if (is(success, "try-error")) return(NULL)
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
  
  y <- if (!is.na(x@type)){
    as(x, paste(x@type, "subcorpus", sep = "_"))
  } else { 
    as(x, "subcorpus")
  }
  
  y@cpos <- as.matrix(dt[, c("cpos_left", "cpos_right")])
  dimnames(y@cpos) <- NULL
  y@strucs = dt[["struc"]]
  y@s_attribute_strucs <- s_attr[length(s_attr)]
  y@s_attributes <- lapply(setNames(s_attr, s_attr), function(s) unique(dt[[s]]))
  y@xml <- "flat"
  y@name <- ""
  y@size <- size(y)
  
  y
})


#' @rdname subset
setMethod("subset", "character", function(x, ...){
  subset(x = corpus(x), ...)
})


#' @rdname subset
#' @importFrom RcppCWB s_attr_regions
setMethod("subset", "subcorpus", function(x, subset, ...){
  expr <- substitute(subset)

  if (encoding() != x@encoding)
    expr <- .recode_call(x = expr, from = encoding(), to = x@encoding)

  s_attr <- s_attributes(expr, corpus = x) # get s_attributes present in the expression
  dt <- data.table(struc = x@strucs, cpos_left = x@cpos[,1], cpos_right = x@cpos[,2])
  
  s_attr_sizes <- sapply(
    unique(c(x@s_attribute_strucs, s_attr)),
    function(s)
      cl_attribute_size(
        corpus = x@corpus,
        attribute = s, attribute_type = "s",
        registry = x@registry_dir
      )
  )
  
  if (length(unique(s_attr_sizes)) != 1L){
    
    # If sizes of s-attributes differ, we assume the nested scenario
    
    regions <- lapply(
      setNames(s_attr, s_attr),
      function(s)
        s_attr_regions(
          corpus = x@corpus, registry = x@registry_dir,
          data_dir = x@data_dir,
          s_attr = s
        )
    )

    if (length(s_attr) > 1L){
      # When subsetting on more than one s-attribute at a time, it is required
      # that these are on the same level.
      if (length(unique(s_attr_sizes[-1L])) == 1L){
        stop("structural attributes for subsetting need to have identical size - not true")
      }
      
      # The second check is whether regions of s-attributes for subsetting are
      # identical.
      for (i in 2L:length(s_attr))
        if (!identical(regions[[1]], regions[[i]]))
          stop("structural attributes need to define the same regions")
      
    }
    
    for (s in s_attr){
      strucs <- cl_cpos2struc(
        corpus = x@corpus, registry = x@registry_dir,
        s_attribute = s, cpos = x@cpos[,1]
      )
      
      if (any(is.na(strucs)) || any(strucs < 0L)){
        descendant <- TRUE
        break
      }

      r <- regions[[s]][strucs + 1L]
      if (all(r[,1] <= x@cpos[,1]) && all(r[,2] >= x@cpos[,2])){
        descendant <- FALSE
        str <- cl_struc2str(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s, struc = strucs
        )
        Encoding(str) <- x@encoding
        dt[, (s_attr) := str]
      } else {
        descendant <- TRUE
        break
      }
    }
    
    if (descendant){
      # Slower by necessity
      strucs <- cl_cpos2struc(
        x@corpus, registry = x@registry_dir,
        s_attribute = s_attr[1], cpos = cpos(x@cpos)
      )
      strucs <- unique(strucs[strucs >= 0])
      ranges <- get_region_matrix(
        x@corpus, registry = x@registry_dir,
        s_attribute = s_attr[1], strucs = strucs
      )
      str <- cl_struc2str(
        corpus = x@corpus, registry = x@registry_dir,
        s_attribute = s_attr[1], struc = strucs
      )
      Encoding(str) <- x@encoding
      dt <- data.table(
        struc = strucs,
        cpos_left = ranges[,1],
        cpos_right = ranges[,2]
      )
      dt[, (s_attr[1]) := str]
      if (length(s_attr) > 1L){
        for (s in s_attr[-1]){
          str <- cl_struc2str(
            corpus = x@corpus, registry = x@registry_dir,
            s_attribute = s, struc = strucs
          )
          Encoding(str) <- x@encoding
          dt[, (s) := str]
        }
      }
    }
    
  } else {
    for (s in s_attr){
      str <- cl_struc2str(
        corpus = x@corpus, registry = x@registry_dir,
        s_attribute = s, struc = dt[["struc"]]
      )
      Encoding(str) <- x@encoding
      dt[, (s) := str]
    }
    
  }
  
  setindexv(dt, cols = s_attr)
  dt_min <- dt[eval(expr, envir = dt)]

  y <- new(
    "subcorpus",
    corpus = x@corpus,
    registry_dir = x@registry_dir,
    template = x@template,
    info_file = x@info_file,
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
  expr <- if (is.call(try(eval(expr), silent = TRUE))) eval(expr) else expr
  sc <- ocpu_exec(fn = "subset", corpus = x@corpus, server = x@server, restricted = x@restricted, do.call = FALSE, x = as(x, "corpus"), subset = expr)
  y <- as(sc, "remote_subcorpus")
  # Capture information on accessibility status and the server which is not included
  # in the object that is returned.
  y@restricted <- x@restricted
  y@server <- x@server
  y
})

