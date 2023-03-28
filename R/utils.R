#' Tools for CQP queries.
#' 
#' Test whether a character string is a CQP query, or turn a character
#' vector into CQP queries.
#' 
#' The `is.cqp()` function guesses whether `query` is a CQP query 
#' and returns the respective logical value (`TRUE`/`FALSE`).
#' 
#' The `as.cqp()` function takes a character vector as input and converts it
#' to a CQP query by putting the individual strings in quotation marks.
#' 
#' @param query A `character` vector with at least one CQP query.
#' @param warn A (length-one) `logical` value, whether to issue a warning if a
#'   query may be buggy.
#' @name cqp
#' @references CQP Query Language Tutorial
#'   (\url{https://cwb.sourceforge.io/files/CQP_Tutorial.pdf})
#' @rdname cqp
#' @export is.cqp
#' @examples 
#' is.cqp("migration") # will return FALSE
#' is.cqp('"migration"') # will return TRUE
#' is.cqp('[pos = "ADJA"] "migration"') # will return TRUE
#' 
#' as.cqp("migration")
#' as.cqp(c("migration", "diversity"))
#' as.cqp(c("migration", "diversity"), collapse = TRUE)
#' as.cqp("migration", normalise.case = TRUE)
#' @return \code{is.cqp} returns a logical value, \code{as.cqp} a character
#'   vector, \code{check_cqp_query} a logical value that is \code{TRUE} if all
#'   queries are valid, or \code{FALSE} if not.
is.cqp <- function(query){
  isQuery <- unique(grepl('["\']', query))
  if (length(isQuery) == 2)
    stop("Test whether query is a CQP query (or not) does not result in an unambigious result. Please check queries and/or state logical value explicitly.")
  isQuery
} 

#' @rdname cqp
#' @export check_cqp_query
#' @details The \code{check_cqp_query}-function will check that opening
#'   quotation marks are matched by closing quotation marks, to prevent crashes
#'   of CQP and the R session.
#' @examples
#' 
#' check_cqp_query('"Integration.*"') # TRUE, the query is ok
#' check_cqp_query('"Integration.*') # FALSE, closing quotation mark is missing
#' check_cqp_query("'Integration.*") # FALSE, closing quotation mark is missing
#' check_cqp_query(c("'Integration.*", '"Integration.*')) # FALSE too
check_cqp_query <- function(query, warn = TRUE){
  
  disclaimer <- paste(
    "If this is a false positive, omit check of the query syntax by setting", 
    "argument `check` to `FALSE`."
  )
  
  msg <- paste(c(
    "An issue occurred when checking query: %s\n",
    paste(
      "Number of quotation marks is not divisable by 2:",
      "Opening quotation marks are not matched by closing quotation marks, or vice versa.",
      "Aborting to avoid a potential crash of CQP and the entire R session.",
      "Please check query.", disclaimer,
      collapse = " "
    )
  ), collapse = "")
  
  sapply(
    query,
    function(q){
      query_ok <- TRUE
      chars <- strsplit(q, split = "")[[1]]
      if (length(which(chars == '"')) %% 2 != 0) query_ok <- FALSE
      if (length(which(chars == "'")) %% 2 != 0) query_ok <- FALSE
      if (!query_ok && isTRUE(warn)) warning(sprintf(msg, q))
      if (length(which(chars == '(')) != length(which(chars == ')'))){
        query_ok <- FALSE
        if (isTRUE(warn))
          warning(
            "Opening brackets are not matched by closing brackets in CQP query: ",
            q, " ", disclaimer
          )
      }
      if (length(which(chars == '[')) != length(which(chars == ']'))){
        query_ok <- FALSE
        if (isTRUE(warn))
          warning(
            "Number of opening square brackets are not matched by closing brackets in CQP query: ",
            q, " ", disclaimer
          )
      }
      
      query_ok
    }
  )
}


#' @param collapse A `logical` value, whether to collapse the queries into one.
#' @param normalise.case A `logical` value, if `TRUE`, a flag will be
#'   added to the query/queries to omit matching case.
#' @param check A `logical` value whether to run `check_cqp_query()` on queries.
#' @export as.cqp
#' @rdname cqp
#' @name as.cqp
as.cqp <- function(query, normalise.case = FALSE, collapse = FALSE, check = TRUE, warn = TRUE){
  
  if (!is.character(query))
    stop("Function as.cqp() expects argument 'query' to be a character vector.")

  if (!is.logical(normalise.case))
    stop("normalise.case needs to be a logical value")
  
  if (!is.logical(collapse))
    stop("collapse needs to be a logical value")
  
  cqp <- unlist(lapply(
    query,
    function(x){
      query <- gsub("\\s+", " ", x)
      cqp_raw <- lapply(
        unlist(strsplit(query, "\\s")),
        function(q){
          if ((substr(q, 1L, 1L) == '[') && (substr(q, nchar(q), nchar(q)) == ']')){
            retval <- q
          } else {
            retval <- paste('"', q, '"', sep = '')
            if (normalise.case == TRUE) retval <- paste(retval, "%c", sep = " ")
          }
          retval
        })
      retval <- paste(cqp_raw, collapse = " ")
      
      if (check) retval <- if (check_cqp_query(retval, warn = warn))
        retval
      else
        NULL
      
      retval
    }
  ))
  if (length(cqp) > 1L && collapse == TRUE){
    cqp <- paste('(', paste(cqp, sep = '', collapse='|'), ')', sep = "")
  }    
  cqp
}

#' Capitalize character vector.
#' 
#' Make the first character of the elements of a `character` vector have upper
#' case and the rest lower case.
#' 
#' The `capitalize()` function may be useful when applying lowercased
#' dictionaries of stoplists, a sentiment dictionary etc. on a CWB corpus that
#' maintains capitalization (tokens are not lowercased).
#' 
#' This function is inspired by a method Python offers for string objects.
#' @param x A `character` vector.
#' @export
#' @examples
#' capitalize(c("oil", "corpus", "data"))
capitalize <- function(x){
  if (!is.character(x)) stop("capitalize() requires character vector as input")
  retval <- paste0(
    toupper(substr(x, start = 1L, stop = 1L)),
    tolower(substr(x, start = 2L, stop = nchar(x)))
  )
  retval[is.na(x)] <- NA_character_
  retval
}


#' Check whether s-attributes of corpus are nested
#' 
#' Simple test whether the attribute size of all s-attributes of a corpus is 
#' identical (flat import XML) or not (nested import XML).
#' @param x A `character` vector with corpus ID or a `corpus` object.
#' @return A `logical` value: `FALSE` is data structure is flat and `TRUE` if 
#' data structure is nested.
#' @export is_nested
#' @examples 
#' use("polmineR")
#' use("RcppCWB")
#' @importFrom RcppCWB cl_attribute_size
is_nested <- function(x){
  if (is.character(x)) x <- corpus(x)
  sizes <- lapply(
    s_attributes(x),
    function(s_attr)
      cl_attribute_size(
        corpus = x@corpus,
        attribute = s_attr,
        attribute_type = "s",
        registry = x@registry_dir
      )
  )
  if (length(unique(unlist(sizes))) == 1L) FALSE else TRUE
}

#' 
#' @importFrom RcppCWB cl_struc_values
s_attr_has_values <- function(s_attribute, x){

  s_attr_files <- sprintf("^%s.(avs|avx|rng)$", s_attribute) |>
    grep(list.files(x@data_dir), value = TRUE) |>
    strsplit("\\.") |>
    sapply(`[[`, 2)

  if (length(s_attr_files) == 0L){
    return(NA)
  } else if (all(c("avs", "avs", "rng") %in% s_attr_files)){
    return(TRUE)
  } else if (s_attr_files == "rng"){
    return(FALSE)
  } else {
    warning("s_attr_values() encountered unknown issue")
    return(NA)
  }
  
  # cl_struc_values(
  #   corpus = x@corpus,
  #   s_attribute = s_attribute,
  #   registry = x@registry_dir
  # )
}


#' Get ID for token.
#' 
#' Helper function for context method. Get ids for tokens.
#' 
#' @param corpus the CWB corpus to use
#' @param p_attribute the p-attribute to use
#' @param token character tokens to turn into ids (character vector length >= 1)
#' @param regex logical
#' @noRd
.token2id <- function(corpus, p_attribute, token = NULL, regex = FALSE){
  regdir <- corpus_registry_dir(corpus)
  stopifnot(
    corpus %in% cqp_list_corpora(),
    p_attribute %in% corpus_p_attributes(corpus, regdir)
  )
  
  if (is.null(token)) return( NULL )
  
  if (is.numeric(token)){
    return( token ) # do nothing if token is already numeric (i.e. presumably integer)
  } else {
    if (regex){
      retval <- unlist(lapply(
        token,
        function(x)
          cl_regex2id(
            corpus = corpus, registry = regdir,
            p_attribute = p_attribute, regex = x
          )
      ))
    } else {
      retval <- cl_str2id(
        corpus = corpus, registry = regdir,
        p_attribute = p_attribute, str = token
      )
    }
    return( retval )
  }
}


.umlaute2punkt <- function(cmd){
  return(gsub('[\u00e4\u00f6\u00fc\u00df\u00c4\u00d6\u00dc]','.', cmd))
}

#' @export punctuation
#' @rdname trim-method
punctuation <- c(".", ",", ";", ":", "!", "?", "-", "--", "(", ")", "[", "]", "/")



#' \code{flatten} may be useful if you have a list of \code{partition_bundle} objects. This function will flatten the data
#' structure and return a \code{partition_bundle} object.
#' 
#' @return a \code{partition_bundle} object
#' @export flatten
#' @rdname partition_bundle-class
#' @name flatten
flatten <- function(object){
  retval <- new("partition_bundle")
  for (i in 1L:length(object)){
    if(!is.null(object[[i]])){
      if (length(object[[i]]@objects) > 0L){
        retval <- retval + object[[i]]
      }
    }
  } 
  retval
}

.statisticalSummary <- function(object) {
  if (object@method %in% c("ll", "chiSquare")){
    criticalValue <- c(3.84, 6.63, 7.88, 10.83)
    propability <- c(0.05, 0.01, 0.005, 0.001)
    no <- vapply(
      criticalValue,
      function(x) length(which(object@stat[[object@method]]>x)),
      FUN.VALUE=1
    )
    result <- data.frame(propability, criticalValue, no)
    result <- result[order(result$propability, decreasing = FALSE),]
    return(result)
  } else {
    
  }
  
}


# used by features,cooccurrences-method
# listed here because it may be used by other methods
.minMaxId <- function(row){
  if (row["nodeId"] == row["cooccurrenceId"]){
    retval <- c(row, idMin=row["nodeId"], idMax=row["cooccurrenceId"])
  } else {
    idMin <- min(row["nodeId"], row["cooccurrenceId"])
    idMax <- max(row["nodeId"], row["cooccurrenceId"])
    retval <- c(row, idMin=idMin, idMax=idMax)
  }
  return(retval)
}


#' Get objects of a certain class.
#'
#' @param class character, class to be looked for
#' @param envir character string, namespace to be searched
#' @return a list with the partitions found in the namespace
#' @noRd
.get_objects <- function(class, envir = .GlobalEnv) {
  rawList <- sapply(ls(envir), function(x) class(get(x, envir = envir))[1])
  availableObjectsList <- rawList[rawList %in% class]
  names(unlist(availableObjectsList))
}


# used in shiny app
.get_slot <- function(x, name){
  if ("R6" %in% class(x)) x[[name]] else slot(x, name)
}


.message <- function(..., verbose = TRUE, type = "message", shiny = getOption("polmineR.shiny")){
  msg <- paste(unlist(list(...)), collapse = " ")
  
  # print(shiny)
  if (is.null(shiny)) shiny <- FALSE
  
  
  if (shiny){
    if (requireNamespace(package = "shiny", quietly = TRUE)){
      if (type %in% c("default", "message")){
        shiny::incProgress(amount = 1, detail = msg)
      } else {
        shiny::showNotification(msg, type = type)
      }
    }
  } else {
    if (type == "error"){
      stop(msg)
    } else if (type == "warning"){
      warning(msg)
    } else {
      if (verbose) message(paste("...", msg))
    }
  }
  
}


#' Round numeric columns of data.table
#' 
#' The object is modified in place, return value is NULL.
#' 
#' @param x A \code{data.table}.
#' @param digits Number of digits.
#' @noRd
round.data.table <- function(x, digits = 2L){
  column_classes <- sapply(x, function(column) is(column)[1])
  numeric_columns <- which(column_classes == "numeric")
  for (i in numeric_columns) x[, colnames(x)[i] := round(x[[i]], digits)]
  invisible( NULL )
}


#' @importFrom magrittr %>%
#' @export %>%
magrittr::`%>%`

#' @importFrom data.table as.data.table
#' @export as.data.table
data.table::as.data.table

default_template <- list(
  document = list(
    sAttribute = "text",
    format = c("### ","")
  )
)

cpos2id <- function(x, p_attribute, cpos){
  cl_cpos2id(
    corpus = x@corpus, registry = x@registry_dir,
    p_attribute = p_attribute, cpos = cpos
  )
}

cpos2struc <- function(x, s_attr, cpos){
  cl_cpos2struc(
    corpus = x@corpus, registry = x@registry_dir,
    s_attribute = s_attr, cpos = cpos
  )
}

struc2str <- function(x, s_attr, struc){
  struc_values <- cl_struc2str(
    corpus = x@corpus, registry = x@registry_dir,
    s_attribute = s_attr, struc = struc
  )
  Encoding(struc_values) <- x@encoding
  enc2native(struc_values)
}

regex2id <- function(x, p_attribute, regex){
  cl_regex2id(
    corpus = x@corpus, registry = x@registry_dir,
    p_attribute = p_attribute, regex = regex
  )
}

id2str <- function(x, p_attribute, id){
  str <- cl_id2str(
    corpus = x@corpus, registry = x@registry_dir,
    p_attribute = p_attribute, id = id
  )
  Encoding(str) <- x@encoding
  str
}


str2id <- function(x, p_attribute, str){
  cl_str2id(
    corpus = x@corpus, registry = x@registry_dir,
    p_attribute = p_attribute,
    str = as.corpusEnc(str, corpusEnc = x@encoding)
  )
}


#' @param corpus The id of a CWB corpus.
#' @param registry Path to the registry directory.
#' @param ... Structural attributes (set of length-one character vectors).
#' @noRd
siblings <- function(corpus, registry, ...){
  s_attrs <- unlist(list(...))
  stopifnot(all(s_attrs %in% s_attributes(corpus)))
  
  if (length(s_attrs) < 2L) return(NA)
  
  # If s-attributes have same number of values, we assume that they cover same
  # regions.

  s_attr_sizes <- lapply(
    s_attrs,
    function(s_attr){
      cl_attribute_size(
        corpus = corpus, registry = registry,
        attribute = s_attr, attribute_type = "s" 
      )
    }
  )
  do.call(identical, s_attr_sizes)
}
