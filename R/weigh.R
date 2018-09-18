#' @include S4classes.R
NULL

#' Apply Weight to Matrix
#' 
#' @param .Object A \code{matrix}, or a \code{count}-object.
#' @param method the kind of weight to apply
#' @param ... further parameters
#' @exportMethod weigh
#' @docType methods
#' @rdname weigh-method
#' @name weigh
setGeneric("weigh", function(.Object, ...) standardGeneric("weigh") )

#' @importFrom slam row_sums col_sums
#' @importFrom tm nDocs
#' @rdname weigh-method
setMethod("weigh", "TermDocumentMatrix", function(.Object, method = "tfidf"){
  if (method == "tfidf"){
    .Object$v <- .Object$v/col_sums(.Object)[.Object$j] * log2(nDocs(.Object)/row_sums(.Object > 0))[.Object$i]  
    attr(.Object, "weighting") <- c(
      "term frequency - inverse document frequency (normalized)",
      "tf-idf"
    )
  } else if (method == "rel"){
    .Object$v <- .Object$v / col_sums(.Object)[.Object$j]
    attr(.Object, "weighting") <- c(
      "term frequency (normalized)",
      "tf-normalized"
    )
  } else if (method == "rank"){
    warning("not implemented")
  }
  return(.Object)
})


#' @rdname weigh-method
setMethod("weigh", "DocumentTermMatrix", function(.Object, method = "tfidf"){
  if (method=="tfidf"){
    .Object$v <- .Object$v/row_sums(.Object)[.Object$i] * log2(nDocs(.Object)/col_sums(.Object > 0))[.Object$j]  
    attr(.Object, "weighting") <- c(
      "term frequency - inverse document frequency (normalized)",
      "tf-idf"
    )
  } else if (method == "rel"){
    .Object$v <- .Object$v/row_sums(.Object)[.Object$i]
    attr(.Object, "weighting") <- c(
      "term frequency (normalized)",
      "tf-normalized"
    )
  } else if (method == "rank"){
    warning("not implemented")
  }
  return(.Object)
})



#' @param with A \code{data.table} used to weigh p-attributes. A column 'weight' with term weights is
#' required, and columns with the p-attributes of \code{.Object} for matching.
#' @examples 
#' \dontrun{
#' library(data.table)
#' if (require("zoo") && require("devtools") && require("magrittr")){
#' 
#' # Source in function 'get_sentiws' from a GitHub gist
#' gist_url <- file.path(
#'   "gist.githubusercontent.com",
#'   "PolMine",
#'   "70eeb095328070c18bd00ee087272adf",
#'   "raw",
#'   "c2eee2f48b11e6d893c19089b444f25b452d2adb",
#'   "sentiws.R"
#'  )
#'   
#' devtools::source_url(sprintf("https://%s", gist_url))
#' SentiWS <- get_sentiws()
#' 
#' # Do the statistical word context analysis
#' use("GermaParl")
#' options("polmineR.left" = 10L)
#' options("polmineR.right" = 10L)
#' df <- context("GERMAPARL", query = "Islam", p_attribute = c("word", "pos")) %>%
#'   partition_bundle(node = FALSE) %>% 
#'   set_names(s_attributes(., s_attribute = "date")) %>%
#'   weigh(with = SentiWS) %>%
#'   summary()
#' 
#' # Aggregate by year
#' df[["year"]] <- as.Date(df[["name"]]) %>% format("%Y-01-01")
#' df_year <- aggregate(df[,c("size", "positive_n", "negative_n")], list(df[["year"]]), sum)
#' colnames(df_year)[1] <- "year"
#' 
#' # Use shares instead of absolute counts 
#' df_year$negative_share <- df_year$negative_n / df_year$size
#' df_year$positive_share <- df_year$positive_n / df_year$size
#' 
#' # Turn it into zoo object, and plot it
#' Z <- zoo(
#'   x = df_year[, c("positive_share", "negative_share")],
#'   order.by = as.Date(df_year[,"year"])
#' )
#' plot(
#'   Z, ylab = "polarity", xlab = "year",
#'   main = "Word context of 'Islam': Share of positive/negative vocabulary",
#'   cex = 0.8,
#'   cex.main = 0.8
#' )
#' 
#' # Note that we can uses the kwic-method to check for the validity of our findings
#' words_positive <- SentiWS[weight > 0][["word"]]
#' words_negative <- SentiWS[weight < 0][["word"]]
#' kwic("GERMAPARL", query = "Islam", positivelist = c(words_positive, words_negative)) %>%
#'   highlight(lightgreen = words_positive, orange = words_negative) %>%
#'   tooltips(setNames(SentiWS[["word"]], SentiWS[["weight"]]))
#'   
#' }
#' }
#' @rdname weigh-method
setMethod("weigh", "count", function(.Object, with){
  setkeyv(x = .Object@stat, cols = .Object@p_attribute)
  setkeyv(x = with, cols = .Object@p_attribute)
  with_min <- with[, c(.Object@p_attribute, "weight"), with = FALSE]
  .Object@stat <- with_min[.Object@stat]
  .Object
})

#' @rdname weigh-method
setMethod("weigh", "count_bundle", function(.Object, with){
  .Object@objects <- lapply(.Object@objects, function(x) weigh(x, with = with))
  .Object
})
