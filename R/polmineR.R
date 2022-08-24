#' @include polmineR.R  S4classes.R
NULL


#' @importFrom graphics dotchart grid par plot points rect text
#' @importFrom stats formula ftable setNames xtabs
#' @importFrom utils View browseURL download.file setTxtProgressBar txtProgressBar untar write.csv
#' @importFrom tm TermDocumentMatrix DocumentTermMatrix
#' @importFrom tm as.TermDocumentMatrix as.DocumentTermMatrix
#' @importFrom data.table data.table setorderv dcast setnames setkeyv setcolorder as.data.table rbindlist setkey dcast.data.table
#' @importFrom Matrix rowSums colSums
#' @importFrom RcppCWB cqp_is_initialized cqp_initialize
#' @importFrom RcppCWB cl_attribute_size cl_lexicon_size cl_cpos2struc cl_cpos2id cl_struc2cpos cl_id2str cl_struc2str
#' @importFrom RcppCWB cl_id2str cl_struc2str cl_regex2id cl_str2id cl_cpos2str cl_id2freq cl_id2cpos cl_cpos2lbound cl_cpos2rbound
#' @importFrom RcppCWB cqp_query cqp_dump_subcorpus
NULL

# defined globaly to avoid R CMD check errors, as recommende by vignette in data.table package
`:=` <- function(...) NULL
.BY <- .GRP <- .SD <- .N <- NULL


setOldClass("htmlwidget")
setOldClass("quosure")


#' polmineR-package
#' 
#' A library for corpus analysis using the Corpus Workbench (CWB) as an
#' efficient back end for indexing and querying large corpora.
#' 
#' The package offers functionality to flexibly create partitions and to carry
#' out basic statistical operations (count, co-occurrences etc.). The original
#' full text of documents can be reconstructed and inspected at any time. Beyond
#' that, the package is intended to serve as an interface to packages
#' implementing advanced statistical procedures. Respective data structures
#' (document term matrices, term co- occurrence matrices etc.) can be created
#' based on the indexed corpora.
#' 
#' A session registry directory (see \code{registry()}) combines the registry
#' files for corpora that may reside in anywhere on the system. Upon loading
#' \code{polmineR}, the files in the registry directory defined by the
#' environment variable CORPUS_REGISTRY are copied to the session registry
#' directory. To see whether the environment variable CORPUS_REGISTRY is set,
#' use the `Sys.getenv()`-function. Corpora wrapped in R data packages can be
#' activated using the function \code{use()}.
#' 
#' The package includes a draft shiny app that can be called using
#' \code{polmineR()}.
#' 
#' @section Package options:
#' \itemize{
#'   \item{\emph{polmineR.p_attribute}:} {The default positional attribute}
#'   \item{\emph{polmineR.left}:} {Default value for left context.}
#'   \item{\emph{polmineR.lineview}:} {A logical value, whether ...}
#'   \item{\emph{polmineR.pagelength}:} {10L}
#'   \item{\emph{polmineR.meta}:} {}
#'   \item{\emph{polmineR.mc}:} {}
#'   \item{\emph{polmineR.cores}:} {}
#'   \item{\emph{polmineR.browse}:} {}
#'   \item{\emph{polmineR.buttons}:} {}
#'   \item{\emph{polmineR.specialChars}:} {}
#'   \item{\emph{polmineR.cutoff}:} {}
#'   \item{\emph{polmineR.mdsub}:}{A list of pairs of character vectors
#'   defining regular expression substitutions applied as part of preprocessing
#'   documents for html display. Intended usage: Remove characters that would be
#'   misinterpreted as markdown formatting instructions.}
#'   \item{\emph{polmineR.corpus_registry}:} {The system corpus registry
#'   directory defined by the environment variable CORPUS_REGISTRY before the
#'   polmineR package has been loaded. The polmineR package uses a temporary
#'   registry directory to be able to use corpora stored at multiple
#'   locations in one session. The path to the system corpus registry directory
#'   captures this setting to keep it available if necessary.}
#'   \item{\emph{polmineR.shiny}:} {A \code{logical} value, whether polmineR is
#'   used in the context of a shiny app. Used to control the apprearance of
#'   progress bars depending on whether shiny app is running, or not.}
#'   \item{\emph{polmineR.warn.size}:} {When generating HTML table widgets (e.g.
#'   when preparing kwic output to be displayed in RStudio's Viewe pane), the
#'   function \code{DT::datatable()} that is used internally will issue a
#'   warning by default if the object size of the table is greater than 1500000.
#'   The warning adresses a client-server scenario that is not applicable in the
#'   context of a local RStudio session, so you may want to turn it of.
#'   Internally, the warning can be suppressed by setting the option
#'   \code{DT.warn.size} to \code{FALSE}. The polmineR option
#'   \code{polmineR.warn.size} is processed by functions calling DT::datatable()
#'   to set and reset the value of \code{DT.warn.size}. Please note: The
#'   formulation of the warning does not match the scenario of a local RStudio
#'   session, but it may still be useful to get a warning when tables are large
#'   and slow to process. Therefore, the default value of the setting is
#'   \code{FALSE}.}
#'  }
#' @author Andreas Blaette (andreas.blaette@@uni-due.de)
#' @keywords package
#' @docType package
#' @aliases polmineR polmineR-package
#' @rdname polmineR-package
#' @name polmineR-package
#' @references Jockers, Matthew L. (2014): \emph{Text Analysis with R for Students of Literature}.
#' Cham et al: Springer.
#' @references Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum.
#' @export polmineR
#' @importFrom RcppCWB cqp_initialize cqp_is_initialized
#' @importFrom parallel detectCores
#' @examples
#' use("polmineR") # activate demo corpora included in the package
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' # The package includes two sample corpora
#' corpus("REUTERS") %>% show_info()
#' corpus("GERMAPARLMINI") %>% show_info()
#' 
#' # Core methods applied to corpus
#' 
#' C <- count("REUTERS", query = "oil")
#' C <- count("REUTERS", query = c("oil", "barrel"))
#' C <- count("REUTERS", query = '"Saudi" "Arab.*"', breakdown = TRUE, cqp = TRUE)
#' D <- dispersion("REUTERS", query = "oil", s_attribute = "id")
#' K <- kwic("REUTERS", query = "oil")
#' CO <- cooccurrences("REUTERS", query = "oil")
#' 
#' 
#' # Core methods applied to partition
#' 
#' kuwait <- partition("REUTERS", places = "kuwait", regex = TRUE)
#' C <- count(kuwait, query = "oil")
#' D <- dispersion(kuwait, query = "oil", s_attribute = "id")
#' K <- kwic(kuwait, query = "oil", meta = "id")
#' CO <- cooccurrences(kuwait, query = "oil")
#'
#'
#' # Go back to full text
#' 
#' p <- partition("REUTERS", id = 127)
#' if (interactive()) read(p)
#' h <- html(p)
#' h_highlighted <- highlight(h, highlight = list(yellow = "oil"))
#' if (interactive()) h_highlighted
#'
#'
#' # Generate term document matrix
#' 
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' cnt <- count(pb, p_attribute = "word")
#' tdm <- as.TermDocumentMatrix(cnt, col = "count")
#' @importFrom utils packageVersion
polmineR <- function(){
  # The code is adapted from the pkgload library
  # https://github.com/r-lib/pkgload/blob/master/R/utils.R
  .conditional_install <- function(pkg){
    v <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
    if (is.na(v)) {
      msg <- sprintf("Package '%s' needed to run the shiny app.", pkg)
      if (interactive()){
        message(sprintf("%s Would you like to install it?", msg))
        if (utils::menu(c("Yes", "No")) == 1) {
          utils::install.packages(pkg)
        } else {
          stop(msg, call. = FALSE)
        }
      } else {
        stop(msg, call. = FALSE)
      }
    }
  }
  
  .conditional_install("shiny")
  .conditional_install("shinythemes")
  .conditional_install("highlight")

  if (requireNamespace("shiny", quietly = TRUE) && requireNamespace("shinythemes", quietly = TRUE) && requireNamespace("highlight", quietly = TRUE)){
    shiny::onStop(function() options(polmineR.shiny = FALSE))
    shiny::runApp(system.file("shiny", package = "polmineR"))
  }
}



# setOldClass("dfmSparse") # class defined in quanteda-package
setOldClass("igraph")
setOldClass("html")


#' @exportMethod name
#' @noRd
setGeneric("name", function(x) standardGeneric("name"))

#' @exportMethod name<-
#' @noRd
setGeneric("name<-", function(x, value) standardGeneric("name<-"))


#' @title Generic methods defined in the polmineR package
#' @description This documentation object gives an overview over the generic
#'   methods defined in the polmineR package that have no individual man page
#'   but are documented directly with the classes they are defined for.
#' @param x An S4 class object.
#' @rdname polmineR-generics
#' @name polmineR-generics
NULL
