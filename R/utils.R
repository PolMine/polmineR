#' Get the corpus positions for a query
#' 
#' Get the corpus positions for a query in a given partition
#' 
#' The function works for single words, which is the quick optin.
#' Also, you can use the full CQP syntax. In this case, make sure to
#' use single quotes, and double quotes for individual words
#' (see example below). Input needs to be in UTF-8
#' 
#' @param query a query (encoding: UTF-8), see details
#' @param partition a partition object
#' @param pAttribute either 'word' or 'lemma', not used 
#' @return a data frame with two columns with cpos (start and end positions of hits)
#' @author Andreas Blaette
#' @example
#' \dontrun{
#' foo <- .queryCpos("Integration", partitionObject)
#' foo <- .queryCpos('"Menschen" "mit" "Migrationshintergrund"', partitionObject)
#' }
#' @noRd
.queryCpos <- function(query, Partition, pAttribute=drillingControls$pAttribute) {
  query <- .adjustEncoding(query, Partition@encoding)
  if (grepl('"', query) == FALSE) {
    pAttr <- paste(Partition@corpus, '.', pAttribute, sep='')
    cpos <- try(cqi_id2cpos(pAttr, cqi_str2id(pAttr, query)), silent=TRUE)
    if (is(cpos)[1] != "try-error"){
      hits <- as.matrix(data.frame(cpos, cpos))
    }
  } else if (grepl('"', query) == TRUE) {
    cqi_query(Partition@corpus, "Hits", query)
    cpos <- try(cqi_dump_subcorpus(paste(Partition@corpus, ":Hits", sep="")), silent=TRUE)
    if(is(cpos)[1] != "try-error"){
      hits <- matrix(cpos[,1:2], ncol=2)
    }
  }
  if (is(cpos)[1] != "try-error") {
    strucHits <- cqi_cpos2struc(
      paste(
        Partition@corpus, ".",
        names(Partition@sattributes)[length(Partition@sattributes)],
        sep=""),
      hits[,1]
      )
    hits <- hits[which(strucHits %in% Partition@strucs),]
    if (is(hits)[1] == "integer") {
      hits <- matrix(hits, nrow=1)
    }
  } else {
    hits = NULL
  }
  hits
}


#' get the values of a s-attribute in a corpus
#' 
#' The function is called by \code{partition}
#' 
#' @param corpus e.g. "PLPRBTTXT"
#' @param sattribute a structural attribute, e.g. "text_year"
#' @return returns a character vector
#' @author Andreas Blaette
#' @noRd
.sattribute2values <- function(corpus, sattribute){
  s <- paste(corpus, ".", sattribute, sep="")
  unique(cqi_struc2str(s, c(1:cqi_attribute_size(s))))
}

#' The current time 
#' 
#' Helper function for ouput to estimate the time that functions require
#' @noRd
.takeoff <- function(){strsplit(date(), ' ')[[1]][5]}

#' bla
#'
#' is called by: 
#' @noRd
.restrictionsCmd <- function(sattributes) {
  sapply(
    names(sattributes),
    function(x) paste("?", x,"=/",
                      gsub('\\s','.', paste(sattributes[[x]], collapse='\\|')),
                      "/",sep=""))
}

.umlaute2punkt <- function(cmd){
  return(gsub('[\u00e4\u00f6\u00fc\u00df\u00c4\u00d6\u00dc]','.', cmd))
}

#' @param ns character string, namespace to be searched
#' @param class character, class to be looked for
#' @return a list with the partitions found in the namespace
#' @noRd
.getClassObjects <- function(ns, class) {
  rawList <- sapply(ls(ns), function(x) class(get(x, ns))[1])
  list <- rawList[rawList %in% class]
  sapply(names(list), function(x) get(x, ns), USE.NAMES=TRUE)
}

#' adjust encoding
#' 
#' use case: encoding of console input is UTF-8, the corpus encoding is latin-1
#'
#' @param characterVector the character vector to be adjusted
#' @param partitionEncoding the encoding to be set
#' @return a character vector with adjusted encoding
#' @noRd
.adjustEncoding <- function(characterVector, partitionEncoding){
  if (!drillingControls$consoleEncoding==partitionEncoding) {
    characterVector <- iconv(
      characterVector,
      from=drillingControls$consoleEncoding,
      to=partitionEncoding)
  }
  return(characterVector)
}

# #include <Rcpp.h>
# using namespace Rcpp;
# 
# // [[Rcpp::export]]
# IntegerVector cpos2struc(IntegerVector x){
#   int max_struc = which_max(x);
#   IntegerVector result(max_struc);    
#   int max_cpos = x.size();
#   int count = 0;
#   int cpos = 0;
#   for (int i = 0; i < max_struc; i++) {
#     if (x[i] == count+1) {
#       result[count] = cpos;
#       count++;
#       cpos = i;
#     }
#   }
#   return(result);
# } 
# 
