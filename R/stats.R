#' @include textstat.R features.R context.R S4classes.R cooccurrences.R
NULL

#' @rdname pmi
setGeneric("pmi", function(.Object) standardGeneric("pmi") )

#' Calculate Pointwise Mutual Information
#' @param .Object An object.
#' @rdname pmi
setMethod("pmi", "context", function(.Object){
  .Object@stat[, "pmi" := log2((.Object@stat[["count_window"]]/.Object@size_partition)/((.Object@count/.Object@size_partition)*(.Object@stat[["count_partition"]]/.Object@size_partition)))]
  .Object <- sort(.Object, by="pmi")
  .Object@stat[, "rank_pmi" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "pmi")
  invisible(.Object)
})



#' Compute Log-likelihood Statistics.
#' 
#' Apply the log-likelihood statistic to detect cooccurrences or keywords.
#' 
#' The log-likelihood test to detect cooccurrences is a standard approach to
#' find collocations (Dunning 1993, Evert 2005, 2009).
#' (a) The basis for computing for the log-likelihood statistic is a contingency
#' table of observationes, which is prepared for every single token in the
#' corpus. It reports counts for a token to inspect and all other tokens in a
#' corpus of interest (coi) and a reference corpus (ref):
#' \tabular{rccc}{
#' \tab coi   \tab ref \tab TOTAL\cr
#' count token \tab \eqn{o_{11}}{o11}  \tab \eqn{o_{12}}{o12} \tab \eqn{r_{1}}{r1}\cr
#' other tokens \tab \eqn{o_{21}}{o21}    \tab \eqn{o_{22}}{o22} \tab \eqn{r_{2}}{r2}\cr
#' TOTAL \tab \eqn{c_{1}}{c1}    \tab \eqn{c_{2}}{c2} \tab N\cr
#' }
#' (b) Based on the contingency table(s) with observed counts, expected values
#' are calculated for each cell, as the product of the column and margin sums,
#' divided by the overall number of tokens.
#' (c) The standard formula for calculating the log-likelihood test is as
#' follows.
#' \deqn{G^{2} = 2 \sum{O_{ij} log(\frac{O_{ij}}{E_{ij}})}}{G2 = 2(o11 *
#' log(o11/e11) + o12 * log(o12/e12) + o21 * log(o21/e21) + o22 * log(o22/e22))}
#' Note: Before polmineR v0.7.11, a simplification of the formula was used 
#' (Rayson/Garside 2000), which omits the third and fourth term of the previous
#' formula:
#' \deqn{ll = 2(o_{11} log (\frac{o_{11}}{E_{11}}) + o_{12} log(\frac{o_{12}}{E_{12}}))}{ll =
#' 2*((o11 * log (o11/e11)) + (o12 * log (e12/e12)))}
#' There is a (small) gain of computational efficiency using this simplified
#' formula and the result is almost identical with the standard formula; see
#' however the critical discussion of Ulrike Tabbert (2015: 84ff). The
#' implementation in the \code{ll}-method uses a vectorized approach of the
#' computation, which is substantially faster than iterating the rows of a
#' table, generating individual contingency tables etc. As using the standard
#' formula is not significantly slower than relying on the simplified formula,
#' polmineR has moved to the standard computation.
#' A notorious difficulty of the log likelihood statistic is that it is not
#' possible to compute the statistical test value if the number of observed
#' counts in the reference corpus is 0, i.e. if a term only occurrs exclusively
#' in the neighborhood of a node word. When filtering out rare words from the
#' result table, respective \code{NA} values will usually disappear.
#' @references Dunning, Ted (1993): Accurate Methods for the Statistics of
#'   Surprise and Coincidence. \emph{Computational Linguistics}, Vol. 19, No. 1,
#'   pp. 61-74.
#' @references Rayson, Paul; Garside, Roger (2000): Comparing Corpora using
#'   Frequency Profiling. \emph{The Workshop on Comparing Corpora}. 
#'   \url{http://aclweb.org/anthology/W00-0901}.
#' @references Evert, Stefan (2005): \emph{The Statistics of Word Cooccurrences.
#'   Word Pairs and Collocations.} URN urn:nbn:de:bsz:93-opus-23714.
#'   \url{https://elib.uni-stuttgart.de/bitstream/11682/2573/1/Evert2005phd.pdf}
#' @references Evert, Stefan (2009). Corpora and Collocations. In: A. Ludeling
#'   and M. Kyto (eds.), \emph{Corpus Linguistics. An International Handbook}. Mouton
#'   de Gruyter, Berlin, pp. 1212-1248 (ch. 58).
#' @references Tabbert, Ulrike (2015): \emph{Crime and Corpus. The Linguistic
#'   Representation of Crime in the Press}. Amsterdam: Benjamins.
#' @param .Object An object of class \code{cooccurrence}, \code{context}, or
#'   \code{features}.
#' @param ... Further arguments (such as \code{verbose}).
#' @exportMethod ll
#' @rdname ll
setGeneric("ll", function(.Object, ...) standardGeneric("ll") )



#' @rdname ll
setMethod("ll", "features", function(.Object){
  
  o21 <- .Object@size_coi - .Object@stat[["count_coi"]]
  o22 <- .Object@size_ref - .Object@stat[["count_ref"]]
  
  N <- .Object@size_ref + .Object@size_coi
  r1 <- .Object@stat[["count_ref"]] + .Object@stat[["count_coi"]]
  .Object@stat[, "exp_coi" := .Object@size_coi * (r1 / N)] # equivalent to e11
  .Object@stat[, "exp_ref" := .Object@size_ref * (r1 / N)] # equivalent to e12
  r2 <- o21 + o22
  e21 <- .Object@size_coi * (r2 / N) 
  e22 <- .Object@size_ref * (r2 / N)

  f11 <- .Object@stat[["count_coi"]] / .Object@stat[["exp_coi"]]
  f12  <- .Object@stat[["count_ref"]] / .Object@stat[["exp_ref"]]
  f21 <- o21 / e21
  f22 <- o22 / e22
  
  bracket <- .Object@stat[["count_coi"]] * log(f11) + .Object@stat[["count_ref"]] * log(f12) + o21 * log(f21) + o22 * log(f22)
  direction <- ifelse(.Object@stat[["count_coi"]] >= .Object@stat[["exp_coi"]], 1L, -1L)
  .Object@stat[, "ll" := 2 * bracket * direction]
  .Object@stat[, "ll" := ifelse(is.nan(ll), NA, ll)]
  .Object <- sort(.Object, by = "ll")
  .Object@stat[, "rank_ll" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "ll")
  .Object
})

#' @rdname ll
setMethod("ll", "context", function(.Object) callNextMethod(.Object))

#' @rdname ll
setMethod("ll", "cooccurrences", function(.Object) callNextMethod(.Object))


#' @rdname ll
#' @param verbose Logical, whether to output messages.
setMethod("ll", "Cooccurrences", function(.Object, verbose = TRUE){
  
  a_cols_id <- if (length(.Object@p_attribute) == 1L) "a_id" else paste("a", .Object@p_attribute, "id", sep = "_")
  b_cols_id <- if (length(.Object@p_attribute) == 1L) "b_id" else paste("b", .Object@p_attribute, "id", sep = "_")
  
  if (verbose) message("... adding window size")
  
  setkeyv(.Object@window_sizes, a_cols_id)
  dt <- .Object@stat # due to the reference logic of data.tables, manipulating dt changes .Object@stat; 
  
  setkeyv(dt, a_cols_id)
  dt[, "size_coi" := .Object@window_sizes[dt][["size_coi"] ]]
  
  if (nrow(.Object@partition@stat) > 0L){
    cnt <- .Object@partition@stat
  } else {
    cnt <- count(.Object@partition, p_attribute = .Object@p_attribute, decode = FALSE)@stat
  }

  
  setkeyv(cnt, paste(.Object@p_attribute, "id", sep = "_"))
  
  setkeyv(dt, cols = a_cols_id)
  dt[, "a_count" := cnt[dt][["count"]] ]
  
  setkeyv(dt, cols = b_cols_id)
  dt[, "b_count" := cnt[dt][["count"]] ]
  
  if (verbose) message('... log likelihood-Test')
  
  N <- .Object@partition@size - dt[["a_count"]]
  dt[, "size_ref" := N - dt[["size_coi"]] ]
  
  dt[, "obs_ref" := dt[["b_count"]] - dt[["ab_count"]] ]
  
  # it is possible that matches for a cooccurrence are counted multiple
  # times, resulting in a obs_ref value below 0
  dt[, "obs_ref":= ifelse(dt[["obs_ref"]] < 0L, 0L, dt[["obs_ref"]])]
  dt[, "o21" := dt[["size_coi"]] - dt[["ab_count"]] ]
  dt[, "o22" := dt[["size_ref"]] - dt[["obs_ref"]] ]
  
  exp_1 <- dt[["b_count"]] / N
  dt[, "exp_coi" := dt[["size_coi"]] * exp_1]
  dt[, "exp_ref" := dt[["size_ref"]] * exp_1]
  dt[, "e21" := dt[["size_coi"]] * ((dt[["o21"]] + dt[["o22"]]) / (dt[["size_coi"]] + dt[["size_ref"]])) ]
  dt[, "e22" := dt[["size_ref"]] * ((dt[["o21"]] + dt[["o22"]]) / (dt[["size_coi"]] + dt[["size_ref"]])) ]
  
  ll <- 2 * (
    dt[["ab_count"]] * log(dt[["ab_count"]] / dt[["exp_coi"]]) + 
    dt[["obs_ref"]] * log(dt[["obs_ref"]] / dt[["exp_ref"]]) + 
    dt[["o21"]] * log(dt[["o21"]]/dt[["e21"]]) + 
    dt[["o22"]] * log(dt[["o22"]]/dt[["e22"]])
  )
  
  direction <- ifelse(dt[["ab_count"]] >= dt[["exp_coi"]], 1L, -1L)
  dt[, "ll" := ll * direction]
  dt[, "ll" := ifelse(is.nan(ll), NA, ll)]
  
  setorderv(dt, cols = "ll", order = -1)
  dt[, "rank_ll" := 1L:nrow(dt)]
  
  dt[, "o21" := NULL]
  dt[, "o22" := NULL]
  dt[, "e21" := NULL]
  dt[, "e22" := NULL]
  
  invisible(.Object)
  
})



#' Perform chisquare-text.
#' 
#' Perform Chisquare-Test based on a table with counts
#' 
#' This function deliberately uses a self-made chi-square test for performance
#' reason
#' 
#' @param .Object object
#' @param ... further parameters
#' @exportMethod chisquare
#' @return a table
#' @author Andreas Blaette
#' @rdname chisquare-method
#' @keywords textstatistics
setGeneric("chisquare", function(.Object, ...){standardGeneric("chisquare")})

#' @rdname chisquare-method
setMethod("chisquare", "features", function(.Object){
  size_coi <- .Object@size_coi
  size_ref <- .Object@size_ref
  size_total <- size_coi + size_ref
  count_x_coi <- .Object@stat[["count_coi"]]
  count_x_ref <- .Object@stat[["count_ref"]]
  count_x_total <- count_x_coi + count_x_ref
  count_notx_coi <- size_coi - count_x_coi
  count_notx_ref <- size_ref - count_x_ref
  count_notx_total <- size_total - count_x_total
  options(digits = 20)
  exp_x_coi <- (count_x_total / size_total) * size_coi
  exp_x_ref <- (count_x_total / size_total) * size_ref
  exp_notx_coi <- (count_notx_total/size_total) * size_coi
  exp_notx_ref <- (count_notx_total/size_total) * size_ref
  chi1 <- ((exp_x_coi - count_x_coi) ** 2) / exp_x_coi
  chi2 <- ((exp_x_ref - count_x_ref) ** 2) / exp_x_ref
  chi3 <- ((exp_notx_coi - count_notx_coi) ** 2) / exp_notx_coi
  chi4 <- ((exp_notx_ref - count_notx_ref) ** 2) / exp_notx_ref
  chi <- chi1 + chi2 + chi3 + chi4
  chi <- chi * apply(cbind(count_x_coi, exp_x_coi), 1, function(x) ifelse(x[1] > x[2], 1, -1))
  options(digits = 7)
  .Object@stat[, "exp_coi" := exp_x_coi]
  .Object@stat[, "chisquare" := chi]
  .Object <- sort(.Object, by = "chisquare")
  .Object@stat[, "rank_chisquare" := 1:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "chisquare")
  return(.Object)
})


#' @rdname chisquare-method
setMethod("chisquare", "context", function(.Object) callNextMethod(.Object))

#' @rdname chisquare-method
setMethod("chisquare", "cooccurrences", function(.Object) callNextMethod(.Object))