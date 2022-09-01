#' @include textstat.R features.R context.R S4classes.R cooccurrences.R
NULL

#' @rdname pmi
#' @param ... Arguments methods may require.
setGeneric("pmi", function(.Object, ...) standardGeneric("pmi") )

#' Calculate Pointwise Mutual Information (PMI).
#' 
#' Calculate Pointwise Mutual Information as an information-theoretic approach
#' to find collocations.
#' 
#' Pointwise mutual information (PMI) is calculated as follows (see
#' Manning/Schuetze 1999):
#' \deqn{I(x,y) = log\frac{p(x,y)}{p(x)p(y)}}{I(x,y) = log(p(x,y)/(p(x)p(y)))}
#' 
#' The formula is based on maximum likelihood estimates: When we know the number
#' of observations for token x, \eqn{o_{x}}{o(x)}, the number of observations
#' for token y, \eqn{o_{y}}{o(y)} and the size of the corpus N, the
#' propabilities for the tokens x and y, and for the co-occcurence of x and y
#' are as follows:
#' \deqn{p(x) = \frac{o_{x}}{N}}{p(x) = o(x) / N}
#' \deqn{p(y) = \frac{o_{y}}{N}}{p(y) = o(y) / N}
#' 
#' The term p(x,y) is the number of observed co-occurrences of x and y.
#' 
#' Note that the computation uses log base 2, not the natural logarithm you find
#' in examples (e.g. \url{https://en.wikipedia.org/wiki/Pointwise_mutual_information}).
#' @param .Object An object.
#' @rdname pmi
#' @references Manning, Christopher D.; Schuetze, Hinrich (1999): \emph{Foundations of Statistical Natural Language
#' Processing}. MIT Press: Cambridge, Mass., pp. 178-183.
#' @family statistical methods
#' @examples
#' y <- cooccurrences("REUTERS", query = "oil", method = "pmi")
#' N <- size(y)[["partition"]]
#' I <- log2((y[["count_coi"]]/N) / ((count(y) / N) * (y[["count_partition"]] / N)))
setMethod("pmi", "context", function(.Object){
  .Object@stat[, "pmi" := log2((.Object@stat[["count_coi"]]/.Object@size_partition)/((.Object@count/.Object@size_partition)*(.Object@stat[["count_partition"]]/.Object@size_partition)))]
  setorderv(.Object@stat, cols = "pmi")
  .Object@stat[, "rank_pmi" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "pmi")
  invisible(.Object)
})


#' @rdname pmi
#' @export
setMethod("pmi", "Cooccurrences", function(.Object){
  if (!"a_count" %in% colnames(.Object) || !"b_count" %in% colnames(.Object)) enrich(.Object)
  p_ab <- .Object@stat[["ab_count"]] / .Object@partition@size
  p_a <- .Object@stat[["a_count"]] / .Object@partition@size
  p_b <- .Object@stat[["b_count"]] / .Object@partition@size
  .Object@stat[, "pmi" := log2(p_ab / (p_a * p_b))]
  setorderv(.Object@stat, cols = "pmi", order = -1L, na.last = TRUE)
  .Object@stat[, "rank_pmi" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "pmi")
  invisible(.Object)
})

#' @param p_attribute The positional attribute which shall be considered. Relevant only
#'   if ngrams have been calculated for more than one p-attribute.
#' @param observed A \code{count}-object with the numbers of the observed
#'   occurrences of the tokens in the input \code{ngrams} object.
#' @rdname pmi
#' @export
#' @examples 
#' use("polmineR")
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' dt <- decode(
#'   "REUTERS",
#'   p_attribute = "word",
#'   s_attribute = character(), 
#'   to = "data.table",
#'   verbose = FALSE
#' )
#' n <- ngrams(dt, n = 2L, p_attribute = "word")
#' obs <- count("REUTERS", p_attribute = "word")
#' phrases <- pmi(n, observed = obs)
setMethod("pmi", "ngrams", function(.Object, observed, p_attribute = p_attributes(.Object)[1]){
  if (length(p_attribute) > 1L) stop("pmi-method for ngrams objects not yet implemented for length(p_attribute) > 1")
  
  setkeyv(observed@stat, cols = p_attribute)
  setnames(.Object@stat, old = "count", new = "ngram_count")
  
  for (i in 1L:.Object@n){
    setkeyv(.Object@stat, cols = paste(p_attribute, i, sep = "_"))
    .Object@stat <- .Object@stat[observed@stat[,c(p_attribute, "count"), with = FALSE]]
    setnames(.Object@stat, old = "count", new = paste(p_attribute, i, "count", sep = "_"))
  }
  
  p_ngram <- .Object@stat[["ngram_count"]] / observed@size
  denominator <- 1L
  for (i in 1L:.Object@n){
    denominator <- denominator * .Object@stat[[paste(p_attribute, i, "count", sep = "_")]] / observed@size
  }
  
  .Object@stat[, "pmi" := log2(p_ngram / denominator)]
  setorderv(.Object@stat, cols = "pmi", order = -1L)
  .Object@stat[, "rank_pmi" := 1L:nrow(.Object@stat)]
  invisible(.Object)
})


#' Compute Log-likelihood Statistics.
#' 
#' Apply the log-likelihood statistic to detect cooccurrences or keywords.
#' 
#' The log-likelihood test to detect cooccurrences is a standard approach to
#' find collocations (Dunning 1993, Evert 2005, 2009).
#' 
#' (a) The basis for computing for the log-likelihood statistic is a contingency
#' table of observationes, which is prepared for every single token in the
#' corpus. It reports counts for a token to inspect and all other tokens in a
#' corpus of interest (coi) and a reference corpus (ref):
#' \tabular{rccc}{
#'   \tab coi   \tab ref \tab TOTAL \cr
#'   count token \tab \eqn{o_{11}}{o11} \tab \eqn{o_{12}}{o12} \tab \eqn{r_{1}}{r1} \cr
#'   other tokens \tab \eqn{o_{21}}{o21} \tab \eqn{o_{22}}{o22} \tab \eqn{r_{2}}{r2} \cr
#'   TOTAL \tab \eqn{c_{1}}{c1} \tab \eqn{c_{2}}{c2} \tab N
#' }
#' (b) Based on the contingency table(s) with observed counts, expected values
#' are calculated for each cell, as the product of the column and margin sums,
#' divided by the overall number of tokens (see example).
#' 
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
#' however the critical discussion of Ulrike Tabbert (2015: 84ff).
#' 
#' The implementation in the \code{ll}-method uses a vectorized approach of the
#' computation, which is substantially faster than iterating the rows of a
#' table, generating individual contingency tables etc. As using the standard
#' formula is not significantly slower than relying on the simplified formula,
#' polmineR has moved to the standard computation.
#' 
#' An inherent difficulty of the log likelihood statistic is that it is not
#' possible to compute the statistical test value if the number of observed
#' counts in the reference corpus is 0, i.e. if a term only occurrs exclusively
#' in the neighborhood of a node word. When filtering out rare words from the
#' result table, respective \code{NA} values will usually disappear.
#' @references Dunning, Ted (1993): Accurate Methods for the Statistics of
#'   Surprise and Coincidence. \emph{Computational Linguistics}, Vol. 19, No. 1,
#'   pp. 61-74.
#' @references Rayson, Paul; Garside, Roger (2000): Comparing Corpora using
#'   Frequency Profiling. \emph{The Workshop on Comparing Corpora}. 
#'   \url{https://aclanthology.org/W00-0901/}.
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
#' @family statistical methods
#' @exportMethod ll
#' @rdname ll
#' @examples 
#' # use ll-method explicitly
#' oil <- cooccurrences("REUTERS", query = "oil", method = NULL)
#' oil <- ll(oil)
#' oil_min <- subset(oil, count_coi >= 3)
#' if (interactive()) View(format(oil_min))
#' summary(oil)
#' 
#' # use ll-method on 'Cooccurrences'-object
#' \dontrun{
#' R <- Cooccurrences("REUTERS", left = 5L, right = 5L, p_attribute = "word")
#' ll(R)
#' decode(R)
#' summary(R)
#' }
#' 
#' # use log likelihood test for feature extraction
#' x <- partition(
#'   "GERMAPARLMINI", speaker = "Merkel",
#'   interjection = "speech", regex = TRUE,
#'   p_attribute = "word"
#' )
#' f <- features(x, y = "GERMAPARLMINI", included = TRUE, method = "ll")
#' f <- features(x, y = "GERMAPARLMINI", included = TRUE, method = NULL)
#' f <- ll(f)
#' summary(f)
#' 
#' \dontrun{
#' 
#' # A sample do-it-yourself calculation for log-likelihood:
#' # Compute ll-value for query "oil", and "prices"
#' 
#' oil <- context("REUTERS", query = "oil", left = 5, right = 5)
#' 
#' # (a) prepare matrix with observed values
#' o <- matrix(data = rep(NA, 4), ncol = 2) 
#' o[1,1] <- as(oil, "data.table")[word == "prices"][["count_coi"]]
#' o[1,2] <- count("REUTERS", query = "prices")[["count"]] - o[1,1]
#' o[2,1] <- size(oil)[["coi"]] - o[1,1]
#' o[2,2] <- size(oil)[["ref"]] - o[1,2]
#' 
#' 
#' # (b) prepare matrix with expected values, calculate margin sums first
#' r <- rowSums(o)
#' c <- colSums(o)
#' N <- sum(o)
#' 
#' e <- matrix(data = rep(NA, 4), ncol = 2) # matrix with expected values
#' e[1,1] <- r[1] * (c[1] / N)
#' e[1,2] <- r[1] * (c[2] / N)
#' e[2,1] <- r[2] * (c[1] / N)
#' e[2,2] <- r[2] * (c[2] / N)
#' 
#' 
#' # (c) compute log-likelihood value
#' ll_value <- 2 * (
#'   o[1,1] * log(o[1,1] / e[1,1]) +
#'   o[1,2] * log(o[1,2] / e[1,2]) +
#'   o[2,1] * log(o[2,1] / e[2,1]) +
#'   o[2,2] * log(o[2,2] / e[2,2])
#' )
#' 
#' df <- as.data.frame(cooccurrences("REUTERS", query = "oil"))
#' subset(df, word == "prices")[["ll"]]
#' }
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
  setorderv(.Object@stat, cols = "ll", order = -1L, na.last = TRUE)
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
  
  setorderv(dt, cols = "ll", order = -1, na.last = TRUE)
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
#' The basis for computing for the chi square test is a contingency table of
#' observationes, which is prepared for every single token in the corpus. It
#' reports counts for a token to inspect and all other tokens in a corpus of
#' interest (coi) and a reference corpus (ref):
#' \tabular{rccc}{
#' \tab coi   \tab ref \tab TOTAL\cr
#' count token \tab \eqn{o_{11}}{o11}  \tab \eqn{o_{12}}{o12} \tab \eqn{r_{1}}{r1}\cr
#' other tokens \tab \eqn{o_{21}}{o21}    \tab \eqn{o_{22}}{o22} \tab \eqn{r_{2}}{r2}\cr
#' TOTAL \tab \eqn{c_{1}}{c1}    \tab \eqn{c_{2}}{c2} \tab N\cr
#' }
#' Based on the contingency table, expected values are calculated for each cell,
#' as the product of the column and margin sums, divided by the overall number
#' of tokens (see example). The standard formula for calculating the chi-square
#' test is computed as follows. \deqn{X^{2} = \sum{\frac{(O_{ij} -
#' E_{ij})^2}{O_{ij}}}}{X2 = (o11 - e11)^2/e11 + (o12 - e12)^2/e12 + (o12 -
#' e12)^2/e12 + (o22 - e22)^2/e22}
#' Results from the chisquare test are only robust for at least 5 observed
#' counts in the corpus of interest. Usually, results need to be filtered
#' accordingly (see examples).
#' @param .Object A \code{features} object, or an object inheriting from it
#'   (\code{context}, \code{cooccurrences}).
#' @exportMethod chisquare
#' @return Same class as input object, with enriched table in the
#'   \code{stat}-slot.
#' @references Manning, Christopher D.; Schuetze, Hinrich (1999):
#'   \emph{Foundations of Statistical Natural Language Processing}. MIT Press:
#'   Cambridge, Mass., pp. 169-172.
#' @references Kilgarriff, A. and Rose, T. (1998): Measures for corpus
#'   similarity and homogeneity. \emph{Proc. 3rd Conf. on Empirical Methods in
#'   Natural Language Processing}. Granada, Spain, pp 46-52.
#' @author Andreas Blaette
#' @family statistical methods
#' @rdname chisquare-method
#' @keywords textstatistics
#' @examples
#' use("polmineR")
#' library(data.table)
#' m <- partition(
#'   "GERMAPARLMINI", speaker = "Merkel", interjection = "speech",
#'   regex = TRUE, p_attribute = "word"
#' )
#' f <- features(m, "GERMAPARLMINI", included = TRUE)
#' f_min <- subset(f, count_coi >= 5)
#' summary(f_min)
#' 
#' \dontrun{
#' 
#' # A sample do-it-yourself calculation for chisquare:
#' 
#' # (a) prepare matrix with observed values
#' o <- matrix(data = rep(NA, 4), ncol = 2) 
#' o[1,1] <- as.data.table(m)[word == "Weg"][["count"]]
#' o[1,2] <- count("GERMAPARLMINI", query = "Weg")[["count"]] - o[1,1]
#' o[2,1] <- size(f)[["coi"]] - o[1,1]
#' o[2,2] <- size(f)[["ref"]] - o[1,2]
#' 
#' 
#' # prepare matrix with expected values, calculate margin sums first
#' 
#' r <- rowSums(o)
#' c <- colSums(o)
#' N <- sum(o)
#' 
#' e <- matrix(data = rep(NA, 4), ncol = 2) 
#' e[1,1] <- r[1] * (c[1] / N)
#' e[1,2] <- r[1] * (c[2] / N)
#' e[2,1] <- r[2] * (c[1] / N)
#' e[2,2] <- r[2] * (c[2] / N)
#' 
#' 
#' # compute chisquare statistic
#' 
#' y <- matrix(rep(NA, 4), ncol = 2)
#' for (i in 1:2) for (j in 1:2) y[i,j] <- (o[i,j] - e[i,j])^2 / e[i,j]
#' chisquare_value <- sum(y)
#' 
#' as(f, "data.table")[word == "Weg"][["chisquare"]]
#' }
setGeneric("chisquare", function(.Object){standardGeneric("chisquare")})

#' @rdname chisquare-method
setMethod("chisquare", "features", function(.Object){
  size_total <- .Object@size_coi + .Object@size_ref
  count_x_total <- .Object@stat[["count_coi"]] + .Object@stat[["count_ref"]]
  count_notx_coi <- .Object@size_coi - .Object@stat[["count_coi"]]
  count_notx_ref <- .Object@size_ref - .Object@stat[["count_ref"]]
  count_notx_total <- size_total - count_x_total
  digits_restore_value <- options(digits = 20)
  on.exit(options(digits = digits_restore_value[["digits"]]))
  exp_x_coi <- (count_x_total / size_total) * .Object@size_coi
  exp_x_ref <- (count_x_total / size_total) * .Object@size_ref
  exp_notx_coi <- (count_notx_total/size_total) * .Object@size_coi
  exp_notx_ref <- (count_notx_total/size_total) * .Object@size_ref
  chi1 <- ((exp_x_coi - .Object@stat[["count_coi"]]) ** 2) / exp_x_coi
  chi2 <- ((exp_x_ref - .Object@stat[["count_ref"]]) ** 2) / exp_x_ref
  chi3 <- ((exp_notx_coi - count_notx_coi) ** 2) / exp_notx_coi
  chi4 <- ((exp_notx_ref - count_notx_ref) ** 2) / exp_notx_ref
  chi <- chi1 + chi2 + chi3 + chi4
  chi <- chi * ifelse(.Object@stat[["count_coi"]] >= exp_x_coi, 1L, -1L)
  .Object@stat[, "exp_coi" := exp_x_coi]
  .Object@stat[, "chisquare" := chi]
  setorderv(.Object@stat, cols = "chisquare", order = -1L)
  .Object@stat[, "rank_chisquare" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "chisquare")
  invisible(.Object)
})


#' @rdname chisquare-method
setMethod("chisquare", "context", function(.Object) callNextMethod(.Object))

#' @rdname chisquare-method
setMethod("chisquare", "cooccurrences", function(.Object) callNextMethod(.Object))


#' @include S4classes.R
NULL

#' Perform t-test.
#' 
#' Compute t-scores to find collocations.
#' 
#' The calculation of the t-test is based on the formula
#' \deqn{t = \frac{\overline{x} - \mu}{\sqrt{\frac{s^2}{N}}}}{t = (x - u) / sqrt(s^2 / N)}
#' where \eqn{\mu}{u} is the mean of the distribution, x the sample mean,
#' \eqn{s^2}{s^2} the sample variance, and N the sample size.
#' 
#' Following Manning and Schuetze (1999), to test whether two tokens (a and b)
#' are a collocation, the sample mean \eqn{\mu}{u} is the number of observed
#' co-occurrences of a and b divided by corpus size N:
#' \deqn{\mu = \frac{o_{ab}}{N}}{u = o(ab) / N}
#' 
#' For the mean of the distribution \eqn{\overline{x}}{x}, maximum likelihood estimates
#' are used. Given that we know the number of observations of token a, \eqn{o_{a}}{o(a)}, the
#' number of observations of b, \eqn{o_{b}}{o(b)} and the size of the corpus N, the
#' propabilities for the tokens a and b, and for the co-occcurence of a and be
#' are as follows, if independence is assumed:
#' \deqn{P(a) = \frac{o_{a}}{N}}{P(a) = o(a) / N}
#' \deqn{P(b) = \frac{o_{b}}{N}}{P(b) = o(b) / N}
#' \deqn{P(ab) = P(a)P(b)}{P(ab) = P(a) * P(b)}
#' 
#' See the examples for a sample calulation of the t-test, and Evert (2005: 83)
#' for a critical discussion of the "highly questionable" assumptions when using
#' the t-test for detecting co-occurrences.
#' @param .Object A \code{context} or \code{features} object
#' @references Manning, Christopher D.; Schuetze, Hinrich (1999):
#'   \emph{Foundations of Statistical Natural Language Processing}. MIT Press:
#'   Cambridge, Mass., pp. 163-166.
#' @references Church, Kenneth W. et al. (1991): Using Statistics in Lexical
#'   Analysis. In: Uri Zernik (ed.), \emph{Lexical Acquisition}. Hillsdale,
#'   NJ:Lawrence Erlbaum, pp. 115-164
#'   \doi{https://doi.org/10.4324/9781315785387-8}
#'   
#' @references Evert, Stefan (2005): \emph{The Statistics of Word Cooccurrences.
#'   Word Pairs and Collocations.} URN urn:nbn:de:bsz:93-opus-23714.
#'   \url{https://elib.uni-stuttgart.de/bitstream/11682/2573/1/Evert2005phd.pdf}
#' @rdname t_test
#' @name t_test
#' @family statistical methods
#' @examples
#' use("polmineR")
#' y <- cooccurrences("REUTERS", query = "oil", left = 1L, right = 0L, method = "t_test")
#' # The critical value (for a = 0.005) is 2.579, so "crude" is a collocation
#' # of "oil" according to t-test.
#' 
#' # A sample calculation
#' count_oil <- count("REUTERS", query = "oil")
#' count_crude <- count("REUTERS", query = "crude")
#' count_crude_oil <- count("REUTERS", query = '"crude" "oil"', cqp = TRUE)
#' 
#' p_crude <- count_crude$count / size("REUTERS")
#' p_oil <- count_oil$count / size("REUTERS")
#' p_crude_oil <- p_crude * p_oil
#' 
#' x <- count_crude_oil$count / size("REUTERS")
#'
#' t_value <- (x - p_crude_oil) / sqrt(x / size("REUTERS"))
#' # should be identical with previous result:
#' as.data.frame(subset(y, word == "crude"))$t_test
setGeneric("t_test", function(.Object) standardGeneric("t_test") )

#' @rdname t_test
setMethod("t_test", "context", function(.Object){
  p_random <- (.Object@stat[["count_partition"]] / .Object@size_partition) * ( .Object@count / .Object@size_partition)
  p_sample <- .Object@stat[["count_coi"]] / .Object@size_partition
  t_values <- (p_sample - p_random) / sqrt( p_sample / .Object@size_partition )
  .Object@stat[, "t-score" := t_values]
  setorderv(x = .Object@stat, cols = "t-score", order = -1L)
  .Object@stat[, "rank_t" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "t_test")
  invisible(.Object)
})

