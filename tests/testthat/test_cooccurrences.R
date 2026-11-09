library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("cooccurrences")

test_that(
  "cooccurrences-method for corpus",
  {
    y <- cooccurrences("REUTERS", query = "oil", p_attribute = "word")
    expect_equal(subset(y, count_ref > 0L)[["word"]][1:4], c("prices", "crude", "world", "markets"))
    expect_equal(y[["word"]][1:4], c("prices", "crude", "world", "markets"))
    
    y <- cooccurrences("REUTERS", query = '"barrel.*"', p_attribute = "word")
    expect_equal(subset(y, count_ref > 0L)[["word"]][1:5], c("dlrs", "a", "mln", "reserve", "brings"))
    expect_equal(y[["word"]][1:5], c("18", "dlrs", "per", "a", "mln"))
    
    # handle more than one p-attribute
    p_attrs <- c("word", "pos")
    dt <- corpus("GERMAPARLMINI") %>%
      cooccurrences(query = "Arbeit", p_attribute = p_attrs) %>%
      format()
    expect_true(all(p_attrs %in% colnames(dt)))

    expect_equal(
      cooccurrences("REUTERS", query = "asdfasdf", p_attribute = "word"),
      NULL
    )
    
    expect_equal(
      cooccurrences("REUTERS", query = '"asdfasdfasdfasd.*"', cqp = TRUE),
      NULL
    )
    
    
  }
)

test_that(
  "cooccurrences-method for partition",
  {
    P <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    
    y <- cooccurrences(P, query = "oil", p_attribute = "word")
    expect_equal(subset(y, count_ref > 0L)[["word"]][1:5], c("prices", "below", "its", "crude", "market"))
    expect_equal(y[["word"]][1:5], c("prices", "GCC", "boost", "each", "fall"))
    
    y <- cooccurrences(P, query = '"barrel.*"', cqp = TRUE, p_attribute = "word")
    expect_false(anyNA(y[["ll"]]))
    expect_true(all(c("10", "17.52", "18", "1986", "3.5") %in% subset(y, count_ref == 0L)[["word"]]))
    
    expect_equal(
      cooccurrences(P, query = "asdfasdf", p_attribute = "word"),
      NULL
    )
    
    expect_equal(
      cooccurrences(P, query = '"asdfasdfasdfasd.*"', cqp = TRUE, p_attribute = "word"),
      NULL
    )
  }
)


test_that(
  "Check log-likelihood formula",
  {
    cooc <- cooccurrences("GERMAPARLMINI", query = "Integration")
    cooc_dt <- cooc@stat[!is.na(ll)]
    data.table::setorderv(cooc_dt, cols = "ll", order = -1L)
    
    for (i in seq.int(from = 1L, to = 10L)){
      
      o11 <- cooc_dt[["count_coi"]][i]
      o12 <- cooc_dt[["count_ref"]][i]
      o21 <- cooc@size_coi - o11
      o22 <- cooc@size_ref - o12

      N <- o21 + o22 + o11 + o12
      
      e11 <- (o11 + o21) * ((o11 + o12) / N)
      e12 <- (o12 + o22) * ((o11 + o12) / N)
      e21 <- (o11 + o21) * ((o21 + o22) / N)
      e22 <- (o12 + o22) * ((o21 + o22) / N)
      
      # A term with an observed count of 0 is 0 (limit of o * log(o/e) for o -> 0).
      term <- function(o, e) if (o == 0L) 0 else o * log(o / e)
      ll <- 2 * (term(o11, e11) + term(o12, e12) + term(o21, e21) + term(o22, e22))

      expect_identical(round(cooc_dt[["ll"]][i], 3), round(ll, 3))
    }
  }
)





test_that(
  "Identity of cooccurrences and Cooccurrences",
  {
    testthat::skip_on_cran()
    rm <- noise(
      terms("REUTERS", p_attribute = "word"),
      specialChars = NULL, minNchar = 2L, stopwordsLanguage = "en"
    )
    stopwords <- unname(unlist(rm))
    r <- Cooccurrences("REUTERS", p_attribute = "word", left = 5L, right = 5L, stoplist = stopwords)
    stm <- as.simple_triplet_matrix(r)

    decode(r)
    spm <- as.sparseMatrix(r)

    coocs <- list(
      c("oil", "prices"),
      c("Saudi", "Arabia"),
      c("Sheikh", "Ali"),
      c("barrel", "dlrs")
    )

    lapply(
      coocs,
      function(tokens){
        a2b <- as.integer(as.matrix(stm[tokens[1], tokens[2]]))
        b2a <- as.integer(as.matrix(stm[tokens[2], tokens[1]]))
        expect_equal(a2b, b2a)

        expect_equal(spm[tokens[1], tokens[2]], spm[tokens[2], tokens[1]])

        a2b_cqp <- count("REUTERS", sprintf('"%s" []{0,4} "%s"', tokens[1], tokens[2]), cqp = TRUE)
        b2a_cqp <- count("REUTERS", sprintf('"%s" []{0,4} "%s"', tokens[2], tokens[1]), cqp = TRUE)
        expect_equal(a2b_cqp[["count"]] + b2a_cqp[["count"]], a2b)

        expect_equal(a2b_cqp[["count"]] + b2a_cqp[["count"]], spm[tokens[1], tokens[2]])

        expect_equal(
          a2b,
          cooccurrences("REUTERS", query = tokens[1], deduplicate = FALSE)@stat[word == tokens[2]][["count_coi"]]
        )

        NULL
    })



    ll(r)
    decode(r)

    a <- data.table::as.data.table(cooccurrences(r, query = "oil"))
    expect_false(anyNA(a[["ll"]]))
    # Cooccurrences() does not deduplicate by default
    b <- data.table::as.data.table(cooccurrences("REUTERS", query = "oil", deduplicate = FALSE))[!word %in% stopwords]
    expect_false(anyNA(b[["ll"]]))
    
    # Tokens may be counted several times if windows overlap, so count_coi can
    # exceed the count of a token in the corpus. The two methods derive expected
    # values differently in this scenario (using the corpus count or count_coi),
    # so they can be compared only for the remaining tokens.
    multicounted <- b[count_coi > count_partition][["word"]]
    a <- a[!word %in% multicounted]
    b <- b[!word %in% multicounted]
    
    library(data.table)
    setkeyv(a, cols = "word")
    setkeyv(b, cols = "word")
    m <- a[b]

    expect_equal(m[["count_coi"]], m[["i.count_coi"]])
    expect_equal(m[["obs_ref"]], m[["i.count_ref"]])
    expect_equal(m[["exp_coi"]], m[["i.exp_coi"]])
    expect_equal(m[["exp_ref"]], m[["i.exp_ref"]])
    expect_equal(m[["ll"]], m[["i.ll"]])
    expect_equal(a[["word"]][1:14], b[["word"]][1:14])
    
  }
)


test_that(
  "Cooccurences-method for subcorpus and partition objects",
  {
    testthat::skip_on_cran()
    merkel <- partition(
      "GERMAPARLMINI",
      speaker = "Merkel",
      date = "2009-11-10",
      interjection = "speech",
      regex = TRUE
    )
    merkel_cooc <- Cooccurrences(
      merkel,
      p_attribute = c("word", "pos"),
      left = 3L, right = 3L,
      verbose = TRUE
    )
    ll(merkel_cooc)
    decode(merkel_cooc)
    
    expect_identical(
      unique(merkel_cooc@stat[a_word == "und"][["a_count"]]),
      count(merkel, "und")[["count"]]
    )
    
    #######
    
    merkel_sc <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-10") %>%
      subset(grep("Merkel", speaker)) %>%
      subset(interjection == "speech")
      
    merkel_cooc_sc <- Cooccurrences(
      merkel_sc,
      p_attribute = c("word", "pos"),
      left = 3L, right = 3L,
      verbose = TRUE
    )
    ll(merkel_cooc_sc)
    decode(merkel_cooc_sc)
    
    expect_identical(
      unique(merkel_cooc_sc@stat[a_word == "und"][["a_count"]]),
      count(merkel_sc, "und", verbose = FALSE)[["count"]]
    )
    
    expect_identical(merkel_cooc@stat, merkel_cooc_sc@stat)
  }
)

test_that(
  "Cooccurrences with multiple p-attributes keeps id columns for ll()",
  {
    # Not skipped on CRAN, so that a plain R CMD check catches this.
    p <- partition(
      "GERMAPARLMINI",
      speaker = "Merkel",
      date = "2009-11-10",
      interjection = "speech",
      regex = TRUE
    )
    y <- Cooccurrences(
      p,
      p_attribute = c("word", "pos"),
      left = 3L, right = 3L,
      verbose = FALSE
    )
    expect_identical(
      colnames(y@window_sizes),
      c("a_word_id", "a_pos_id", "size_coi")
    )
    expect_identical(
      colnames(y@stat),
      c("a_word_id", "a_pos_id", "b_word_id", "b_pos_id", "ab_count")
    )
    ll(y, verbose = FALSE)
    expect_true("ll" %in% colnames(y@stat))
    expect_gt(sum(!is.na(y@stat[["ll"]])), 0L)
  }
)

test_that(
  "ll values are computed if observed counts are 0",
  {
    # Expected value from a complete contingency table. Cells with an observed
    # count of 0 contribute 0.
    g2 <- function(o11, o12, o21, o22){
      o <- matrix(c(o11, o21, o12, o22), nrow = 2L)
      e <- outer(rowSums(o), colSums(o)) / sum(o)
      2 * sum(ifelse(o == 0, 0, o * log(o / e)))
    }
    
    # context/cooccurrences (method for features class)
    y <- cooccurrences("REUTERS", query = "oil", p_attribute = "word")
    dt <- y@stat[count_ref == 0L][count_coi <= count_partition]
    expect_gt(nrow(dt), 0L)
    expect_false(anyNA(y@stat[["ll"]]))
    expect_equal(
      dt[["ll"]][1],
      g2(dt[["count_coi"]][1], 0L, y@size_coi - dt[["count_coi"]][1], y@size_ref)
    )
    
    # Cooccurrences
    p <- partition(
      "GERMAPARLMINI",
      speaker = "Merkel",
      date = "2009-11-10",
      interjection = "speech",
      regex = TRUE
    )
    r <- Cooccurrences(p, p_attribute = "word", left = 3L, right = 3L, verbose = FALSE)
    ll(r, verbose = FALSE)
    expect_false(anyNA(r@stat[["ll"]]))
    row <- r@stat[obs_ref == 0L][1]
    expect_equal(
      row[["ll"]],
      g2(row[["ab_count"]], 0L, row[["size_coi"]] - row[["ab_count"]], row[["size_ref"]])
    )
    
    # features
    f <- features(
      partition("GERMAPARLMINI", speaker = "Merkel", interjection = "speech", regex = TRUE, p_attribute = "word"),
      "GERMAPARLMINI", included = TRUE, method = "ll"
    )
    expect_gt(sum(f@stat[["count_ref"]] == 0L), 0L)
    expect_false(anyNA(f@stat[["ll"]]))
  }
)

test_that(
  "each corpus position is counted once if deduplicate is TRUE",
  {
    # TRUE is the default for cooccurrences()
    expect_identical(
      cooccurrences("REUTERS", query = "oil", deduplicate = TRUE)@stat,
      cooccurrences("REUTERS", query = "oil")@stat
    )
    
    # "domestic" occurs 3 times, 2 occurrences are in the windows of 2 matches
    # each ("low oil prices on the domestic oil industry"), 1 is in no window
    y <- cooccurrences("REUTERS", query = "oil", left = 5L, right = 5L, deduplicate = TRUE)
    dt <- y@stat[word == "domestic"]
    expect_identical(dt[["count_partition"]], 3L)
    expect_identical(dt[["count_coi"]], 2L)
    expect_identical(dt[["count_ref"]], 1L)
    expect_true(all(y@stat[["count_coi"]] <= y@stat[["count_partition"]]))
    expect_false(anyNA(y@stat[["ll"]]))
    expect_identical(y@size_coi + y@size_ref + y@size_match, y@size_partition)
    
    # Without deduplication, the token is counted 4 times
    y_dup <- cooccurrences("REUTERS", query = "oil", left = 5L, right = 5L, deduplicate = FALSE)
    expect_identical(y_dup@stat[word == "domestic"][["count_coi"]], 4L)
    expect_lt(y@size_coi, y_dup@size_coi)
    
    # The context retains the number of tokens in windows
    ctxt <- context("REUTERS", query = "oil", left = 5L, right = 5L)
    expect_identical(ctxt@stat[word == "domestic"][["count_coi"]], 4L)
    
    r <- Cooccurrences("REUTERS", p_attribute = "word", left = 5L, right = 5L, deduplicate = TRUE)
    a <- str2id(corpus("REUTERS"), p_attribute = "word", str = "oil")
    b <- str2id(corpus("REUTERS"), p_attribute = "word", str = "domestic")
    expect_identical(r@stat[a_id == a & b_id == b][["ab_count"]], 2L)
    # Not symmetric: 4 distinct positions of "oil" are in windows of "domestic"
    expect_identical(r@stat[a_id == b & b_id == a][["ab_count"]], 4L)
    
    # cooccurrences() and Cooccurrences() yield identical statistics
    ll(r, verbose = FALSE)
    decode(r)
    m <- merge(
      data.table::as.data.table(cooccurrences(r, query = "oil")),
      data.table::as.data.table(y),
      by = "word"
    )
    expect_gt(nrow(m), 0L)
    expect_equal(m[["count_coi.x"]], m[["count_coi.y"]])
    expect_equal(m[["ll.x"]], m[["ll.y"]])
  }
)

test_that(
  "Cooccurrences with deduplicate = TRUE matches a brute force count",
  {
    p <- partition("GERMAPARLMINI", speaker = "Merkel", date = "2009-11-10", interjection = "speech", regex = TRUE)
    p_attrs <- c("word", "pos")
    ids <- lapply(
      setNames(nm = p_attrs),
      function(x) RcppCWB::cpos2id(corpus = "GERMAPARLMINI", p_attribute = x, cpos = 0L:(size("GERMAPARLMINI") - 1L), registry = p@registry_dir)
    )
    # Pairs of node and position in window, each position once per node
    windows <- data.table::rbindlist(lapply(
      seq_len(nrow(p@cpos)),
      function(i) data.table::rbindlist(lapply(
        p@cpos[i, 1]:p@cpos[i, 2],
        function(node){
          w <- c((node - 2L):(node - 1L), (node + 1L):(node + 4L))
          w <- w[w >= p@cpos[i, 1] & w <= p@cpos[i, 2]]
          list(a_word_id = rep(ids$word[node + 1L], length(w)), a_pos_id = rep(ids$pos[node + 1L], length(w)), b_cpos = w)
        }
      ))
    ))
    windows <- unique(windows, by = c("a_word_id", "a_pos_id", "b_cpos"))
    windows[, "b_word_id" := ids$word[windows[["b_cpos"]] + 1L]]
    windows[, "b_pos_id" := ids$pos[windows[["b_cpos"]] + 1L]]
    
    # one p-attribute
    brute <- unique(windows, by = c("a_word_id", "b_cpos"))[, .N, by = c("a_word_id", "b_word_id")]
    data.table::setnames(brute, c("a_word_id", "b_word_id"), c("a_id", "b_id"))
    r <- Cooccurrences(p, p_attribute = "word", left = 2L, right = 4L, deduplicate = TRUE)
    joined <- merge(brute, r@stat, by = c("a_id", "b_id"), all = TRUE)
    expect_identical(joined[["N"]], joined[["ab_count"]])
    
    # several p-attributes (symmetric window, see the swap of left and right)
    windows3 <- data.table::rbindlist(lapply(
      seq_len(nrow(p@cpos)),
      function(i) data.table::rbindlist(lapply(
        p@cpos[i, 1]:p@cpos[i, 2],
        function(node){
          w <- setdiff((node - 3L):(node + 3L), node)
          w <- w[w >= p@cpos[i, 1] & w <= p@cpos[i, 2]]
          list(a_word_id = rep(ids$word[node + 1L], length(w)), a_pos_id = rep(ids$pos[node + 1L], length(w)), b_cpos = w)
        }
      ))
    ))
    windows3 <- unique(windows3, by = c("a_word_id", "a_pos_id", "b_cpos"))
    windows3[, "b_word_id" := ids$word[windows3[["b_cpos"]] + 1L]]
    windows3[, "b_pos_id" := ids$pos[windows3[["b_cpos"]] + 1L]]
    id_cols <- c("a_word_id", "a_pos_id", "b_word_id", "b_pos_id")
    brute <- windows3[, .N, by = id_cols]
    m <- Cooccurrences(p, p_attribute = p_attrs, left = 3L, right = 3L, deduplicate = TRUE)
    joined <- merge(brute, m@stat, by = id_cols, all = TRUE)
    expect_identical(joined[["N"]], joined[["ab_count"]])
  }
)

test_that(
  "helper to count each corpus position once",
  {
    # "oil prices oil": 1 for "prices" as a cooccurrence of "oil", 2 for "oil" as
    # a cooccurrence of "prices"
    ids <- c(1L, 2L, 1L)
    expect_identical(
      polmineR:::.cooccurrence_pairs(ids, offset = -1L, offsets_done = integer()),
      list(a = c(2L, 1L), b = c(1L, 2L))
    )
    expect_identical(
      polmineR:::.cooccurrence_pairs(ids, offset = 1L, offsets_done = -1L),
      list(a = 2L, b = 1L)
    )
  }
)

test_that(
  "Cooccurrences with several p-attributes respects left and right",
  {
    p <- partition("GERMAPARLMINI", speaker = "Merkel", date = "2009-11-10", interjection = "speech", regex = TRUE)
    p_attrs <- c("word", "pos")
    id_cols <- c("a_word_id", "a_pos_id", "b_word_id", "b_pos_id")
    ids <- lapply(
      setNames(nm = p_attrs),
      function(x) RcppCWB::cpos2id(corpus = "GERMAPARLMINI", p_attribute = x, cpos = 0L:(size("GERMAPARLMINI") - 1L), registry = p@registry_dir)
    )
    brute <- function(left, right, deduplicate){
      offsets <- c(if (left > 0L) -left:-1L else integer(), if (right > 0L) 1L:right else integer())
      windows <- data.table::rbindlist(lapply(
        seq_len(nrow(p@cpos)),
        function(i) data.table::rbindlist(lapply(
          p@cpos[i, 1]:p@cpos[i, 2],
          function(node){
            w <- node + offsets
            w <- w[w >= p@cpos[i, 1] & w <= p@cpos[i, 2]]
            list(a_word_id = rep(ids$word[node + 1L], length(w)), a_pos_id = rep(ids$pos[node + 1L], length(w)), b_cpos = w)
          }
        ))
      ))
      if (deduplicate) windows <- unique(windows, by = c("a_word_id", "a_pos_id", "b_cpos"))
      windows[, "b_word_id" := ids$word[windows[["b_cpos"]] + 1L]]
      windows[, "b_pos_id" := ids$pos[windows[["b_cpos"]] + 1L]]
      windows
    }
    for (window in list(c(2L, 4L), c(0L, 1L), c(1L, 0L))){
      for (deduplicate in c(FALSE, TRUE)){
        m <- Cooccurrences(p, p_attribute = p_attrs, left = window[1], right = window[2], deduplicate = deduplicate)
        windows <- brute(window[1], window[2], deduplicate)
        joined <- merge(windows[, .N, by = id_cols], m@stat, by = id_cols, all = TRUE)
        expect_identical(joined[["N"]], joined[["ab_count"]])
        sizes <- merge(windows[, .N, by = c("a_word_id", "a_pos_id")], m@window_sizes, by = c("a_word_id", "a_pos_id"), all = TRUE)
        expect_identical(sizes[["N"]], sizes[["size_coi"]])
      }
    }
    
    # Results for one p-attribute and several p-attributes are consistent
    w <- Cooccurrences(p, p_attribute = "word", left = 0L, right = 1L)
    m <- Cooccurrences(p, p_attribute = p_attrs, left = 0L, right = 1L)
    expect_identical(sum(w@stat[["ab_count"]]), sum(m@stat[["ab_count"]]))
  }
)
