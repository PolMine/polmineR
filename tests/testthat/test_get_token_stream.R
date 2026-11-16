library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

test_that(
  "get_token_stream()-method - decode entire corpus",
  {
    fulltext <- corpus("REUTERS") %>%
      get_token_stream(p_attribute = "word")
    
    expect_identical(length(fulltext), 4050L)
    expect_identical(
      head(fulltext),
      c("Diamond", "Shamrock", "Corp", "said", "that", "effective")
    )
  }
)


test_that(
  "get_token_stream()-method for numeric input object",
  {
    expect_identical(
      get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word"),
      c("Guten", "Morgen", ",", "meine", "sehr", "verehrten", "Damen", "und", "Herren", "!")
    )
    
    # Still need to think hard how to test for the encoding
    ts <- get_token_stream(0:25, corpus = "GERMAPARLMINI", p_attribute = "word", encoding = "latin1")
    
    # check argument cpos = TRUE
    expect_identical(
      names(get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", cpos = TRUE)),
      as.character(0:9)
    )
    
    expect_identical(
      get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", cutoff = 5L),
      c("Guten", "Morgen", ",", "meine", "sehr")
    )
    
    expect_warning(
      get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", cutoff = 5L, collapse = TRUE)
    )
    
    expect_error(
      get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", cutoff = 5L, collapse = c(" ", "!"))
    )
    
    expect_identical(
      get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", collapse = " ", beautify = FALSE),
      "Guten Morgen , meine sehr verehrten Damen und Herren !"
    )
    
    expect_identical(
      get_token_stream(0:9, corpus = "REUTERS", p_attribute = "word", collapse = " "),
      "Diamond Shamrock Corp said that effective today it had cut"
    )
    

    expect_identical(
      get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", decode = FALSE),
      0:9
    )
  }
)


test_that(
  "get_token_stream()-method for numeric input object with collapse",
  {
    expect_identical(
      get_token_stream(0:9, corpus = "GERMAPARLMINI", p_attribute = "word", collapse = " "),
      "Guten Morgen, meine sehr verehrten Damen und Herren!"
    )
  }
)



test_that(
  "get_token_stream()-method for matrix input",
  {
    rngs <- matrix(c(0L,9L,10L,25L), ncol = 2L, byrow = TRUE)
    ts_rm <- get_token_stream(rngs, corpus = "GERMAPARLMINI", p_attribute = "word", encoding = "latin1", collapse = " ")
    expect_identical(nchar(ts_rm), 159L)
    
    r <- new("regions", cpos = rngs, corpus = "GERMAPARLMINI", encoding = "latin1")
    ts_r <- get_token_stream(r, p_attribute = "word", collapse = " ")
    expect_identical(ts_rm, ts_r)
  }
)


test_that(
  "get_token_stream()-method - decode partition, subcorpus and subcorpus_bundle",
  {
    skip_on_cran()
    
    reuters_1 <- corpus("REUTERS") %>% subset(id == "127") %>% get_token_stream(p_attribute = "word")
    expect_identical(length(reuters_1), 92L)
    expect_identical(table(reuters_1)[["the"]], 4L)
    
    reuters_p <- partition("REUTERS", id = "127") %>% get_token_stream(p_attribute = "word")
    expect_identical(length(reuters_p), 92L)
    expect_identical(table(reuters_p)[["the"]], 4L)
    expect_identical(reuters_1, reuters_p)
    
    
    y <- corpus("REUTERS") %>% split(s_attribute = "id") %>% get_token_stream(p_attribute = "word")
    expect_identical(y[["127"]], reuters_1)
    
    skip_on_cran()
    # When decode = FALSE, result required to be list of integer vectors
    int_list <- corpus("REUTERS") %>%
      split(s_attribute = "id", verbose = FALSE) %>%
      get_token_stream(p_attribute = "word", decode = FALSE, verbose = FALSE)
    expect_true(all(unlist(lapply(int_list, is.integer))))
  }
)

test_that(
  "",
  {
    y <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-10") %>%
      subset(speaker == "Angela Dorothea Merkel") %>%
      as("String")
    expect_identical(nchar(y), 51328L)
  }
)

test_that(
  "get_token_stream() with two attributes", 
  {
    skip_on_cran()
    sp <- corpus("GERMAPARLMINI") %>%
      as.speeches(s_attribute_name = "speaker", s_attribute_date = "date", progress = FALSE)
    p2 <- get_token_stream(sp, p_attribute = c("word", "pos"), verbose = FALSE)
    
    spl <- strsplit(p2[[1]], "//")
    word <- sapply(spl, `[`, 1L)
    pos <- sapply(spl, `[`, 1L)
    expect_identical(
      word[1:100], get_token_stream(sp[[1]], p_attribute = "word")[1:100]
    )
    
    # Apply filter
    p_sub <- get_token_stream(
      sp, p_attribute = c("word", "pos"),
      subset = {!grepl("\\$.$", pos)}
    )
    expect_identical(length(grep("\\$.$", p_sub[[1]])), 0L)
  }
)




test_that(
  "Check workflow to filter subcorpus_bundle",
  {
    sp <- corpus("GERMAPARLMINI") %>%
      as.speeches(s_attribute_name = "speaker", s_attribute_date = "date", progress = FALSE)
    queries <- c('"freiheitliche" "Grundordnung"', '"Bundesrepublik" "Deutschland"' )
    
    phr <- corpus("GERMAPARLMINI") %>%
      cpos(query = queries) %>%
      as.phrases(corpus = "GERMAPARLMINI")
    
    kill <- tm::stopwords("de")
    assign("kill", tm::stopwords("de"), envir = .GlobalEnv)
    
    ts_phr <- get_token_stream(
      sp,
      p_attribute = c("word", "pos"),
      subset = {!word %in% kill  & !grepl("(\\$.$|ART)", pos)},
      phrases = phr,
      progress = FALSE,
      verbose = FALSE
    )
    
    testthat::expect_identical(
      FALSE,
      any(tm::stopwords("de") %in% gsub("^(.*?)//.*?$", "\\1", ts_phr[[1]]))
    )
    
  }
)





test_that(
  "get_token_stream() with subset, collapse and beautify (#290)",
  {
    sc <- corpus("GERMAPARLMINI") %>%
      subset(protocol_date == "2009-10-27") %>%
      subset(speaker == "Volker Kauder")
    # The subset expression is evaluated in the method, so the tokens to drop
    # are stated in the expression rather than in a variable.
    kept <- get_token_stream(
      sc, p_attribute = "word",
      subset = {!word %in% c("der", "die", "das", "und", polmineR::punctuation)}
    )
    collapsed <- get_token_stream(
      sc, p_attribute = "word", collapse = " ",
      subset = {!word %in% c("der", "die", "das", "und", polmineR::punctuation)}
    )
    # Previously, tokens were recycled, so the string was longer than the
    # string without subset.
    expect_identical(collapsed, paste(kept, collapse = " "))
    expect_lt(
      nchar(collapsed),
      nchar(get_token_stream(sc, p_attribute = "word", collapse = " "))
    )

    # corpus positions are selected like tokens
    with_cpos <- get_token_stream(
      sc, p_attribute = "word", cpos = TRUE,
      subset = {!word %in% c("der", "die", "das", "und", polmineR::punctuation)}
    )
    expect_identical(unname(with_cpos), kept)
    all_tokens <- get_token_stream(sc, p_attribute = "word")
    expect_identical(
      as.integer(names(with_cpos)),
      RcppCWB::ranges_to_cpos(slot(sc, "cpos"))[!all_tokens %in% c("der", "die", "das", "und", polmineR::punctuation)]
    )
  }
)


test_that(
  "beautify does not recycle tokens if the last token is an opening bracket (#290)",
  {
    reg <- registry()
    bracket <- RcppCWB::cl_id2cpos(
      "GERMAPARLMINI", p_attribute = "word",
      id = RcppCWB::cl_str2id("GERMAPARLMINI", p_attribute = "word", str = "(", registry = reg),
      registry = reg
    )[1]
    cpos <- (bracket - 3L):bracket
    tokens <- get_token_stream(cpos, corpus = "GERMAPARLMINI", p_attribute = "word")
    expect_identical(tail(tokens, 1L), "(")
    y <- get_token_stream(cpos, corpus = "GERMAPARLMINI", p_attribute = "word", collapse = " ")
    expect_identical(substr(y, nchar(y), nchar(y)), "(")
    expect_identical(
      y,
      paste(paste(polmineR:::.whitespace(tokens), tokens, sep = ""), collapse = "")
    )
  }
)


test_that(
  "beautify works for a partition_bundle with one object (#290)",
  {
    pb <- corpus("GERMAPARLMINI") %>%
      subset(protocol_date == "2009-10-27") %>%
      split(s_attribute = "speaker")
    y <- get_token_stream(pb[1], p_attribute = "word", beautify = TRUE, verbose = FALSE)
    expect_identical(
      lengths(y),
      lengths(get_token_stream(pb[1], p_attribute = "word", verbose = FALSE))
    )
  }
)


test_that(
  ".whitespace() returns a vector of the length of tokens",
  {
    expect_identical(polmineR:::.whitespace(character()), character())
    expect_identical(polmineR:::.whitespace("a"), "")
    expect_identical(
      polmineR:::.whitespace(c("a", "(", "b", ")", ".", "c", "(")),
      c("", " ", "", "", "", " ", " ")
    )
    expect_identical(
      polmineR:::.whitespace(c("a", "b", "c", "d"), no_space = c(3L, 10L)),
      c("", " ", "", " ")
    )
  }
)
