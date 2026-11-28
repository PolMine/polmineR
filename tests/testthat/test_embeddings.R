library(polmineR)
use("polmineR")

# Random vectors for the regions of s-attribute 'speaker' of GERMAPARLMINI
# stand in for embeddings computed with a language model.
n_speaker <- RcppCWB::cl_attribute_size(
  corpus = "GERMAPARLMINI", attribute = "speaker", attribute_type = "s",
  registry = registry()
)
set.seed(42)
m <- matrix(rnorm(n_speaker * 8L), nrow = n_speaker)
emb <- embeddings("GERMAPARLMINI", vectors = m, s_attribute = "speaker")
kauder <- corpus("GERMAPARLMINI") |> subset(speaker == "Volker Kauder")


test_that(
  "embeddings() creates an embeddings object with backend 'matrix'",
  {
    expect_s4_class(emb, "embeddings")
    expect_s4_class(emb, "corpus")
    expect_identical(length(emb), n_speaker)
    expect_identical(slot(emb, "dim"), 8L)
    expect_equal(rowSums(slot(emb, "vectors")^2), rep(1, n_speaker))
    expect_true(validObject(emb))
    expect_s4_class(embeddings(kauder, vectors = m, s_attribute = "speaker"), "embeddings")
  }
)

test_that(
  "embeddings() checks input",
  {
    expect_error(embeddings("GERMAPARLMINI", vectors = m[-1L, ], s_attribute = "speaker"))
    expect_error(embeddings("GERMAPARLMINI", vectors = m))
    expect_error(embeddings("GERMAPARLMINI"), "does not declare embeddings")
  }
)

test_that(
  "semantic_search() with a numeric vector as query (corpus)",
  {
    hits <- semantic_search("GERMAPARLMINI", query = m[1L, ], embeddings = emb, k = 5L)
    expect_s4_class(hits, "semantic_hits")
    expect_s4_class(hits, "subcorpus")
    expect_length(slot(hits, "similarity"), 5L)
    expect_true(validObject(hits))

    # the region the query vector is taken from is the best hit
    dt <- as.data.table(hits)
    expect_identical(dt[["struc"]][1], 0L)
    expect_equal(dt[["similarity"]][1], 1)
    expect_identical(dt[["rank"]], 1:5)

    # brute force reference
    v <- slot(emb, "vectors")
    sim <- as.vector(v %*% (m[1L, ] / sqrt(sum(m[1L, ]^2))))
    expect_identical(dt[["struc"]], order(-sim)[1:5] - 1L)

    # regions are in corpus order
    expect_false(is.unsorted(slot(hits, "cpos")[, 1]))
    expect_identical(
      slot(hits, "cpos"),
      RcppCWB::get_region_matrix(
        "GERMAPARLMINI", s_attribute = "speaker", strucs = slot(hits, "strucs"),
        registry = registry()
      ) |> unname()
    )
  }
)

test_that(
  "semantic_search() with a subcorpus as query (more like this)",
  {
    hits <- semantic_search("GERMAPARLMINI", query = kauder, embeddings = emb, k = 5L)
    expect_length(slot(hits, "strucs"), 5L)
    expect_length(intersect(slot(hits, "strucs"), slot(kauder, "strucs")), 0L)
    expect_s4_class(slot(hits, "query"), "subcorpus")

    # With a single region as query, the region itself is the best hit,
    # unless it is excluded.
    k1 <- kauder
    slot(k1, "cpos") <- slot(kauder, "cpos")[1L, , drop = FALSE]
    slot(k1, "strucs") <- slot(kauder, "strucs")[1L]
    hits_incl <- semantic_search(
      "GERMAPARLMINI", query = k1, embeddings = emb, k = 5L,
      exclude_query = FALSE
    )
    dt <- as.data.table(hits_incl)
    expect_identical(dt[["struc"]][1], slot(k1, "strucs"))
    expect_equal(dt[["similarity"]][1], 1)
    hits_excl <- semantic_search("GERMAPARLMINI", query = k1, embeddings = emb, k = 5L)
    expect_false(slot(k1, "strucs") %in% slot(hits_excl, "strucs"))
  }
)

test_that(
  "semantic_search() is restricted to the regions of a subcorpus",
  {
    spd <- corpus("GERMAPARLMINI") |> subset(party == "SPD")
    hits <- semantic_search(spd, query = kauder, embeddings = emb, k = 10L)
    expect_identical(s_attributes(hits, "party"), "SPD")
    expect_true(all(slot(hits, "strucs") %in% slot(spd, "strucs")))
  }
)

test_that(
  "semantic_search() for a subcorpus_bundle returns hits for each subcorpus",
  {
    sc_bundle <- corpus("GERMAPARLMINI") |> split(s_attribute = "party", verbose = FALSE)
    hits <- semantic_search(sc_bundle, query = kauder, embeddings = emb, k = 3L)
    expect_s4_class(hits, "subcorpus_bundle")
    expect_identical(names(hits), names(sc_bundle))
    expect_true(all(sapply(slot(hits, "objects"), is, "semantic_hits")))
    for (p in names(hits)){
      expect_identical(s_attributes(hits[[p]], "party"), p)
    }
  }
)

test_that(
  "subset() keeps similarity values aligned",
  {
    hits <- semantic_search("GERMAPARLMINI", query = kauder, embeddings = emb, k = 20L)
    party <- s_attributes(hits, "party", unique = FALSE)
    p <- names(sort(table(party), decreasing = TRUE))[1]
    hits_party <- subset(hits, party == !!p)
    expect_s4_class(hits_party, "semantic_hits")
    expect_true(validObject(hits_party))
    expect_identical(slot(hits_party, "strucs"), slot(hits, "strucs")[party == p])
    expect_identical(slot(hits_party, "similarity"), slot(hits, "similarity")[party == p])
    expect_identical(slot(hits_party, "s_attribute_strucs"), "speaker")
  }
)

test_that(
  "methods for subcorpora can be applied to semantic_hits",
  {
    hits <- semantic_search("GERMAPARLMINI", query = kauder, embeddings = emb, k = 5L)
    expect_s4_class(count(hits, p_attribute = "word"), "count")
    expect_identical(size(hits), sum(slot(hits, "cpos")[, 2] - slot(hits, "cpos")[, 1] + 1L))
  }
)

test_that(
  "semantic_search() checks the query",
  {
    old <- options(polmineR.encoder = NULL)
    on.exit(options(old))
    # embeddings with backend "matrix" do not state a model
    expect_error(semantic_search("GERMAPARLMINI", query = "Klimaschutz", embeddings = emb), "do not state a model")
    expect_error(semantic_search("GERMAPARLMINI", query = 1:3, embeddings = emb), "dimensions")

    # a custom encoder
    options(polmineR.encoder = function(x, model, revision, ...) m[2L, , drop = FALSE])
    hits <- semantic_search("GERMAPARLMINI", query = "Klimaschutz", embeddings = emb, k = 1L)
    expect_identical(slot(hits, "strucs"), 1L)
    expect_identical(slot(hits, "query"), "Klimaschutz")
  }
)

test_that(
  "embeddings objects can be serialized",
  {
    f <- tempfile(fileext = ".rds")
    saveRDS(emb, f)
    emb2 <- readRDS(f)
    expect_identical(
      as.data.table(semantic_search("GERMAPARLMINI", query = kauder, embeddings = emb2, k = 3L)),
      as.data.table(semantic_search("GERMAPARLMINI", query = kauder, embeddings = emb, k = 3L))
    )
  }
)


# A directory with the metadata 'hf download --local-dir' writes, standing in
# for a local copy of a model.
fake_model_dir <- function(revision){
  d <- file.path(tempfile(), "gbert-test")
  dir.create(file.path(d, ".cache", "huggingface", "download"), recursive = TRUE)
  writeLines(c(revision, "etag", "123"), file.path(d, ".cache", "huggingface", "download", "config.json.metadata"))
  d
}

emb_model <- embeddings(
  "GERMAPARLMINI", vectors = m, s_attribute = "speaker",
  model = "test/gbert-test", model_revision = "abc123"
)

test_that(
  "encoder() creates an encoder from embeddings, corpus and corpus ID",
  {
    d <- fake_model_dir("abc123")
    enc <- encoder(emb_model, path = d)
    expect_s4_class(enc, "sentence_encoder")
    expect_s4_class(enc, "encoder")
    expect_true(is.function(enc))
    expect_identical(slot(enc, "model"), "test/gbert-test")
    expect_identical(slot(enc, "revision"), "abc123")
    expect_identical(slot(enc, "revision_check"), "verified")
    expect_identical(slot(enc, "path"), normalizePath(d))
    expect_identical(slot(enc, "dimensions"), 8L)
    expect_identical(slot(enc, "s_attribute"), "speaker")
    expect_true(is.na(slot(enc, "query_prefix")))
    out <- capture.output(show(enc), type = "message")
    expect_true(any(grepl("verified", out)))
    expect_true(any(grepl("loaded on first use", out)))

    # local model found via option polmineR.model_dir
    old <- options(polmineR.model_dir = dirname(d))
    on.exit(options(old))
    expect_identical(slot(encoder(emb_model), "path"), normalizePath(d))
    options(old)

    # without a local copy: Hugging Face hub
    expect_true(is.na(slot(encoder(emb_model), "path")))

    # corpus and corpus ID: the embeddings declared for the corpus
    expect_error(encoder("GERMAPARLMINI"), "does not declare embeddings")
    expect_error(encoder(corpus("GERMAPARLMINI")), "does not declare embeddings")
  }
)

test_that(
  "encoder() checks model and revision",
  {
    expect_error(encoder(emb), "do not state a model")
    expect_error(encoder(emb_model, path = fake_model_dir("other")), "revision")
    expect_warning(
      enc <- encoder(emb_model, path = dirname(fake_model_dir("abc123"))),
      "cannot determine"
    )
    expect_identical(slot(enc, "revision_check"), "unknown")
    expect_error(encoder(emb_model, path = tempfile()), "not found")
  }
)


# Tests that load a model require the Python package sentence-transformers
# and a local copy of the model the embeddings of GERMAPARL2 were computed
# with: Set the environment variable POLMINER_TEST_ENCODER_MODEL to its
# directory.
encoder_model <- Sys.getenv("POLMINER_TEST_ENCODER_MODEL")

test_that(
  "an encoder reproduces stored vectors",
  {
    skip_if(encoder_model == "", "POLMINER_TEST_ENCODER_MODEL not set")
    # py_module_available() would import the module, and with it PyTorch,
    # before the encoder can handle the OpenMP runtimes (see ?encoder)
    skip_if_not(
      !is.null(reticulate::import("importlib.util")$find_spec("sentence_transformers"))
    )
    skip_if_not("GERMAPARL2" %in% RcppCWB::cqp_list_corpora())

    enc <- encoder("GERMAPARL2", path = encoder_model, fp16 = TRUE)
    expect_identical(slot(enc, "revision_check"), "verified")
    v <- enc(polmineR:::.region_text(corpus("GERMAPARL2"), "p", c(98406L, 5755999L)), type = "document")
    expect_identical(dim(v), c(2L, 1024L))
    expect_equal(sqrt(rowSums(v^2)), c(1, 1), tolerance = 1e-3)
    out <- capture.output(show(enc), type = "message")
    expect_true(any(grepl("Loaded: yes", out)))

    # one text, a matrix with one row
    expect_identical(dim(enc("Klimaschutz")), c(1L, 1024L))
  }
)

test_that(
  ".local_model_revision() reads the revision written by hf download",
  {
    d <- tempfile()
    dir.create(file.path(d, ".cache", "huggingface", "download"), recursive = TRUE)
    expect_identical(polmineR:::.local_model_revision(d), NA_character_)
    for (f in c("config.json", "vocab.txt")){
      writeLines(c("53404af", "etag", "123"), file.path(d, ".cache", "huggingface", "download", paste0(f, ".metadata")))
    }
    expect_identical(polmineR:::.local_model_revision(d), "53404af")
    writeLines("other", file.path(d, ".cache", "huggingface", "download", "x.metadata"))
    expect_identical(polmineR:::.local_model_revision(d), NA_character_)
  }
)
