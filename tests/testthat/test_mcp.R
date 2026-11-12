library(polmineR)
use("polmineR")

tools <- mcp_tools(corpora = "GERMAPARLMINI")

mcp_call <- function(name, arguments){
  request <- jsonlite::toJSON(
    list(
      jsonrpc = "2.0", id = 1L, method = "tools/call",
      params = list(name = name, arguments = arguments)
    ),
    auto_unbox = TRUE
  )
  response <- jsonlite::fromJSON(mcp_process(request, tools = tools), simplifyVector = FALSE)
  result <- response[["result"]]
  if (isTRUE(result[["isError"]])) return(structure(result[["content"]][[1]][["text"]], class = "mcp_error"))
  jsonlite::fromJSON(result[["content"]][[1]][["text"]])
}

test_that("mcp protocol messages", {
  init <- jsonlite::fromJSON(mcp_process(
    '{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}',
    tools = tools
  ))
  expect_identical(init[["id"]], 0L)
  expect_identical(init[["result"]][["protocolVersion"]], "2025-06-18")
  expect_identical(init[["result"]][["serverInfo"]][["name"]], "polmineR")

  # Unknown protocol versions are answered with the most recent version
  init <- jsonlite::fromJSON(mcp_process(
    '{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"1999-01-01"}}',
    tools = tools
  ))
  expect_identical(init[["result"]][["protocolVersion"]], polmineR:::mcp_protocol_versions[1])

  expect_null(mcp_process('{"jsonrpc":"2.0","method":"notifications/initialized"}', tools = tools))
  expect_identical(
    mcp_process('{"jsonrpc":"2.0","id":"a","method":"ping"}', tools = tools),
    '{"jsonrpc":"2.0","id":"a","result":{}}'
  )

  tool_list <- jsonlite::fromJSON(
    mcp_process('{"jsonrpc":"2.0","id":2,"method":"tools/list"}', tools = tools),
    simplifyVector = FALSE
  )[["result"]][["tools"]]
  expect_setequal(
    vapply(tool_list, `[[`, "name", FUN.VALUE = character(1L)),
    c("list_corpora", "corpus_info", "s_attribute_values", "count", "kwic",
      "dispersion", "cooccurrences", "keyness", "read_text")
  )
  # 'required' needs to be an array, also if there is only one element
  for (tool in tool_list){
    expect_identical(tool[["inputSchema"]][["type"]], "object")
    if (!is.null(tool[["inputSchema"]][["required"]])){
      expect_true(is.list(tool[["inputSchema"]][["required"]]))
    }
  }

  expect_identical(
    jsonlite::fromJSON(mcp_process('{"jsonrpc":"2.0","id":3,"method":"foo"}', tools = tools))[["error"]][["code"]],
    -32601L
  )
  expect_identical(
    jsonlite::fromJSON(mcp_process("not json", tools = tools))[["error"]][["code"]],
    -32700L
  )
  expect_identical(
    jsonlite::fromJSON(mcp_process(
      '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"foo","arguments":{}}}',
      tools = tools
    ))[["error"]][["code"]],
    -32602L
  )
})

test_that("mcp tools for corpus information", {
  y <- mcp_call("list_corpora", structure(list(), names = character()))
  expect_identical(y[["corpora"]][["corpus"]], "GERMAPARLMINI")
  expect_identical(y[["corpora"]][["size"]], size(corpus("GERMAPARLMINI")))

  y <- mcp_call("corpus_info", list(corpus = "GERMAPARLMINI"))
  expect_identical(y[["p_attributes"]], p_attributes("GERMAPARLMINI"))
  expect_setequal(y[["s_attributes"]][["s_attribute"]], s_attributes("GERMAPARLMINI"))

  y <- mcp_call("s_attribute_values", list(corpus = "GERMAPARLMINI", s_attribute = "speaker", pattern = "merkel"))
  expect_identical(y[["values"]][["value"]], "Angela Dorothea Merkel")
  expect_identical(
    y[["values"]][["size"]],
    size(subset(corpus("GERMAPARLMINI"), speaker == "Angela Dorothea Merkel"))
  )
})

test_that("mcp tools for analysis", {
  y <- mcp_call("count", list(corpus = "GERMAPARLMINI", query = c("Integration", "Migration")))
  expect_identical(
    y[["counts"]][["count"]],
    count("GERMAPARLMINI", query = c("Integration", "Migration"))[["count"]]
  )

  # mixing plain words and CQP queries
  y <- mcp_call("count", list(corpus = "GERMAPARLMINI", query = c("Integration", '"Migration.*"')))
  expect_identical(y[["counts"]][["query"]], c('"Integration"', '"Migration.*"'))

  sc <- subset(corpus("GERMAPARLMINI"), party == "SPD" & date == "2009-11-10")
  y <- mcp_call("count", list(
    corpus = "GERMAPARLMINI", query = "Integration",
    subset = list(party = "SPD", date = "2009-11-10")
  ))
  expect_identical(y[["size"]], size(sc))
  expect_identical(y[["counts"]][["count"]], count(sc, query = "Integration")[["count"]])

  y <- mcp_call("kwic", list(
    corpus = "GERMAPARLMINI", query = "Integration",
    s_attributes = list("speaker"), limit = 5L
  ))
  expect_identical(y[["n_matches"]], 23L)
  expect_identical(nrow(y[["matches"]]), 5L)
  expect_identical(colnames(y[["matches"]]), c("speaker", "left", "node", "right"))
  expect_true(all(y[["matches"]][["node"]] == "Integration"))

  y <- mcp_call("kwic", list(corpus = "GERMAPARLMINI", query = "Integration", offset = 20L))
  expect_identical(nrow(y[["matches"]]), 3L)

  y <- mcp_call("dispersion", list(corpus = "GERMAPARLMINI", query = "Integration", s_attribute = "party"))
  expect_identical(sum(y[["dispersion"]][["count"]]), 23L)

  y <- mcp_call("cooccurrences", list(corpus = "GERMAPARLMINI", query = "Integration", min_count = 2L))
  expect_identical(y[["collocates"]][["word"]][1], "Bildung")
  y_all <- mcp_call("cooccurrences", list(corpus = "GERMAPARLMINI", query = "Integration", min_count = 1L, stopwords = "none", limit = 500L))
  expect_true("die" %in% y_all[["collocates"]][["word"]])
  y_de <- mcp_call("cooccurrences", list(corpus = "GERMAPARLMINI", query = "Integration", min_count = 1L, limit = 500L))
  expect_false("die" %in% y_de[["collocates"]][["word"]])

  y <- mcp_call("keyness", list(corpus = "GERMAPARLMINI", subset = list(party = "DIE_LINKE")))
  expect_identical(y[["terms"]][["word"]][1], "LINKEN")
  expect_true(all(y[["terms"]][["count_subcorpus"]] >= 5L))
})

test_that("mcp tool read_text", {
  def <- list(speaker = "Angela Dorothea Merkel", date = "2009-11-10")
  sc <- subset(corpus("GERMAPARLMINI"), speaker == "Angela Dorothea Merkel" & date == "2009-11-10")

  y <- mcp_call("read_text", list(corpus = "GERMAPARLMINI", subset = def, max_tokens = 100L))
  expect_identical(y[["size"]], size(sc))
  expect_identical(y[["n_tokens"]], 100L)
  expect_identical(y[["next_offset"]], 100L)
  expect_true(startsWith(y[["passages"]][["text"]][1], "Herr Präsident!"))

  # Reading the subcorpus in chunks returns all tokens
  n <- 0L
  offset <- 0L
  while (!is.null(offset)){
    y <- mcp_call("read_text", list(corpus = "GERMAPARLMINI", subset = def, max_tokens = 3000L, offset = offset))
    n <- n + y[["n_tokens"]]
    offset <- y[["next_offset"]]
  }
  expect_identical(n, size(sc))

  y <- mcp_call("read_text", list(
    corpus = "GERMAPARLMINI", subset = def, max_tokens = 200L,
    s_attributes = list("speaker", "interjection")
  ))
  expect_identical(colnames(y[["passages"]]), c("speaker", "interjection", "text"))
  expect_true("interjection" %in% y[["passages"]][["interjection"]])
})

test_that("mcp tools report errors to the client", {
  expect_s3_class(mcp_call("count", list(corpus = "FOO", query = "x")), "mcp_error")
  expect_match(
    mcp_call("count", list(corpus = "GERMAPARLMINI", query = "x", subset = list(foo = "x"))),
    "not available"
  )
  expect_match(
    mcp_call("count", list(corpus = "GERMAPARLMINI", query = "x", subset = list(speaker = "Nobody"))),
    "no text matches"
  )
  expect_match(mcp_call("read_text", list(corpus = "GERMAPARLMINI")), "required")
})

test_that("mcp_server reads from and writes to connections", {
  requests <- c(
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}',
    '{"jsonrpc":"2.0","method":"notifications/initialized"}',
    "",
    '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"count","arguments":{"corpus":"GERMAPARLMINI","query":"Integration"}}}'
  )
  output_file <- tempfile()
  output <- file(output_file, open = "w")
  mcp_server(
    corpora = "GERMAPARLMINI", verbose = FALSE,
    input = textConnection(requests), output = output
  )
  close(output)
  responses <- readLines(output_file, encoding = "UTF-8")
  expect_length(responses, 2L)
  expect_true(all(vapply(responses, jsonlite::validate, FUN.VALUE = logical(1L))))
  result <- jsonlite::fromJSON(responses[2])[["result"]]
  expect_false(result[["isError"]])
  expect_identical(jsonlite::fromJSON(result[["content"]][["text"]])[["counts"]][["count"]], 23L)

  expect_error(mcp_server(corpora = "FOO", verbose = FALSE), "not available")
})
