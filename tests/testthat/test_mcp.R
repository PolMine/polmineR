library(polmineR)
use("polmineR")

tools <- mcp_tools(corpora = "GERMAPARLMINI")

mcp_call <- function(name, arguments, tool_list = tools){
  request <- jsonlite::toJSON(
    list(
      jsonrpc = "2.0", id = 1L, method = "tools/call",
      params = list(name = name, arguments = arguments)
    ),
    auto_unbox = TRUE
  )
  response <- jsonlite::fromJSON(mcp_process(request, tools = tool_list), simplifyVector = FALSE)
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
    c("list_corpora", "corpus_info", "entities", "s_attribute_values", "count",
      "kwic", "dispersion", "cooccurrences", "keyness", "read_text")
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
  # GERMAPARLMINI does not declare named-entity annotation
  expect_null(y[["named_entities"]])

  y <- mcp_call("s_attribute_values", list(corpus = "GERMAPARLMINI", s_attribute = "speaker", pattern = "merkel"))
  expect_identical(y[["values"]][["value"]], "Angela Dorothea Merkel")
  expect_identical(
    y[["values"]][["size"]],
    size(subset(corpus("GERMAPARLMINI"), speaker == "Angela Dorothea Merkel"))
  )
})

test_that("corpus_info describes declared named-entity annotation", {
  # s-attributes of GERMAPARLMINI stand in for named-entity annotation
  registry <- ne_test_corpus(
    id = "mcpnetest",
    properties = c(
      '##:: ne_s_attribute = "speaker"',
      '##:: ne_type_s_attribute = "party"',
      '##:: ne_uri_s_attribute = "role"'
    )
  )
  ne_tools <- mcp_tools(corpora = "MCPNETEST")
  call_ne_tool <- function(name, arguments) mcp_call(name, arguments, tool_list = ne_tools)

  ne <- call_ne_tool("corpus_info", list(corpus = "MCPNETEST"))[["named_entities"]]
  types <- sort(s_attributes("MCPNETEST", "party"))
  expect_identical(ne[["s_attribute"]], "speaker")
  expect_identical(ne[["type_s_attribute"]], "party")
  expect_identical(ne[["uri_s_attribute"]], "role")
  expect_identical(ne[["types"]], types)
  expect_identical(ne[["query_mentions"]], "<speaker>[]+</speaker>")
  expect_identical(
    ne[["query_mentions_of_type"]],
    sprintf('<party="%s">[]+</party>', types[1])
  )
  expect_identical(ne[["query_mentions_of_entity"]], '<role="URI">[]+</role>')

  # Each match of the queries is one region (mention), not one token. CQP does
  # not match '[]+' beyond 500 tokens (option 'HardBoundary'). This does not
  # matter for named entities (in GERMAPARL2, mentions have at most 63
  # tokens), but some of the speaker regions standing in for mentions are
  # longer, so only regions of up to 500 tokens are expected to match.
  regions <- function(s_attribute){
    n <- RcppCWB::cl_attribute_size(
      "MCPNETEST", attribute = s_attribute, attribute_type = "s",
      registry = registry
    )
    m <- RcppCWB::get_region_matrix(
      "MCPNETEST", s_attribute = s_attribute, strucs = 0L:(n - 1L),
      registry = registry
    )
    data.frame(
      value = RcppCWB::cl_struc2str(
        "MCPNETEST", s_attribute = s_attribute, struc = 0L:(n - 1L),
        registry = registry
      ),
      n_tokens = m[, 2] - m[, 1] + 1L
    )
  }
  speaker_regions <- regions("speaker")
  y <- call_ne_tool("count", list(corpus = "MCPNETEST", query = ne[["query_mentions"]]))
  expect_identical(
    y[["counts"]][["count"]],
    sum(speaker_regions[["n_tokens"]] <= 500L)
  )
  party_regions <- regions("party")
  y <- call_ne_tool("count", list(corpus = "MCPNETEST", query = ne[["query_mentions_of_type"]]))
  expect_identical(
    y[["counts"]][["count"]],
    sum(party_regions[["value"]] == types[1] & party_regions[["n_tokens"]] <= 500L)
  )
})

test_that("entities counts mentions of named entities", {
  # s-attributes of GERMAPARLMINI stand in for named-entity annotation:
  # speaker regions are mentions, party the type and interjection the URI
  ne_test_corpus(
    id = "mcpentities",
    properties = c(
      '##:: ne_s_attribute = "speaker"',
      '##:: ne_type_s_attribute = "party"',
      '##:: ne_uri_s_attribute = "interjection"'
    )
  )
  ne_tools <- mcp_tools(corpora = c("MCPENTITIES", "GERMAPARLMINI"))
  call_ne_tool <- function(name, arguments) mcp_call(name, arguments, tool_list = ne_tools)

  # by surface form, for a type
  y <- call_ne_tool("entities", list(corpus = "MCPENTITIES", type = "SPD"))
  expected <- slot(count(named_entities("MCPENTITIES", type = "SPD")), "stat")
  expect_identical(y[["n_mentions"]], sum(expected[["count"]]))
  expect_identical(y[["n_entities"]], nrow(expected))
  expect_identical(y[["entities"]][["surface"]], utils::head(expected[["surface"]], 100L))
  expect_identical(y[["entities"]][["count"]], utils::head(expected[["count"]], 100L))
  expect_true(y[["truncated"]])
  expect_null(y[["n_unlinked"]])
  # numbers are written to JSON with 4 significant digits
  expect_equal(
    y[["entities"]][["share"]],
    utils::head(expected[["count"]], 100L) / sum(expected[["count"]]),
    tolerance = 5e-4
  )

  # by URI, with a limit
  y <- call_ne_tool("entities", list(corpus = "MCPENTITIES", by = "uri", limit = 2L))
  expected <- slot(count(named_entities("MCPENTITIES"), by = "uri"), "stat")
  expect_identical(y[["entities"]][["uri"]], utils::head(expected[["uri"]], 2L))
  expect_identical(y[["entities"]][["count"]], utils::head(expected[["count"]], 2L))
  expect_identical(y[["n_unlinked"]], 0L)
  expect_identical(y[["n_mentions"]], nrow(slot(named_entities("MCPENTITIES"), "cpos")))

  # several types, and a subset
  y <- call_ne_tool("entities", list(
    corpus = "MCPENTITIES", type = c("SPD", "FDP"),
    subset = list(date = "2009-11-10")
  ))
  sc <- subset(corpus("MCPENTITIES"), date == "2009-11-10")
  expected <- slot(count(named_entities(sc, type = c("SPD", "FDP"))), "stat")
  expect_identical(y[["n_mentions"]], sum(expected[["count"]]))

  # the second call for a whole corpus uses the cache
  expect_identical(
    call_ne_tool("entities", list(corpus = "MCPENTITIES", type = "SPD")),
    call_ne_tool("entities", list(corpus = "MCPENTITIES", type = "SPD"))
  )

  # errors
  y <- call_ne_tool("entities", list(corpus = "GERMAPARLMINI"))
  expect_s3_class(y, "mcp_error")
  expect_match(y, "does not declare named-entity annotation")
  y <- call_ne_tool("entities", list(corpus = "MCPENTITIES", type = "XYZ"))
  expect_match(y, "type\\(s\\) not available: XYZ")
  y <- call_ne_tool("entities", list(corpus = "MCPENTITIES", by = "name"))
  expect_match(y, "'surface' or 'uri'")
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

  # CQP queries with brackets but without quotation marks are detected
  y <- mcp_call("count", list(corpus = "GERMAPARLMINI", query = "<speaker>[]+</speaker>"))
  expect_identical(
    y[["counts"]][["count"]],
    count("GERMAPARLMINI", query = "<speaker>[]+</speaker>", cqp = TRUE)[["count"]]
  )
  expect_gt(y[["counts"]][["count"]], 0L)

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

test_that("mcp http handler", {
  handler <- polmineR:::mcp_http_handler(tools = tools, token = "secret")

  req <- function(body = "", method = "POST", path = "/mcp", headers = list()){
    input <- list(read = function() charToRaw(body))
    c(
      list(REQUEST_METHOD = method, PATH_INFO = path, rook.input = input),
      headers
    )
  }
  auth <- list(HTTP_AUTHORIZATION = "Bearer secret")
  ping <- '{"jsonrpc":"2.0","id":1,"method":"ping"}'

  # Token required
  expect_identical(handler(req(ping))[["status"]], 401L)
  expect_identical(handler(req(ping, headers = list(HTTP_AUTHORIZATION = "Bearer wrong")))[["status"]], 401L)

  y <- handler(req(ping, headers = auth))
  expect_identical(y[["status"]], 200L)
  expect_identical(y[["headers"]][["Content-Type"]], "application/json")
  expect_identical(y[["body"]], '{"jsonrpc":"2.0","id":1,"result":{}}')

  # Notifications are acknowledged without body
  y <- handler(req('{"jsonrpc":"2.0","method":"notifications/initialized"}', headers = auth))
  expect_identical(y[["status"]], 202L)

  # Tool calls
  y <- handler(req(
    '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"count","arguments":{"corpus":"GERMAPARLMINI","query":"Integration"}}}',
    headers = auth
  ))
  expect_identical(y[["status"]], 200L)
  result <- jsonlite::fromJSON(y[["body"]])[["result"]]
  expect_false(isTRUE(result[["isError"]]))

  expect_identical(handler(req("not json", headers = auth))[["status"]], 400L)
  expect_identical(handler(req(method = "GET", headers = auth))[["status"]], 405L)
  expect_identical(handler(req(ping, path = "/other", headers = auth))[["status"]], 404L)
  expect_identical(
    handler(req(ping, headers = c(auth, list(HTTP_MCP_PROTOCOL_VERSION = "1999-01-01"))))[["status"]],
    400L
  )
  expect_identical(
    handler(req(ping, headers = c(auth, list(HTTP_MCP_PROTOCOL_VERSION = "2025-06-18"))))[["status"]],
    200L
  )

  # Browser origins only if allowed
  expect_identical(
    handler(req(ping, headers = c(auth, list(HTTP_ORIGIN = "https://evil.example"))))[["status"]],
    403L
  )
  handler_origin <- polmineR:::mcp_http_handler(tools = tools, token = "secret", origins = "https://ok.example")
  expect_identical(
    handler_origin(req(ping, headers = c(auth, list(HTTP_ORIGIN = "https://ok.example"))))[["status"]],
    200L
  )

  # Without token, no authorization needed
  handler_open <- polmineR:::mcp_http_handler(tools = tools, token = "")
  expect_identical(handler_open(req(ping))[["status"]], 200L)
})
