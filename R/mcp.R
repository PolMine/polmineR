#' Run polmineR as an MCP server
#'
#' Start a server that implements the Model Context Protocol (MCP), so that
#' an AI assistant (such as Claude Desktop, Claude Code or any other MCP client)
#' can query CWB corpora such as GermaParl with polmineR. The server
#' communicates via JSON-RPC messages on stdin/stdout ("stdio" transport) and
#' offers polmineR functionality as tools.
#'
#' The tools are:
#'
#' - `list_corpora`: Corpora available, with size and encoding.
#' - `corpus_info`: Size, positional and structural attributes of a corpus.
#' - `s_attribute_values`: Values of a structural attribute (e.g. speakers,
#'   parties, dates), with the number of tokens for each value.
#' - `count`: Counts of words or CQP queries.
#' - `kwic`: Keyword-in-context concordances.
#' - `dispersion`: Counts of queries across the values of a structural
#'   attribute (e.g. by year or party).
#' - `cooccurrences`: Collocates of a query (log-likelihood statistic).
#' - `keyness`: Terms over-represented in a subcorpus compared to the corpus
#'   or another subcorpus.
#' - `read_text`: The full text of a subcorpus (e.g. a speech or a debate),
#'   labelled by metadata.
#'
#' All tools except `list_corpora` take a `corpus` argument and, where
#' meaningful, an argument `subset` to restrict the analysis to a subcorpus.
#' `subset` is a JSON object with s-attributes as keys and a value or an array
#' of values, e.g. `{"party": "SPD", "protocol_lp": ["18", "19"]}`, passed to
#' [subset()]. Results are returned as JSON.
#'
#' Messages on stdout are reserved for the protocol, so output from polmineR
#' functions is suppressed while handling requests. Status messages go to
#' stderr.
#'
#' To use the server with Claude Code, register it as follows (in a terminal):
#'
#' `claude mcp add polmineR -- Rscript -e "polmineR::mcp_server()"`
#'
#' For Claude Desktop, add an entry to the `mcpServers` section of
#' `claude_desktop_config.json`:
#'
#' ```
#' "polmineR": {
#'   "command": "Rscript",
#'   "args": ["-e", "polmineR::mcp_server(corpora = 'GERMAPARL2')"]
#' }
#' ```
#'
#' Use the full path to `Rscript` if it is not on the search path of the
#' client. Corpora are looked up in the registry directory defined by the
#' environment variable `CORPUS_REGISTRY` (e.g. set in `.Renviron`) and in
#' corpora loaded with [use()].
#'
#' @param corpora A `character` vector with the ids of the corpora that can be
#'   queried. If `NULL` (default), all corpora returned by [corpus()] are
#'   available.
#' @param use A `character` vector with names of R data packages with corpora
#'   (e.g. `"GermaParl2"`) to load with [use()] before starting the server.
#' @param verbose A `logical` value, whether to output status messages (to
#'   stderr).
#' @param input A connection to read JSON-RPC messages from. Defaults to stdin.
#' @param output A connection to write JSON-RPC messages to. Defaults to
#'   stdout.
#' @return Called for its side effect. Returns `NULL` invisibly when the input
#'   connection is closed by the client.
#' @export mcp_server
#' @rdname mcp_server
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger
#' @examples
#' \dontrun{
#' # Run in a terminal or from an MCP client
#' mcp_server(corpora = c("GERMAPARLMINI", "GERMAPARL2"))
#' }
mcp_server <- function(corpora = NULL, use = NULL, verbose = TRUE, input = file("stdin"), output = stdout()){
  for (pkg in use){
    utils::capture.output(use(pkg = pkg, verbose = FALSE))
  }

  available <- corpus()[["corpus"]]
  if (is.null(corpora)){
    corpora <- available
  } else {
    missing_corpora <- setdiff(corpora, available)
    if (length(missing_corpora) > 0L){
      stop(sprintf(
        "corpora not available: %s", paste(missing_corpora, collapse = ", ")
      ))
    }
  }
  if (length(corpora) == 0L) stop("no corpora available")

  tools <- mcp_tools(corpora = corpora)

  if (!isOpen(input)) open(input, "r")
  if (verbose) cli_alert_success(
    "polmineR MCP server ready (corpora: {.val {corpora}})"
  )

  repeat {
    line <- readLines(input, n = 1L, encoding = "UTF-8", warn = FALSE)
    if (length(line) == 0L) break
    if (!nzchar(trimws(line))) next
    response <- mcp_process(line, tools = tools, verbose = verbose)
    if (!is.null(response)){
      writeLines(response, con = output, useBytes = TRUE)
      flush(output)
    }
  }
  if (verbose) cli_alert_info("input closed, polmineR MCP server stops")
  invisible(NULL)
}


#' @details `mcp_process()` processes a single JSON-RPC message and returns the
#'   JSON-RPC response as a length-one `character` vector, or `NULL` for
#'   notifications. It is the worker of `mcp_server()` and can be used to test
#'   tools without a client.
#' @param message A length-one `character` vector with a JSON-RPC message.
#' @param tools A `list` of tools, as returned by `mcp_tools()`.
#' @export mcp_process
#' @rdname mcp_server
#' @importFrom jsonlite fromJSON toJSON
#' @examples
#' tools <- mcp_tools(corpora = "GERMAPARLMINI")
#' mcp_process(
#'   '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"count",
#'   "arguments":{"corpus":"GERMAPARLMINI","query":"Integration"}}}',
#'   tools = tools
#' )
mcp_process <- function(message, tools, verbose = FALSE){
  msg <- tryCatch(
    fromJSON(message, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(msg) || !is.list(msg) || is.null(names(msg))){
    return(mcp_json(mcp_error(NULL, -32700L, "Parse error")))
  }

  response <- NULL
  noise <- utils::capture.output(
    response <- mcp_dispatch(msg, tools = tools, verbose = verbose)
  )
  if (verbose && length(noise) > 0L){
    cli_alert_info("output suppressed: {length(noise)} line{?s}")
  }
  if (is.null(response)) NULL else mcp_json(response)
}


#' @details `mcp_tools()` returns the definitions of the tools offered by the
#'   server: A `list` of `list` objects with the name, the description, the
#'   JSON Schema of the input (`inputSchema`) and the function implementing the
#'   tool (`fn`).
#' @export mcp_tools
#' @rdname mcp_server
mcp_tools <- function(corpora){
  # Counts for whole corpora are expensive for large corpora (such as
  # GERMAPARL2) and are kept for the lifetime of the server.
  cache <- new.env(parent = emptyenv())

  corpus_count <- function(id, p_attribute){
    key <- paste(id, p_attribute, sep = "::")
    if (is.null(cache[[key]])){
      cache[[key]] <- count(corpus(id), p_attribute = p_attribute, verbose = FALSE)
    }
    cache[[key]]
  }

  get_corpus <- function(args){
    id <- mcp_arg_chr(args, "corpus", required = TRUE)
    if (!id %in% corpora){
      stop(sprintf(
        "corpus '%s' not available - available corpora: %s",
        id, paste(corpora, collapse = ", ")
      ))
    }
    corpus(id)
  }

  get_object <- function(args, param = "subset", required = FALSE){
    x <- get_corpus(args)
    mcp_subcorpus(
      x,
      def = args[[param]],
      regex = mcp_arg_lgl(args, "regex", default = FALSE),
      required = required
    )
  }

  p_attr <- function(args, x){
    p <- mcp_arg_chr(args, "p_attribute", default = "word")
    if (!all(p %in% p_attributes(x))){
      stop(sprintf(
        "p-attribute '%s' not available - available p-attributes: %s",
        paste(p, collapse = ", "), paste(p_attributes(x), collapse = ", ")
      ))
    }
    p
  }

  # Get queries and whether they are CQP queries. If some queries use CQP
  # syntax and others do not, the plain words are turned into CQP queries.
  get_query <- function(args){
    query <- mcp_arg_chr(args, "query", required = TRUE)
    cqp <- mcp_arg_lgl(args, "cqp", default = NA)
    if (is.na(cqp)){
      is_cqp <- vapply(query, is.cqp, FUN.VALUE = logical(1L))
      cqp <- any(is_cqp)
      if (cqp) query[!is_cqp] <- sprintf('"%s"', query[!is_cqp])
    }
    list(query = query, cqp = cqp)
  }

  # JSON Schema fragments re-used by several tools
  schema_corpus <- list(
    type = "string", enum = as.list(corpora),
    description = "ID of the corpus (upper case)."
  )
  schema_subset <- list(
    type = "object",
    description = paste(
      "Restrict the analysis to a subcorpus. Keys are s-attributes (see",
      "tool corpus_info), values are a string or an array of strings, e.g.",
      '{"party": "SPD", "protocol_lp": ["18", "19"]}. Conditions for several',
      "s-attributes are combined (AND). Values are matched exactly, unless",
      "'regex' is true. Use tool s_attribute_values to look up values."
    ),
    additionalProperties = list(
      anyOf = list(
        list(type = "string"),
        list(type = "array", items = list(type = "string"))
      )
    )
  )
  schema_regex <- list(
    type = "boolean", default = FALSE,
    description = "Whether values in 'subset' are regular expressions."
  )
  schema_query <- list(
    type = "string",
    description = paste(
      "A single word (e.g. 'Klimaschutz') or a query in the syntax of the",
      "Corpus Query Processor (CQP). In CQP, tokens are quoted and may use",
      "regular expressions and attributes, e.g. '\"Klima.*\"',",
      "'\"soziale\" \"Marktwirtschaft\"', '[pos=\"ADJA\"] \"Migration\"'.",
      "Add %c to ignore case: '\"klimaschutz\"%c'."
    )
  )
  schema_queries <- list(
    anyOf = list(
      schema_query,
      list(type = "array", items = schema_query)
    ),
    description = paste(
      "One query or an array of queries. Each query is a word or a CQP query,",
      "e.g. '\"Klima.*\"' or '\"soziale\" \"Marktwirtschaft\"'."
    )
  )
  schema_cqp <- list(
    type = "boolean",
    description = paste(
      "Whether queries use CQP syntax. If omitted, this is detected",
      "automatically (CQP queries contain quotation marks or brackets)."
    )
  )
  schema_p_attribute <- list(
    type = "string", default = "word",
    description = "Positional attribute to evaluate (e.g. 'word' or 'lemma')."
  )
  schema_stopwords <- function(default){
    list(
      type = "string", enum = list("de", "en", "fr", "none"), default = default,
      description = paste(
        "Language of a stopword list (function words such as 'und', 'der')",
        "to drop from the result, or 'none'. Punctuation is dropped unless",
        "'none'."
      )
    )
  }
  schema_int <- function(default, min, max, description){
    list(
      type = "integer", default = default, minimum = min, maximum = max,
      description = description
    )
  }

  tools <- list(

    list(
      name = "list_corpora",
      description = paste(
        "List the corpora that can be queried, with their size (number of",
        "tokens) and encoding."
      ),
      inputSchema = list(type = "object", properties = mcp_empty_object()),
      fn = function(args){
        df <- corpus()
        df <- df[df[["corpus"]] %in% corpora, c("corpus", "size", "encoding")]
        df[["name"]] <- vapply(
          df[["corpus"]], function(id) slot(corpus(id), "name"),
          FUN.VALUE = character(1L)
        )
        list(corpora = df)
      }
    ),

    list(
      name = "corpus_info",
      description = paste(
        "Describe a corpus: its size, the positional attributes (token-level",
        "annotation such as 'word', 'pos', 'lemma') and the structural",
        "attributes (metadata such as 'speaker', 'party', 'date') with the",
        "number of distinct values. Call this first to learn how to define",
        "subcorpora."
      ),
      inputSchema = list(
        type = "object",
        properties = list(corpus = schema_corpus),
        required = list("corpus")
      ),
      fn = function(args){
        x <- get_corpus(args)
        s_attrs <- s_attributes(x)
        n_values <- vapply(
          s_attrs,
          function(s) tryCatch(
            length(s_attributes(x, s)),
            error = function(e) NA_integer_
          ),
          FUN.VALUE = integer(1L)
        )
        list(
          corpus = slot(x, "corpus"),
          name = slot(x, "name"),
          size = size(x),
          encoding = slot(x, "encoding"),
          p_attributes = I(p_attributes(x)),
          s_attributes = data.frame(
            s_attribute = s_attrs,
            n_values = n_values,
            row.names = NULL
          )
        )
      }
    ),

    list(
      name = "s_attribute_values",
      description = paste(
        "Get the values of a structural attribute (e.g. the speakers, parties",
        "or dates in a corpus or subcorpus), with the number of tokens for",
        "each value. Use 'pattern' to search values (e.g. a speaker name)."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          corpus = schema_corpus,
          s_attribute = list(
            type = "string",
            description = "The structural attribute, e.g. 'speaker'."
          ),
          pattern = list(
            type = "string",
            description = "Regular expression to filter values (case-insensitive)."
          ),
          subset = schema_subset,
          regex = schema_regex,
          limit = schema_int(100L, 1L, 5000L, "Maximum number of values to return.")
        ),
        required = list("corpus", "s_attribute")
      ),
      fn = function(args){
        x <- get_object(args)
        s_attr <- mcp_arg_chr(args, "s_attribute", required = TRUE)
        if (!s_attr %in% s_attributes(x)){
          stop(sprintf(
            "s-attribute '%s' not available - available s-attributes: %s",
            s_attr, paste(s_attributes(x), collapse = ", ")
          ))
        }
        df <- tryCatch(
          as.data.frame(size(x, s_attribute = s_attr)),
          error = function(e) data.frame(
            value = sort(s_attributes(x, s_attr)), stringsAsFactors = FALSE
          )
        )
        colnames(df)[1] <- "value"
        pattern <- mcp_arg_chr(args, "pattern")
        if (!is.null(pattern)){
          df <- df[grepl(pattern, df[["value"]], ignore.case = TRUE, perl = TRUE), , drop = FALSE]
        }
        limit <- mcp_arg_int(args, "limit", default = 100L, max = 5000L)
        list(
          s_attribute = s_attr,
          n_values = nrow(df),
          truncated = nrow(df) > limit,
          values = utils::head(df, limit)
        )
      }
    ),

    list(
      name = "count",
      description = paste(
        "Count the number of occurrences of one or more queries in a corpus",
        "or subcorpus. Returns absolute counts and relative frequencies",
        "(count divided by the size of the (sub)corpus). With 'breakdown'",
        "true, the different matches of a CQP query are counted",
        "separately (e.g. which words match '\"Klima.*\"')."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          corpus = schema_corpus,
          query = schema_queries,
          cqp = schema_cqp,
          breakdown = list(
            type = "boolean", default = FALSE,
            description = "Whether to count the matches of each query separately."
          ),
          subset = schema_subset,
          regex = schema_regex,
          p_attribute = schema_p_attribute,
          limit = schema_int(100L, 1L, 1000L, "Maximum number of rows to return (with breakdown).")
        ),
        required = list("corpus", "query")
      ),
      fn = function(args){
        x <- get_object(args)
        q <- get_query(args)
        breakdown <- mcp_arg_lgl(args, "breakdown", default = FALSE)
        y <- count(
          x, query = q[["query"]], cqp = q[["cqp"]],
          p_attribute = p_attr(args, x), breakdown = breakdown,
          verbose = FALSE
        )
        limit <- mcp_arg_int(args, "limit", default = 100L, max = 1000L)
        y <- as.data.frame(y)
        list(
          size = size(x),
          n_rows = nrow(y),
          truncated = nrow(y) > limit,
          counts = utils::head(y, limit)
        )
      }
    ),

    list(
      name = "kwic",
      description = paste(
        "Keyword-in-context (concordance): find the occurrences of a query",
        "and return them with the words to the left and to the right, and",
        "metadata (structural attributes) of each match. Use this to see",
        "how a term is used."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          corpus = schema_corpus,
          query = schema_query,
          cqp = schema_cqp,
          subset = schema_subset,
          regex = schema_regex,
          s_attributes = list(
            type = "array", items = list(type = "string"),
            description = paste(
              "Structural attributes to report for each match, e.g.",
              '["speaker", "party", "date"].'
            )
          ),
          left = schema_int(10L, 0L, 100L, "Number of tokens to the left."),
          right = schema_int(10L, 0L, 100L, "Number of tokens to the right."),
          limit = schema_int(20L, 1L, 500L, "Maximum number of matches to return."),
          offset = schema_int(0L, 0L, .Machine$integer.max, "Number of matches to skip (for paging)."),
          sample = list(
            type = "boolean", default = FALSE,
            description = "Return a random sample of matches rather than the first ones."
          )
        ),
        required = list("corpus", "query")
      ),
      fn = function(args){
        x <- get_object(args)
        q <- get_query(args)
        s_attrs <- mcp_arg_chr(args, "s_attributes", default = character())
        unknown <- setdiff(s_attrs, s_attributes(x))
        if (length(unknown) > 0L){
          stop(sprintf("s-attribute(s) not available: %s", paste(unknown, collapse = ", ")))
        }
        k <- kwic(
          x, query = q[["query"]], cqp = q[["cqp"]],
          left = mcp_arg_int(args, "left", default = 10L, max = 100L),
          right = mcp_arg_int(args, "right", default = 10L, max = 100L),
          s_attributes = if (length(s_attrs) > 0L) s_attrs else NULL,
          verbose = FALSE
        )
        if (is.null(k)) return(list(n_matches = 0L, matches = list()))
        df <- as.data.frame(slot(k, "stat"))
        df <- df[, c(s_attrs, "left", "node", "right"), drop = FALSE]
        n <- nrow(df)
        limit <- mcp_arg_int(args, "limit", default = 20L, max = 500L)
        if (isTRUE(mcp_arg_lgl(args, "sample", default = FALSE))){
          rows <- sort(sample(seq_len(n), size = min(limit, n)))
        } else {
          offset <- mcp_arg_int(args, "offset", default = 0L)
          rows <- seq_len(n)[seq_len(n) > offset][seq_len(min(limit, max(0L, n - offset)))]
        }
        list(n_matches = n, returned = length(rows), matches = df[rows, , drop = FALSE])
      }
    ),

    list(
      name = "dispersion",
      description = paste(
        "Count queries across the values of a structural attribute, e.g. the",
        "number of occurrences of a term per year, per party or per speaker.",
        "With 'freq' true, relative frequencies (count divided by the number",
        "of tokens for the value) are added, which makes values of different",
        "size comparable."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          corpus = schema_corpus,
          query = schema_queries,
          cqp = schema_cqp,
          s_attribute = list(
            type = "string",
            description = "The structural attribute, e.g. 'party', 'year' or 'speaker'."
          ),
          freq = list(
            type = "boolean", default = FALSE,
            description = "Whether to add relative frequencies."
          ),
          subset = schema_subset,
          regex = schema_regex,
          limit = schema_int(500L, 1L, 5000L, "Maximum number of rows to return.")
        ),
        required = list("corpus", "query", "s_attribute")
      ),
      fn = function(args){
        x <- get_object(args)
        q <- get_query(args)
        s_attr <- mcp_arg_chr(args, "s_attribute", required = TRUE)
        if (!s_attr %in% s_attributes(x)){
          stop(sprintf("s-attribute '%s' not available", s_attr))
        }
        y <- dispersion(
          x, query = q[["query"]], s_attribute = s_attr, cqp = q[["cqp"]],
          freq = mcp_arg_lgl(args, "freq", default = FALSE),
          progress = FALSE, verbose = FALSE
        )
        y <- as.data.frame(y)
        limit <- mcp_arg_int(args, "limit", default = 500L, max = 5000L)
        list(n_rows = nrow(y), truncated = nrow(y) > limit, dispersion = utils::head(y, limit))
      }
    ),

    list(
      name = "cooccurrences",
      description = paste(
        "Find the collocates of a query: words occurring significantly more",
        "often in a window around the query than expected. Ranked by the",
        "log-likelihood statistic ('ll')."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          corpus = schema_corpus,
          query = schema_query,
          cqp = schema_cqp,
          subset = schema_subset,
          regex = schema_regex,
          p_attribute = schema_p_attribute,
          left = schema_int(5L, 1L, 50L, "Window size to the left."),
          right = schema_int(5L, 1L, 50L, "Window size to the right."),
          min_count = schema_int(3L, 1L, 1000L, "Minimum number of co-occurrences."),
          stopwords = schema_stopwords("de"),
          limit = schema_int(30L, 1L, 500L, "Maximum number of collocates to return.")
        ),
        required = list("corpus", "query")
      ),
      fn = function(args){
        x <- get_object(args)
        q <- get_query(args)
        p <- p_attr(args, x)
        y <- cooccurrences(
          x, query = q[["query"]], cqp = q[["cqp"]], p_attribute = p,
          left = mcp_arg_int(args, "left", default = 5L, max = 50L),
          right = mcp_arg_int(args, "right", default = 5L, max = 50L),
          verbose = FALSE
        )
        if (is.null(y)) return(list(n_collocates = 0L, collocates = list()))
        df <- as.data.frame(y)
        df <- mcp_drop_stopwords(df, p[1], mcp_arg_chr(args, "stopwords", default = "de"))
        df <- df[df[["count_coi"]] >= mcp_arg_int(args, "min_count", default = 3L), , drop = FALSE]
        df <- df[order(df[["ll"]], decreasing = TRUE), , drop = FALSE]
        df <- df[, c(p, "count_coi", "count_partition", "exp_coi", "ll"), drop = FALSE]
        colnames(df)[colnames(df) == "count_coi"] <- "count_window"
        colnames(df)[colnames(df) == "count_partition"] <- "count_total"
        colnames(df)[colnames(df) == "exp_coi"] <- "expected_window"
        limit <- mcp_arg_int(args, "limit", default = 30L, max = 500L)
        list(n_collocates = nrow(df), collocates = utils::head(df, limit))
      }
    ),

    list(
      name = "keyness",
      description = paste(
        "Find the terms that are characteristic of a subcorpus (e.g. the",
        "speeches of one party or speaker): terms that occur significantly",
        "more often than in a reference. The reference is the whole corpus,",
        "or the subcorpus defined by 'reference_subset' (which should not",
        "overlap with 'subset')."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          corpus = schema_corpus,
          subset = schema_subset,
          reference_subset = c(
            list(description = paste(
              "Subcorpus to compare with, defined like 'subset'. If omitted,",
              "the whole corpus is the reference."
            )),
            schema_subset[c("type", "additionalProperties")]
          ),
          regex = schema_regex,
          p_attribute = schema_p_attribute,
          method = list(
            type = "string", enum = list("ll", "chisquare"), default = "ll",
            description = "Test statistic: log-likelihood ('ll') or chi-squared."
          ),
          min_count = schema_int(5L, 1L, 10000L, "Minimum count in the subcorpus."),
          stopwords = schema_stopwords("none"),
          limit = schema_int(50L, 1L, 1000L, "Maximum number of terms to return.")
        ),
        required = list("corpus", "subset")
      ),
      fn = function(args){
        x <- get_object(args, required = TRUE)
        p <- p_attr(args, x)
        method <- mcp_arg_chr(args, "method", default = "ll")
        if (!method %in% c("ll", "chisquare")) stop("method needs to be 'll' or 'chisquare'")
        x_count <- count(x, p_attribute = p, verbose = FALSE)
        if (is.null(args[["reference_subset"]])){
          y_count <- corpus_count(slot(x, "corpus"), p)
          included <- TRUE
        } else {
          ref <- get_object(args, param = "reference_subset", required = TRUE)
          y_count <- count(ref, p_attribute = p, verbose = FALSE)
          included <- FALSE
        }
        f <- features(x_count, y_count, included = included, method = method, verbose = FALSE)
        df <- as.data.frame(slot(f, "stat"))
        df <- mcp_drop_stopwords(df, p[1], mcp_arg_chr(args, "stopwords", default = "none"))
        df <- df[df[["count_coi"]] >=mcp_arg_int(args, "min_count", default = 5L), , drop = FALSE]
        df <- df[df[["count_coi"]] > df[["exp_coi"]], , drop = FALSE]
        df <- df[order(df[[method]], decreasing = TRUE), , drop = FALSE]
        df <- df[, c(p, "count_coi", "count_ref", "exp_coi", method), drop = FALSE]
        colnames(df)[colnames(df) == "count_coi"] <- "count_subcorpus"
        colnames(df)[colnames(df) == "count_ref"] <- "count_reference"
        colnames(df)[colnames(df) == "exp_coi"] <- "expected_subcorpus"
        limit <- mcp_arg_int(args, "limit", default = 50L, max = 1000L)
        list(
          size_subcorpus = size(x),
          size_reference = sum(slot(y_count, "stat")[["count"]]),
          terms = utils::head(df, limit)
        )
      }
    ),

    list(
      name = "read_text",
      description = paste(
        "Read the full text of a subcorpus, e.g. a speech",
        '({"speaker": "...", "date": "..."}) or a debate. The text is split',
        "into passages labelled by structural attributes (by default",
        "'speaker'). Long texts are returned in chunks: use 'offset' with the",
        "value of 'next_offset' to continue reading. Use the kwic or",
        "s_attribute_values tools first to find the subcorpus of interest."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          corpus = schema_corpus,
          subset = schema_subset,
          regex = schema_regex,
          s_attributes = list(
            type = "array", items = list(type = "string"),
            description = paste(
              "Structural attributes to label passages, e.g.",
              '["speaker", "party"]. Defaults to ["speaker"] if available.'
            )
          ),
          max_tokens = schema_int(2000L, 1L, 20000L, "Maximum number of tokens to return."),
          offset = schema_int(0L, 0L, .Machine$integer.max, "Number of tokens to skip.")
        ),
        required = list("corpus", "subset")
      ),
      fn = function(args){
        x <- get_object(args, required = TRUE)
        s_attrs <- mcp_arg_chr(args, "s_attributes", default = NULL)
        if (is.null(s_attrs)) s_attrs <- intersect("speaker", s_attributes(x))
        unknown <- setdiff(s_attrs, s_attributes(x))
        if (length(unknown) > 0L){
          stop(sprintf("s-attribute(s) not available: %s", paste(unknown, collapse = ", ")))
        }
        mcp_read_text(
          x, s_attributes = s_attrs,
          max_tokens = mcp_arg_int(args, "max_tokens", default = 2000L, max = 20000L),
          offset = mcp_arg_int(args, "offset", default = 0L)
        )
      }
    )
  )
  names(tools) <- vapply(tools, `[[`, "name", FUN.VALUE = character(1L))
  tools
}


# Versions of the MCP specification supported, most recent first.
mcp_protocol_versions <- c("2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05")


#' @importFrom utils packageVersion
mcp_dispatch <- function(msg, tools, verbose = FALSE){
  method <- msg[["method"]]
  id <- msg[["id"]]

  # Notifications (no id) do not get a response
  if (is.null(id)){
    if (is.null(method)) return(NULL)
    if (verbose) cli_alert_info("notification: {method}")
    return(NULL)
  }

  if (!is.character(method) || length(method) != 1L){
    return(mcp_error(id, -32600L, "Invalid Request"))
  }

  if (method == "initialize"){
    requested <- msg[["params"]][["protocolVersion"]]
    version <- if (isTRUE(requested %in% mcp_protocol_versions)){
      requested
    } else {
      mcp_protocol_versions[1]
    }
    if (verbose) cli_alert_info("initialize (protocol version {version})")
    return(mcp_result(id, list(
      protocolVersion = version,
      capabilities = list(tools = list(listChanged = FALSE)),
      serverInfo = list(
        name = "polmineR",
        version = as.character(packageVersion("polmineR"))
      ),
      instructions = paste(
        "Tools for corpus analysis with polmineR, using CWB-indexed corpora",
        "such as GermaParl (debates of the German Bundestag). Start with",
        "list_corpora and corpus_info to learn the structural attributes",
        "(metadata) of a corpus, and s_attribute_values to look up values",
        "(e.g. speaker names). Use count, dispersion, kwic, cooccurrences and",
        "keyness for analysis, and read_text to read a speech or debate.",
        "Counts are exact counts in the corpus, not estimates."
      )
    )))
  }

  if (method == "ping") return(mcp_result(id, mcp_empty_object()))

  if (method == "tools/list"){
    return(mcp_result(id, list(
      tools = unname(lapply(tools, function(tool) tool[c("name", "description", "inputSchema")]))
    )))
  }

  if (method == "tools/call"){
    name <- msg[["params"]][["name"]]
    if (!isTRUE(name %in% names(tools))){
      return(mcp_error(id, -32602L, sprintf("Unknown tool: %s", paste(name, collapse = ""))))
    }
    args <- msg[["params"]][["arguments"]]
    if (is.null(args)) args <- list()
    if (verbose) cli_alert_info("tools/call: {name}")
    return(mcp_result(id, mcp_call_tool(tools[[name]], args, verbose = verbose)))
  }

  mcp_error(id, -32601L, sprintf("Method not found: %s", method))
}


# Run a tool, and turn the result (or an error) into the result of a
# tools/call request. Errors are reported to the model (isError = true) rather
# than as protocol errors, so that it can correct the call.
mcp_call_tool <- function(tool, args, verbose = FALSE){
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      suppressMessages(tool[["fn"]](args)),
      warning = function(w){
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(value, "error")){
    if (verbose) cli_alert_danger("{tool[['name']]} failed: {conditionMessage(value)}")
    return(list(
      content = list(list(type = "text", text = conditionMessage(value))),
      isError = TRUE
    ))
  }
  content <- list(list(type = "text", text = mcp_json(value)))
  if (length(warnings) > 0L){
    content[[2]] <- list(
      type = "text",
      text = paste(c("Warnings:", unique(warnings)), collapse = "\n")
    )
  }
  list(content = content, isError = FALSE)
}


# Create a subcorpus from a JSON object with s-attributes as keys. The
# argument is not called 'subset' to avoid masking the generic.
mcp_subcorpus <- function(x, def, regex = FALSE, required = FALSE){
  if (length(def) == 0L){
    if (required) stop("argument 'subset' is required to define a subcorpus")
    return(x)
  }
  if (!is.list(def) || is.null(names(def)) || any(names(def) == "")){
    stop("'subset' needs to be an object with s-attributes as keys")
  }
  unknown <- setdiff(names(def), s_attributes(x))
  if (length(unknown) > 0L){
    stop(sprintf(
      "s-attribute(s) not available: %s - available s-attributes: %s",
      paste(unknown, collapse = ", "), paste(s_attributes(x), collapse = ", ")
    ))
  }
  values <- lapply(def, function(v) as.character(unlist(v)))

  # Subsetting a corpus with s-attributes at different levels of nested XML
  # (e.g. 'protocol_lp' and 'speaker_party' in GERMAPARL2) fails, so
  # conditions are applied one after another. Subsetting a subcorpus requires
  # an expression.
  y <- do.call(subset, c(list(x = x), values[1], list(regex = regex)))
  for (s_attr in names(values)[-1]){
    if (is.null(y)) break
    expr <- if (regex){
      bquote(grepl(.(paste(values[[s_attr]], collapse = "|")), .(as.name(s_attr))))
    } else {
      bquote(.(as.name(s_attr)) %in% .(values[[s_attr]]))
    }
    y <- eval(bquote(subset(y, .(expr))))
  }

  if (is.null(y) || size(y) == 0L){
    stop(sprintf(
      "no text matches subset %s - use s_attribute_values to look up values",
      mcp_json(def)
    ))
  }
  y
}


# Get the text of a subcorpus as passages, labelled by s-attributes.
#' @importFrom data.table rleidv
mcp_read_text <- function(x, s_attributes, max_tokens, offset){
  cpos <- slot(x, "cpos")
  len <- cpos[, 2] - cpos[, 1] + 1L
  end <- cumsum(len)
  start <- end - len
  total <- end[length(end)]

  to <- min(offset + max_tokens, total)
  keep <- which(end > offset & start < to)
  if (length(keep) == 0L){
    return(list(size = total, offset = offset, n_tokens = 0L, next_offset = NULL, passages = list()))
  }

  regions <- cpos[keep, , drop = FALSE]
  regions[1, 1] <- regions[1, 1] + (offset - start[keep[1]]) * (offset > start[keep[1]])
  last <- nrow(regions)
  regions[last, 2] <- regions[last, 2] - max(0L, end[keep[last]] - to)

  text <- vapply(
    seq_len(nrow(regions)),
    function(i) get_token_stream(
      regions[i, 1]:regions[i, 2], corpus = slot(x, "corpus"),
      registry = slot(x, "registry_dir"), encoding = slot(x, "encoding"),
      p_attribute = "word", collapse = " ", beautify = TRUE
    ),
    FUN.VALUE = character(1L)
  )

  if (length(s_attributes) > 0L){
    meta <- s_attributes(x, s_attributes, unique = FALSE)
    # For one s-attribute, the return value is a vector, not a table
    meta <- if (is.data.frame(meta)){
      as.data.frame(meta)
    } else {
      structure(data.frame(meta, stringsAsFactors = FALSE), names = s_attributes)
    }
    if (nrow(meta) == nrow(cpos)){
      meta <- meta[keep, s_attributes, drop = FALSE]
    } else {
      meta <- NULL
    }
  } else {
    meta <- NULL
  }

  if (is.null(meta)){
    passages <- data.frame(text = paste(text, collapse = "\n"))
  } else {
    # Consecutive regions with the same metadata are one passage
    group <- rleidv(meta)
    passages <- meta[!duplicated(group), , drop = FALSE]
    passages[["text"]] <- unname(vapply(
      split(text, group), paste, collapse = " ", FUN.VALUE = character(1L)
    ))
    rownames(passages) <- NULL
  }

  list(
    size = total,
    offset = offset,
    n_tokens = to - offset,
    next_offset = if (to < total) to else NULL,
    passages = passages
  )
}


# Drop rows of a table with stopwords and punctuation in column 'col'.
#' @importFrom tm stopwords
mcp_drop_stopwords <- function(df, col, language){
  if (identical(language, "none")) return(df)
  if (!language %in% c("de", "en", "fr")){
    stop("stopwords needs to be 'de', 'en', 'fr' or 'none'")
  }
  token <- df[[col]]
  drop <- tolower(token) %in% stopwords(kind = language) |
    grepl("^[\\p{P}\\p{S}]+$", token, perl = TRUE)
  df[!drop, , drop = FALSE]
}


mcp_result <- function(id, result) list(jsonrpc = "2.0", id = id, result = result)

mcp_error <- function(id, code, message){
  list(jsonrpc = "2.0", id = id, error = list(code = code, message = message))
}

mcp_empty_object <- function() structure(list(), names = character())

mcp_json <- function(x){
  as.character(toJSON(
    x, auto_unbox = TRUE, null = "null", na = "null",
    dataframe = "rows", digits = I(4), POSIXt = "ISO8601"
  ))
}

mcp_arg_chr <- function(args, name, default = NULL, required = FALSE){
  value <- args[[name]]
  if (is.null(value)){
    if (required) stop(sprintf("argument '%s' is required", name))
    return(default)
  }
  as.character(unlist(value))
}

mcp_arg_lgl <- function(args, name, default){
  value <- args[[name]]
  if (is.null(value)) return(default)
  value <- as.logical(unlist(value))
  if (length(value) != 1L || is.na(value)){
    stop(sprintf("argument '%s' needs to be true or false", name))
  }
  value
}

mcp_arg_int <- function(args, name, default, max = .Machine$integer.max){
  value <- args[[name]]
  if (is.null(value)) return(default)
  value <- suppressWarnings(as.integer(unlist(value)))
  if (length(value) != 1L || is.na(value) || value < 0L){
    stop(sprintf("argument '%s' needs to be a non-negative integer", name))
  }
  min(value, as.integer(max))
}
