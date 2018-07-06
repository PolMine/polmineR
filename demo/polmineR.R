library(polmineR)

watson.tts.process(
  creds = "~/Lab/tmp/watson.json",
  "What is the PolmIne Project about? We offer the code and the data we need to make progress. The polmIneR package offers: Counting, dispersion, kwic, concordances, co-occurrences.",
  11
)

# check whether CORPUS_REGISTRY environment variable is set
Sys.getenv("CORPUS_REGISTRY")
readline("press key to continue ...")

# check corpora that are available
corpus()
readline("press key to continue ...")



use("europarl.en")

P <- partition("EUROPARL-EN", text_year = "2005")

kwic(P, "Brussels")
