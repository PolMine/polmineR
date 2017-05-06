library(polmineR)

message("Make sure that the CORPUS_REGISTRY environment variable is set ...")
  
Sys.getenv("CORPUS_REGISTRY")

message("Check which corpora are available ...")

corpus()

use("europarl.en")

P <- partition("EUROPARL-EN", text_year = "2005")

kwic(P, "Brussels")
