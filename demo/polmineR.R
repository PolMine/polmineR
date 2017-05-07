library(polmineR)


# check whether CORPUS_REGISTRY environment variable is set
Sys.getenv("CORPUS_REGISTRY")
readline("press key to continue ...")

# check corpora that are available
corpus()
readline("press key to continue ...")



use("europarl.en")

P <- partition("EUROPARL-EN", text_year = "2005")

kwic(P, "Brussels")
