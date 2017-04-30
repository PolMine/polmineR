library(polmineR)

use("europarl.en")

P <- partition("EUROPARL-EN", text_year = "2005")

kwic(P, "Brussels")
