## ----loading_polmineR---------------------------------------------------------
library(polmineR)

## ----get_registry-------------------------------------------------------------
registry()

## ---- eval = FALSE------------------------------------------------------------
#  Sys.getenv("CORPUS_REGISTRY")

## ----use_polmineR_data, message = FALSE, eval = TRUE--------------------------
use("polmineR")

## ---- eval = TRUE, message = FALSE--------------------------------------------
corpus()

## ---- eval = FALSE, message = FALSE, results = 'hide'-------------------------
#  options()[grep("polmineR", names(options()))]

## -----------------------------------------------------------------------------
options("polmineR.left" = 15)
options("polmineR.right" = 15)
options("polmineR.mc" = FALSE)

## ---- echo = FALSE, message = FALSE-------------------------------------------
options("polmineR.pagelength" = 3L)

## ---- eval = TRUE, render = knit_print----------------------------------------
kwic("REUTERS", "oil")

## ---- render = knit_print-----------------------------------------------------
kwic("REUTERS", "oil", s_attributes = "places")

## ---- render = knit_print-----------------------------------------------------
kwic("REUTERS", "oil", s_attributes = c("id", "places"))

## ---- render = knit_print-----------------------------------------------------
kwic("REUTERS", '"oil" "price.*"')

## ---- eval = TRUE-------------------------------------------------------------
count("REUTERS", "Kuwait")
count("REUTERS", c("Kuwait", "USA", "Bahrain"))
count("REUTERS", c('"United" "States"', '"Saudi" "Arabia.*"'), cqp = TRUE)

## ---- eval = TRUE, message = FALSE--------------------------------------------
oil <- dispersion("REUTERS", query = "oil", s_attribute = "id", progress = FALSE)
saudi_arabia <- dispersion(
  "REUTERS", query = '"Saudi" "Arabia.*"',
  sAttribute = "id", cqp = TRUE, progress = FALSE
  )

## ---- eval = TRUE-------------------------------------------------------------
barplot(height = saudi_arabia[["count"]], names.arg = saudi_arabia[["id"]], las = 2)

## ---- eval = TRUE, message = FALSE--------------------------------------------
oil <- cooccurrences("REUTERS", query = "oil")
sa <- cooccurrences("REUTERS", query = '"Saudi" "Arabia.*"', left = 10, right = 10)
top5 <- subset(oil, rank_ll <= 5)

## ---- render = knit_print-----------------------------------------------------
top5

## ---- eval = TRUE-------------------------------------------------------------
as.data.frame(top5)

## ---- eval = TRUE, message = FALSE, results = 'hide'--------------------------
kuwait <- partition("REUTERS", places = "kuwait", regex = TRUE)

## ---- eval = TRUE-------------------------------------------------------------
kuwait

## ---- eval = TRUE, message = FALSE--------------------------------------------
saudi_arabia <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
s_attributes(saudi_arabia, "id")

## ---- eval = TRUE, message = FALSE--------------------------------------------
saudi_arabia <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
oil <- cooccurrences(saudi_arabia, "oil", p_attribute = "word", left = 10, right = 10)

## ---- eval = TRUE-------------------------------------------------------------
df <- as.data.frame(oil)
df[1:5, c("word", "ll", "rank_ll")]

## ---- eval = TRUE-------------------------------------------------------------
q1 <- dispersion(saudi_arabia, query = 'oil', s_attribute = "id", progress = FALSE)
q2 <- dispersion(saudi_arabia, query = c("oil", "barrel"), s_attribute = "id", progress = FALSE)

## ---- eval = TRUE, message = FALSE--------------------------------------------
saudi_arabia <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
saudi_arabia <- enrich(saudi_arabia, p_attribute = "word")

saudi_arabia_features <- features(saudi_arabia, "REUTERS", included = TRUE)
saudi_arabia_features_min <- subset(saudi_arabia_features, rank_chisquare <= 10.83 & count_coi >= 5)
as(round(saudi_arabia_features_min), "htmlwidget")

## ---- eval = TRUE, message = FALSE--------------------------------------------
df <- as.data.frame(saudi_arabia_features_min)
df_min <- df[,c("word", "count_coi", "count_ref", "chisquare")]

## ---- eval = TRUE-------------------------------------------------------------
articles <- corpus("REUTERS") %>% partition_bundle(s_attribute = "id", progress = FALSE)
articles_count <- count(articles, p_attribute = "word")
tdm <- as.TermDocumentMatrix(articles_count, col = "count", verbose = FALSE)

class(tdm) # to see what it is
show(tdm)
m <- as.matrix(tdm) # turn it into an ordinary matrix
m[c("oil", "barrel"),]

## ---- eval = TRUE, message = FALSE--------------------------------------------
P <- partition("REUTERS", id = "248")
H <- html(P, height = "250px")
H

## ---- eval = FALSE------------------------------------------------------------
#  Sys.setenv(CORPUS_REGISTRY = "C:/PATH/TO/YOUR/REGISTRY")

