library(stringi)
library(magrittr)
library(data.table)

Rdir <- "~/Lab/github/polmineR/R"

Y <- lapply(
  list.files(path = Rdir, full.names = TRUE),
  function(filename){
    content <- scan(file = filename, what = character(), sep = "\n", quiet = TRUE)
    hits <- stri_match_all(content, regex = '^setMethod\\("(.*?)"\\s*,\\s*"(.*?)".*')
    M <- do.call(rbind, lapply(hits, as.vector))
    M[!is.na(M[,1]),]
  })
dt <- data.table(do.call(rbind, Y))
dt <- dt[,2:3, with = FALSE]
colnames(dt) <- c("method", "class")
# dt is a two-column data.table, with methods (first column) defined for classes (second column)

# to get methods defines for a class
dummy <- sapply(paste(dt[class == "cooccurrences"][["method"]], "\n", sep = ""), cat)

# to crosstabulate methods / classes
dt2 <- dcast(dt, class~method)
View(dt2)
