## ---- eval = TRUE-------------------------------------------------------------
library(polmineR)

## ---- eval = FALSE------------------------------------------------------------
#  gparl <- corpus("GERMAPARL", server = "http://opencpu.politik.uni-due.de")

## ---- eval = FALSE------------------------------------------------------------
#  is(gparl)

## ---- eval = FALSE------------------------------------------------------------
#  size(gparl)

## ---- eval = FALSE------------------------------------------------------------
#  s_attributes(gparl)

## ---- eval = FALSE------------------------------------------------------------
#  gparl2006 <- subset(gparl, year == "2006")

## ---- eval = FALSE------------------------------------------------------------
#  is(gparl2006)

## ---- eval = FALSE------------------------------------------------------------
#  count(gparl, query = "Integration")

## ---- eval = FALSE------------------------------------------------------------
#  count(gparl2006, query = "Integration")

## ---- render = knit_print, message = FALSE, eval = FALSE----------------------
#  kwic(gparl, query = "Islam", left = 15, right = 15, meta = c("speaker", "party", "date"))

## ---- render = knit_print, message = FALSE, eval = FALSE----------------------
#  kwic(gparl2006, query = "Islam", left = 15, right = 15, meta = c("speaker", "party", "date"))

## ---- eval = FALSE------------------------------------------------------------
#  ##
#  ## registry entry for corpus GERMAPARLSAMPLE
#  ##
#  
#  # long descriptive name for the corpus
#  NAME "GermaParlSample"
#  # corpus ID (must be lowercase in registry!)
#  ID   germaparlsample
#  # path to binary data files
#  HOME http://localhost:8005
#  # optional info file (displayed by ",info;" command in CQP)
#  INFO https://zenodo.org/record/3823245#.XsrU-8ZCT_Q
#  
#  # corpus properties provide additional information about the corpus:
#  ##:: user = "YOUR_USER_NAME"
#  ##:: password = "YOUR_PASSWORD"

