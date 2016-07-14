# library(inline)
# src <- c('
#   const char *registry = {"/Users/blaette/Lab/cwb/registry"};
#   const char *corpus = {"NEXIS"};
#   char * registry_2  = strdup(registry);
#   char * corpus_2 = strdup(corpus);
#   TCorpus * foo2 = cl_new_corpus(registry_2, corpus_2);
# return(0);
# '
# )
# 
# 
# 
# foo <- cfunction(sig=c(foo="integer"),
#             body=src,
#             # libargs="-L/Library/Frameworks/R.framework/Versions/3.2/Resources/library/rcqp/libs -lrcqp",
#           libargs="-static -lcl",
#           includes="#include <cwb/cl.h>"
#           )