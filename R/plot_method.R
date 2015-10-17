# setGeneric("plot", function(x, ...){UseMethod("plot")})
# 
# #' @param x the partitionBundle
# #' @param y the pAttribute
# #' @param weight whether to attach a weight
# #' @noRd
# setMethod("plot", "partitionBundle", function(x, y, weight="rel", ...){
#   tab <- as.matrix(x, pAttribute=y, weight=weight)
#   if (ncol(tab) == 2){
#     plot(tab[,1], tab[,2], ...)
#   } else if (ncol(tab) > 2){
#     pairs(tab, ...)
#   }
# })
