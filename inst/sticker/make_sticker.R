library(hexSticker)
library(RColorBrewer)

imgurl <- "https://cdn.pixabay.com/photo/2017/09/27/22/28/text-mining-icon-2793702_960_720.png"
imgurl <- "https://cdn3.iconfinder.com/data/icons/ecology-43/64/x-17-512.png"
imgurl <- "https://www.conrad.com/medias/global/ce/4000_4999/4900/4980/4989/498985_BB_07_FB.EPS_1000.jpg"
sticker(
  imgurl,
  package = "polmineR",
  p_size = 8,
  s_x = 1, s_y = 0.8, s_width = 0.5, s_height = 0.3,
  h_fill = "white",
  h_color = "#004C93",
  p_color = "#004C93",
  filename="~/Lab/tmp/hexsticker_polmineR.png",
  url = "https://polmine.github.io/polmineR",
  u_size = 1.35,
  u_color = "#004C93",
  spotlight = FALSE,
  l_x = 0.8,
  l_y = 0.7,
  l_alpha = 0.8,
  l_width = 10,
  l_height = 4
)
