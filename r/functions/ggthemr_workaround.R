# ggthemr workaround for ggplot error: -----------------------------------
# https://github.com/Mikata-Project/ggthemr/issues/44

swatch_pal <- function() {
  function(n) {
    if(n > length(swatch()) - 1) warning(paste("Swatch only has", length(swatch())-1), " colours")
    color_list <- as.character(swatch()[2:length(swatch())])
    return(color_list[1:n])
  }
}

scale_colour_ggthemr_d <- function(...) {
  ggplot2::discrete_scale(
    "colour", "ggthemr_swatch_color",
    swatch_pal(),
    ...
  )
}