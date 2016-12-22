#' Colorado State Demography Office ggplot2 Theme
#'
#' Custom \code{ggplot2} theme that borrows heavily from the
#'\code{theme_fivethirtyeight()} in ggthemes.
#'
#' @param base_size Base font size.
#' @param base_family Plot text font family.


theme_codemog <- function(base_size = 12, base_family = "sans"){
  codemog_pal=c(
    dkblu=rgb(31,73,125, max=255),
    dkred=rgb(192,80,77, max=255),
    dkgray = rgb(78, 87, 88, max = 255),
    medgray = rgb(210, 210, 210, max = 255),
    ltgray = rgb(208, 210, 211, max = 255),
    green = rgb(119, 171, 67, max = 255)
  )
  theme(
    line = element_line(),
    rect = element_blank(),
    text = element_text(colour = codemog_pal['dkgray'], size=base_size),
    axis.title = element_text(family=base_family, colour=codemog_pal['dkgray']),
    axis.text = element_text(colour=codemog_pal['dkgray'], family=base_family),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.background = element_rect(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    panel.grid = element_line(colour = NULL),
    panel.grid.major = element_line(colour = codemog_pal['medgray'], size=base_size*.05),
    panel.grid.minor = element_line(colour = codemog_pal['medgray'], size=base_size*.05),
    plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
    plot.margin = unit(c(.2, .2, .2, .2), "lines"),
    strip.background=element_rect())
}

#' Colorado State Demography Office Color Palette for ggplot2
#'
#'Custom color palette using a mix of SDO colors and DOLA
#' Brand Colors from Brand Colorado.
#'
#'
codemog_pal=c(
  dkblu=rgb(31,73,125, max=255),
  dkred=rgb(192,80,77, max=255),
  dkgray = rgb(78, 87, 88, max = 255),
  medgray = rgb(210, 210, 210, max = 255),
  ltgray = rgb(208, 210, 211, max = 255),
  green = rgb(119, 171, 67, max = 255)
)
