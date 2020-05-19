#background beige hex codes: E1C699 CFB997 F9E4B7 E8DCCA


#' @export
theme_arthur_simple <- function (bw=FALSE)
{
  #SET LATEX FONT#
  ### download font from: www.fontsquirrel.com/fonts/latin-modern-roman
  ### follow tutorial: https://medium.com/@fulowa/latex-font-in-a-ggplot-9120caaa5250

  sysfonts::font_add(family = "tex", regular = "C:/Windows/Fonts/LATIN MODERN ROMAN 10.ttf")

  ## Automatically use showtext to render text
  showtext::showtext_auto()

  if (bw == FALSE)
  {
    background1<-"white"
    background2<-"#E8DCCA"
    line1<-"white"
    line2<-"#707070"
    fontType<-"tex"
  }else{
    background1<-"white"
    background2<-"white"
    line1<-"#E0E0E0"
    line2<-"#707070"
    fontType<-"tex"
  }
  ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = background1),
                   panel.background = ggplot2::element_rect(fill = background2, colour = background2),
                   panel.grid.major = ggplot2::element_line(linetype = "dashed", color = line1),
                   panel.grid.minor = ggplot2::element_line(linetype = "dashed", color = line1),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   panel.border = ggplot2::element_rect(linetype = "dotted", color = line2, fill = NA),
                   #panel.border = ggplot2::element_blank(),
                   legend.position="bottom", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = background1, colour = background1),
                   axis.line= ggplot2::element_line(linetype = "solid", color = line2),
                   axis.title.x = ggplot2::element_blank(),
                   text= ggplot2::element_text(family=fontType))

}


#' @export
theme_marginal <- function (bw=FALSE)
{

  #SET LATEX FONT#
  ### download font from: www.fontsquirrel.com/fonts/latin-modern-roman
  ### follow tutorial: https://medium.com/@fulowa/latex-font-in-a-ggplot-9120caaa5250

  sysfonts::font_add(family = "tex", regular = "C:/Windows/Fonts/LATIN MODERN ROMAN 10.ttf")

  ## Automatically use showtext to render text
  showtext::showtext_auto()

  if (bw == FALSE)
  {
    background<-"#E8DCCA"
    fontType<-"tex"
  }else{
    background<-"white"
    fontType<-"tex"
  }
  ggplot2::theme_bw() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background, color = background),
                   panel.background = ggplot2::element_rect(fill = background, colour = background),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   panel.border = ggplot2::element_rect(linetype = "solid", color = background, fill = NA),
                   legend.position="none", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 12, colour = "black"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   text=ggplot2::element_text(family=fontType))

}

#' @export
theme_cnetwork <- function (bw=FALSE)
{

  #SET LATEX FONT#
  ### download font from: www.fontsquirrel.com/fonts/latin-modern-roman
  ### follow tutorial: https://medium.com/@fulowa/latex-font-in-a-ggplot-9120caaa5250

  sysfonts::font_add(family = "tex", regular = "C:/Windows/Fonts/LATIN MODERN ROMAN 10.ttf")

  ## Automatically use showtext to render text
  showtext::showtext_auto()

  if (bw == FALSE)
  {
    background<-"#E8DCCA"
    fontType<-"tex"
  }else{
    background<-"white"
    fontType<-"tex"
  }
  ggplot2::theme_bw() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background, color = background),
                   panel.background = ggplot2::element_rect(fill = background, colour = background),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, family=fontType),
                   panel.border = ggplot2::element_rect(linetype = "solid", color = background, fill = NA),
                   legend.position="none", legend.direction="horizontal", legend.title = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(size = 12, colour = "black"),
                   axis.text.y = ggplot2::element_text(size=7, family="serif"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(family=fontType),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())
}

#' @export
theme_colors <- list(
  #"blue"   = "#1c76ff",
  "purple"    = "#6c6dcc",
  "green"  = "#30f2d5",
  "pink" = "#b31460",
  "orange"   = "#bc7e36",
  #"black" = "080808",
  "yellow" = "#ffe41a"
  # "grey"   = "#9290a2"
)

#' @export
theme_pal <- function(primary = "purple", other = "orange", direction = 1)
{
  stopifnot(primary %in% names(theme_colors))

  function(n) {
    if (n > 6) warning("Arthur's Color Palette only has 6 colors.")

    if (n == 2) {
      other <- if (!other %in% names(theme_colors)) {
        other
      } else {
        theme_colors[other]
      }
      color_list <- c(other,theme_colors[primary])
    } else {
      color_list <- theme_colors[1:n]
    }

    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}

#' @export
scale_colour_arthur <- function(primary = "purple", other = "orange", direction = 1, ...)
{
  ggplot2::discrete_scale(
    "colour", "theme",
    theme_pal(primary, other, direction),
    ...
  )
}

#' @export
scale_fill_arthur <- function(primary = "purple", other = "orange", direction = 1, ...)
{
  ggplot2::discrete_scale(
    "fill", "theme",
    theme_pal(primary, other, direction),
    ...
  )
}
