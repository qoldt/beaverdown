#' Render LaTeX captions
#'
#' Captions are not rendered to latex by default. To aid in writing these, This
#' function will take in your markdown-formatted caption, and give you a latex
#' formatted caption.
#'
#' @param caption the markdown-formatted text you want to render as a figure
#'   caption.
#' @param figname the name of the figure (as to not crowd the )
#'
#' @return a figure caption rendered in LaTeX
#' @export
#' @details This works by utilizing the \pkg{knitr} function
#'   \code{\link[knitr]{pandoc}}. It allows you to include references in your
#'   captions, which are not automatically rendered.
#'
#' @examples
#'
#' # Setup for the example
#' # Note that this will already be set for you when you run the document
#' rootdir <- find.package("beaverdown")
#' rootdir <- paste0(rootdir, "/rmarkdown/templates/oregonstate/skeleton/")
#' knitr::opts_knit$set(root.dir = rootdir)
#'
#'
#' caption <- "This is some *text* to use as a caption [@angel2000]!"
#' # Make sure to set your index to whatever your project is called!
#' render_caption(caption, index = "skeleton.Rmd")
#'
#'
#' @importFrom yaml yaml.load
render_caption <- function(caption, figname = "fig1", index = "index.Rmd"){
  the_root  <- knitr::opts_knit$get("root.dir")
  yml       <- readLines(file.path(the_root, index))
  yml_lines <- which(yml == "---")[1:2]
  yml_lines <- seq(f = yml_lines[1] + 1, t = yml_lines[2] - 1, b = 1)
  yml       <- yaml::yaml.load(paste(yml[yml_lines], collapse = "\n"))

  tmpdir <- tempdir()
  bib    <- file.path(the_root, yml$bibliography)
  bib    <- paste(bib, collapse = "\n    ")
  csl    <- file.path(the_root, yml$csl)

  txt <- paste0("<!--pandoc
t: latex
s:
mathjax:
number-sections:
bibliography: ", bib, "\n",
"csl: ", csl, "\n",
"o: ", paste0(tmpdir, "/", figname, ".tex"), "\n",
"-->

CUT=======

", caption,"

CUT=======
")

  intmp <- paste0(tmpdir, "/", figname, ".Rmd")
  cat(txt, file = intmp)

  outfile <- knitr::pandoc(intmp)
  tmptxt  <- readLines(outfile)
  outinds <- which(tmptxt == "CUT=======")
  inds    <- seq(from = outinds[1] + 2, to = outinds[2] - 2, by = 1)
  return(paste(tmptxt[inds], collapse = "\n"))
}
