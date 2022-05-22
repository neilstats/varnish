
#' Varnish a table
#'
#' @param x
#' @param v
#' @param c
#' @param underline_i
#' @param underline_j
#' @param widths
#' @param unit
#'
#' @return
#' @export
#'
#' @examples
varnish <- function(x,
                    v = NULL,
                    c = NULL,
                    underline_i = 1,
                    underline_j = NULL,
                    widths = NULL,
                    unit = "mm"){
  x %>%
    add_headers(v = v, c = c) %>%
    style_table() %>%
    underline_header(i = underline_i, j = underline_j) %>%
    colwidths(w = widths,
              unit = unit)
}


#' Add header rows
#'
#' @param x A flextable
#' @param headers A list of
#'
#' @return
#' @export
#'
#' @examples
add_headers <- function(x, v = NULL, c = NULL){
  if (!is.list(v)) v <- list(v)
  if (!is.list(c)) c <- list(c)
  x <- flextable::delete_part(x)
  for (i in 1:length(v)){
    x <- flextable::add_header_row(x,
                                   values = rev(v)[[i]],
                                   colwidths = rev(c)[[i]])
  }
  x
}



#' Apply overall style to a flextable
#'
#' @param x A flextable
#'
#' @return A flextable
#' @export
#'
#' @examples
style_table <- function(x){
  x %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::hline_top(part = "header", border = officer::fp_border()) %>%
    flextable::hline_bottom(part = "header", border = officer::fp_border()) %>%
    flextable::hline_bottom(part = "all", border = officer::fp_border()) %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "right", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::padding(padding.left = 4, padding.right = 4, padding.top = 1, padding.bottom = 1, part = "all")
}



#' Underline column headings in a flextable
#'
#' @param x A flextable
#' @param i Row
#' @param j Column(s)
#'
#' @return A flextable
#' @export
#'
#' @examples
underline_header <- function(x,
                             i = 1,
                             j){
  if (is.null(j)) return(x)
  flextable::style(x,
                   i = i, j = j,
                   part = "header",
                   pr_p = officer::fp_par(border.bottom = officer::fp_border(),
                                          text.align = "center",
                                          padding.right  = 4,
                                          padding.left   = 4,
                                          padding.top    = 1,
                                          padding.bottom = 1)) %>%
    flextable::align(i = i, j = j, align = "center", part = "header")
}


#' Add footnote to a flextable
#'
#' @param x A flextable
#' @param str The footnote text (a character string).
#' @param symb Footnote symbol (Default: "")
#' @param i Row
#' @param j Column
#' @param size Text size (Default: 8)
#' @param part Part of flextable (Default: "body")
#' @param inline whether to add footnote on same line as previous footnote or not (Default: TRUE)
#' @param sep character string to use as a separator between footnotes (Default: "\\n")
#'
#' @return A flextable
#' @export
#'
#' @examples
add_footnote <- function(
  x,
  str,
  symb = "",
  i = NULL,
  j = NULL,
  size = 8,
  part = "body",
  inline = TRUE,
  sep = "\n"
  ) {
  flextable::footnote(x,
                      i = i,
                      j = j,
                      value = flextable::as_paragraph(
                        flextable::as_chunk(paste0(" ", str),
                                 props = officer::fp_text(font.size = size))),
                      ref_symbols = symb,
                      part        = part,
                      inline      = inline,
                      sep         = sep)
}

#' Convenience function for common footnote markers
#'
#' @param x "asterisk", "dagger", "ddagger", or "silcrow" (Default: "asterisk)
#'
#' @return A character string
#' @export
#'
#' @examples
marker <- function(x = "asterisk"){
  switch(x,
         asterisk = "*",
         dagger = "\u2020",
         ddagger = "\u2021",
         silcrow = "\u00A7"
         )
}


#' Control all column widths in a flextable
#'
#' @param x A flextable
#' @param w A numeric vector of widths
#' @param unit Unit for widths, one of "in", "cm", "mm" (Default: "mm")
#'
#' @return A flextable.
#' @export
#'
#' @examples
colwidths <- function(x,
                      w,
                      unit = "mm"){
  if (!is.null(w)) {
    for (i in 1:length(w)){x <- flextable::width(x, j = i, width = w[[i]], unit = unit)}
  }
  x
}
