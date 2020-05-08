
globalVariables(c('idx', 'x', 'xoffset', 'stroke'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create data.frame of glyph information for the given line of text.
#'
#' @inheritParams create_text_df
#'
#' @return data.frame with coordinates for all the glyphs with characters offset
#'        appropriately.  \code{char_idx} is the index of the character within
#'        the given text string
#'
#' @importFrom utils head
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_text_df_single_row <- function(text, font = 'original', dx = 0) {

  stopifnot(length(text) == 1)

  if (nchar(text) == 0) {
    return(data.frame())
  }


  font_df <- switch(
    font,
    original = gridfont::original_df,
    smooth   = gridfont::smooth_df,
    stop("No such font: ", font)
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # split text into characters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text <- tolower(text)
  text <- strsplit(text, '')[[1]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Replace any unknown chars with a blank space
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bad_idx <- !(text %in% font_df$char)
  text[bad_idx] <- ' '

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Merge the text info with the data.frame for each character from `arcade_df`
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- data.frame(
    char     = text,
    char_idx = seq_along(text),
    stringsAsFactors = FALSE
  )

  string_df <- merge(string_df, font_df, sort = FALSE, all.x = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure correct ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- with(string_df, string_df[order(char_idx, stroke, idx),])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the character offset
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  width_df <- subset(string_df, stroke == 1 & idx == 1)
  width_df <- width_df[, c('char_idx', 'width')]
  width_df$xoffset <- c(0, cumsum(head(width_df$width, -1) + dx))
  width_df$width <- NULL

  string_df <- merge(string_df, width_df, sort = FALSE, all.x = TRUE)
  string_df <- transform(string_df, x = x + xoffset)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure correct ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- with(string_df, string_df[order(char_idx, stroke, idx),])


  string_df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create data.frame of glyph information for the given text.
#'
#' Text input can contain multiple lines separated by carriage returns
#'
#' @param text single character string
#' @param dx,dy character spacing in original units
#' @param font choose font: 'original' or 'smooth'. default: original
#'
#' @return data.frame with coordinates for all the glyphs with characters offset
#'        appropriately.  \code{char_idx} is the index of the character within
#'        the given text string
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_text_df <- function(text, font = c('original', 'smooth'), dx = 0, dy = 0) {

  stopifnot(length(text) == 1)
  font <- match.arg(font)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the text at "\n" boundaries
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  texts  <- strsplit(text, "\n")[[1]]
  nchars <- cumsum(nchar(texts))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a string for each line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dfs <- lapply(texts, create_text_df_single_row, font=font, dx = dx)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update line numbering and character indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(dfs)) {
    if (nrow(dfs[[i]]) == 0) next
    dfs[[i]]$line     <- i
    if (i > 1) {
      dfs[[i]]$char_idx <- dfs[[i]]$char_idx + nchars[i-1]
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # combined all data.frames for each line, offset the y for each line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- do.call(rbind, dfs)
  res$y <- res$y - (res$line - 1) * (8 + dy)

  res
}



if (FALSE) {
  library(ggplot2)
  plot_df <- create_text_df('a\n\nb', font='smooth')

  ggplot(plot_df, aes(x, y)) +
    geom_path(aes(group = interaction(char_idx, stroke)), na.rm=TRUE) +
    coord_equal() +
    theme_void()
}














