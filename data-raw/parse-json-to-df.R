
library(dplyr)
library(purrr)
library(stringr)
source(here::here("data-raw", "lex.R"))

relative_commands <- c(
  'p', 'N', 't', 'L', 'R', 'e', 'S', 'q'
)

absolute_commands <- c(
  'Z', 'W', 'w', 'H', 'h'
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse the header of a symbol description to a list of information
#'
#' Currently the only information is the size of the font grid given as "Sx,y"
#'
#' @param symbol_header just the header part of a symbol description e.g. "S2.9"
#'
#' @return list of information: width, height
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_symbol_header_to_list <- function(symbol_header) {
  symbol_header <- substr(symbol_header, 2, nchar(symbol_header))
  grid_dims <- strsplit(symbol_header, ",")[[1]]
  list(
    width  = as.numeric(grid_dims[1]),
    height = as.numeric(grid_dims[2])
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse symbol string to a data.frame of coordinate information
#'
#' @param symbol symbol string e.g. \code{S4,9:M1,4DRtpLeS2qRt} for the letter 'e' in
#'        the original font from Inconvergent.
#'
#' @return font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_symbol_string_to_paths <- function(symbol) {
  symbol  <- gsub("\\s", '', symbol)
  bits    <- strsplit(symbol, ':')[[1]]
  header  <- bits[1]
  strokes <- bits[2]

  info <- parse_symbol_header_to_list(header)
  paths <- strsplit(strokes, "\\|")[[1]]

  paths
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' stuff
#' @param symbol symbol string
#' @import dplyr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_symbol_string_to_df <- function(symbol) {
  symbol  <- gsub("\\s", '', symbol)
  bits    <- strsplit(symbol, ':')[[1]]
  header  <- bits[1]
  strokes <- bits[2]

  info <- parse_symbol_header_to_list(header)
  paths <- strsplit(strokes, "\\|")[[1]]
  dfs <- lapply(paths, convert_path_to_df, width = info$width, height = info$height)

  df <- dplyr::bind_rows(dfs, .id = 'stroke')
  df$stroke <- as.numeric(df$stroke)

  if (nrow(df) == 0) {
    df <- data.frame(stroke = 1, x = NA, y = NA, idx = 1)
  }

  df$width  <- info$width
  df$height <- info$height

  df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Multiple append
#'
#' This is similar to the built-in 'append' except it allows for insertion
#' of the given value at multiple locations.
#'
#' @param x original vector
#' @param value the value to be inserted
#' @param after the locations at which to insert the values
#'
#' @return updated x vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
multi_append <- function(x, value, after) {
  after <- sort(after, decreasing = TRUE)
  for (pos in after) {
    x <- append(x, value, pos)
  }
  x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a token to a numeric value
#'
#' @param token a 'num' or 'fraction' token
#'
#' @return numeric value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_token_to_numeric <- function(token) {
  if (names(token) == 'num') {
    value   <- as.numeric(token)
  } else if (names(token) == 'fraction') {
    values <- strsplit(token, '/')[[1]]
    value   <- as.numeric(values[1]) / as.numeric(values[2])
  } else {
    stop("unknown token - can't convert to numeric: ", names(token))
  }
  value
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a single stroke path to a data.frame representation
#'
#' @param path string representing a path e.g. 'S2DWe3,6'
#' @param width width of character grid
#' @param height height of character grid
#'
#' @return data.frame of points in the path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_path_to_df <- function(path, width, height) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert path to tokens
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- lex(path, symbol_patterns)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure each relative command is followed by a 'num' or 'fraction',
  # if not then add a 'num' = 1 after it.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rel_idx <- which(names(tokens) == 'relative')
  num_idx <- rel_idx + 1
  idx_missing_num <- which(!names(tokens[num_idx]) %in% c('num', 'fraction') |
                             is.na(tokens[num_idx]))
  rel_idx_missing_num <- rel_idx[idx_missing_num]

  tokens <- multi_append(tokens, c(num = 1), rel_idx_missing_num)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Starting conditions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- integer(0)
  y      <- integer(0)
  prevx  <- 0
  prevy  <- 0
  firstx <- 0
  firsty <- 0
  drawing <- FALSE


  while (length(tokens) != 0) {

    command <- tokens[ 1]
    tokens  <- tokens[-1]

    thisx <- prevx
    thisy <- prevy

    if (command == 'D') {
      drawing <- TRUE
      firstx <- thisx
      firsty <- thisy
    } else if (command == 'M') {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Argument to 'M' and relative commands may either be
      #  - a single numeric (meaning the same value in x and y)
      #  - or x and y values separated by a ","
      # So search ahead for a "," to figure out which case it may be.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (length(tokens) > 1 && tokens[2] == ',') {
        thisx <- convert_token_to_numeric(tokens[1])
        thisy <- convert_token_to_numeric(tokens[3])
        tokens <- tokens[-(1:3)]
      } else {
        thisx <- convert_token_to_numeric(tokens[1])
        thisy <- thisx
        tokens <- tokens[-1]
      }
    } else if (command %in% relative_commands) {
      if (length(tokens) > 1 && tokens[2] == ',') {
        multx <- convert_token_to_numeric(tokens[1])
        multy <- convert_token_to_numeric(tokens[3])
        tokens <- tokens[-(1:3)]
      } else {
        multx <- multy <- convert_token_to_numeric(tokens[1])
        tokens <- tokens[-1]
      }
      switch(
        command,
        p = { thisx <- prevx + -1 * multx; thisy = prevy + -1 * multy },
        N = { thisx <- prevx +  0 * multx; thisy = prevy + -1 * multy },
        t = { thisx <- prevx +  1 * multx; thisy = prevy + -1 * multy },
        L = { thisx <- prevx + -1 * multx; thisy = prevy +  0 * multy },
        R = { thisx <- prevx +  1 * multx; thisy = prevy +  0 * multy },
        e = { thisx <- prevx + -1 * multx; thisy = prevy +  1 * multy },
        S = { thisx <- prevx +  0 * multx; thisy = prevy +  1 * multy },
        q = { thisx <- prevx +  1 * multx; thisy = prevy +  1 * multy },
        stop("No such relative command: ", command)
      )
    } else if (command %in% absolute_commands) {
      switch(
        command,
        Z = { thisx <- firstx; thisy <- firsty },
        W = { thisx <-  width - 1  },
        w = { thisx <-          0  },
        H = { thisy <-          0  },
        h = { thisy <- height - 1  },
        stop("No such absolute command: ", command)
      )
    } else {
      stop("No such command: ", command)
    }

    prevx <- thisx
    prevy <- thisy
    if (drawing) {
      x <- c(x, thisx)
      y <- c(y, thisy)
    }
  }

  data.frame(x = x, y = y, idx = seq_along(x))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a font specified in Inconvergent json format to a data.frame of strokes and points
#'
#' @param font_json json file with font data
#'
#' @import purrr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_font_json_to_df <- function(font_json) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out all the group definitions and ensure that the "(name)" of the
  # group can be used as a regular expression
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  group_symbols <- font_json$groups
  group_names <- names(group_symbols)
  group_names <- gsub("[(]", "\\\\(", group_names, perl = TRUE)
  group_names <- gsub("[)]", "\\\\)", group_names, perl = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out all the symbols
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  descs <- font_json$symbols %>% purrr::map_chr('raw')


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # group definitions for symbols are recursively defined in the 'smooth'
  # font, so run 'symbol/group' look-up more than once to ensure all
  # replacements are made
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (j in 1:4) {
    for (i in seq_along(group_symbols)) {
      descs <- gsub(group_names[i], group_symbols[[i]], descs)
    }
  }

  dfs <- lapply(descs, parse_symbol_string_to_df)

  df <- dplyr::bind_rows(dfs, .id = 'char')

  as_tibble(df)
}



library(jsonlite)


original_json <- jsonlite::read_json(here::here("data-raw", "original.json"))
smooth_json   <- jsonlite::read_json(here::here("data-raw", "smooth.json"))

original_df <- convert_font_json_to_df(original_json)
smooth_df   <- convert_font_json_to_df(smooth_json)

# Inconvergent defines the fonts as being 0,0 in top left, where
# in R it is naturally bottom right. So flip the glyphs
original_df$y <- 9 - original_df$y
smooth_df$y   <- 9 - smooth_df$y

usethis::use_data(original_json, smooth_json,
                  original_df, smooth_df,
                  internal = FALSE, overwrite = TRUE)



if (FALSE) {

  library(dplyr)
  library(ggplot2)

  # font_json <- original_json
  font_json <- smooth_json

  group_symbols <- font_json$groups
  group_names <- names(group_symbols)
  group_names <- gsub("[(]", "\\\\(", group_names, perl = TRUE)
  group_names <- gsub("[)]", "\\\\)", group_names, perl = TRUE)

  library(purrr)
  descs <- font_json$symbols %>% map_chr('raw')

  # gropu definitions for symbols are recursively defined in the 'smooth'
  # font, so run 'symbol/group' lookup more than once
  for (j in 1:4) {
    for (i in seq_along(group_symbols)) {
      descs <- gsub(group_names[i], group_symbols[[i]], descs)
    }
  }


  symbol <- descs[['c']]

  paths <- parse_symbol_string_to_paths(symbol)
  paths
  # convert_path_to_df(paths[[1]])

  plot_df <- parse_symbol_string_to_df(symbol)


  ggplot(plot_df, aes(x, y)) +
    geom_point() +
    geom_path(aes(colour = as.factor(stroke))) +
    coord_equal() +
    theme_bw() +
    scale_y_reverse()


}


if (FALSE) {

  aa <- 1
  zz <- 'a'

  switch(
    zz,
    a = {aa <- aa + 1},
    b = 3
  )


}















