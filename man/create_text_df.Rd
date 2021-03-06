% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-text.R
\name{create_text_df}
\alias{create_text_df}
\title{Create data.frame of glyph information for the given text.}
\usage{
create_text_df(text, font = c("original", "smooth"), dx = 0, dy = 0)
}
\arguments{
\item{text}{single character string}

\item{font}{choose font: 'original' or 'smooth'. default: original}

\item{dx, dy}{character spacing in original units}
}
\value{
data.frame with coordinates for all the glyphs with characters offset
       appropriately.  \code{char_idx} is the index of the character within
       the given text string
}
\description{
Text input can contain multiple lines separated by carriage returns
}
