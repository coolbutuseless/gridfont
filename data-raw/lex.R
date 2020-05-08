


#-----------------------------------------------------------------------------
#' Break a string into labelled tokens based upon a set of patterns
#'
#' @param text a single character string
#' @param patterns a named vector of character strings.  Each string represents
#'                 a regex to match a token, and the name of the string is the
#'                 label for the token.  If the regex contains a captured group
#'                 it will be left as is, otherwise the whole regex will be
#'                 turned into a captured group. The patterns are used in order
#'                 such that an early match takes precedence over any later match.
#' @param debug print more debugging information about the matching. default: FALSE
#'
#' @return a named character vector with the names representing the token type
#'         and the contents representing the tokens
#'
#' @examples
#' lex("hello there 123.45", patterns=c(number=pattern_number, word="\\w+", whitespace="\\s+"))
#'
#' @importFrom stringr str_match_all
#' @export
#-----------------------------------------------------------------------------
lex <- function(text, patterns, debug=FALSE) {

  #---------------------------------------------------------------------------
  # disallow multiple capture groups in a single pattern.
  # i.e. patterns = c("(a|b)", "(c)|(d)")
  #---------------------------------------------------------------------------
  captured_groups <- str_match_all(patterns, "\\([^?]")
  n_captured_groups <- vapply(captured_groups, FUN = nrow, integer(1))
  if (any(n_captured_groups > 1)) {
    stop("Multiple captured groups in a single pattern are not allowed. Fix patterns: ", deparse(patterns[n_captured_groups > 1]))
  }


  #---------------------------------------------------------------------------
  # Insert a default pattern to match anything missed by the provided patterns
  #---------------------------------------------------------------------------
  patterns        <- c(patterns, .missing=".")
  pattern_labels  <- names(patterns)

  #---------------------------------------------------------------------------
  # if a pattern doesn't contain a captured group,
  # then turn the whole thing into a captured group
  #---------------------------------------------------------------------------
  patterns_wrap   <- ifelse(
    grepl("[^\\]\\([^?]|^\\([^?]", patterns), # if pattern contains a captured group
    patterns,                                 # leave it alone
    paste0('(', patterns, ')')                # otherwise convert whole pattern to captured group
  )

  #---------------------------------------------------------------------------
  # Combine all the patterns into a single regex
  #---------------------------------------------------------------------------
  pattern         <- paste(patterns_wrap, collapse='|')

  #---------------------------------------------------------------------------
  # Match all regex against the text
  #---------------------------------------------------------------------------
  token_matching  <- stringr::str_match_all(text, pattern)[[1]]

  if (debug) {
    colnames(token_matching) <- c("all", pattern_labels)
    print(token_matching)
  }

  #---------------------------------------------------------------------------
  # Extract the actual token and the pattern which matched the token
  #---------------------------------------------------------------------------
  pattern_idx    <- apply(token_matching[, -1, drop=FALSE], 1, function(x) {  which(!is.na(x))})
  tokens         <- apply(token_matching[, -1, drop=FALSE], 1, function(x) {x[which(!is.na(x))]})
  names(tokens)  <- pattern_labels[pattern_idx]


  #---------------------------------------------------------------------------
  # If any tokens were captured by the '.missing' pattern, then show
  # a warning message
  #---------------------------------------------------------------------------
  if (any(names(tokens) == '.missing')) {
    not_captured <- sort(unique(tokens[names(tokens) == '.missing']))
    warning("The following characters were not captured: ", deparse(not_captured))
  }

  tokens
}




#-----------------------------------------------------------------------------
#' Regex to match numbers
#' @export
#-----------------------------------------------------------------------------
pattern_number    <- '[+\\-]?(?:0|[1-9]\\d*)(?:\\.\\d*)?'




symbol_patterns = c(
  draw     = "D",
  move     = "M",
  relative = "[pNtLReSq]",
  sep      = ",",
  fraction = paste0(pattern_number, "/", pattern_number),
  num      = pattern_number,
  absolute = "[ZWwHh]",
  group    = "\\(.*?\\)"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Debugging symbol_patterns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  library(stringr)
  lex("S3DtS4", symbol_patterns)


  lex("(c-arc)", symbol_patterns)
  lex("M3,2DS4", symbol_patterns)

  library(purrr)
  original_json$symbols %>% map_chr('raw') -> symbol_descriptions


  for (ss in symbol_descriptions) {
    print("======================================")
    print(ss)
    ss <- strsplit(ss, ":")[[1]][2]
    bits <- strsplit(ss, "\\|")[[1]]

    for (bit in bits) {
      res <- lex(bit, symbol_patterns)
      print(unlist(res))
    }
  }

}
















