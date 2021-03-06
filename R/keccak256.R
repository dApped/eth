#' Return the Keccak-256 hash for a string
#'
#' Hash a string with Keccak-256.
#'
#' @param x Character. The string to hash.
#' @param truncate Logical. Truncate the hash to the first 4 bytes (8
#'   characters)?
#' @param prepend_0x Logical. Should the hash be prepended with 0x? Default =
#'   `FALSE`.
#' @return Character. The hash of `x`, truncated to 8 characters if
#'   `truncate` is `TRUE`.
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @references Uses a local copy of
#'   \href{https://github.com/emn178/js-sha3}{sha3.min.js (v0.8.0)} by Yi-Cyuan
#'   Chen. This is licensed under MIT.
#' @importFrom V8 v8
#' @export
keccak256 <- function(x, truncate=TRUE, prepend_0x=FALSE) {
  ct <- v8(global='window')
  ct$source(system.file('js', 'sha3.min.js', package='eth'))
  hash <- sapply(x, function(x) {
    if(is.na(x)) {
      NA
    } else {
      x <- ct$call('keccak256', x)
      if(isTRUE(truncate)) substr(x, 1, 8) else x
    }
  })
  if(isTRUE(prepend_0x)) paste0('0x', hash) else hash
}
