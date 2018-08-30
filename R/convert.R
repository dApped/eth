#' Convert between Ether units
#'
#' Convert between Ether units.
#'
#' @param x Numeric. A vector giving values to be converted.
#' @param from Character. The units of Ether in which \code{x} is given. See
#'   \code{\link{eth_units}} for valid values. Default is \code{'wei'}.
#' @param to Character. The target unit of Ether to which \code{x} will be
#'   converted. See \code{\link{eth_units}} for valid values.
#' @return A vector giving \code{x} in units specified by \code{to}.
#' @keywords Ethereum, units, currency, ETH
#' @seealso \code{\link{eth_units}}
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#' @export
convert <- function(x, from='wei', to) {
  from <- tolower(from)
  to <- tolower(to)
  if(!from %in% names(units))
    stop(from, ' is not a valid unit. See `eth_units()`.', call.=FALSE)
  if(!to %in% names(units))
    stop(to, ' is not a valid unit. See `eth_units()`.', call.=FALSE)
  unname(x * units[from]/units[to])
}
