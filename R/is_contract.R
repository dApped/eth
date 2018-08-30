#' Check whether an address is a contract address
#'
#' Check whether an Ethereum address is a contract address or an external
#' address.
#'
#' @param address Character. A single ethereum address as a character string (40
#'   hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @return Logical. \code{TRUE} if the address is a contract address.
#' @details \code{is_contract} uses the Etherscan API. Register for an API key
#'   at the \href{https://etherscan.io/apis}{\emph{Etherscan Developer APIs
#'   page}}.
#' @section Warning:
#' As per the Etherscan documentation, \emph{the Etherscan Ethereum Developer APIs are
#' provided as a community service and without warranty, so please just use what
#' you need and no more. They support both GET/POST requests and a rate limit of
#' 5 requests/sec.}
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @export
is_contract <- function(address, api_key) {
  address <- tolower(address)
  if(missing(api_key)) api_key <- ''
  j <- fromJSON(sprintf(
    'https://api.etherscan.io/api?module=proxy&action=eth_getCode&address=%s&apikey=%s',
    address, api_key))
  if(!is.null(j$error)) stop('Invalid address.\n', j$error$message)
  if(j$result == '0x') FALSE else TRUE
}
