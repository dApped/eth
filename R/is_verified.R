#' Check whether a contract has verified source code
#'
#' Check whether a contract has been verified by Etherscan.
#'
#' @param address Character. A single ethereum contract address as a character
#'   string (40 hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @return Logical. `TRUE` if the contract is verified.
#' @details `is_verified` uses the Etherscan API. Register for an API key
#'   at the [Etherscan Developer APIs page](https://etherscan.io/apis).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs
#' are provided as a community service and without warranty, so please just use
#' what you need and no more. They support both GET/POST requests and a rate
#' limit of 5 requests/sec._
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @export
is_verified <- function(address, api_key) {
  address <- tolower(address)
  if(missing(api_key)) api_key <- ''
  if(!is_contract(address)) stop('Not a contract address.')
  u <- sprintf(
    'http://api.etherscan.io/api?module=contract&action=getabi&address=%s&format=raw&apikey=%s',
    address, api_key)
  j <- suppressWarnings(readLines(u))
  j != 'Contract source code not verified'
}
