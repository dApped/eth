#' Get Ethereum balance for an address
#'
#' Get normal or internal transactions for an Ethereum address.
#'
#' @param address Character. A character vector of one or more ethereum addresses (40
#'   hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @return A numeric vector of balances (in ether units) corresponding to the
#'   elements of `address`.
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs are
#' provided as a community service and without warranty, so please just use what
#' you need and no more. They support both GET/POST requests and a rate limit of
#' 5 requests/sec._
#' @keywords Ethereum, transaction, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @export
get_balance <- function(address, api_key) {
  address <- tolower(address)
  if(missing(api_key)) api_key <- ''
  if(length(address)==1) {
    j <- jsonlite::fromJSON(sprintf(
      'https://api.etherscan.io/api?module=account&action=balance&address=%s&tag=latest&apikey=%s',
      address, api_key))
  } else {
    j <- jsonlite::fromJSON(sprintf(
      'https://api.etherscan.io/api?module=account&action=balancemulti&address=%s&tag=latest&apikey=%s',
      paste0(address, collapse=','), api_key))
  }
  if(j$status != '1') {
    stop('Invalid address', call. = FALSE)
  }
  if(is.list(j$result)) {
    balance <- j$result$balance
  } else {
    balance <- j$result
  }
  as.numeric(balance)/1e18
}
