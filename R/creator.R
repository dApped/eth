#' Return the address of a contract's creator
#'
#' Return the Ethereum address of the creator of a contract.
#'
#' @param address Character. A single ethereum contract address as a character
#'   string (40 hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @return Character. The address that created the contract at `address`.
#' @details `creator` uses the Etherscan API. Register for an API key
#'   at the [_Etherscan Developer APIs_ page](https://etherscan.io/apis).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs are
#' provided as a community service and without warranty, so please just use what
#' you need and no more. They support both GET/POST requests and a rate limit of
#' 5 requests/sec._
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @export
creator <- function(address, api_key) {
  address <- tolower(address)
  if(missing(api_key)) api_key <- ''
  j <- fromJSON(sprintf(
    'https://api.etherscan.io/api?module=account&action=txlist&address=%s&startblock=0&page=1&offset=1&apikey=%s',
    address, api_key))
  if(j$status != '1') {
    stop('Invalid address', call. = FALSE)
  }
  if(j$result$to == '' & j$result$contractAddress == address) {
    j$result$from
  } else {
    stop('Address is not a contract')
  }
}
