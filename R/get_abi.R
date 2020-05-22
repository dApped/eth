#' Return the ABI of a contract
#'
#' Check whether a contract has been verified by Etherscan.
#'
#' @param address Character. A single ethereum contract address as a character
#'   string (40 hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @return An `abi` object (inherits `json`) giving the ABI of the
#'   contract.
#' @details `get_abi` uses the Etherscan API. Register for an API key
#'   at the [Etherscan Developer APIs page](https://etherscan.io/apis).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer
#' APIs are provided as a community service and without warranty, so please just
#' use what you need and no more. They support both GET/POST requests and a rate
#' limit of 5 requests/sec._
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @seealso [parse_abi()]
#' @importFrom jsonlite prettify
#' @export
get_abi <- function(address, api_key) {
  address <- tolower(address)
  if(missing(api_key)) api_key <- ''
  if(!is_verified(address)) stop('Contract is not verified.')
  u <- sprintf(
    'http://api.etherscan.io/api?module=contract&action=getabi&address=%s&format=raw&apikey=%s',
    address, api_key)
  j <- jsonlite::prettify(suppressWarnings(readLines(u)))
  class(j) <- c('abi', class(j))
  j
}
