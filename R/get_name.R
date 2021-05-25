#' Get the name of a verified contract
#'
#' Return the name of a verified contract.
#'
#' @param address Character. A single verified ethereum contract address as a
#'   character string (40 hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @return The name of the contract.
#' @details `get_abi` uses the Etherscan API. Register for an API key
#'   at the [Etherscan Developer APIs page](https://etherscan.io/apis).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer
#' APIs are provided as a community service and without warranty, so please just
#' use what you need and no more. They support both GET/POST requests and a rate
#' limit of 5 requests/sec._
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @seealso [get_abi()]
#' @importFrom crul HttpRequest AsyncQueue
#' @importFrom jsonlite fromJSON
#' @export
get_name <- function(address, api_key) {
  address <- tolower(address)
  if(missing(api_key)) {
    api_key <- ''
    warning('No API key provided. Rate limited to 1 request per 5 seconds.')
  }
  urls <- sprintf(
    'http://api.etherscan.io/api?module=contract&action=getsourcecode&address=%s&apikey=%s',
    address, api_key)
  reqs <- lapply(urls, function(u) crul::HttpRequest$new(u)$get())
  if(api_key == '') {
    out <- crul::AsyncQueue$new(.list = reqs, bucket_size = 1, sleep = 5)
  } else {
    out <- crul::AsyncQueue$new(.list = reqs, bucket_size = 5, sleep = 1)
  }
  out$request()
  results <- lapply(out$responses(), function(x) jsonlite::fromJSON(x$parse('UTF-8')))
  contract_names <- sapply(results, function(x) x$result$ContractName)

  contract_names
}
