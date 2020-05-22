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
#' @importFrom jsonlite prettify
#' @importFrom rvest html_node html_text
#' @importFrom xml2 read_html
#' @export
get_name <- function(address, api_key) {
  address <- tolower(address)
  if(missing(api_key)) api_key <- ''
  if(!is_verified(address)) stop('Contract is not verified.')

  # u <- sprintf(
  #   'http://api.etherscan.io/api?module=contract&action=getsourcecode&address=%s&apikey=%s',
  #   address, api_key)
  # j <- jsonlite::fromJSON(u)
  # if(j$status != '1') {
  #   stop('Invalid address', call. = FALSE)
  # }
  # j$result$ContractName

  # Faster but less robust
  h <- xml2::read_html(
    sprintf('https://etherscan.io/address/%s#code', address))
  rvest::html_node(
    h, css='#ContentPlaceHolder1_contractCodeDiv > div:nth-child(2) > table > tr:nth-child(1) > td:nth-child(2)') %>%
    rvest::html_text() %>%
    gsub('\\n', '', .)
}
