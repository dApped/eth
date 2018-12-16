#' Get the name of a verified contract
#'
#' Return the name of a verified contract.
#'
#' @param address Character. A single verified ethereum contract address as a
#'   character string (40 hexadecimal characters prepended by '0x').
#' @param api_key An Etherscan API key (see Details).
#' @return The name of the contract.
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @seealso \code{\link{get_abi}}
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
