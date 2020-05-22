#' Find all the contracts created by an Ethereum address
#'
#' Return the addresses of all contracts created by an Ethereum address.
#'
#' @param address Character. A single ethereum address as a character string (40
#'   hexadecimal characters prepended by '0x').
#' @param simple Logical. If `TRUE`, only a vector of addresses is
#'   returned. If `FALSE`, details regarding transactions creating
#'   contracts are returned.
#' @param api_key An Etherscan API key (see Details).
#' @return If `simple` is `TRUE`, a `tbl_df` containing details
#'   (as per [get_txs()]) of transactions that created contracts. If
#'   `simple` is `FALSE`, a vector of contract addresses.
#' @details `created` uses the Etherscan API. Register for an API key at
#'   the [_Etherscan Developer APIs page_](https://etherscan.io/apis).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs are
#' provided as a community service and without warranty, so please just use what
#' you need and no more. They support both GET/POST requests and a rate limit of
#' 5 requests/sec._
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @export
created <- function(address, simple=FALSE, api_key) {
  txs <- get_txs(address, api_key) %>%
    dplyr::filter_(~ contractAddress != '')
  if(isTRUE(simple)) txs$contractAddress else txs
}
