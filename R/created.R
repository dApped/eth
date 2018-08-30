#' Find all the contracts created by an Ethereum address
#'
#' Return the addresses of all contracts created by an Ethereum address.
#'
#' @param address Character. A single ethereum address as a character string (40
#'   hexadecimal characters prepended by '0x').
#' @param simple Logical. If \code{TRUE}, only a vector of addresses is
#'   returned. If \code{FALSE}, details regarding transactions creating
#'   contracts are returned.
#' @param api_key An Etherscan API key (see Details).
#' @return If \code{simple} is \code{TRUE}, a \code{tbl_df} containing details
#'   (as per \code{\link{get_txs}}) of transactions that created contracts. If
#'   \code{simple} is \code{FALSE}, a vector of contract addresses.
#' @details \code{created} uses the Etherscan API. Register for an API key at
#'   the \href{https://etherscan.io/apis}{\emph{Etherscan Developer APIs page}}.
#' @section Warning:
#' As per the Etherscan documentation, \emph{the Etherscan Ethereum Developer APIs are
#' provided as a community service and without warranty, so please just use what
#' you need and no more. They support both GET/POST requests and a rate limit of
#' 5 requests/sec.}
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @export
created <- function(address, simple=FALSE, api_key) {
  txs <- get_txs(address, api_key) %>%
    dplyr::filter_(~ contractAddress != '')
  if(isTRUE(simple)) txs$contractAddress else txs
}
