#' Get event logs
#'
#' Get event logs for a given contract address and topic
#'
#' @param address Character. A single Ethereum contract address as a character
#'   string (40 hexadecimal characters prepended by '0x').
#' @param topic0 Character. The topic0 keccak256 hash.
#' @param fromBlock Numeric. The minimum block number of interest.
#' @param toBlock Numeric. The maximum block number of interest. Also accepts
#'   `'latest'`.
#' @param api_key An Etherscan API key (see Details).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs
#' are provided as a community service and without warranty, so please just use
#' what you need and no more. They support both GET/POST requests and a rate
#' limit of 5 requests/sec._
#' @keywords Ethereum, transaction, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble distinct bind_rows
#' @export
get_events <- function(address, topic0, fromBlock, toBlock, api_key) {
  message('Getting events...')
  u <- sprintf('https://api.etherscan.io/api?module=logs&action=getLogs&fromBlock=%s&toBlock=%s&address=%s&topic0=%s&apikey=%s',
               fromBlock, toBlock, address, topic0, api_key)
  out <- list()
  out[[1]] <- events <- jsonlite::fromJSON(u)$result
  while(nrow(events) == 1000) {
    message('Getting events...')
    lastBlockNumber <- tail(as.numeric(events$blockNumber), 1)
    u <- sprintf('https://api.etherscan.io/api?module=logs&action=getLogs&fromBlock=%s&toBlock=%s&address=%s&topic0=%s&apikey=%s',
                 lastBlockNumber, toBlock, address, topic0, api_key)
    events <- jsonlite::fromJSON(u)$result
    out <- c(out, list(events))
  }
  dplyr::distinct(dplyr::as_tibble(dplyr::bind_rows(out)))
}
