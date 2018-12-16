#' get_tx
#'
#' get_tx
#'
#' @param tx A character vector of one or more transaction hashes (hexadecimal,
#'   64 characters, with 0x prepended).
#' @param api_key An Etherscan API key (see Details).
#' @return A \code{data.frame} with one row per element of \code{tx}, with 
#'   columns: \code{blockHash}, \code{blockNumber}, \code{from}, \code{gas}, 
#'   \code{gasPrice}, \code{hash}, \code{input}, \code{nonce}, \code{to}, 
#'   \code{transactionIndex}, \code{value}, \code{v}, \code{r}, \code{s}.
#' @details \code{get_tx} uses the Etherscan API to source information about
#'   transactions to and from an Ethereum address. Register for an API key at
#'   the \href{https://etherscan.io/apis}{\emph{Etherscan Developer APIs page}}.
#' @section Warning:
#' As per the Etherscan documentation, \emph{the Etherscan Ethereum Developer APIs are
#' provided as a community service and without warranty, so please just use what
#' you need and no more. They support both GET/POST requests and a rate limit of
#' 5 requests/sec.}
#' @keywords Ethereum, transaction, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows %>%
#' @export
get_tx <- function(tx, api_key) {
  if(missing(api_key)) api_key <- ''
  lapply(tx, function(x) {
    j <- jsonlite::fromJSON(
      sprintf('https://api.etherscan.io/api?module=proxy&action=eth_getTransactionByHash&txhash=%s&apikey=%s',
              x, api_key)
    )$result
    j
  }) %>% 
    dplyr::bind_rows() %>% 
    mutate(blockNumber=as.numeric(blockNumber),
           gas=as.numeric(gas),
           gasPrice=as.numeric(gasPrice)/1e9,
           nonce=as.numeric(nonce),
           transactionIndex=as.numeric(transactionIndex))
}
