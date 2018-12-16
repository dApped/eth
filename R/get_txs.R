#' Get transactions for an address
#'
#' Get normal or internal transactions for an Ethereum address.
#'
#' @param address Character. A single ethereum address as a character string (40
#'   hexadecimal characters prepended by '0x').
#' @param internal Logical. Should normal (\code{FALSE}, default) or internal
#'   (\code{TRUE}) transactions be queried?
#' @param api_key An Etherscan API key (see Details).
#' @param no_errors Logical. Should unsuccessful transactions be omitted
#'   (\code{FALSE}, default)?
#' @return If \code{internal} is \code{FALSE}, a \code{tbl_df} with the
#'   following elements:
#'  \itemize{
#'    \item{\code{blockNumber (<dbl>)}}{}
#'    \item{\code{timeStamp (<dttm>)}}{}
#'    \item{\code{hash (<chr>)}}{}
#'    \item{\code{nonce (<dbl>)}}{}
#'    \item{\code{blockHash (<chr>)}}{}
#'    \item{\code{transactionIndex (<dbl>)}}{}
#'    \item{\code{from (<chr>)}}{}
#'    \item{\code{to (<chr>)}}{}
#'    \item{\code{value (<dbl>)}}{}
#'    \item{\code{gas (<dbl>)}}{}
#'    \item{\code{gasPrice (<dbl>)}}{}
#'    \item{\code{isError (<dbl>)}}{}
#'    \item{\code{txreceipt_status (<chr>)}}{}
#'    \item{\code{input (<chr>)}}{}
#'    \item{\code{contractAddress (<chr>)}}{}
#'  }
#'  If \code{internal} is \code{TRUE}, a \code{tbl_df} with the following
#'  elements:
#'  \itemize{
#'    \item{\code{blockNumber (<dbl>)}}{}
#'    \item{\code{timeStamp (<dttm>)}}{}
#'    \item{\code{hash (<chr>)}}{}
#'    \item{\code{from (<chr>)}}{}
#'    \item{\code{to (<chr>)}}{}
#'    \item{\code{value (<dbl>)}}{}
#'    \item{\code{contractAddress (<chr>)}}{}
#'    \item{\code{input (<chr>)}}{}
#'    \item{\code{type (<chr>)}}{}
#'    \item{\code{gas (<dbl>)}}{}
#'    \item{\code{gasUsed (<dbl>)}}{}
#'    \item{\code{traceId (<dbl>)}}{}
#'    \item{\code{isError (<dbl>)}}{}
#'    \item{\code{errCode (<chr>)}}{}
#'    \item{\code{value_eth (<dbl>)}}{}
#' }
#' @details \code{get_txs} uses the Etherscan API to source information about
#'   transactions to and from an Ethereum address. Register for an API key at
#'   the \href{https://etherscan.io/apis}{\emph{Etherscan Developer APIs page}}.
#'   Note that a maximum of 10000 transactions are returned.
#' @section Warning:
#' As per the Etherscan documentation, \emph{the Etherscan Ethereum Developer APIs are
#' provided as a community service and without warranty, so please just use what
#' you need and no more. They support both GET/POST requests and a rate limit of
#' 5 requests/sec.}
#' @keywords Ethereum, transaction, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter as.tbl mutate %>%
#' @export
get_txs <- function(address, api_key, internal=FALSE, no_errors=TRUE) {
  address <- tolower(address)
  if(missing(api_key)) api_key <- ''
  j <- jsonlite::fromJSON(sprintf(
    'http://api.etherscan.io/api?module=account&action=txlist%s&address=%s&startblock=0&endblock=99999999&sort=asc&apikey=%s',
    ifelse(isTRUE(internal), 'internal', ''), address, api_key))
  if(j$status != '1') {
    stop('Invalid address', call. = FALSE)
  }
  j <- if(isTRUE(no_errors))
    dplyr::filter_(j$result, ~ isError=='0') else j$result
  if(isTRUE(internal)) {
    j %>%
      dplyr::mutate_(
        timeStamp= ~ as.numeric(timeStamp),
        timeStamp= ~ as.POSIXct(timeStamp, origin='1970-01-01'),
        blockNumber= ~ as.numeric(blockNumber),
        value= ~ as.numeric(value),
        value_eth= ~ value/1e18,
        gas= ~ as.numeric(gas),
        isError= ~ as.numeric(isError),
        gasUsed= ~ as.numeric(gasUsed)) %>%
      dplyr::as.tbl()
  } else {
    j %>%
      dplyr::mutate_(
        timeStamp= ~ as.numeric(timeStamp),
        timeStamp= ~ as.POSIXct(timeStamp, origin='1970-01-01'),
        blockNumber= ~ as.numeric(blockNumber),
        nonce= ~ as.numeric(nonce),
        value= ~ as.numeric(value),
        transactionIndex= ~ as.numeric(transactionIndex),
        value_eth= ~ value/1e18,
        gas= ~ as.numeric(gas),
        gasPrice= ~ as.numeric(gasPrice),
        gasPrice_gwei= ~ gasPrice/1e9,
        isError= ~ as.numeric(isError),
        cumulativeGasUsed= ~ as.numeric(cumulativeGasUsed),
        gasUsed= ~ as.numeric(gasUsed),
        confirmations= ~ as.numeric(confirmations)) %>%
      dplyr::as.tbl()
  }
}
