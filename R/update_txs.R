#' Update transactions for an address
#'
#' Update normal, internal, ERC-20, or ERC-721 transactions for an Ethereum
#' address.
#'
#' @param txs An object returned by [get_txs()].
#' @param api_key An Etherscan API key (see Details).
#' @param quiet Logical. Suppress messages? Default is `FALSE`.
#' @details `get_txs` uses the Etherscan API to source information about
#'   transactions to and from an Ethereum address. Register for an API key at
#'   the [Etherscan Developer APIs page](https://etherscan.io/apis).
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs
#' are provided as a community service and without warranty, so please just use
#' what you need and no more. They support both GET/POST requests and a rate
#' limit of 5 requests/sec._
#' @keywords Ethereum, transaction, blockchain, cryptocurrency, crypto, ETH
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter as_tibble mutate distinct select matches %>%
#' @importFrom lubridate with_tz now
#' @export
update_txs <- function(txs, api_key, quiet=FALSE) {
  if(missing(api_key)) stop('API Key required. See https://etherscan.io/apis')
  address <- attr(txs, 'address')
  network <- attr(txs, 'network')
  network <- ifelse(network=='mainnet', '', paste0('-', network))
  no_errors <- attr(txs, 'no_errors')
  type <- attr(txs, 'type')

  txtype <- switch(
    type, normal='txlist', internal='txlistinternal',
    ERC20='tokentx', ERC721='tokennfttx')

  .get_txs <- function(network, txtype, address, startblock, sort, api_key) {
    j <- jsonlite::fromJSON(sprintf(
      'http://api%s.etherscan.io/api?module=account&action=%s&address=%s&startblock=%s&endblock=99999999&sort=%s&apikey=%s',
      network, txtype, address, startblock, sort, api_key))
    if(j$status != '1') {
      stop('Invalid address', call. = FALSE)
    }
    j <- if(isTRUE(no_errors) && type %in% c('normal', 'internal'))
      dplyr::filter(j$result, isError=='0') else j$result
    switch(type,
           normal={
             j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 nonce=as.numeric(nonce),
                 value=as.numeric(value),
                 transactionIndex=as.numeric(transactionIndex),
                 value_eth=value/1e18,
                 gas=as.numeric(gas),
                 gasPrice=as.numeric(gasPrice),
                 gasPrice_gwei=gasPrice/1e9,
                 isError=as.numeric(isError),
                 cumulativeGasUsed=as.numeric(cumulativeGasUsed),
                 gasUsed=as.numeric(gasUsed),
                 confirmations=as.numeric(confirmations)
               ) %>%
               dplyr::as_tibble()
           },
           internal={
             j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 value=as.numeric(value),
                 value_eth=value/1e18,
                 gas=as.numeric(gas),
                 isError=as.numeric(isError),
                 gasUsed=as.numeric(gasUsed)
               ) %>%
               dplyr::as_tibble()
           },
           ERC20={
             j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 nonce=as.numeric(nonce),
                 tokenDecimal=as.numeric(tokenDecimal),
                 value=as.numeric(value)*10^-tokenDecimal,
                 transactionIndex=as.numeric(transactionIndex),
                 gas=as.numeric(gas),
                 gasPrice=as.numeric(gasPrice),
                 gasPrice_gwei=gasPrice/1e9,
                 gasUsed=as.numeric(gasUsed),
                 cumulativeGasUsed=as.numeric(cumulativeGasUsed),
                 confirmations=as.numeric(confirmations)
               ) %>%
               dplyr::as_tibble()
           },
           ERC721={
             j %>%
               dplyr::mutate(
                 timeStamp=as.numeric(timeStamp),
                 timeStamp=as.POSIXct(timeStamp, origin='1970-01-01'),
                 blockNumber=as.numeric(blockNumber),
                 nonce=as.numeric(nonce),
                 tokenID=as.integer(tokenID),
                 tokenDecimal=as.numeric(tokenDecimal),
                 transactionIndex=as.numeric(transactionIndex),
                 gas=as.numeric(gas),
                 gasPrice=as.numeric(gasPrice),
                 gasPrice_gwei=gasPrice/1e9,
                 gasUsed=as.numeric(gasUsed),
                 cumulativeGasUsed=as.numeric(cumulativeGasUsed),
                 confirmations=as.numeric(confirmations)
               ) %>%
               dplyr::as_tibble()
           }
    ) %>%
      dplyr::mutate(timeStamp=lubridate::with_tz(timeStamp, 'UTC'))
  }

  if(!quiet) message('Getting transactions...')
  .txs <- .get_txs(network, txtype, address, max(txs$blockNumber), 'asc', api_key) %>%
    dplyr::select(dplyr::matches('^((?!confirmations).)*$', perl=T))
  n <- nrow(.txs)
  txs <- rbind(txs, .txs)
  while(n == 10000) {
    if(!quiet) message('Getting more transactions...')
    .txs <- .get_txs(network, txtype, address, max(txs$blockNumber), 'asc', api_key) %>%
      dplyr::select(dplyr::matches('^((?!confirmations).)*$', perl=T))
    n <- nrow(.txs)
    txs <- rbind(txs, .txs)
  }
  now <- lubridate::now(tzone='UTC')
  txs <- txs %>% dplyr::distinct()
  attr(txs, 'last_updated') <- now
  txs
}
