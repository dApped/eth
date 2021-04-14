#' Get ERC20 token balances
#'
#' Get event logs for a given contract address and topic
#'
#' @param address Character. A single Ethereum contract address as a character
#'   string (40 hexadecimal characters prepended by '0x').
#' @param block Either `'latest'` or a number giving the block of interest.
#'   Default = `'latest'`.
#' @param api_key An Etherscan API key.
#' @param quiet Logical. Suppress progress messages? Default = `FALSE`.
#' @section Warning:
#' As per the Etherscan documentation, _the Etherscan Ethereum Developer APIs
#' are provided as a community service and without warranty, so please just use
#' what you need and no more. They support both GET/POST requests and a rate
#' limit of 5 requests/sec._
#' @keywords Ethereum, transaction, blockchain, cryptocurrency, crypto, ETH
#' @importFrom dplyr select mutate tibble arrange desc
#' @importFrom tidyr pivot_longer
#' @importFrom gmp as.bigz
#' @importFrom magrittr %>%
#' @export
erc20_balance <- function(address, block='latest', api_key, quiet=FALSE) {
  topic0 <- keccak256('Transfer(address,address,uint256)', truncate=FALSE,
                      prepend_0x=TRUE)
  transfers <- get_events(address, topic0, 0, block, api_key) %>%
    dplyr::mutate(blockNumber=as.numeric(blockNumber),
                  timeStamp=as.numeric(timeStamp),
                  logIndex=suppressWarnings(
                    ifelse(logIndex=='0x', 0, as.numeric(logIndex))
                  ),
                  from=sub('0x0{24}', '0x', sapply(topics, '[[', 2)),
                  to=sub('0x0{24}', '0x', sapply(topics, '[[', 3))) %>%
    dplyr::select(blockNumber, timeStamp, transactionHash, logIndex, from,
                  to, amount=data) %>%
    tidyr::pivot_longer(c(from, to), names_to='direction', values_to='address') %>%
    as.data.frame

  amounts <- gmp::as.bigz(transfers$amount)
  amounts[transfers$direction=='from'] <- -amounts[transfers$direction=='from']

  amounts_by_address <- split(amounts, transfers$address)
  balances <- lapply(seq_along(amounts_by_address), function(i) {
    if(!quiet) cat(sprintf('\r%s (%.02f%%)', names(amounts_by_address)[i],
                           i/length(amounts_by_address)*100))
    sum(amounts_by_address[[i]])
  }) %>% setNames(names(amounts_by_address))

  dplyr::tibble(address=names(balances),
                amount_num=sapply(balances, as.numeric),
                amount_txt=sapply(balances, as.character)) %>%
    dplyr::arrange(dplyr::desc(amount_num))
}
