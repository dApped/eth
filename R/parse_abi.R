#' Parse an ABI
#'
#' Parse an \code{abi} object returned by \code{\link{get_abi}}.
#'
#' @param abi An \code{abi} object returned by \code{\link{get_abi}}.
#' @return A \code{tbl_df} with the following columns:
#'   \itemize{
#'     \item{\code{constant (<lgl>)}}{}
#'     \item{\code{name (<chr>)}}{}
#'     \item{\code{input_indexed (<chr>)}}{}
#'     \item{\code{input_name (<chr>)}}{}
#'     \item{\code{input_type (<chr>)}}{}
#'     \item{\code{output_name (<chr>)}}{}
#'     \item{\code{output_type (<chr>)}}{}
#'     \item{\code{payable (<lgl>)}}{}
#'     \item{\code{stateMutability (<chr>)}}{}
#'     \item{\code{type (<chr>)}}{}
#'     \item{\code{anonymous (<lgl>)}}{}
#'     \item{\code{signature (<chr>)}}{}
#'     \item{\code{method (<chr>)}}{}
#'   }
#' @details The values given in the \code{method} column are the first 8
#'   characters (4 bytes) of the keccak256 hashes of the method signatures.
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @seealso \code{\link{get_abi}} \code{\link{keccak256}}
#' @importFrom jsonlite fromJSON prettify
#' @importFrom dplyr %>% bind_rows mutate select one_of
#' @export
parse_abi <- function(abi) {
  if(!inherits(abi, 'abi'))
    stop('Use `get_abi` to return an object of class "abi"')
  methods <- jsonlite::fromJSON(abi)
  inputs <- list(
    input_indexed=sapply(methods$inputs, function(x) paste(x$indexed, collapse=',')),
    input_name=sapply(methods$inputs, function(x) paste(x$name, collapse=',')),
    input_type=sapply(methods$inputs, function(x) paste(x$type, collapse=',')),
    output_name=sapply(methods$outputs, function(x) paste(x$name, collapse=',')),
    output_type=sapply(methods$outputs, function(x) paste(x$type, collapse=','))
  ) %>%
    dplyr::bind_rows()
  suppressWarnings(cbind(methods, inputs) %>%
    dplyr::select(dplyr::one_of(
      c('constant', 'name', 'input_indexed', 'input_name',
        'input_type', 'output_name', 'output_type',
        'payable', 'stateMutability', 'type', 'anonymous')))) %>%
    dplyr::mutate_(
      signature= ~ ifelse(is.na(name), NA, sprintf('%s(%s)', name, input_type)),
      method= ~ unname(keccak256(signature))) %>%
    lapply(function(x) ifelse(nchar(x)==0, NA, x)) %>%
    dplyr::bind_rows()
}
