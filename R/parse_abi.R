#' Parse an ABI
#'
#' Parse an `abi` object returned by [get_abi()].
#'
#' @param abi An `abi` object returned by [get_abi()].
#' @return A `tbl_df` with the following columns:
#'   \itemize{
#'     \item{`constant (<lgl>)`}{}
#'     \item{`name (<chr>)`}{}
#'     \item{`input_indexed (<chr>)`}{}
#'     \item{`input_name (<chr>)`}{}
#'     \item{`input_type (<chr>)`}{}
#'     \item{`output_name (<chr>)`}{}
#'     \item{`output_type (<chr>)`}{}
#'     \item{`payable (<lgl>)`}{}
#'     \item{`stateMutability (<chr>)`}{}
#'     \item{`type (<chr>)`}{}
#'     \item{`anonymous (<lgl>)`}{}
#'     \item{`signature (<chr>)`}{}
#'     \item{`method (<chr>)`}{}
#'   }
#' @details The values given in the `method` column are the first 8
#'   characters (4 bytes) of the keccak256 hashes of the method signatures.
#' @keywords Ethereum, contract, blockchain, cryptocurrency, crypto, ETH
#' @seealso [get_abi()] [keccak256()]
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
