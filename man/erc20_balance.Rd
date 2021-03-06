% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/erc20_balance.R
\name{erc20_balance}
\alias{erc20_balance}
\title{Get ERC20 token balances}
\usage{
erc20_balance(address, block = "latest", api_key, quiet = FALSE)
}
\arguments{
\item{address}{Character. A single Ethereum contract address as a character
string (40 hexadecimal characters prepended by '0x').}

\item{block}{Either \code{'latest'} or a number giving the block of interest.
Default = \code{'latest'}.}

\item{api_key}{An Etherscan API key.}

\item{quiet}{Logical. Suppress progress messages? Default = \code{FALSE}.}
}
\description{
Get ERC20 token balance for a specific token, at a specific block.
}
\section{Warning}{

As per the Etherscan documentation, \emph{the Etherscan Ethereum Developer APIs
are provided as a community service and without warranty, so please just use
what you need and no more. They support both GET/POST requests and a rate
limit of 5 requests/sec.}
}

\keyword{ETH}
\keyword{Ethereum,}
\keyword{blockchain,}
\keyword{crypto,}
\keyword{cryptocurrency,}
\keyword{transaction,}
