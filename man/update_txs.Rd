% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_txs.R
\name{update_txs}
\alias{update_txs}
\title{Update transactions for an address}
\usage{
update_txs(txs, api_key, quiet = FALSE)
}
\arguments{
\item{txs}{An object returned by \code{\link[=get_txs]{get_txs()}}.}

\item{api_key}{An Etherscan API key (see Details).}

\item{quiet}{Logical. Suppress messages? Default is \code{FALSE}.}
}
\description{
Update normal, internal, ERC-20, or ERC-721 transactions for an Ethereum
address.
}
\details{
\code{get_txs} uses the Etherscan API to source information about
transactions to and from an Ethereum address. Register for an API key at
the \href{https://etherscan.io/apis}{Etherscan Developer APIs page}.
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
