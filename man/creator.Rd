% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creator.R
\name{creator}
\alias{creator}
\title{Return the address of a contract's creator}
\usage{
creator(address, api_key)
}
\arguments{
\item{address}{Character. A single ethereum contract address as a character
string (40 hexadecimal characters prepended by '0x').}

\item{api_key}{An Etherscan API key (see Details).}
}
\value{
Character. The address that created the contract at \code{address}.
}
\description{
Return the Ethereum address of the creator of a contract.
}
\details{
\code{creator} uses the Etherscan API. Register for an API key
at the \href{https://etherscan.io/apis}{\emph{Etherscan Developer APIs} page}.
}
\section{Warning}{

As per the Etherscan documentation, \emph{the Etherscan Ethereum Developer APIs are
provided as a community service and without warranty, so please just use what
you need and no more. They support both GET/POST requests and a rate limit of
5 requests/sec.}
}

\keyword{ETH}
\keyword{Ethereum,}
\keyword{blockchain,}
\keyword{contract,}
\keyword{crypto,}
\keyword{cryptocurrency,}
