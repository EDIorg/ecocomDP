% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_community.R
\name{plot_sp_shared_among_sites}
\alias{plot_sp_shared_among_sites}
\title{Plot species shared among sites}
\usage{
plot_sp_shared_among_sites(dswide, id, path, vunit)
}
\arguments{
\item{dswide}{(data.frame) Wide form of return from \code{format_for_comm_plots()}}

\item{id}{(character) Dataset id}

\item{path}{(character) Path to where plots will be written}

\item{vunit}{(character) Unit of measurement variable}
}
\value{
\item{sp_shared_among_sites.pdf}{Species shared among each site}
}
\description{
Plot species shared among sites
}
