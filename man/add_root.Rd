% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_root}
\alias{add_root}
\title{Add a singular root node to an existing nonsingular tree}
\usage{
add_root(tree_old, root_edge_length, root_label, leaf_label)
}
\arguments{
\item{tree_old}{the original nonsingular "phylo" tree}

\item{root_edge_length}{a number in (0, 1) representing the distance
between the new and the original root nodes}

\item{root_label}{a character label of the new root node}

\item{leaf_label}{a character label of the leaf node}
}
\value{
a singular "phylo" tree
}
\description{
Add a singular root node to an existing nonsingular tree
}
