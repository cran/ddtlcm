% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MH_tree.R
\name{proposal_log_prob}
\alias{proposal_log_prob}
\title{Calculate proposal likelihood}
\usage{
proposal_log_prob(
  old_tree_phylo4,
  tree_kept,
  old_detach_pa_div_time,
  old_pa_detach_node_label,
  old_detach_node_label,
  new_div_time,
  new_attach_root,
  new_attach_to,
  c,
  c_order = 1
)
}
\arguments{
\item{old_tree_phylo4}{the old "phylo4" object}

\item{tree_kept}{the remaining "phylo" tree after detachment}

\item{old_detach_pa_div_time}{a number in (0, 1) indicating the divergence time
of the detached node on the old tree}

\item{old_pa_detach_node_label}{a character label of the parent of the detached node
on the old tree}

\item{old_detach_node_label}{a character label of the detached node
on the old tree}

\item{new_div_time}{a number in (0, 1) indicating the divergence time at which
the detached subtree will be re-attached on the proposal tree}

\item{new_attach_root, new_attach_to}{a character label of the starting and ending
nodes of the branch on the proposal tree, which the detached subtree will be re-attached to}

\item{c}{hyparameter of divergence function a(t)}

\item{c_order}{equals 1 (default) or 2 to choose divergence function}
}
\value{
a list containing the following elements:
\describe{
\item{\code{q_new}}{a "phylo" tree detached from the input tree}
\item{\code{q_old}}{the remaining "phylo" tree after detachment}
}
}
\description{
Given an old tree, propose a new tree and calculate the original
and proposal tree likelihood in the DDT process
}
