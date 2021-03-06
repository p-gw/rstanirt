% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posterior_predict.R
\name{posterior_predict}
\alias{posterior_predict}
\title{Predicting observations from posterior predictive distribution}
\usage{
posterior_predict(fit, data, new_theta = NULL, draws = NULL, seed = NULL)
}
\arguments{
\item{fit}{a stanfit object containing an item response model}

\item{data}{a dataset in stan_list format}

\item{new_theta}{draw new person parameters from the prior (suggested)}

\item{draws}{number of draws from the posterior distribution}

\item{seed}{set a seed for the random number generator}
}
\description{
Given a model, a data set and (optional) additional parameters, 
this function calculates observations from the posterior predictive distribution.
}
