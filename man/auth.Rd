% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_token.R
\name{auth}
\alias{auth}
\alias{get_set_api_token}
\alias{get_set_data_center}
\title{Attempt to retrieve Qualtrics API token and data center from keyring
If not found, will prompt user to enter API token and data center}
\usage{
auth(reset_api_token = FALSE, custom_data_center = NULL)

get_set_api_token(reset)

get_set_data_center(custom_value = NULL)
}
\arguments{
\item{reset_api_token}{If TRUE, will prompt user to enter new API token}

\item{custom_data_center}{If not NULL, will set data center to this value}
}
\description{
Attempt to retrieve Qualtrics API token and data center from keyring
If not found, will prompt user to enter API token and data center

Retrieve Qualtrics API token

Retrieve Data Center
}
