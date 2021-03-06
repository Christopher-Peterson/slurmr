% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slurm_files.R
\name{make_slurm}
\alias{make_slurm}
\title{Creates a SLURM file}
\usage{
make_slurm(
  slurm_config,
  launcher_file = "",
  launcher_prefix = "",
  launcher_suffix = "",
  ...
)
}
\arguments{
\item{slurm_config}{the configuration header, from \code{make_slurm_config()}}

\item{launcher_file}{the job file for launcher scripts; if empty the
launcher isn't used}

\item{launcher_prefix, launcher_suffix}{code to run before and after the
launcher code is run}

\item{...}{unused}
}
\value{
text of a slurm file
}
\description{
Creates a SLURM file
}
