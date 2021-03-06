% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{load_questions}
\alias{load_questions}
\title{Load up questions from a csv}
\usage{
load_questions(csv_path = "inst/PVQ_questions.csv", pvq_type = 21, lang = "fr")
}
\arguments{
\item{csv_path}{csv path}

\item{pvq_type}{either 21 or 40}

\item{lang}{pick which language}
}
\value{
questions tibble of filtered questions
}
\description{
Load up questions from a csv
}
