% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.learningData.R
\name{load.learningData}
\alias{load.learningData}
\title{Loading data from the DA assay: adding a column indicating the replicate for different files from a common folder.}
\usage{
load.learningData(check.learning.data = T, na.col.omit = T,
  path = getwd(), strain = "N2", learning.phenotype = T,
  threshold = 0.7, learning.cutpoints = c(-1, (threshold - 0.2),
  (threshold - 0.1), (threshold - 0.05), (threshold), 1),
  non.learning.cutpoints = c(-1, (threshold - 0.15), (threshold),
  (threshold + 0.1), (threshold + 0.2), 1))
}
\arguments{
\item{check.learning.data}{Default = T; would you like to check whether the DA assay worked for wildtype (N2) worms and only use those datasets for whom it did?}

\item{na.col.omit}{Default = T; would you like to delete row from the CI column that contain NaN values? Usually these are rows for which there were no data entered.}

\item{path}{Default: wd; choose the path in which the function will find your learningData.}

\item{strain}{Default = "N2"; Optional if check.learning.data = T. what strain would you like to check up on? Default: N2}

\item{learning.phenotype}{Default = T; Optional if check.learning.data = T. Is the strain learning (true) or shouldn't it be (false)? Default: True (learning)}

\item{threshold}{Default = 0.7; Optional if check.learning.data = T. The threshold CI value for which the assay went well. For naive: CI > threshold is good, whereas for conditioned CI < threshold is better}

\item{learning.cutpoints}{The cutpoints you used to determine the significance of the learning phenotype}

\item{NONE}{Be sure to set your working directory to the folder of which you want to access the data. Your data should be formatted as follows: One column in which there is the data for the DA spot, one column in which there is the data for the EtOH spot and another column in which there is the data for all other worms. Next to this, the function requires a column that indicates the strain and a column that indicates the condition.}

\item{nonlearning.cutpoints}{The cutpoints you used to determine the significance of the non-learning phenotype}
}
\description{
This function allows you to quickly load all data from one mother folder in which you saved data as .csv files and afterwards still identify what data came from which file.
}
\examples{
load.learningData
}
