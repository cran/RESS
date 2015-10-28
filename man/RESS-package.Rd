\name{RESS-package}
\alias{RESS-package}
\alias{RESS}
\docType{package}
\title{
Integrates R and Essentia.
}
\description{
Contains three functions that query AuriQ Systems' Essentia Database and return the results in R. 

'essQuery' takes a single Essentia command and captures the output
in R, where you can save the output to a dataframe or stream it 
directly into additional analysis. 

'read.essentia' takes an Essentia script and captures the output 
csv data into R, where you can save the output to a dataframe or 
stream it directly into additional analysis. 

'capture.essentia' takes a file containing any number of Essentia
commands and captures the output of the specified statements into
R dataframes. 

Essentia can be downloaded for free at http://www.auriq.com/documentation/source/install/index.html.

%% ~~read.udb can be used to stream multiple files directly into R with~~
%% ~~a single command but essQuery is limited to one file per command.~~

}
\details{
\tabular{ll}{
Package: \tab RESS\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2015-10-26\cr
License: \tab LGPL-3 \cr
}
}
\author{
Ben Waxer, Data Scientist with Auriq Systems.

Maintainer: Ben Waxer <bwaxer@auriq.com>
}
\keyword{ package }
