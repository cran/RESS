\name{read.essentia}
\alias{read.essentia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{read.essentia
}
\description{
Read the essentia commands in the stated file, and save the csv output into R. 
This command is primarily intended to capture the output of a query to the Essentia database.
}
\usage{
read.essentia(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
The essentia script to run and any arguments you want to pass into the bash script. 
}
}
\details{
read.essentia is used to directly query the database using a script with a single
csv formatted output. You can call read.essentia multiple times to run statements 
that produce different outputs and capture their respective output into R.

However, you can also use capture.essentia to read all of the statements
in a file instead and capture the output of any or all of the statements.  
Thus if you plan to run multiple statements that may be somewhat related
to each other, you may want to use capture.essentia.

}
\value{
The value returned is the output from querying the database. This
can be saved into an R dataframe or directly analyzed in R.

}
\references{
See our website at www.auriq.com or our documentation at
www.auriq.com/documentation

}
\author{
Ben Waxer, Data Scientist with Auriq Systems.
}
\note{
The argument, file, can also include the following flag:

\code{#-notitle} : Tell R not to use the first line of the output
as the header.

}
\examples{
\dontrun{
--------------------------------------------------------------------------------------------------

These examples require Essentia to be installed:

queryfile <- file("examplequery.sh","w")
cat("ess exec \"echo -e '11,12,13\\n4,5,6\\n7,8,9'\" \n", file=queryfile)
simpleecho <- read.essentia("examplequery.sh")
print(simpleecho)

--------------------------------------------------------------------------------------------------

This example requires Essentia to have selected a datastore containing purchase log data:

Store these lines as querypurchase.sh:

ess query "select count(refID) from purchase:2014-09-01:2014-09-15 \
where articleID>=46 group by userID"

-----------------------------------------------

Then run these commands in R:

library(RESS)
print(read.essentia("querypurchase.sh"))


--------------------------------------------------------------------------------------------------

The following example requires Essentia to be installed with apache log data stored in it.

Store the following lines as queryapache.sh:

# Query the Essentia database logsapache1 and save the sorted contents of vector1 in R as command2.
ess exec "aq_udb -exp logsapache1:vector1 -sort pagecount -dec"


-----------------------------------------------

Then run these commands in R:

library(RESS)
mydata <- read.essentia("queryapache.sh")

print(mydata)


The references contain more extensive examples that
fully walkthrough how to load and query the Essentia Database.

}
}
