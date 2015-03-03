\name{read.udb}
\alias{read.udb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{read.udb
%%  ~~function to do ... ~~
}
\description{
Read the essentia commands in the stated file, query the UDB
database, and save the results into R as dataframes.

}
\usage{
read.udb(file, linenumber = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
The file containing the essentia commands you want to run.
'ess task stream' and 'ess query' statements will have their output ignored
by R unless there is a '#Rinclude' flag somehwere in the
statement line. 'ess task exec' statements will have their
output included unless there is a '#Rignore' flag somewhere
in the statement line.

The file should contain primarily the query commands but can
contain the entire essentia script including loading the UDB
database if desired. Any command that you want to capture the
output from must have its output in csv format.

}
  \item{linenumber}{
This is set to default to all line numbers so that every
command in the file is executed. You can specify the line
number of the command you wish to run if you dont want to run
the entire set of commands in the file. If your statement spans
multiple lines, you can specify each line using the syntax
c(first_line_number, second_line_number, ....).

}
}
\details{
read.udb reads all of the statements in a file (unless linenumber
is specified, see above) and captures the output of the specified
commands into R dataframes. By default only the output of 'ess
task exec' statements is captured and it's stored in R dataframes
command1 to commandN, where N is the number of captured
statements.

You can include 'ess task stream' or 'ess query' statements by adding a
'#Rinclude' flag. This method can be used to stream multiple files
into R for data exploration or analysis.  If you plan to run
multiple statements that may be somewhat related to each other,
it is recommended that you use read.udb.

}
\value{
There is no value returned. This command creates a set of R
dataframes containing the output from the specified essentia
commands in file.

}
\references{
See our website at www.auriq.net or our documentation at
www.auriq.net/documentation.

}
\author{
Ben Waxer, Data Scientist with Auriq Systems.
}
\note{
The flags added to the essentia commands in file can include:

\code{#Rignore} : Ignore an 'ess task exec' statement. Do not
capture the output of the statement into R.

\code{#Rinclude} : Include an 'ess task stream' or 'ess query' statement. Capture
the output of the statement into R.

\code{#-notitle} : Tell R not to use the first line of the output
as the header.

\code{#Rseparate} : Can be used when saving multiple files into an
R dataframe using an 'ess task stream' command. Saves each
file into a different R dataframe.

\code{#filelist} : Causes an extra dataframe to be stored in R that saves
the list of files streamed into R when streaming multiple files.

\code{#R#name#R#} : Allows any automatically saved dataframe to be
renamed to whatever is entered in place of 'name'. When used with 
#Rseparate, saves the files as name1 to nameN, where N is the number of files. 
Since this still counts as a statement, the next default dataframe saved will 
be stored as command followed by the number of previous statements run plus one. 

}
\examples{
\dontrun{
--------------------------------------------------------------------------------------------------

These examples require Essentia to be installed:

queryfile <- file("examplequery.sh","w")
cat("ess instance local \n",file=queryfile)
cat("ess task exec \"echo -e '11,12,13\\n4,5,6\\n7,8,9'\" #-notitle \n",file=queryfile)
cat("ess task exec \"echo -e '11,12,13\\n4,5,6\\n7,8,9'\" \n", file=queryfile)
cat("ess task exec \"echo -e '11,12,13\\n4,5,6\\n7,8,9'\" #Rignore \n", file=queryfile)
read.udb("examplequery.sh")
print(command1)
print(command2)
print("The last statement is ignored by R and just executed on the command line.")

--------------------------------------------------------------------------------------------------

This example requires Essentia to have selected a datastore containing purchase log data:

Store these lines as querypurchase.sh:

ess query "select price,count(refID) from purchase:2014-09-01:2014-09-15 \
where articleID>=46 group by price" #Rinclude
ess query "select count(distinct userID) from purchase:2014-09-01:2014-09-15 \
where articleID>=46" #Rinclude
ess query "select userID,count(refID) from purchase:2014-09-01:2014-09-15 \
where articleID>=46 group by userID" #Rinclude
ess query "select * from purchase:*:* where articleID <= 20" #Rinclude #R#querystream#R#

-----------------------------------------------

Then run these commands in R:

library(RESS)
read.udb("querypurchase.sh")
print(command1)
print(command2)
print(command3)
print(querystream)

--------------------------------------------------------------------------------------------------

The following example requires Essentia to be installed with apache log data stored in it.

Store the following lines as queryapache.sh:

# Query the Essentia database logsapache3 and save the contents of vector3 in R as command1.
ess task exec "aq_udb -exp logsapache3:vector3" --debug

# Query the Essentia database logsapache1 and save the sorted contents of vector1 in R as command2.
ess task exec "aq_udb -exp logsapache1:vector1 -sort pagecount -dec" --debug

# Stream the last five lines of the file in category 125accesslogs between dates 2014-12-07 and
# 2014-12-07, convert them to csv, return them to R, and store them into an R dataframe singlefile.
ess task stream 125accesslogs '2014-12-07' '2014-12-07' "tail -5 \
| logcnv -f,eok - -d ip:ip sep:' ' s:rlog sep:' ' s:rusr sep:' [' i,tim:time sep:'] \"' \
s,clf,hl1:req_line1 sep:'\" ' i:res_status sep:' ' i:res_size sep:' \"' \
s,clf:referrer sep:'\" \"' s,clf:user_agent sep:'\"' X | cat -" #Rinclude #R#singlefile#R#

# Stream the last five lines of the files in category 125accesslogs between dates 2014-11-30 and
# 2014-12-07, convert them to csv, and save them into R dataframes apachefiles1 and apachefiles2.
ess task stream 125accesslogs '2014-11-30' '2014-12-07' "tail -5 \
| logcnv -f,eok - -d ip:ip sep:' ' s:rlog sep:' ' s:rusr sep:' [' i,tim:time sep:'] \"' \
s,clf,hl1:req_line1 sep:'\" ' i:res_status sep:' ' i:res_size sep:' \"' \
s,clf:referrer sep:'\" \"' s,clf:user_agent sep:'\"' X -notitle | cat -" \
#Rinclude #R#apachefiles#R# #Rseparate

-----------------------------------------------

Then run these commands in R:

library(RESS)
read.udb("queryapache.sh")

print(command1)
print(command2)
print(singlefile)
print(apachefiles1)
print(apachefiles2)

The references contain more extensive examples that
fully walkthrough how to load and query the Essentia Database.

}
}
