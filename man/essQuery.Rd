\name{essQuery}
\alias{essQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{essQuery
}
\description{Query the UDB database and return the results to R.}
\usage{
essQuery(essentia, aq="", flags="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{essentia}{
The essentia command to run. The options are "ess task stream
category startdate enddate", "ess task exec", and "ess query".

Each stream or query command can be used to stream any number of files
directly into your R analysis. Alternatively, each stream command
can save multiple files into separate R dataframes, one file per dataframe.

The default value for the essentia argument is "ess task
exec".

}
  \item{aq}{
This can be any combination of the aq_tools and standard UNIX
commands for "ess task ..." statements or an sql-like statement 
for "ess query" statements. However, the output MUST be in a 
csv format if you want R to capture the output. If you only want to 
run the command without R capturing the output, add "#Rignore" to the
flags argument.

}
  \item{flags}{
Any of the essentia flags can be used here in addition to any
of these RESS-specific flags:

\code{#Rignore} : Ignore an 'ess task exec' statement. Do not
capture the output of the statement into R.

\code{#Rinclude} : Include an 'ess task stream' or 'ess query' statement. 
Capture the output of the statement into R.

\code{#-notitle} : Tell R not to use the first line of the output
as the header.

\code{#Rseparate} : Can be used when saving multiple files into an
R dataframe using an 'ess task stream' command. Saves each
file into a different R dataframe, entitled command1 to
commandN, where N is the number of files.

\code{#filelist} : Causes an extra dataframe to be stored in R that saves 
the list of files streamed into R when streaming multiple files.

\code{#R#name#R#} : Allows any automatically saved dataframe to be
renamed to whatever is entered in place of 'name'. This only
applies in essQuery when streaming multiple files with \code{#Rseparate}.

}
}
\details{
essQuery is used to directly query the database using a single
statement. You can call essQuery multiple times to run different
statements.

However, you can also use read.udb to read all of the statements
in a file instead.  Thus if you plan to run multiple statements
that may be somewhat related to each other, it is recommended that
you use read.udb.

}
\value{
The value returned is the output from querying the database. This
can be saved into an R dataframe or directly analyzed in R.

If you use essQuery to save multiple files into separate R dataframes
using a single stream command, the files are stored automatically
in R dataframes called command1 to commandN (where N is the number of files) 
and no value is returned. To change the name of the stored dataframes, use 
the #R#any_name#R# flag. The dataframes will then be stored as any_name1 to any_nameN.

With #filelist, the extra dataframe is saved as commandN+1 by default, or 
any_nameN+1 if #R#any_name#R# is also used.

}
\references{
See our website at www.auriq.net or our documentation at
www.auriq.net/documentation

}
\author{
Ben Waxer, Data Scientist with Auriq Systems.
}
\examples{
\dontrun{
--------------------------------------------------------------------------------------------------

These examples require Essentia to be installed:

essQuery("ess instance local")
fullexec <- essQuery("ess task exec", "echo -e '11,12,13\\n4,5,6\\n7,8,9' ","#-notitle")
print(fullexec)
defaultexec <- essQuery("echo -e '11,12,13\\n4,5,6\\n7,8,9' ","#-notitle")
print(defaultexec)
essQuery("echo -e '11,12,13\\n4,5,6\\n7,8,9' ","#Rignore")
print("This last statement is ignored by R and just executed on the command line.")

--------------------------------------------------------------------------------------------------

This example requires Essentia to have selected a datastore containing purchase log data:

command1 <- essQuery("ess query","select price,count(refID) from purchase:2014-09-01:2014-09-15 \
where articleID>=46 group by price","#Rinclude")
command2 <- essQuery("ess query", "select count(distinct userID) from \
purchase:2014-09-01:2014-09-15 where articleID>=46", "#Rinclude")
command3 <- essQuery("ess query", "select userID,count(refID) from \
purchase:2014-09-01:2014-09-15 where articleID>=46 group by userID", "#Rinclude")
querystream <- essQuery("ess query", "select * from purchase:*:* where articleID <= 20", "\
#Rinclude")

Then run these commands to view the saved dataframes:

print(command1)
print(command2)
print(command3)
print(querystream)

--------------------------------------------------------------------------------------------------

The following example requires Essentia to be installed with apache log data stored in it:

# Query the Essentia database logsapache3 and return the contents of vector3 into R.
command1 <- essQuery("aq_udb -exp logsapache3:vector3", "--debug")

# Query the Essentia database logsapache1 and return the sorted contents of vector1 into R.
command2 <- essQuery("ess task exec", "aq_udb -exp logsapache1:vector1 -sort pagecount -dec", "\\
--debug")

# Stream the last five lines of the file in category 125accesslogs between dates 2014-12-07 and
# 2014-12-07, convert them to csv, return them to R, and store them into an R dataframe singlefile.
singlefile <- essQuery("ess task stream 125accesslogs '2014-12-07' '2014-12-07'","tail -5 \\
| logcnv -f,eok - -d ip:ip sep:' ' s:rlog sep:' ' s:rusr sep:' [' i,tim:time sep:'] \\\"' \\
s,clf,hl1:req_line1 sep:'\\\" ' i:res_status sep:' ' i:res_size sep:' \\\"' \\
s,clf:referrer sep:'\\\" \\\"' s,clf:user_agent sep:'\\\"' X | cat -","#Rinclude")

# Stream the last five lines of the files in category 125accesslogs between dates 2014-11-30 and 
# 2014-12-07, convert them to csv, and save them into R dataframes apachefiles1 and apachefiles2.
essQuery("ess task stream 125accesslogs '2014-11-30' '2014-12-07'","tail -5 \\
| logcnv -f,eok - -d ip:ip sep:' ' s:rlog sep:' ' s:rusr sep:' [' i,tim:time sep:'] \\\"' \\
s,clf,hl1:req_line1 sep:'\\\" ' i:res_status sep:' ' i:res_size sep:' \\\"' \\
s,clf:referrer sep:'\\\" \\\"' s,clf:user_agent sep:'\\\"' X -notitle | cat -","\\
#Rinclude #R#apachefiles#R# #Rseparate")

print(command1)
print(command2)
print(singlefile)
print(apachefiles1)
print(apachefiles2)

The references contain more extensive examples that 
fully walkthrough how to load and query the Essentia Database.
}
}
