% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_1000.R
\name{convert_to_bytes_1000}
\alias{convert_to_bytes_1000}
\title{Convert prefixed byte string to bytes, conversion factor 1000}
\usage{
convert_to_bytes_1000(values)
}
\arguments{
\item{values}{the size in bytes containing a unit}
}
\value{
the size in bytes
}
\description{
This function converts a string containing value and a byte size unit into the
corresponding raw byte value. It is assumed that 1000 byte = 1 kilobyte etc.
}
\details{
Function adapted from https://www.garysieling.com/blog/r-convert-from-gib-mib-kib-to-bytes/
}
\examples{
convert_to_bytes_1000("10 MB")
convert_to_bytes_1000("2kB")
convert_to_bytes_1000("42 B")
}
