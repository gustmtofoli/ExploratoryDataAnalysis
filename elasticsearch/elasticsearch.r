install.packages("curl")
install.packages("openssl")
install.packages("httr")

install.packages("elastic")

library("curl")
library("openssl")
library("httr")
library("elastic")

connect("http://192.168.0.1")
count()

shakespeare <- system.file("examples", "shakespeare_data.json", package = "elastic")
docs_bulk(shakespeare)

