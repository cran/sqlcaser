## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sqlcaser)

## -----------------------------------------------------------------------------
samp <- system.file("extdata", "sample.csv", package = "sqlcaser")
mapping <- read.csv(samp)
mapping

## -----------------------------------------------------------------------------
statement <- casewhen(samp)

## -----------------------------------------------------------------------------
query <- paste("SELECT id, ", statement, " END AS status "," \nFROM table;")
cat(query)

## -----------------------------------------------------------------------------
samplepath <- system.file("extdata", "sample.csv", package = "sqlcaser")

