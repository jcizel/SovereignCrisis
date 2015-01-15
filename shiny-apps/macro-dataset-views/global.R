library(shiny)
library(data.table)
library(pipeR)
library(rlist)
library(plyr)

source('./utilities.R')

load(file = './data/macro-data.RData')

macro <- macro
lookup <- attributes(macro)[['lookup']]

lookupSelections <- local({
    setkey(lookup,varcode)
    l <- lookup %>>% unique
    
    o <- l$varcode %>>% as.character
    names(o) <- l$label %>>% as.character 
    o
})


