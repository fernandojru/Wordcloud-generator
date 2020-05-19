# Wordcloud generator
This is an user-friendly web application to generate word clouds. **It was created with R Shiny**

![Ex](/wordcloud-generator/UI.png)

## Access the tool: 

It can be accessed with the following options
* 1. [Just click here](https://fernandojru.shinyapps.io/Wordcloud_creator/?_ga=2.98759838.1479433610.1589865133-1383316383.1589527346)
* 2. Locally, by executing the following code on your R console **(Rstudio console preferably)**

``` r
library(shiny)
library(shinythemes)
library(colourpicker)
library(wordcloud2)
library(tm)

runGitHub("fernandojru/Wordcloud-generator","fernandojru")
```
