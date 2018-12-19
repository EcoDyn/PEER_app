# PEER_app

Shiny app for crowdsourcing ground truth data collection, supporting mapping of Amazon wetland habitats. 

To run it locally on your computer:

1) download and extract the [PEER_app.zip](https://github.com/EcoDyn/PEER_app/raw/master/PEER_app.zip) file and extract to a folder in your computer.

2) Install the following packages: 

```{R}
install.packages(c('shiny','shinyjs','shinydashboard','data.table','DT','tidyverse','leaflet','googlesheets'),dep=T)
```
3) Open the App.R file on RStudio, and click on "Run App" at the top right corner of the scripting window
