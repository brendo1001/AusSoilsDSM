

require("RCurl") 
library(xml2)
library(XML)
library(htmltools)


curlSetOpt(timeout = 2000)

result <- getURL("http://qld.auscover.org.au/public/data/landsat/seasonal_fractional_cover/fractional_cover/nt/",verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = F)
View(result)

htmltidy::xml_view(result)
getHTMLLinks(result, xpQuery = "//a/@href[contains(., '.tif')]")
