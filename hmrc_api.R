library(httr)
library(jsonlite)
library(stringr)

url <-  "https://api.uktradeinfo.com/OTS?$filter=Hs6Code eq '080510' and MonthId eq '201901' and FlowTypeId eq '1'"#&$expand=OTS($filter=MonthId eq '201901' and FlowTypeId eq 1)"
#url <-  "https://api.uktradeinfo.com/OTS?$filter=MonthId eq 201901 and Hs6Code eq '010129'"


res <- GET(str_replace_all(url, ' ', '%20'))

res$status_code
data <- fromJSON(rawToChar(res$content))
foo <- as.data.frame(data$value)


