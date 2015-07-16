install_load <- function (package1, ...)  {   
  
  # convert arguments to vector
  packages <- c(package1, ...)
  
  # start loop to determine if each package is installed
  for(package in packages){
    
    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # if package is not installed locally, download, then load
    else {
      install.packages(package,repos="http://cran.rstudio.com/")
      do.call("library", list(package))
    }
  } 
}


install_load("scales")
install_load("shiny")
install_load("ReporteRs")
install_load("data.table")
install_load("stringr")
install_load("ggplot2")
install_load("ggthemes")
install_load("gridExtra")
install_load("reshape2")
install_load("RCurl")
install_load("rjson")
testimontials<-data.table(read.csv("PBpublicSectorTestimonials.csv",header=T,sep=","))
references<-data.table(read.csv("PBreferences.csv",header=T,sep=","))
Dollar<-function(x,r){
  paste("$",format(round(x,r),big.mark=",",scientific=F),sep="")
}
curToNum<-function(x){
  as.numeric(str_replace_all(str_replace_all(x,"\\$",""),",",""))}

printCurrency <- function(value, currency.sym="$", digits=2, sep=",", decimal=".") {
  paste(
    currency.sym,
    formatC(value, format = "f", big.mark = sep, digits=digits, decimal.mark=decimal),
    sep=""
  )
}
printPercent<-function(x){
  paste(round(x*100,2),"%",sep="")
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
setClass (Class = "YChartsApiClient",
          representation = representation(
            api_key="ANY",
            base_url="ANY",
            funct = "function"
          ),
          prototype = list ( api_key="", base_url="https://ycharts.com/api/v2/",
                             funct = function (.Object) { print (.Object) }   ),
)

setMethod ('initialize', 'YChartsApiClient',
           function(.Object, api_key='', base_url='') {
             if(nargs() > 1) .Object@api_key  = api_key
             if(nargs() > 2) .Object@base_url = base_url
             .Object
           })


setGeneric("fetch_data",  function(.Object, y_url, params="") standardGeneric("fetch_data"))
setMethod("fetch_data",  "YChartsApiClient",
          function(.Object, y_url, params='') {
            if ( params != "" ) y_url = paste(y_url, params, sep='', collapse='')
            headers=list('X-YCHARTSAUTHORIZATION'=.Object@api_key)
            print (headers)
            return (getURL ( y_url, httpheader = headers,.opts = list(ssl.verifypeer = FALSE) ))
          })

setGeneric("verify_list_or_string", function(.Object, imp_data) standardGeneric("verify_list_or_string"))
setMethod("verify_list_or_string", "YChartsApiClient",
          function(.Object, imp_data) {
            if ( class(imp_data) == 'character' ) return (imp_data)
            if ( class(imp_data) == 'list' ) return (paste(imp_data, sep=',', collapse=','))
            return ("not_a_string_passed")
          })

setGeneric("verify_date", function(.Object, imp_data) standardGeneric("verify_date"))
setMethod("verify_date", "YChartsApiClient",
          function(.Object, imp_data) {
            if ( ( class(imp_data) == 'POSIXct' ) || ( class(imp_data) == 'POSIXlt')) {  return ( strftime (imp_data, "%Y-%m-%d")) }
            if ( class(imp_data) == 'character' || class(imp_data)=='numeric' ) return ( imp_data )
            return ("")
          })


# Arguments:

#   companies (string or array of strings): The ticker (+ exchange
#     symbol for non-US companies) of companies for which we want info.
#   	eg. ['AAPL', 'GOOG'] or 'AAPL'

# Returns an array of information about the requested company/companies including
# things like the company name, exchange, profile, industry, etc.


setGeneric("get_company_info", function(.Object, companies) standardGeneric("get_company_info"))
setMethod("get_company_info", "YChartsApiClient",
          function(.Object, companies) {
            info_url = 'companies/'
            companies = verify_list_or_string(.Object, companies)
            y_url = paste (.Object@base_url, paste(info_url, companies, sep='/', collapse=NULL), sep='', collapse=NULL)
            json_data = fetch_data(.Object, y_url)
            #return (json_data);
            return (fromJSON(json_data))
          })

setGeneric("get_industry_info", function(.Object, industries) standardGeneric("get_industry_info"))
setMethod("get_industry_info", "YChartsApiClient",
          function(.Object, industries) {
            info_url = 'classifications/USA/industries/'
            companies = verify_list_or_string(.Object, industries)
            y_url = paste (.Object@base_url, paste(info_url, industries, sep='/', collapse=NULL), sep='', collapse=NULL)
            json_data = fetch_data(.Object, y_url)
            #return (json_data);
            return (fromJSON(json_data))
          })

#Arguments:
#
#	companies (string or array of strings): The ticker (+ exchange
#		symbol for non-US companies) of companies for which we want info.
#		eg. ['AAPL', 'GOOG'] or 'AAPL'
#
#	metrics (string or array of strings): the financial metrics we want to
#		return.  Eg. 'net_income' or ['net_income', 'revenues']
#
#	date_attr (string or integer): the date for which we want
#		data points.  If there is no data point on that date, we will return
#		the first observed datapoint before that date.
#		String must be of the form YYYY-MM-DD
#		Integers will look back from today's date (eg. -1 will look back
#			one day for daily data, one quarter for quarterly data, etc.)
#
#Returns an array with the requested information.


setGeneric("get_company_data_point", function(.Object, companies, metrics, date_attr='' ) standardGeneric("get_company_data_point"))
setMethod("get_company_data_point", "YChartsApiClient",
          function(.Object, companies, metrics, date_attr='') {
            point_url='companies/%s/points/%s'
            companies = verify_list_or_string(.Object, companies)
            metrics = verify_list_or_string(.Object, metrics)
            
            y_url = paste (.Object@base_url, paste('companies', companies, 'points', metrics, sep='/', collapse=NULL), sep='', collapse=NULL)
            if (date_attr != '' ) {
              params = URLencode( paste('?date=', verify_date(.Object, date_attr), sep='', collapse=NULL) )
            } else {
              params = ''
            }
            
            #print(y_url)
            
            json_data = fetch_data(.Object, y_url, params)
            
            #return (json_data);
            return (fromJSON(json_data))
          })



#Arguments:
#
#	companies (string or array of strings): The ticker (+ exchange
#		symbol for non-US companies) of companies for which we want info.
#		eg. ['AAPL', 'GOOG'] or 'AAPL'
#
#	metrics (string or array of strings): the financial metrics we want to
#		return.  Eg. 'net_income' or ['net_income', 'revenues']
#
#	start_date (string or integer): the start date for which
#		we want data points.
#		String must be of the form YYYY-MM-DD or -I
#		Integers will look back from today's date (eg. -1 will look back
#			one day for daily data, one quarter for quarterly data, etc.)
#
#	end_date (string): the end date for which
#		we want data points.
#
#Returns an array with the requested information.

setGeneric("get_company_data_timeseries", function(.Object, companies, metrics, start_date='', end_date='') standardGeneric("get_company_data_timeseries"))
setMethod("get_company_data_timeseries", "YChartsApiClient",
          function(.Object, companies, metrics, start_date='', end_date='') {
            
            companies = verify_list_or_string( .Object, companies )
            metrics = verify_list_or_string( .Object, metrics )
            y_url = paste (.Object@base_url, 'companies', companies, 'series', metrics, sep='/', collapse=NULL)
            
            params = URLencode ( paste('?start_date=', verify_date(.Object, start_date), '&', 'end_date=', verify_date(.Object, end_date), sep='', collapse=NULL ))
            
            json_data = fetch_data( .Object, y_url, params )
            
            
            #return (json_data);
            return (fromJSON( json_data ))
          })










# Sample Code Using the API Client

FN<-function(x) {
  as.numeric(as.character(x))
}
sac <- new ("YChartsApiClient", api_key="NWh0HNOObqf20CO3boO8NQ") # Enter the Key here. See http://ycharts.com/accounts/my_account


