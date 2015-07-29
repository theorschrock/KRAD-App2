F
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("source.R")

shinyServer(function(input,output,session) {
  
  output$metrics<-renderUI({
    input$metricSet
    mlist<-switch(input$metricSet,
                  "Profitability"=c("gross_profit_margin_ttm","ebitda_margin_ttm","profit_margin_ttm"),
                  "Employee Metrics"=c("ebitda_per_employee_annual","revenue_per_employee_annual","cfo_per_employee_annual","op_exp_per_employee_annual","sga_exp_per_employee_annual"))
    selectizeInput('metricA',
                   "Metric",
                   choices=mlist,
                   selected=NULL,
                   multiple=F)
    
  })
  
  
  Ycharts<-reactive({
    
    print(input$company)
    print(input$metricA)
    if(is.null(input$company))return()
    if(input$company=="")return()
    if(is.null(input$metricA))return()
    
    if(length(input$competitors)==0){ companies<-toupper(input$company)}else{
      comp<-toupper(input$competitors)
      companies<-lapply(comp,as.character)
      companies[[length(companies)+1]]<-toupper(input$company)
    }
    print(companies)
    Emetrics<-input$metricA
    years<-input$years
    if(grepl("annual",input$metricA)==F)years<-input$years*4
    years<-as.numeric(as.character(years))
    EmployeeMetrics <- get_company_data_timeseries(sac, companies, Emetrics, -years, NULL)
    print(EmployeeMetrics)
    All<-data.table()
    cmpy<-unlist(companies)
    for(c in cmpy){
      for(m in Emetrics){
        sa<-EmployeeMetrics$response[[c]]$data[[m]]$data
        T1<-data.table(matrix(unlist(sa),ncol=2,byrow=T))
        T1[,Metric:=m]
        T1[,Company:=c]
        All<-rbindlist(list(All,T1))
      }}
    print(All)
    All
    
  })
  
  observe({
    Ycharts()
    CompInfo()  
    
  })
  
  observe({
    if(is.null(input$competitors))return()
    if(input$competitors=="")return()
    CompetInfo()
  })
  plotTmp<-reactive({
    if(is.null(Ycharts()))return()
    All<-Ycharts()
    metric<-trim(str_replace_all(str_replace_all(str_replace_all(input$metricA,"_"," "),"ttm",""),"annual",""))
    P<-ggplot(All,aes(as.Date(V1),FN(V2),color=Company))+geom_line(size=1)+geom_point(size=2)+
      theme_igray()+
      scale_color_tableau("tableau10",name=element_blank())+
      theme(                                                                                                                                                       
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.direction = "horizontal")+ggtitle(simpleCap(metric))
    
    P
  })
  
  output$plot<-renderPlot({
    if(is.null(Ycharts()))return()
    All<-Ycharts()
    P<-ggplot(All,aes(as.Date(V1),FN(V2),color=Company))+geom_line(size=1)+geom_point(size=2)+
      theme_igray()+
      scale_color_tableau("tableau10",name=element_blank())+
      theme(                                                                                                                                                       
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.direction = "horizontal")
    P
  })
  
  CompInfo<-reactive({
    if(input$company==""|is.null(input$company))return()
    test2 = get_company_info(sac, toupper(input$company))
    print(test2)
    
    name<-test2$response[[input$company]]$name
    ticker<-paste("Ticker: ",test2$response[[input$company]]$exchange_symbol)
    profile<-paste("Description: ",test2$response[[input$company]]$profile,sep="")
    industry<-paste("Industry: ",test2$response[[input$company]]$industry,"- ",test2$response[[input$company]]$naics,sep="")
    CI<-as.character(c(name,ticker,profile,industry))
    print(CI)
    CI
  })
  CompetInfo<-reactive({
    if(input$competitors==""|is.null(input$competitors))return()
    competitors<-list()
    for(i in 1:length(input$competitors)){
      test2 = get_company_info(sac, toupper(input$competitors[i]))
      print(test2)
      
      name<-paste("Competitor ",i,": ",test2$response[[input$competitors[i]]]$name,sep="")
      ticker<-paste("Ticker: ",test2$response[[input$competitors[i]]]$exchange_symbol)
      profile<-paste("Description: ",test2$response[[input$competitors[i]]]$profile,sep="")
      industry<-paste("Industry: ",test2$response[[input$competitors[i]]]$industry,"- ",test2$response[[input$competitors[i]]]$naics,sep="")
      CI<-as.character(c(name,ticker,profile,industry))
      competitors[[input$competitors[i]]]<-CI
    }
    print(competitors)
    competitors
  })
  
  sentance1<-reactive({
    if(is.null(Ycharts()))return()
    if(is.null(Ycharts()))return()
    name1<-paste(input$company,"'s",sep="")
    Ycharts<-Ycharts()
    Ycharts[,V2:=FN(V2)]
    metric<-trim(str_replace_all(str_replace_all(str_replace_all(input$metricA,"_"," "),"ttm",""),"annual",""))
    
    Intro<-paste(sample(c("Analyzing","Comparing","Over the course of"),size=1), " the last ",input$years," years, ",sep="")
    cStats<-Ycharts[Company==input$company]
    maxDate<-as.character(cStats[which.max(V2),V1])
    
    
    Metnames<-c("currentMetric","maxMetric","minMetric","diffMetFall","diffMetRise")
    
    dateText<-ifelse(cStats[,max(as.Date(V1))]==maxDate,"the most recent quarter",maxDate)
    metStats<-c(FN(cStats[which.max(as.Date(V1)),V2]),
                cStats[,max(FN(V2))],
                cStats[,min(FN(V2))],
                cStats[,max(FN(V2))]-cStats[,FN(cStats[which.max(as.Date(V1)),V2])],
                cStats[,FN(cStats[which.max(as.Date(V1)),V2])]-cStats[,min(FN(V2))])
    print( cStats[,max(FN(V2))]-cStats[,min(FN(V2))])
    Mk<-data.table(Metnames,metStats)
    ifelse(grepl("margin",input$metricA),MT<-sapply(Mk$metStats,printPercent),MT<-sapply(Mk$metStats,printCurrency))
    Mk[,MetText:=MT]
    
    
    sent.5<-paste(Intro,name1," ",metric," hit a high of ",Mk[Metnames=="maxMetric",MetText]," in ",dateText,sep="")
    
    if(grepl("margin",input$metricA)){
      sent1end<-c( ", suggesting operational cost inflation relative to revenue.",
                   ", suggesting operational improvements.")} else if(grepl("exp",input$metricA)){
                     sent1end<-c( ", suggesting operational cost improvement relative to the company's number of employees.",
                                  ", suggesting operational cost inflation relative to the company's number of employees.")  } else {
                                    sent1end<-c( ", suggesting a decline in labor force productivity.",
                                                 ", suggesting productivity improvements.")       
                                  }
    
    
    if(cStats[,max(as.Date(V1))]==maxDate){
      sent.5l<-paste(", rising by ",Mk[Metnames=="diffMetRise",MetText]," from its low in ",cStats[which.min(V2),V1],sent1end[2],sep="")
    }else{
      sent.5l<-paste(" and has since declined by ",
                     paste("-",Mk[Metnames=="diffMetFall",MetText],sep=""),
                     " to its current level of ",
                     Mk[Metnames=="currentMetric",MetText],
                     sent1end[1],sep="")
    }
    
    paste(sent.5,sent.5l,sep="")
    
  })
  
  sentance2<-reactive({
    if(is.null(Ycharts()))return()
    if(length(input$competitors)==0)return()
    All<-Ycharts()
    All[,V1:=as.Date(V1)]
    All[,V2:=FN(V2)]
    All[,MinD:=min(V1),by="Company"]
    All[,MaxD:=max(V1),by="Company"]
    All[,MinDM:=V2[V1==MinD],by="Company"]
    All[,MaxDM:=V2[V1==MaxD],by="Company"]
    All[,ChangeA:=MaxDM-MinDM,by="Company"]
    All[,ChangeP:=(MaxDM-MinDM)/MinDM,by="Company"]
    RQ<-All[V1==MaxD]
    
    Comps<-RQ[Company!=input$company]
    
    
    Comps[,AnalizeCMet:=RQ[Company==input$company,V2]]
    Comps[,AnalizeCCA:=RQ[Company==input$company,ChangeA]]
    Comps[,AnalizeCCP:=RQ[Company==input$company,ChangeP]]
    
    Comps[,compIsBetter:=AnalizeCMet>V2]
    Comps[,compChangeIsBetter:=AnalizeCCA>ChangeA]
    Comps[,AvgC:=mean(ChangeA)]
    Comps[,AvgV2:=mean(V2)]
    Comps[,HL:=ifelse(AvgV2>AnalizeCMet,"lower than average.","higher than average.")]
    ifelse(grepl("margin",input$metricA),AvgCt<-sapply(Comps$AvgC,printPercent),AvgCt<-sapply(Comps$AvgC,printCurrency))
    ifelse(grepl("margin",input$metricA),AvgCtc<-sapply(Comps$AnalizeCCA,printPercent),AvgCtc<-sapply(Comps$AnalizeCCA,printCurrency))
    Comps[,AvgCtext:=AvgCt]
    Comps[,AvgCtextComp:=AvgCtc]
    name1<-paste(input$company,"'s",sep="")
    metric<-trim(str_replace_all(str_replace_all(input$metricA,"_"," "),"ttm",""))
    
    
    Sent1All<-paste("All top competitors in this comparison have a ",ifelse(Comps$compIsBetter[1]==T,"lower ","higher "),
                    metric," than ",input$company,".",sep="")
    
    Sent1Allno<-paste("When compared to top competitors, ",name1," ",metric," is ",Comps$HL[1],sep="")
    
    lowerSentCost<-paste(" This indicates that ",input$company," may be experiencing operating cost pressures due to poor management of controllable costs such as labor.",sep="")
    lowerSentProd<-paste("This indicates that ",input$company," could improve labor productivity by implementing better labor management practices.",sep="")
    higherSent<-paste("This indicates that ",input$company," may have a competitive advantage in this area.",sep="")
    lowerSentGross<-paste("This indicates that ",input$company,"'s rise in Cost of Goods Sold is not necessarily an industry trend and may be a cost management issue.  The company may have initiatives to reduce controllable costs that fall into the COGS catagory. This includes labor associated with the manufacturing of the company's goods or services.",sep="")
    
    Sent3<-paste(" Over the past ",input$years," years, the competitor(average) change in ",metric," was ", Comps$AvgCtext[1],
                 " while ",name1," change in ",metric," was ",Comps$AvgCtextComp[1],".",sep="")
    
    
    print(Comps$AvgCtextComp[1])
    
    Sent1<-ifelse(all(Comps$compIsBetter==T)|all(Comps$compIsBetter==F),Sent1All,Sent1Allno)
    
    if(grepl("exp",input$metricA)){  Sent2<-ifelse(Comps$HL[1]=="higher than average.",lowerSentCost,higherSent)}
    if(grepl("exp",input$metricA)==F) Sent2<-ifelse(Comps$HL[1]=="higher than average.",higherSent,lowerSentCost)
    if(grepl("revenue",input$metricA)) Sent2<-ifelse(Comps$HL[1]=="higher than average.",higherSent,lowerSentProd)
    if(grepl("gross",input$metricA)) Sent2<-ifelse(Comps$HL[1]=="higher than average.",higherSent,lowerSentGross)
    
    paste(Sent1,Sent2,Sent3)
  })
  
  output$sentance1<-renderText({
    
    sentance1()
  })
  
  output$sentance2<-renderText({
    sentance2()
  })
  
  output$graphHeader<-renderText({
    metric<-trim(str_replace_all(str_replace_all(input$metricA,"_"," "),"ttm",""))
  })
  
  observe({
    if(input$company==""|is.null(input$company))return()
    if(is.null(CompInfo()))return()
    input$competitors
    isolate({
      
      CI<-CompInfo()
      print(CI)
      doc<<-docx(template="surveyDocTemplate.docx")
      doc<<-addParagraph(doc,value=paste(input$company," Financial Profile",sep=""),stylename="KronosDocumentTitle")
      doc<<-addParagraph(doc,value="Company Information",stylename="Heading2")
      doc<<-addParagraph(doc,value=CI[1],stylename="BodyText")
      doc<<-addParagraph(doc,value=CI[2],stylename="BodyText")
      doc<<-addParagraph(doc,value=CI[3],stylename="BodyText")
      doc<<-addParagraph(doc,value=CI[4],stylename="BodyText")
      if(length(input$competitors)>0){
        
        for (i in input$competitors){
          doc<<-addParagraph(doc,value=CompetInfo()[[i]][1],stylename="Heading2")
          doc<<-addParagraph(doc,value=CompetInfo()[[i]][2],stylename="BodyText")
          doc<<-addParagraph(doc,value=CompetInfo()[[i]][3],stylename="BodyText")
          doc<<-addParagraph(doc,value=CompetInfo()[[i]][4],stylename="BodyText")
        }
      }
      print("pptGen")
    })
  })
  observe({
    if(input$generateReport==0)return()
    isolate({
      tmpWD<-getwd()
      setwd("~/KRAD")
      writeDoc( doc, file = paste(input$company,"FinProfile",".docx",sep=""))
      setwd(tmpWD)
    })
    
  })
  observe({
    input$addToDR
    if(input$addToDR==0)return()
    if(is.null(input$addToDR))return()
    print(input$addToDR)
    
    
    isolate({
      if(is.null(plotTmp()))return
      print("adding graphs to report")
      ##word Doc
      plotTmp<-plotTmp()
      
      doc<<-addPlot( doc, function( ) print( plotTmp ), vector.graphic = TRUE,height=3.2,width=6.8)
      doc<<-addParagraph(doc, value = sentance1(),stylename="BodyText" )
      doc<<-addParagraph(doc, value = sentance2(),stylename="BodyText" )
      ##pptx
    })
    
  })
  
  
  
  output$text<-renderText({
    "Yo Bro"
  })
  
  ##observer Testimonials  
  testimonial<-reactive({
    tSet<-switch(input$sVertical,
                 "K-12"=testimontials[,list(valueArea,K12Testimonials)],
                 "Municipal Government"=testimontials[,list(valueArea,municipal.Government)],
                 "State Government"=testimontials[,list(valueArea,stateGovernment)],
                 "Higher Ed"=testimontials[,list(valueArea,higherEd)])
    setnames(tSet,c("valueArea","Ts"))
    tSet
  })
  refs<-reactive({
    rSet<-switch(input$sVertical,
                 "K-12"=references[,list(type,K12Testimonials)],
                 "Municipal Government"=references[,list(type,municipal.Government)],
                 "State Government"=references[,list(type,stateGovernment)],
                 "Higher Ed"=references[,list(type,higherEd)])
    setnames(rSet,c("type","Ts"))
    rSet
  })
  observe({
    if(is.na(input$nEE))return()
    print(input$nEE)
    output$eeSplitCheckbox<-renderUI({
      checkboxInput("eeSplit","Can you find the split exempt and non exempt employees?",
                    value=T)
    })
  })
  output$nEEexemptui<-renderUI({
    numericInput("nEEexempt","# of Exempt EEs",value=NULL)
  })
  output$nEEnonExemptui<-renderUI({
    numericInput("nEEnonExempt","# of Non-Exempt EEs",value=NULL)
  })
  output$eeSliderui<-renderUI({
    sliderInput("eeSlider","Exempt/Non Exempt Employee Split",
                min=0,max=1,step=.05,value=.7)
  })
  nEEexempt<-reactive({
    if(is.null(input$eeSlider))return()
    input$eeSlider*input$nEE
  })
  nEEnonExempt<-reactive({
    if(is.null(input$eeSlider))return()
    (1-input$eeSlider)*input$nEE
  })
  
  output$nEEexemptText<-renderText({
    as.character(nEEexempt())
  })
  output$nEEnonExemptText<-renderText({
    as.character(nEEnonExempt())
  })
  
  
  observe({
    if(is.na(input$annualPayroll))return()
    print(input$annualPayroll)
    output$paySplitCheckbox<-renderUI({
      checkboxInput("paySplit","Can you find the split exempt and non exempt payroll?",
                    value=T)
    })
  })
  output$payExemptui<-renderUI({
    numericInput("annualPayrollExempt","Exempt Annual Payroll",value=NULL)
    
  })
  
  output$payNonExemptui<-renderUI({
    numericInput("annualPayrollNonExempt","Non-Exempt Annual Payroll",value=NULL)
  })
  
  output$paySliderui<-renderUI({
    sliderInput("paySlider","Exempt/Non Exempt Payroll Split",
                min=0,max=1,step=.05,value=.8)
  })
  
  payExempt<-reactive({
    if(is.null(input$paySlider))return()
    format((input$paySlider)*input$annualPayroll,scientific=F)
  })
  payNonExempt<-reactive({
    if(is.null(input$paySlider))return()
    format((1-input$paySlider)*input$annualPayroll,scientific=F)
  })
  
  output$payExemptText<-renderText({
    as.character(payExempt())
  })
  output$payNonExemptText<-renderText({
    as.character(payNonExempt()) 
  })
  
  
  
  observe({
    if(is.na(input$annualPayroll))return()
    print(input$annualPayroll)
    output$overtimeCheckbox<-renderUI({
      checkboxInput("OT","Can you find Annual OT?",
                    value=T)
    })
  })
  output$overtimeSliderui<-renderUI({
    sliderInput("overtimeSlider","OT% of Hourly Payroll",
                min=0,max=.20,step=.01,value=.05)
  })
  
  overtime<-reactive({
    if(is.null(input$overtimeSlider))return()
    if(input$paySplit==F){
      format(input$overtimeSlider*(1-input$paySlider)*input$annualPayroll,scientific=F)
    }else{
      format(input$overtimeSlider*input$annualPayrollNonExempt,scientific=F)
    }
    
  })
  output$overtimeText<-renderText({
    as.character(overtime())
  })
  
  PBinputs<-reactive({
    if(is.na(input$annualPayroll))return()
    if(is.na(input$nEE))return()
    if(input$eeSplit==T){
      if(is.na(input$nEEexempt))return()
      ExemptEEs<-input$nEEexempt
    }else{
      if(is.null(nEEexempt()))return()
      ExemptEEs<-as.numeric(nEEexempt())
    }
    if(input$eeSplit==T){
      if(is.na(input$nEEnonExempt))return()
      nonExemptEEs<-input$nEEnonExempt
    }else{
      if(is.null(nEEnonExempt()))return()
      nonExemptEEs<-as.numeric(nEEnonExempt())
    }
    
    if(input$paySplit==T){
      if(is.na(input$annualPayrollNonExempt))return()
      nonExemptPayroll<-input$annualPayrollNonExempt
    }else{
      if(is.null(payNonExempt()))return()
      nonExemptPayroll<-as.numeric(payNonExempt())
    }
    if(input$paySplit==T){
      if(is.na(input$annualPayrollExempt))return()
      ExemptPayroll<-input$annualPayrollExempt
    }else{
      if(is.null(payExempt()))return()
      ExemptPayroll<-as.numeric(payExempt())
    }
    
    if(input$OT==T){
      if(is.na(input$OTamount))return()
      AnnualOvertime<-input$OTamount
    }else{
      if(is.null(overtime()))return()
      AnnualOvertime<-overtime()
    }
    AvgAnnualPay<-as.numeric(input$annualPayroll)/as.numeric(input$nEE)
    Mets<-c(format(input$annualPayroll,scientific=F),input$nEE,ExemptEEs,nonExemptEEs,ExemptPayroll,nonExemptPayroll,AnnualOvertime,AvgAnnualPay,ExemptPayroll/ExemptEEs,nonExemptPayroll/nonExemptEEs,
            ExemptPayroll/ExemptEEs/2080,
            nonExemptPayroll/nonExemptEEs/2080)
    
    names(Mets)<-NULL
    print(Mets)
    
    Metrics<-data.table(Metric=c("Annual Payroll",
                                 "Total Employees",
                                 "Exempt EEs",
                                 "Non Exempt EEs",
                                 "Exempt Payroll",
                                 "Non Exempt Payroll",
                                 "Annual Overtime",
                                 "Annual Pay Per Employee",
                                 "Annual Pay Exempt",
                                 "Annual Pay Non-Exempt",
                                 "Hourly Rate Exempt",
                                 "Hourly Rate Non-Exempt"),Value=as.numeric(Mets))  
  })
  
  output$table<-renderTable({
    if(is.null(PBinputs()))return()
    PBinputs()
    
  })
  
  output$dataQuality<-renderTable({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    print(inputData)
    c1<-(inputData[Metric=="Annual Pay Per Employee",Value]>30000)&(inputData[Metric=="Annual Pay Per Employee",Value]<90000)
    c2<-(inputData[Metric=="Annual Pay Exempt",Value]>30000)&(inputData[Metric=="Annual Pay Exempt",Value]<90000)
    c3<-inputData[Metric=="Hourly Rate Non-Exempt",Value]>9
    c4<-inputData[Metric=="Annual Pay Non-Exempt",Value]<inputData[Metric=="Annual Pay Exempt",Value]
    
    print(c(c1,c2,c3,c4))
    
    print(inputData[Metric=="Hourly Rate Non-Exempt",Value])
    print(inputData[Metric=="Hourly Rate Non-Exempt",Value]>9)
    data.table(Condition=c("Average Annual Pay/Employee between $30,000 and $90,000",
                           "Average Annual Pay for Exempt EE between $30,000 and $90,000",
                           "Hourly Rate for Non-Exempt greater than $9/hr",
                           "Annual Exempt Pay greater than Annual Non-Exempt Pay"),
               MeetsRequirement=c(c1,c2,c3,c4))
  })
  
  
  observe({
    if(input$otsavings==F)return()
    
    output$otSavingsSlider<-renderUI({
      sliderInput("otSavingsPerc","Overtime Savings Percentage",value=.10,min=0,max=.3,step=.01)
    })
  })
  
  overtimeSavings<-reactive({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    tableNames<-c("Total Annual Overtime",
                  "Reduction in Overtime Due to Visibility & Tools",
                  "Total Overtime Cost Savings Potential")
    tableMetrics<-c(Dollar(inputData[Metric=="Annual Overtime",Value],0),
                    paste(100*input$otSavingsPerc,"%",sep=""),
                    Dollar(inputData[Metric=="Annual Overtime",Value]*input$otSavingsPerc,0))
    tableOut<-cbind(tableNames,tableMetrics)
    print(tableOut)
    tableOut                
  })
  payrollInflationSavings<-reactive({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    tableNames<-c("Number of Non-Exempt Employees",
                  "Percentage of Employees Receiving Extra Pay",
                  "Total Extra Pay Employees",
                  'Minutes "gamed" per week',
                  "Average Hourly Rate",
                  "Dollars Lost to Inflation Per Week",
                  "Potential Annual Dollar Loss to Payroll Inflation")
    tableMetrics<-c(inputData[Metric=="Non Exempt EEs",Value],
                    paste(100*input$percEEextraPay,"%",sep=""),
                    round(inputData[Metric=="Non Exempt EEs",Value]*input$percEEextraPay,0),
                    input$minGamedPerWeek,
                    Dollar(inputData[Metric=="Hourly Rate Non-Exempt",Value],2),
                    Dollar(inputData[Metric=="Non Exempt EEs",Value]*input$percEEextraPay*inputData[Metric=="Hourly Rate Non-Exempt",Value],0),
                    Dollar(inputData[Metric=="Non Exempt EEs",Value]*input$percEEextraPay*inputData[Metric=="Hourly Rate Non-Exempt",Value]*52,0))
    tableOut<-cbind(tableNames,tableMetrics)
    print(tableOut)
    tableOut                
  })
  leaveSavings<-reactive({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    tableNames<-c("Total Number of Employees",
                  "% of Employees Underreporting",
                  "# of Employees Underreporting",
                  "Average PTO Inflation Hours",
                  "Average Hourly Wage",
                  "Leave Inflation Reduction Savings Potential")
    tableMetrics<-c(inputData[Metric=="Total Employees",Value],
                    paste(100*input$kronosPTOEstimate,"%",sep=""),
                    round(inputData[Metric=="Total Employees",Value]*input$kronosPTOEstimate,2),
                    input$PTOhours,
                    Dollar(inputData[Metric=="Annual Pay Per Employee",Value]/2080,2),
                    Dollar(input$PTOhours*inputData[Metric=="Annual Pay Per Employee",Value]/2080*inputData[Metric=="Total Employees",Value]*input$kronosPTOEstimate,0)
    )
    tableOut<-cbind(tableNames,tableMetrics)
    print(tableOut)
    tableOut                
  })
  absenceSavings<-reactive({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    tableNames<-c("Total Annual Payroll",
                  "% Lost to Incidental Unplanned Absences",
                  "Cost of Absenteeism",
                  "Kronos Ability to Impact",
                  "Total Cost Saving Potential")
    tableMetrics<-c(Dollar(as.numeric(inputData[Metric=="Annual Payroll",Value]),0),
                    paste(100*input$absenceRate,"%",sep=""),
                    Dollar(inputData[Metric=="Annual Payroll",Value]*input$absenceRate,0),
                    paste(100*input$kronosAbsenceImpact,"%",sep=""),
                    Dollar(inputData[Metric=="Annual Payroll",Value]*input$absenceRate*input$kronosAbsenceImpact,0)
    )
    tableOut<-cbind(tableNames,tableMetrics)
    print(tableOut)
    tableOut                
  })
  calcErrorSavings<-reactive({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    tableNames<-c("Total Annual Non-Exempt Payroll",
                  "Potential Calculation Error",
                  "Total Annual Dollar Loss to Calculation Error",
                  "Percentage of Calculation Error Savings",
                  "Potential Annual Savings in Calculation Error Reduction")
    tableMetrics<-c(Dollar(inputData[Metric=="Non Exempt Payroll",Value],0),
                    paste(100*input$payrollErrorRate,"%",sep=""),
                    Dollar(inputData[Metric=="Non Exempt Payroll",Value]*input$payrollErrorRate,0),
                    paste(100*input$kronosCalcErrorImpact,"%",sep=""),
                    Dollar(inputData[Metric=="Non Exempt Payroll",Value]*input$kronosCalcErrorImpact*input$payrollErrorRate,0)
    )
    tableOut<-cbind(tableNames,tableMetrics)
    print(tableOut)
    tableOut                
  })
  flsaSavings<-reactive({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    tableNames<-c("Total Annual Non-Exempt Payroll",
                  "Aveage Payroll %",
                  "FLSA Compliance Risk")
    tableMetrics<-c(Dollar(inputData[Metric=="Non Exempt Payroll",Value],0),
                    "0.02%",
                    Dollar(inputData[Metric=="Non Exempt Payroll",Value]*.0002,0)                
    )
    tableOut<-cbind(tableNames,tableMetrics)
    print(tableOut)
    tableOut                
  })
  flmaSavings<-reactive({
    if(is.null(PBinputs()))return()
    inputData<-PBinputs()
    tableNames<-c("Average Cost to Prepare Case for Trial",
                  "Average Cost to Defend an FMLA Violation",
                  "FMLA Compliance Risk")
    tableMetrics<-c("$150,000",
                    "$78,000",
                    "$228,000")                
    tableOut<-cbind(tableNames,tableMetrics)
    print(tableOut)
    tableOut                
  })
  output$otLook<-renderTable({
    overtimeSavings()
  })
  output$piLook<-renderTable({
    payrollInflationSavings()
  })
  output$leLook<-renderTable({
    leaveSavings()
  })
  output$abLook<-renderTable({
    absenceSavings()
  })
  output$ceLook<-renderTable({
    calcErrorSavings()
  })
  output$flsaLook<-renderTable({
    flsaSavings()
  })
  output$flmaLook<-renderTable({
    flmaSavings()
  })
  
  
  observe({
    if(input$buildPB==0)return()
    if(input$orgName=="")return()
    isolate({
      doc<-docx(template="surveyDocTemplate.docx")
      title1<-paste("Potential Savings: ",input$orgName)
      title2<-"Overview"
      intro1<-paste('Labor is ',input$orgName,'’s largest operating expense. An automated workforce management system can make an immediate impact on how effectively and strategically ',input$orgName,' manages its workforce to assist in controlling labor costs.',sep="")
      intro2<-paste('Many issues exist related to labor tracking for ',input$orgName,' employees. These challenges may include cumbersome manual processes, excessive premium overtime, “buddy punching” causing payroll inflation, payroll calculation error, labor compliance risk, and limited analysis of detailed personnel costs associated with department appropriations and programs.',sep="")
      intro3<-paste('This Potential ROI Analysis was completed to give ',input$orgName,' an understanding of the savings that can be realized with the implementation of a workforce management solution.',sep="")
      
      doc<-addParagraph(doc,value=title1,stylename="KronosDocumentTitle")
      doc<-addParagraph(doc,value=title2,stylename="KronosDocumentTitle")
      doc<-addParagraph(doc,value=intro1,stylename="BodyText")
      doc<-addParagraph(doc,value=intro2,stylename="BodyText")
      doc<-addParagraph(doc,value=intro3,stylename="BodyText")
      sumTable<-data.frame() 
      totalSavings<-c(flsaSavings()[nrow(flsaSavings()),2],
                      flmaSavings()[nrow(flmaSavings()),2])
      if(input$otsavings==T){
        sumTable<-rbind(sumTable,cbind("Overtime Control and Reduction",overtimeSavings()[nrow(overtimeSavings()),2]))
        totalSavings<-c(totalSavings,overtimeSavings()[nrow(overtimeSavings()),2])
      }
      if(input$payrollInflation==T){
        sumTable<-rbind(sumTable,cbind("Reduction in Payroll Inflation",payrollInflationSavings()[nrow(payrollInflationSavings()),2]))
        totalSavings<-c(totalSavings,payrollInflationSavings()[nrow(payrollInflationSavings()),2])
      }
      if(input$calculationError==T){
        sumTable<-rbind(sumTable,cbind("Reduction in Calculation Error",calcErrorSavings()[nrow(calcErrorSavings()),2]))
        totalSavings<-c(totalSavings,calcErrorSavings()[nrow(calcErrorSavings()),2])
      }
      if(input$absenteeism==T){
        sumTable<-rbind(sumTable,cbind("Reduction in Unscheduled Absenteeism",absenceSavings()[nrow(absenceSavings()),2]))
        totalSavings<-c(totalSavings,absenceSavings()[nrow(absenceSavings()),2])
      }
      colnames(sumTable)[1]<-"Annual Financial Savings"
      colnames(sumTable)[2]<-" "
      MyFlex<-FlexTable(data= sumTable,
                        header.cell.props=cellProperties(background.color="white",border.width=4),
                        header.text.props=textBold( color="black"))
      MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
      MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
      MyFlex[,2:ncol(sumTable),to ="header"] <- parRight()
      MyFlex[,2:ncol(sumTable)] <- parRight()
      MyFlex<- setFlexTableBorders(MyFlex
                                   , inner.vertical = borderProperties( color="white", style="none"  )
                                   , inner.horizontal = borderProperties( color = "white", style = "none" )
                                   , outer.vertical = borderProperties( color = "white", style = "none"  )
                                   , outer.horizontal = borderProperties( color = "white", style = "none" )
      )
      doc<-addFlexTable(doc,MyFlex)
      
      if(input$leaveInflation==T){
        totalSavings<-c(totalSavings,leaveSavings()[nrow(leaveSavings()),2])
        sumleavetab<-data.frame(cbind("Leave Inflation Savings due to Manual Leave Request Process",
                                      leaveSavings()[nrow(leaveSavings()),2]))
        
        colnames(sumleavetab)[1]<-"Reduction in Leave Liability"
        colnames(sumleavetab)[2]<-" "
        MyFlex<-FlexTable(data= sumleavetab,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol(sumTable),to ="header"] <- parRight()
        MyFlex[,2:ncol(sumTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        doc<-addFlexTable(doc,MyFlex)
      }
      sumRiskTab<-data.frame(rbind(cbind("FLSA Compliance Risk",flsaSavings()[nrow(flsaSavings()),2]),
                                   cbind("FMLA Compliance Risk",flmaSavings()[nrow(flmaSavings()),2])))
      colnames(sumRiskTab)[1]<-"Compliance Risk Mitigation"
      colnames(sumRiskTab)[2]<-" "
      MyFlex<-FlexTable(data= sumRiskTab,
                        header.cell.props=cellProperties(background.color="white",border.width=4),
                        header.text.props=textBold( color="black"))
      MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
      MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
      MyFlex[,2:ncol(sumTable),to ="header"] <- parRight()
      MyFlex[,2:ncol(sumTable)] <- parRight()
      MyFlex<- setFlexTableBorders(MyFlex
                                   , inner.vertical = borderProperties( color="white", style="none"  )
                                   , inner.horizontal = borderProperties( color = "white", style = "none" )
                                   , outer.vertical = borderProperties( color = "white", style = "none"  )
                                   , outer.horizontal = borderProperties( color = "white", style = "none" )
      )
      doc<-addFlexTable(doc,MyFlex)
      print(totalSavings)
      names(totalSavings)<-NULL
      sumTotalSavings<-Dollar(sum(curToNum(totalSavings)),0)
      Final<-data.frame(cbind("Total Direct and Indirect Savings Potential",sumTotalSavings))
      MyFlex<-FlexTable(data=Final,header.columns=F,body.text.props=textBold(color="black",font.size=14))
      MyFlex<- setFlexTableBorders(MyFlex
                                   , inner.vertical = borderProperties( color="white", style="none"  )
                                   , inner.horizontal = borderProperties( color = "white", style = "none" )
                                   , outer.vertical = borderProperties( color = "white", style = "none"  )
                                   , outer.horizontal = borderProperties( color = "white", style = "none" )
      )
      MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
      MyFlex[,2:ncol(sumTable)] <- parRight()
      doc<-addFlexTable(doc,MyFlex)
      doc<-addParagraph(doc,value="Customer Testimonial",stylename="Heading3")
      doc<-addParagraph(doc,value=as.character(testimonial()[valueArea=="Total Cost Savings",Ts]),stylename="BodyText")
      
      if(input$otsavings==T){
        otTitle<-"Area of Savings Detail: Overtime Control & Reduction"
        otIntro<-"Organizations that do not have real time labor reporting, which warns supervisors that employees approaching overtime tend to run excessive overtime. Without adequate tools, supervisors cannot assess and make decisions to offer overtime at the least cost or use resources from other areas.  Kronos provides these tools, and Kronos customers see real reductions in controllable overtime spend." 
        BCintro<-"Basis for Calculation"
        otBCText<-'According to Forester Research “With Improved visibility, workflow and labor management data, corporate managers and supervisors can better manage labor resources and save at least 10% in overtime costs”.' 
        otTable<-overtimeSavings()
        colnames(otTable)[1]<-"Area of Savings"
        colnames(otTable)[2]<-" "
        doc<-addParagraph(doc,value=otTitle,stylename="Heading1")
        doc<-addParagraph(doc,value=otIntro,stylename="BodyText")
        MyFlex<-FlexTable(data= otTable,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol(otTable),to ="header"] <- parRight()
        MyFlex[,2:ncol(otTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        MyFlex[nrow(otTable),]<-textProperties(font.weight="bold")
        doc<-addFlexTable(doc,MyFlex)
        doc<-addParagraph(doc,value=BCintro,stylename="Heading3")
        doc<-addParagraph(doc,value=otBCText,stylename="BodyText")
        doc<-addParagraph(doc,value="Customer Testimonial",stylename="Heading3")
        doc<-addParagraph(doc,value=as.character(testimonial()[valueArea=="Overtime",Ts]),stylename="BodyText")
        
      }
      
      
      
      if(input$calculationError==T){
        ceTitle<-"Area of Savings Detail: Reduction in Calculation Error"
        ceIntro<-"Organizations that are paper based, allow employees to key in worked hours, or rely on non-integrated systems which are prone to calculation and transcription errors when recording labor related data."
        ceBEText<-paste("The industry average payroll error rate is 1.2 percent, and varies by industry.  Kronos conservatively has assumed that ",input$orgName," experiences ½ of the payroll rate error average.  Note that the actual payroll error rate may be higher if an organization is larger, more complex, has a dispersed workforce, or if it has a relatively high level of work-rule complexity.  Kronos believes that it can eliminate 80-100% of an organization’s calculation error.",sep="")                    
        BCintro<-"Basis for Calculation"      
        ceTable<-calcErrorSavings()
        colnames(ceTable)[1]<-"Area of Savings"
        colnames(ceTable)[2]<-" "
        doc<-addParagraph(doc,value=ceTitle,stylename="Heading1")
        doc<-addParagraph(doc,value=ceIntro,stylename="BodyText")
        MyFlex<-FlexTable(data= ceTable,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol(ceTable),to ="header"] <- parRight()
        MyFlex[,2:ncol(ceTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        MyFlex[nrow(ceTable),]<-textProperties(font.weight="bold")
        doc<-addFlexTable(doc,MyFlex)
        doc<-addParagraph(doc,value=BCintro,stylename="Heading3")
        doc<-addParagraph(doc,value=ceBEText,stylename="BodyText")
        doc<-addParagraph(doc,value="Customer Testimonial",stylename="Heading3")
        doc<-addParagraph(doc,value=as.character(testimonial()[valueArea=="Calc Error",Ts]),stylename="BodyText")
      }
      if(input$payrollInflation==T){
        piTitle<-"Area of Savings: Payroll Inflation Due to Time Stamp" 
        piIntro<-'Payroll Inflation is a result of employees working, or "gaming" the system. For example, when time recording is inaccurately captured in favor of the employee or the employee tends to come in early and leave late and gets paid for the difference, it results in payroll inflation.'
        piBEText<-paste('A study conducted by Forrester Consulting Group estimates nearly 12 percent of the hourly or non-exempt workforce regularly overstates two hours of work per pay period that can be saved.  Assuming a bi-weekly pay period cycle, Kronos estimates that 12% of ',input$orgName,'’s employees are overstating time worked by 60 minutes each week.',sep="")                
        
        piTable<-payrollInflationSavings()
        colnames(piTable)[1]<-"Area of Savings"
        colnames(piTable)[2]<-" "
        doc<-addParagraph(doc,value=piTitle,stylename="Heading1")
        doc<-addParagraph(doc,value=piIntro,stylename="BodyText")
        MyFlex<-FlexTable(data= piTable,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol(piTable),to ="header"] <- parRight()
        MyFlex[,2:ncol(piTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        MyFlex[nrow(piTable),]<-textProperties(font.weight="bold")
        doc<-addFlexTable(doc,MyFlex)
        doc<-addParagraph(doc,value=BCintro,stylename="Heading3")
        doc<-addParagraph(doc,value=piBEText,stylename="BodyText")
        doc<-addParagraph(doc,value="Customer Testimonial",stylename="Heading3")
        doc<-addParagraph(doc,value=as.character(testimonial()[valueArea=="Payroll Inflation",Ts]),stylename="BodyText")
        
      }
      if(input$leaveInflation==T){
        llTitle<-'Area of Savings Detail: Reduction in Leave Liability'
        llIntro<-'Organizations that track paid time off manually using paper processes, who allow employees to key in absence hours, or have numerous non-integrated systems, lack visibility and control of leave balances. In this type of environment, employees have the opportunity to artificially inflate their time off balances by not recording time away from work appropriately.'
        llBEText<-paste("On average, employees earn 1.25 days (10 hours) a year of unreported PTO. Kronos conservatively estimates that 25% of ",input$orgName,"'s employees are underreporting PTO.",sep="")
        
        llTable<-leaveSavings()
        colnames(llTable)[1]<-"Area of Savings"
        colnames(llTable)[2]<-" "
        doc<-addParagraph(doc,value=llTitle,stylename="Heading1")
        doc<-addParagraph(doc,value=llIntro,stylename="BodyText")
        MyFlex<-FlexTable(data= llTable,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol(llTable),to ="header"] <- parRight()
        MyFlex[,2:ncol(llTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        MyFlex[nrow(llTable),]<-textProperties(font.weight="bold")
        doc<-addFlexTable(doc,MyFlex)
        doc<-addParagraph(doc,value=BCintro,stylename="Heading3")
        doc<-addParagraph(doc,value=llBEText,stylename="BodyText")
        doc<-addParagraph(doc,value="Customer Testimonial",stylename="Heading3")
        doc<-addParagraph(doc,value=as.character(testimonial()[valueArea=="Leave Inflation",Ts]),stylename="BodyText")
      }
      if(input$absenteeism==T){
        abTitle<-'Area of Savings Detail: Reduction in Unplanned Absenteeism'
        abIntro1<-'Employee absence is an area that is often not carefully tracked or even when it is, does not easily reveal its full costs or identify areas of significant abuse. Most managers know that absences do affect an organization’s ability to provide services and meet business objectives.'
        abIntro2<-'Mercer defines unplanned incidental absences as absences of 5 days or less, such as casual sick days, where the occurrence was not known and approved ahead of time by the employee’s supervisor. Alternative absence types would be planned absences and extended leave-type absences.'
        abBEText<-'In the 2010 survey, The Total Financial Impact of Employee Absences, Mercer Research found that the average total costs of incidental unplanned absences amount to 6% of payroll. A conservative impact of 5% was used for the resulting cost savings.'
        abTest<-'According to a survey on the Total Financial Impact of Employee Absences, 35 percent of payroll is linked to employee absence. This includes the cost of paying absent employees as well indirect costs, such as lost productivity and the hiring of replacement workers.  Absence is a productivity killer — on average, incidental, unplanned absences result in 19 percent of net lost productivity per day.'
        
        abTable<-  absenceSavings()
        colnames(abTable)[1]<-"Area of Savings"
        colnames(abTable)[2]<-" " 
        doc<-addParagraph(doc,value=abTitle,stylename="Heading1")
        doc<-addParagraph(doc,value=abIntro1,stylename="BodyText")
        doc<-addParagraph(doc,value=abIntro2,stylename="BodyText")
        MyFlex<-FlexTable(data= abTable,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol(abTable),to ="header"] <- parRight()
        MyFlex[,2:ncol(abTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        MyFlex[nrow(abTable),]<-textProperties(font.weight="bold")
        doc<-addFlexTable(doc,MyFlex)
        doc<-addParagraph(doc,value=BCintro,stylename="Heading3")
        doc<-addParagraph(doc,value=abBEText,stylename="BodyText")
        doc<-addParagraph(doc,value="Statistics Survey",stylename="Heading3")
        doc<-addParagraph(doc,value=abTest,stylename="BodyText")
      }
      if(input$flsa==T){
        crTitle<-'Area of Savings Detail: Compliance Risk Mitigation'
        crIntro<-"These potential savings are related to mitigating the risk of non-compliance and fines and payouts associated with areas related to federal Wage and Hour and FMLA standards."
        flsa<-'FLSA Compliance Risk Reduction'
        flsaIntro<-'Any single investigation resulting in a payment to an employee is typically followed up with an extensive investigation of the remaining non-exempt workforce. The manual nature of the current time management process and the opportunity for subjectivity between employees places the district at risk of ensuring that a uniform application of wage and hour rules is being applied.'
        flsaBE<-'According to Nucleus Research, on average, .02% of an organization’s annual hourly payroll represents the amount paid to an employee who is found to have had their wage and hour rights violated under the terms of the Fair Labor Standards Act (FLSA).'
        fmla<-'FMLA Compliance Risk Reduction'
        fmlaIntro1<-'The direct cost of FMLA leave to employers was $21 billion in 2010 in terms of lost productivity, continued health benefits and net labor cost. In the case of compliance issues, there is significant cost to defending FMLA violation claims, in addition to the actual fines and penalties.'
        fmlaIntro2<-'Tracking FMLA leave properly results in better compliance and provides the capability to pass audits, which results in reduced risk and cost.' 
        fmlaBE<-'According to the Department of Labor, it costs $78,000 to defend an FMLA violation and according to Jackson Hewitt LLP, it costs an additional $150,000 to prepare the case for trial.'
        
        flsaTable<-flsaSavings()
        fmlaTable<-flmaSavings()
        colnames(flsaTable)[1]<-"Area of Savings"
        colnames(flsaTable)[2]<-" "
        colnames(fmlaTable)[1]<-"Area of Savings"
        colnames(fmlaTable)[2]<-" "
        doc<-addParagraph(doc,value=crTitle,stylename="Heading1")
        doc<-addParagraph(doc,value=crIntro,stylename="BodyText")
        doc<-addParagraph(doc,value=flsa,stylename="Heading2")
        doc<-addParagraph(doc,value=flsaIntro,stylename="BodyText")
        MyFlex<-FlexTable(data= flsaTable,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol( flsaTable),to ="header"] <- parRight()
        MyFlex[,2:ncol( flsaTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        MyFlex[nrow( flsaTable),]<-textProperties(font.weight="bold")
        doc<-addFlexTable(doc,MyFlex)
        doc<-addParagraph(doc,value=BCintro,stylename="Heading3")
        doc<-addParagraph(doc,value=flsaBE,stylename="BodyText")
        doc<-addParagraph(doc,value=fmla,stylename="Heading2")
        doc<-addParagraph(doc,value=fmlaIntro1,stylename="BodyText")
        doc<-addParagraph(doc,value=fmlaIntro2,stylename="BodyText")
        MyFlex<-FlexTable(data= fmlaTable,
                          header.cell.props=cellProperties(background.color="white",border.width=4),
                          header.text.props=textBold( color="black"))
        MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
        MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
        MyFlex[,2:ncol( fmlaTable),to ="header"] <- parRight()
        MyFlex[,2:ncol( fmlaTable)] <- parRight()
        MyFlex<- setFlexTableBorders(MyFlex
                                     , inner.vertical = borderProperties( color="white", style="none"  )
                                     , inner.horizontal = borderProperties( color = "white", style = "none" )
                                     , outer.vertical = borderProperties( color = "white", style = "none"  )
                                     , outer.horizontal = borderProperties( color = "white", style = "none" )
        )
        MyFlex[nrow( fmlaTable),]<-textProperties(font.weight="bold")
        doc<-addFlexTable(doc,MyFlex)
        doc<-addParagraph(doc,value=BCintro,stylename="Heading3")
        doc<-addParagraph(doc,value=fmlaBE,stylename="BodyText")
      }
      
      
      disclosureText<-paste("The following table shows the data that Kronos used to calculate potential savings in the document, some of which may have been estimated using industry benchmarks if",
                            input$orgName," did not provide Kronos with these metrics.  These estimations are used within most of the savings formulas. It is strongly recommended that ",input$orgName," verify the accuracy of these estimations to ensure realistic potential savings.",sep="")
      doc<-addParagraph(doc,value="Disclosure",stylename="Heading2")
      doc<-addParagraph(doc,value=disclosureText,stylename="BodyText")
      metT<-PBinputs()
      metT[,Value:=Dollar(Value,2)]     
      metT[Metric%in%c("Total Employees","Exempt EEs","Non Exempt EEs"),Value:=str_replace_all(Value,"\\$","")]
      metT<-metT[Metric%in%c("Annual Pay Per Employee","Annual Pay Exempt","Annual Pay Non-Exempt")==F]
      colnames(metT)[2]<-" "
      MyFlex<-FlexTable(data=  metT,
                        header.cell.props=cellProperties(background.color="white",border.width=4),
                        header.text.props=textBold( color="black"))
      MyFlex<-setFlexTableWidths( MyFlex,widths=c(5,2))
      MyFlex<-setZebraStyle(MyFlex,odd="#FFEBCD",even="white")
      MyFlex[,2:ncol(metT),to ="header"] <- parRight()
      MyFlex[,2:ncol(metT)] <- parRight()
      MyFlex<- setFlexTableBorders(MyFlex
                                   , inner.vertical = borderProperties( color="white", style="none"  )
                                   , inner.horizontal = borderProperties( color = "white", style = "none" )
                                   , outer.vertical = borderProperties( color = "white", style = "none"  )
                                   , outer.horizontal = borderProperties( color = "white", style = "none" )
      )
      doc<-addFlexTable(doc,MyFlex)
      
      titleR<-refs()[type=="Title"]
      link<-refs()[type=="Source"]
      RT<-"References"
      RStart<-"Payroll and employee demographic data used to calculate this Potential ROI Analysis and news articles can be found in the following sources:"
      
      doc<-addParagraph(doc,value=RT,stylename="KronosDocumentTitle")
      doc<-addParagraph(doc,value=RStart,stylename="BodyText")
      for(i in 1:5){
        doc<-addParagraph(doc,value=as.character(titleR[i,Ts]),stylename="Heading3")
        doc<-addParagraph(doc,value=as.character(link[i,Ts]),stylename="BodyText")
      }
      tmpWD<-getwd()
      setwd("~/KRAD")
      writeDoc( doc, file = paste("Potential Savings-",input$orgName,".docx",sep=""))
      setwd(tmpWD)
    })
  })
  
  ACAdata<-reactive({
    if(is.na(input$acaPTEETotal))return()
    if(is.na(input$acaFTEETotal))return()
    if(is.na(input$acaNumEETotal))return()
    if(is.na(input$BenEx))return()
    if(is.na(input$COLA))return()
    Plus10pt<-input$acaPTEETotal-(input$acaPTEETotal*.1)
    Minus10ft<-input$acaFTEETotal-(input$acaFTEETotal*.1)
    Plus10ft<-input$acaNumEETotal-Plus10pt
    Minus10pt<-input$acaNumEETotal-Minus10ft
    
    data<-data.table(c(input$acaFTEETotal,Plus10ft,Minus10ft))
    data2<-data.table(c(input$acaFTEETotal*input$BenEx,
                        Plus10ft*input$BenEx,
                        Minus10ft*input$BenEx))
    data2[,col1:=(V1)]
    data[,col1:=(V1-80)*2000]
    data<-rbind(data,data2)
    data[,y2:=col1*(1+input$COLA)]
    data[,y3:=y2*(1+input$COLA)]
    data[,y4:=y3*(1+input$COLA)]
    data[,y5:=y4*(1+input$COLA)]
    data[,cos:=col1+y2+y3+y4+y5]
    
    data[,y5:=(V1-80)*2000]
    data[,y5:=(V1-80)*2000]
    
    data[,mix:=ordered(c("Current Mix","If 10% of PT reclassify to FT","If 10% of FT reclassify to PT"))]
    data[,graph:=ordered(c(rep("Estimate of Subsection (a) Assessment",3),rep("Estimate of Benefits Expense",3)))]
    
    data[,mix:=ordered(mix,c("If 10% of FT reclassify to PT","Current Mix","If 10% of PT reclassify to FT"))]
    data[,graph:=ordered(graph,c("Estimate of Subsection (a) Assessment","Estimate of Benefits Expense"))]
    dataG<-data[,list(Year1=col1,yearcon5=cos,mix,graph)]
    
    readyG<-data.table(melt(dataG,id.vars=c("mix","graph")))
    readyG[variable=="Year1",variable:="Year One"]
    readyG[variable=="yearcon5",variable:="5 Years Consolidated"]
    print(readyG)
    readyG
  })
  
  graph<-reactive({
    if(is.null(ACAdata))return()
    if(any(is.na(ACAdata()$value)))return()
  })
  
  observe({
    if(is.null(ACAdata))return()
    if(any(is.na(ACAdata()$value)))return()
    if(any(c(input$ACAorgName,
             input$subvertACA,
             input$repName,
             input$repEmail,
             input$repNumber,
             input$date)==""))return()
    
    output$genACA<-renderUI({
      actionButton("genACAdoc","Generate ACA Brief")
    })
    
  })
  
  observe({
    if(is.null(input$genACAdoc))return()
    if(input$genACAdoc==0)return()
    readyG<-ACAdata()
    text2<-paste(input$ACAorgName," currently employs approximately  ",input$acaNumEETotal,
                 " employees, including ",input$acaFTEETotal,"  (",round(input$acaFTEETotal/input$acaNumEETotal*100,1),
                 "%) full time (FT), and ",input$acaPTEETotal," (",round(input$acaPTEETotal/input$acaNumEETotal*100,1),"%) part time (PT).  The ACA requires all large employers to offer healthcare benefits to employees with at least 30 hours of service per week or 130 hours of service per month, or pay a $2,000 per FT employee assessment called a Subsection (a) penalty.  How many PT employees work near or above the 30/130 threshold, and how might that affect actual to budget performance?  Consider the scenarios presented below, whereby ",
                 input$ACAorgName,"’s current mix is contrasted with either 10% of PT employees reclassified as FT, or 10% of FT employees reclassified as PT:",sep="")
    
    pay<-paste("“Pay”- Potential Year 1 Cost for Subsection (a) Assessment: ",Dollar(readyG[2,value],0),sep="")
    play<-paste("“Play”- Potential Year 1 Cost for Healthcare Benefits:  ",Dollar(readyG[5,value],0),sep="")
    Est<-paste("Estimates are based on a healthcare benefits cost average per employee of ",
               Dollar(input$BenEx,0)," and an inflationary or cost of living adjustment of ",100*input$COLA,"%.",sep="")
    text3<-paste("Does ",input$ACAorgName," have the tools it needs to ensure its employee mix aligns with organizational strategy and budget?  Kronos possesses extensive workforce management experience in the ",
                 input$subvertACA," industry coupled with an understanding of ACA reporting requirements and the various financial scenarios organizations are facing.  Please contact ",
                 input$repName," at ",input$repEmail," or ",input$repNumber,
                 " to find out how Kronos can help your organization today!",sep="")
    Heading<-input$ACAorgName
    
    text1<-paste(input$subvertACA," organizations have been required to adapt to and comply with hundreds of regulatory changes over the years.  Change generates uncertainty- and the Affordable Care Act (ACA) is no exception.  Is your organization prepared for the ACA?",sep="")
    
    
    g1<-readyG[graph=="Estimate of Subsection (a) Assessment"]
    g2<-readyG[graph=="Estimate of Benefits Expense"]
    gg<-ggplot(g1,aes(x=variable,y=value,fill=mix))
    gg<-gg+geom_bar(position="dodge",stat="identity")
    p1<-gg+theme_calc()+
      theme(                                                                                                                                                       
        axis.ticks = element_blank(),                                                                            
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none")+ scale_y_continuous(labels = dollar)+ labs(fill="")+
      scale_fill_tableau("tableau10")+
      ggtitle("Estimate of Subsection (a) Assessment")+ 
      theme(plot.title = element_text(size=10, face="bold"))
    
    gg<-ggplot(g2,aes(x=variable,y=value,fill=mix))
    gg<-gg+geom_bar(position="dodge",stat="identity")
    p2<-gg+theme_calc()+
      theme(                                                                                                                                                       
        axis.ticks = element_blank(),                                                                            
        
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")+ scale_y_continuous(labels = dollar)+ labs(fill="")+
      scale_fill_tableau("tableau10")+
      ggtitle("Estimate of Benefits Expense")+
      theme(plot.title = element_text(size=10, face="bold"))
    
    grid<-list(p1,p2)
    g <- ggplotGrob(p1 + theme(legend.position="bottom",legend.direction="horizontal"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    arglist<-c(grid,list(widths=c(1/2,1/2),ncol=2))
    gs<-do.call(arrangeGrob,arglist)
    mydoc<-docx(template="ACAtemplate.docx")
    mydoc <- addParagraph( mydoc, value = Heading,stylename="DocHead",bookmark = "Heading" )
    mydoc <- addParagraph( mydoc, value =text1 ,stylename="BT2", bookmark = "First" )
    mydoc <- addParagraph( mydoc, value =text2  ,stylename="BT2", bookmark = "Second" )
    mydoc<-addPlot(mydoc , fun = function()grid.arrange(gs,legend,ncol=1, heights=c(7, 1)), bookmark = "graph1",height=3,width=7,vector.graphic = TRUE)
    mydoc <- addParagraph( mydoc, value =Est  ,stylename="SmallItc", bookmark = "est" )
    mydoc <- addParagraph( mydoc, value =pay  ,stylename="StyleRed", bookmark = "Pay" )
    mydoc <- addParagraph( mydoc, value =play  ,stylename="StyleRed", bookmark = "Play" )
    mydoc <- addParagraph( mydoc, value =text3  ,stylename="BT2", bookmark = "Third" )
    tmpWD<-getwd()
    setwd("~/KRAD")
    writeDoc( mydoc,  paste(input$ACAorgName,"ACA Executive Brief",".docx",sep="") )
    setwd(tmpWD)
    
  })
  
  observe({
    input$companType
    if(input$companType=="Publically Traded") {
      output$hmtlOut<-renderUI({
        
        list(p("-Annual Reports (10k)"),
             p("-Quarterly Reports (10Q)"),
             p("-Investor Presentation"),
             p("-News/Press Release"),
             br(),
             p("Most of the information above can be found on 
               the company's ",strong("investor relations page.")),
             br(),
             p(strong("Additional Sources")),
             br(),
             p("-",a("Stock Reports",href="https://www.fidelity.com/"),
               ": Stock Reports can be found on ",
               a("Fidelity.com",href="https://www.fidelity.com/"),
               ". Generally the best stock report they offer is ", 
               strong("Zacks Investment Research.")," *You will need to open a free 
               brokerage account with Fidelity to access these reports."),
             p("- ",a("Justia Dockets",href="https://dockets.justia.com/"),
               ": Use this source to search for labor litigation"))
        
      })
    }
    if(input$companType=="Privately Held") {
      output$hmtlOut<-renderUI({
        
        list(p("-About us"),
             p("-Mission, Goals, Values"),
             p("-Company History"),
             p("-Locations"),
             br(),
             p("Most of the information above can be found on 
               the company's ",strong("website.")),
             br(),
             p(strong("Additional Sources")),
             br(),
             p("-",a("Hoovers.com",href="http.www.hoovers.com/")),
             p("- ",a("Justia Dockets",href="https://dockets.justia.com/"),": Use this source to search for labor litigation"))
        
      })
    }
    if(input$companType=="City and State Government") {
      output$hmtlOut<-renderUI({
        list(p("-Comprehensive Annual Financial Reports"),
             p("-Budget Documents"),
             p("Capital Improvement Plans"),
             p("-Open Data Portals"),
             p("-Audit Reports"),
             p("- ",a("Justia Dockets",href="https://dockets.justia.com/"),": Use this source to search for labor litigation"),
             br(),
             p("Most of the information above can be found on 
               the organization's ",strong("website.")))
      })
      }
    if(input$companType=="Higher Education and K-12") {
      output$hmtlOut<-renderUI({
        list(p("-Comprehensive Annual Financial Reports"),
             p("-Budget Documents"),
             p("Capital Improvement Plans"),
             p("-Open Data Portals"),
             p("-Audit Reports"),
             br(),
             p("Most of the information above can be found on 
               the organization's ",strong("website.")),
             br(),
             p(strong("Additional Sources")),
             br(),
             p("- ",a("Justia Dockets",href="https://dockets.justia.com/"),": Use this source to search for labor litigation"),
             p("- ",a("IPEDS Data Center",href="https://nces.ed.gov/ipeds/datacenter/"),
               ":Use this source for statistics on all universities and public schools.
               Information provided includes employment statistics and financials"))
        
      })
    }
    })
  
  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
  })
  
})
