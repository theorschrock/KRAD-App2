
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("source.R")

shinyUI(
  navbarPage("KRAD",
             tabPanel("Organizational Profile",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Sources",
                                           sidebarLayout(position="right",
                                                         sidebarPanel(
                                                           p("Company websites are where you will 
                                                             retrieve the majority of information 
                                                             while creating an organizational profile. 
                                                             If the company is publically traded, the 
                                                             amount of information that will be 
                                                             available on these sites is usually very 
                                                             comprehensive. A good place to start your 
                                                             research for a publically traded company 
                                                             is in its annual reports or investor 
                                                             presentations. If the company is privately
                                                             held, information will be limited, but 
                                                             even basic information found on their 
                                                             website can be beneficial to a Kronos 
                                                             sales rep."),
                                                           br(),
                                                           p("While doing research on a public sector account, the most useful information comes from CAFR reports and Budget Documents. Sometimes you will be able to find summaries of these documents, which usually pulls out the most pertinent information."),
                                                           br(),
                                                           p("Hoovers is a good place to find supplemental information for your research. Here you can easily find employment numbers, competitors, basic financials, and industry info."),
                                                           br(),
                                                           p("Standard Google searches and Google News will help you find a company’s recent activity. It is also a great way to find out if an organization has ever been involved in labor litigation.")
                                                           ),
                                                         mainPanel(
                                                           selectInput(inputId="companType",label=h3("Company Type"), 
                                                                       choices=c("Publically Traded",
                                                                                 "Privately Held",
                                                                                 "City and State Government",
                                                                                 "Higher Education and K-12"),
                                                                       selected="Publically Traded"),
                                                           uiOutput("hmtlOut")
                                                         )
                                                         )))),
             tabPanel("Prospecting Brief",
                      
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          textInput("orgName","Organization",value=NULL),
                          selectizeInput('sVertical',"Sub-Vertical",choices=c("K-12","Municipal Government","State Government","Higher Ed"),
                                         selected="Municipal Government",multiple=F),
                          numericInput("nEE","Total Number of Employees",value=NULL),
                          uiOutput("eeSplitCheckbox"),
                          conditionalPanel(
                            condition="input.eeSplit==true",
                            uiOutput("nEEexemptui"),
                            uiOutput("nEEnonExemptui")
                          ),
                          conditionalPanel(
                            condition="input.eeSplit==false",
                            uiOutput("eeSliderui"),
                            h5(strong("Total Number of Exempt Employees")),
                            uiOutput("nEEexemptText"),
                            h5(strong("Total Number of Non-Exempt Employees")),
                            uiOutput("nEEnonExemptText")
                          ),
                          numericInput("annualPayroll","Annual Payroll",value=NULL),
                          uiOutput("paySplitCheckbox"),
                          conditionalPanel(
                            condition="input.paySplit==true",
                            uiOutput("payExemptui"),
                            uiOutput("payNonExemptui")
                          ),
                          conditionalPanel(
                            condition="input.paySplit==false",
                            uiOutput("paySliderui"),
                            h5(strong("Annual Exempt Payroll")),
                            uiOutput("payExemptText"),
                            h5(strong("Annual Non-Exempt Payroll")),
                            uiOutput("payNonExemptText")
                          ),
                          uiOutput("overtimeCheckbox"),
                          conditionalPanel(
                            condition="input.OT==false",
                            uiOutput("overtimeSliderui"),
                            h5(strong("Annual Overtime Estimate")),
                            uiOutput("overtimeText")
                          ),
                          conditionalPanel(
                            condition="input.OT==true",
                            numericInput("OTamount","Annual Overtime",value=NULL)
                          )
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          h3("Metrics"),
                          tableOutput("table"),
                          h3("Data Quality Check"),
                          tableOutput("dataQuality"),
                          h3("Value Areas"),
                          fluidRow(
                            column(3,
                                   checkboxInput("otsavings","Overtime Reduction",value=T)
                            ),
                            column(3,
                                   checkboxInput("payrollInflation","Payroll Inflation",value=T)
                            ),
                            column(3,
                                   checkboxInput("leaveInflation","Leave Inflation",value=T)
                            )
                          ),
                          fluidRow(
                            column(3,
                                   checkboxInput("absenteeism","Unscheduled Absenteeism",value=T)
                            ),
                            column(3,
                                   checkboxInput("calculationError","Calculation Error",value=T)
                            ),
                            column(3,
                                   checkboxInput("flsa","FLSA/FMLA Compliance",value=T)
                            )),
                          fluidRow(
                            conditionalPanel(
                              condition="input.otsavings==true",
                              h3("Reduction in Controllable Overtime"),   
                              uiOutput("otSavingsSlider"),
                              tableOutput("otLook")),
                            conditionalPanel(
                              condition="input.payrollInflation==true",
                              h3("Payroll Inflation Due to Time Stamp"),
                              sliderInput("percEEextraPay","Percentage of Employees Recieving Extra Pay",value=.12,min=0,max=.5,step=.01),
                              sliderInput("minGamedPerWeek","Minutes Gamed Per Week",value=60,min=0,max=150,step=1),
                              tableOutput("piLook")
                            ),
                            conditionalPanel(
                              condition="input.leaveInflation==true",
                              h3("Leave Inflation Reduction"),
                              sliderInput("PTOhours","Number of unreported PTO hours per year per employee",value=10,min=0,max=30,step=1),
                              sliderInput("kronosPTOEstimate","Kronos Estimate of Underreporting Employees",value=.25,min=0,max=1,step=.01),
                              tableOutput("leLook")
                            ),
                            conditionalPanel(
                              condition="input.absenteeism==true",
                              h3("Reduction in Unscheduled Absenteeism Costs"),
                              sliderInput("absenceRate","Average total costs of incidental unplanned absences(% of payroll)",value=.06,min=0,max=.2,step=.01),
                              sliderInput("kronosAbsenceImpact","Kronos Ability to Impact",value=.05,min=0,max=.25,step=.01),
                              tableOutput("abLook")
                            ),
                            conditionalPanel(
                              condition="input.calculationError==true",
                              h3("Reduction in Calculation Error"),
                              sliderInput("payrollErrorRate","Annual Payroll Error Rate",value=.012,min=0,max=.04,step=.001),
                              sliderInput("kronosCalcErrorImpact","Kronos Ability to Impact",value=.80,min=0,max=1,step=.01),
                              tableOutput("ceLook")
                            ),
                            actionButton("buildPB","Generate Document"),
                            hr()
                          )
                          
                        )
                      )
             ),
             tabPanel("ACA Brief",
                      fluidRow(
                        column(6,
                               textInput("ACAorgName","Organization Name:"),
                               textInput("subvertACA","Sub-Vertical:"),
                               textInput("date","Date:")),
                        column(6,
                               textInput("repName","Rep Name:"),
                               textInput("repEmail","Rep Email:"),
                               textInput("repNumber","Rep Number:")
                        )
                      ),
                      hr(),
                      numericInput("acaNumEETotal","Total Number of Employees",value=NULL),
                      numericInput("acaFTEETotal","Total FT",value=NULL),
                      numericInput("acaPTEETotal","Total PT",value=NULL),
                      numericInput("BenEx","Estimated Benefit Expense",value=10000),
                      numericInput("COLA","Estimated Cost of Living Adjustment",value=.02),
                      uiOutput("genACA")
             ),
             tabPanel("Financial Profile",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("company","Company Ticker",choices=NULL,selected=NULL,multiple=F,
                                         options = list(create = TRUE)),
                          selectizeInput('competitors',"Competitor Tickers",choices=NULL,selected=NULL,multiple=T,
                                         options = list(create = TRUE)),
                          numericInput("years","Years to Analyze",value=4,min=1),
                          selectizeInput('metricSet',"Metric Set",choices=
                                           c("Profitability","Employee Metrics"),
                                         selected=NULL,multiple=F),
                          uiOutput("metrics"),
                          actionButton("addToDR","Add Graph to Profile"),
                          actionButton("generateReport","Generate Profile")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          h3(textOutput("graphHeader")),
                          plotOutput("plot"),
                          textOutput("sentance1"),
                          br(),
                          textOutput("sentance2")
                        )
                      )
             )
  ))
