
shinyUI(bootstrapPage(
    # withMathJax(),
    # Application title
    br(),
    # img(src="logo.png", width="25%", align="right"),
    
    titlePanel("Confidence Interval",
               
    ),
    tabsetPanel(type = "tabs",
                
                tabPanel("Single Proportion",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 radioButtons(
                                     "singlePropTail","Select tails", 
                                     c("Left-tailed" = "left", "Two-tailed" = "two", "Right-tailed" = "right")),
                                 helpText("Confidence Level is between 0.5 to 1"),
                                 numericInput(
                                     inputId = "singlePropCL",
                                     label = "Confidence Level (\\(1-\\alpha\\)):", 
                                     value = 0.95, step = 0.01, min = 0.5, max = 1
                                 ),
                                 hr(),
                                 
                                 helpText("Use positive integers for sample size"),
                                 numericInput(
                                     inputId = "singlePropSampleSize",
                                     label = "Sample Size (\\(n\\)):", 
                                     value = 50, step = 1, min = 20
                                 ),
                                 br(),
                                 # hr(),
                                 helpText("Proportion is between 0 to 1"),
                                 numericInput(inputId = "singlePropSampleProp",
                                              label = "Sample Proportion (\\(\\hat{p}\\)):", 
                                              value = 0.5, step = 0.01, min = 0, max = 1
                                 ),
                                 br(),
                                 helpText("Null Hypothesis is between 0 to 1"),
                                 numericInput(
                                     inputId = "singlePropNull",
                                     label = "Null Hypothesis (\\(p_0\\)): ", 
                                     value = 0.5, step = 0.01, min = 0, max = 1
                                 ),
                                 hr(),
                                 HTML('<p>Report a bug or feedback to speedy.jiang@canterbury.ac.nz.</p>')
                             ),
                             mainPanel(
                                 width = 9,
                                 htmlOutput("single_Prop_condition"),
                                 conditionalPanel(
                                     condition = "input.singlePropSampleSize*input.singlePropSampleProp < 10 |
                                                        input.singlePropSampleSize*(1-input.singlePropSampleProp) < 10",
                                     br(),
                                     HTML(paste(tags$span(style="color:red", "\\(Condition\\ not\\ met!\\)")))
                                 ),
                                 conditionalPanel(
                                     condition = "input.singlePropSampleSize*input.singlePropSampleProp >= 10 &
                                                        input.singlePropSampleSize*(1-input.singlePropSampleProp) >= 10",
                                     br(),
                                     paste("\\(Condition\\ met!\\)"),
                                     hr(style="border-top :1px solid #FF0000"),
                                     helpText("Confidence Interval for \\(p\\)"),
                                     hr(),
                                     
                                     helpText("\\(Standard\\ Error: \\)"),
                                     htmlOutput("single_prop_se"),
                                     
                                     helpText("\\(z^{*}: \\)"),
                                     htmlOutput('single_prop_z'),
                                     
                                     helpText("\\(Confidence\\ Interval: \\)"),
                                     htmlOutput("single_prop_ci"),
                                     hr(style="border-top :1px solid #FF0000"),
                                     helpText("Hypothesis Test for \\(p\\)"),
                                     
                                     hr(),
                                     helpText("\\(Standard\\ Error\\ for\\ Hypothesis\\ Test: \\)"),
                                     htmlOutput("single_prop_test_se"),
                                     helpText("\\(Test\\ Statistics: \\)"),
                                     htmlOutput("single_prop_test_stat"),
                                     
                                     helpText("\\(p-value(6 dp): \\)"),
                                     htmlOutput("single_prop_pvalue")
                                 )
                             )
                         )
                ),
                
                #single mean page
                tabPanel("Single Mean",
                         sidebarLayout(
                             sidebarPanel(width = 3,
                                          radioButtons(
                                              "singleMeanTail","Select tails", 
                                              c("Left-tailed" = "left", 
                                                "Two-tailed" = "two", 
                                                "Right-tailed" = "right")),
                                          helpText("Confidence Level is between 0.5 to 1"),
                                          numericInput(
                                              inputId = "singleMeanCL",
                                              label = "Confidence Level (\\(1-\\alpha\\)):", 
                                              value = 0.95, step = 0.01, min = 0.5, max = 1
                                          ),
                                          hr(),
                                          numericInput(
                                              inputId = "singleMeanSampleMean",
                                              label = "Sample Mean (\\(\\bar{x}\\)):", 
                                              value = 0, step = 1
                                          ),
                                          br(),
                                          numericInput(
                                              inputId = "singleMeanSampleSd",
                                              label = "Sample Standard Deviation (\\(s\\)):", 
                                              value = 1, step = 1
                                          ),
                                          # br(),
                                          helpText("Use positive integers for sample size"),
                                          numericInput(
                                              inputId = "singleMeanSampleSize",
                                              label = "Sample Size (\\(n\\)):", 
                                              value = 50, step = 1
                                          ),
                                          
                                          br(),
                                          # hr(),
                                          # helpText("Null Hypothesis is between 0 to 1"),
                                          numericInput(
                                              inputId = "singleMeanNull",
                                              label = "Null Hypothesis (\\(\\mu\\)): ", 
                                              value = 5, step = 1
                                          ),
                                          hr(),
                                          HTML('<p>Report a bug or feedback to speedy.jiang@canterbury.ac.nz.</p>')
                                          
                             ),
                             mainPanel (
                                 htmlOutput("single_mean_condition"),
                                 br(),
                                 conditionalPanel(
                                     condition = "input.singleMeanSampleSize >= 30",
                                     paste("\\(n\\ is\\ big\\ enough,\\ CLT\\ applies.\\)")
                                 ),
                                 conditionalPanel(
                                     condition = "input.singleMeanSampleSize < 30",
                                     HTML(paste(tags$span(style="color:red", 
                                                          "\\(It's\\ a\\ small\\ sample,
                                                          \\ if\\ the\\ population\\ is\\ Normally\\ distributed:\\)")))
                                 ),
                                 hr(style="border-top :1px solid #FF0000"),
                                 helpText("Confidence Interval for \\(\\mu\\)"),
                                 hr(),
                                 helpText("\\(Standard\\ Error: \\)"),
                                 htmlOutput("single_mean_se"),
                                 
                                 helpText("\\(t^{*}-distribution: \\)"),
                                 htmlOutput('single_mean_t'),
                                 
                                 helpText("\\(Confidence\\ Interval: \\)"),
                                 htmlOutput("single_mean_ci"),
                                 hr(style="border-top :1px solid #FF0000"),
                                 helpText("Hypothesis Test for \\(\\mu\\)"),
                                 hr(),
                                 
                                 helpText("\\(Test\\ Statistics: \\)"),
                                 htmlOutput("single_mean_test_stat"),
                                 
                                 helpText("\\(p-value(6 dp): \\)"),
                                 htmlOutput("single_mean_pvalue")
                                 
                                 
                             )
                         )),
                tabPanel("Difference in Proportions",
                         sidebarLayout(
                             sidebarPanel(width = 3,
                                          radioButtons("propDiffTail","Select tails", 
                                                       c("Left-tailed" = "left", 
                                                         "Two-tailed" = "two", 
                                                         "Right-tailed" = "right")),
                                          helpText("Confidence Level is between 0.5 to 1"),
                                          numericInput(
                                              inputId = "propDiffCL",
                                              label = "Confidence Level (\\(1-\\alpha\\)):", 
                                              value = 0.95, step = 0.01, min = 0.5, max = 1
                                          ),
                                          hr(),
                                          
                                          helpText("Use positive integers for counts and sample sizes "),
                                          fluidRow(
                                              column(6, 
                                                     numericInput(
                                                         inputId = "propDiffCount1",
                                                         label = "Sample 1 count (\\(n_1\\hat{p}_{1}\\)):", 
                                                         value = 20, step = 1, min = 0
                                                     )),
                                              column(6,
                                                     numericInput(
                                                         inputId = "propDiffCount2",
                                                         label = "Sample 2 count (\\(n_2\\hat{p}_{2}\\)):", 
                                                         value = 30, step = 1, min = 0
                                                     ))
                                          ),
                                          fluidRow(
                                              column(6, 
                                                     numericInput(
                                                         inputId = "propDiffSampleSize1",
                                                         label = "Size for sample 1 (\\(n_{1}\\)):", 
                                                         value = 50, step = 1, min = 20
                                                     )),
                                              column(6, 
                                                     numericInput(
                                                         inputId = "propDiffSampleSize2",
                                                         label = "Size for sample 2 (\\(n_{2}\\)):", 
                                                         value = 50, step = 1, min = 20
                                                     ))
                                          ),
                                          
                                          br(),
                                          # hr(),
                                          # helpText("Proportion is between 0 to 1"),
                                          
                                          br(),
                                          helpText("Null Hypothesis is between 0 to 1"),
                                          numericInput(
                                              inputId = "propDiffNull",
                                              label = "Null Hypothesis (\\(p_1-p_2\\)): ", 
                                              value = 0, step = 0.01, min = 0, max = 1
                                          ),
                                          hr(),
                                          HTML('<p>Report a bug or feedback to speedy.jiang@canterbury.ac.nz.</p>')
                                          
                             ),
                             mainPanel(width = 9,
                                       htmlOutput("diff_prop_condition"),
                                       conditionalPanel(
                                           condition = "input.propDiffCount1 < 10 |
                                                        input.propDiffCount2 < 10 |
                                                        input.propDiffSampleSize1 - input.propDiffCount1 < 10 |
                                                        input.propDiffSampleSize2 - input.propDiffCount2 < 10
                                                        ",
                                           br(),
                                           HTML(paste(tags$span(style="color:red", "\\(Condition\\ not\\ met!\\)")))
                                       ),
                                       conditionalPanel(
                                           condition = "input.propDiffCount1 >= 10 &
                                                        input.propDiffCount2 >= 10 &
                                                        input.propDiffSampleSize1 - input.propDiffCount1 >= 10 &
                                                        input.propDiffSampleSize2 - input.propDiffCount2 >= 10",
                                           br(),
                                           paste("\\(Condition\\ met!\\)"),
                                           hr(style="border-top :1px solid #FF0000"),
                                           helpText("Confidence Interval for \\(\\hat{p_1}-\\hat{p_2}\\)"),
                                           hr(),
                                           helpText("\\(Standard\\ Error: \\)"),
                                           htmlOutput("prop_diff_se"),
                                           
                                           helpText("\\(z^{*}: \\)"),
                                           htmlOutput('prop_diff_z'),
                                           
                                           helpText("\\(Confidence\\ Interval: \\)"),
                                           htmlOutput("prop_diff_ci"),
                                           hr(style="border-top :1px solid #FF0000"),
                                           helpText("Hypothesis Test for \\(p_1-p_2\\)"),
                                           hr(),
                                           helpText("\\(Pooled\\ proportion\\ \\hat{p}\\ for\\ p_1\\ and\\ p_2: \\)"),
                                           htmlOutput("prop_diff_pool_prop"),
                                           
                                           helpText("\\(Standard\\ Error\\ with\\ pooled\\ proportion: \\)"),
                                           htmlOutput("pooled_se"),
                                           
                                           helpText("\\(Test\\ Statistics: \\)"),
                                           htmlOutput("prop_diff_test_stat"),
                                           
                                           helpText("\\(p-value(6 dp): \\)"),
                                           htmlOutput("prop_diff_pvalue")
                                       ),
                                       
                             )
                         )),
                tabPanel("Difference in means",
                         sidebarLayout(
                             sidebarPanel(width = 3,
                                          radioButtons(
                                              "diffMeanTail","Select tails",
                                              c("Left-tailed" = "left",
                                                "Two-tailed" = "two",
                                                "Right-tailed" = "right")),
                                          helpText("Confidence Level is between 0.5 to 1"),
                                          numericInput(
                                              inputId = "diffMeanCL",
                                              label = "Confidence Level (\\(1-\\alpha\\)):",
                                              value = 0.95, step = 0.01, min = 0.5, max = 1
                                          ),
                                          hr(),
                                          
                                          helpText("Use positive values for sample sizes and standard deviations"),
                                          fluidRow(# sample mean
                                              column(6, 
                                                     numericInput(
                                                         inputId = "diffMeanXabr1",
                                                         label = "Sample 1 mean (\\(\\bar{x_{1}}\\)):", 
                                                         value = -1, step = 1)
                                              ),
                                              column(6,
                                                     numericInput(
                                                         inputId = "diffMeanXbar2",
                                                         label = "Sample 2 mean (\\(\\bar{x_{2}}\\)):", 
                                                         value = 1, step = 1)
                                              )
                                          ),
                                          fluidRow(#sample sd
                                              column(6,
                                                     numericInput(
                                                         inputId = "diffMeanSd1",
                                                         label = "Sample 1 standard deviation(\\(s_{1}\\)):",
                                                         value = 1, step = 0.1, min = 0)
                                              ),
                                              column(6,
                                                     numericInput(
                                                         inputId = "diffMeanSd2",
                                                         label = "Sample 2 standard deviation(\\(s_{2}\\)):",
                                                         value = 1, step = 0.1, min = 0)
                                              )
                                          ),
                                          fluidRow(#sample size
                                              column(6,
                                                     numericInput(
                                                         inputId = "diffMeanSize1",
                                                         label = "Sample 1 sample size(\\(n_{1}\\)):",
                                                         value = 50, step = 1, min = 0)
                                              ),
                                              column(6,
                                                     numericInput(
                                                         inputId = "diffMeanSize2",
                                                         label = "Sample 2 samples zie(\\(n_{2}\\)):",
                                                         value = 50, step = 1, min = 0)
                                              )
                                          ),
                                          
                                          # hr(),
                                          helpText("Null Hypothesis is between 0 to 1"),
                                          numericInput(
                                              inputId = "diffMeanNull",
                                              label = "Null Hypothesis (\\(\\mu_{1}-\\mu_{2}\\)): ",
                                              value = 0, step = 1
                                          ),
                                          hr(),
                                          HTML('<p>Report a bug or feedback to speedy.jiang@canterbury.ac.nz.</p>')
                                          
                             ),
                             mainPanel (
                                 htmlOutput("diff_mean_condition"),
                                 br(),
                                 conditionalPanel(
                                     condition = "input.diffMeanSize1 >= 30 & input.diffMeanSize2 >= 30",
                                     paste("\\(Sample\\ sizes\\ are\\ big\\ enough,\\ CLT\\ applies.\\)")
                                 ),
                                 conditionalPanel(
                                     condition = "input.diffMeanSize1 < 30 | input.diffMeanSize2 < 30",
                                     HTML(paste(tags$span(style="color:red",
                                                          "\\(Sample\\ sizes\\ are\\ not\\ big\\ enough,\\ 
                                                          if\\ both\\ populations\\ are\\ Normally\\ distributed:\\)")))
                                 ),
                                 hr(style="border-top :1px solid #FF0000"),
                                 helpText("Confidence Interval for \\(\\mu_{1}-\\mu_{2}\\)"),
                                 hr(),
                                 helpText("\\(Standard\\ Error: \\)"),
                                 htmlOutput("diff_mean_se"),
                                 
                                 helpText("\\(t^{*}-distribution: \\)"),
                                 htmlOutput('diff_mean_t'),
                                 
                                 helpText("\\(Confidence\\ Interval: \\)"),
                                 htmlOutput("diff_mean_ci"),
                                 hr(style="border-top :1px solid #FF0000"),
                                 helpText("Hypothesis Test for \\(\\mu_{1}-\\mu_{2}\\)"),
                                 hr(),
                                 helpText("\\(Test\\ Statistics: \\)"),
                                 htmlOutput("diff_mean_test_stat"),
                                 
                                 helpText("\\(p-value(6 dp): \\)"),
                                 htmlOutput("diff_mean_pvalue")
                                 
                             )
                         )
                ),
                tabPanel("Paired Difference in Means",
                         sidebarLayout(
                             sidebarPanel(width = 3,
                                          radioButtons(
                                              "pairMeanTail","Select tails",
                                              c("Left-tailed" = "left",
                                                "Two-tailed" = "two",
                                                "Right-tailed" = "right")),
                                          helpText("Confidence Level is between 0.5 to 1"),
                                          numericInput(
                                              inputId = "pairMeanCL",
                                              label = "Confidence Level (\\(1-\\alpha\\)):",
                                              value = 0.95, step = 0.01, min = 0.5, max = 1
                                          ),
                                          hr(),
                                          numericInput(
                                              inputId = "pairMeanXbar",
                                              label = "Sample Mean (\\(\\bar{x_{d}}\\)):", 
                                              value = 0, step = 1
                                          ),
                                          br(),
                                          numericInput(
                                              inputId = "pairMeanSampleSd",
                                              label = "Sample Standard Deviation (\\(s_{d}\\)):", 
                                              value = 1, step = 1
                                          ),
                                          # br(),
                                          helpText("Use positive integers for sample size"),
                                          numericInput(
                                              inputId = "pairMeanSampleSize",
                                              label = "Sample Size (\\(n_{d}\\)):", 
                                              value = 50, step = 1
                                          ),
                                          
                                          br(),
                                          # hr(),
                                          helpText("Null Hypothesis is between 0 to 1"),
                                          numericInput(
                                              inputId = "pairMeanNull",
                                              label = "Null Hypothesis (\\(\\mu_{D}\\)): ", 
                                              value = 0, step = 1
                                          ),
                                          hr(),
                                          HTML('<p>Report a bug or feedback to speedy.jiang@canterbury.ac.nz.</p>')
                                          
                                          
                             ),
                             mainPanel (
                                 htmlOutput("paired_mean_condition"),
                                 br(),
                                 conditionalPanel(
                                     condition = "input.pairMeanSampleSize >= 30",
                                     paste("\\(Sample\\ size\\ is\\ big\\ enough,\\ CLT\\ applies.\\)")
                                 ),
                                 conditionalPanel(
                                     condition = "input.pairMeanSampleSize < 30",
                                     HTML(paste(tags$span(style="color:red",
                                                          "\\(Sample\\ size\\ is\\ not\\ big\\ enough,\\
                                                          if\\ the\\ population\\ of\\ differences\\ is\\ Normally\\ distributed:\\)")))
                                 ),
                                 hr(style="border-top :1px solid #FF0000"),
                                 helpText("Confidence Interval for \\(\\mu_{D}\\)"),
                                 hr(),
                                 helpText("\\(Standard\\ Error: \\)"),
                                 htmlOutput("paired_mean_se"),
                                 
                                 helpText("\\(t^{*}-distribution: \\)"),
                                 htmlOutput('pair_mean_t'),
                                 
                                 helpText("\\(Confidence\\ Interval: \\)"),
                                 htmlOutput("pair_mean_ci"),
                                 hr(style="border-top :1px solid #FF0000"),
                                 helpText("Hypothesis Test for \\(\\mu_{D}\\)"),
                                 hr(),
                                 helpText("\\(Test\\ Statistics: \\)"),
                                 htmlOutput("pair_mean_test_stat"),
                                 
                                 helpText("\\(p-value(6 dp): \\)"),
                                 htmlOutput("pair_mean_pvalue")
                                 
                             )
                         )
                )
    )
    
    
    
    
    
    

)
)
