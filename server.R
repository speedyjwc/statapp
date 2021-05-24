

shinyServer(function(input, output, session) {
    
    
    
    # for single prop page
    # 1.0 conditions
    output$single_Prop_condition <- renderUI({
        withMathJax(
            paste("Check conditions:",
                  sprintf("\\(np_0={%1.0f},\\ n(1-p_0)={%1.0f}\\)",
                          input$singlePropSampleSize*input$singlePropSampleProp,
                          input$singlePropSampleSize*(1-input$singlePropSampleProp)
                  )
            )
        )
        
    })
    
    # 1.1.1 single prop standard error
    output$single_prop_se <- renderUI({
        withMathJax(
            
            paste("\\(SE=\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}=\\)",
                  get_single_prop_se_output(input$singlePropSampleProp, input$singlePropSampleSize),
                  sprintf("\\(=\\fbox{%0.3f}\\)",get_single_prop_se(input$singlePropSampleProp, input$singlePropSampleSize)))
        )
    })
    # 1.1.2 single prop z*
    output$single_prop_z <- renderUI({
        withMathJax(
            # helpText("\\(z^{*}: \\)"),
            paste(sprintf("\\(z^{*}=\\fbox{%0.3f}\\)", 
                          qnorm((1-input$singlePropCL)/2,mean=0, sd=1,lower.tail = FALSE)),
                  sprintf("\\(\\ at\\ \\underline{\\underline{%0.1f}} \\)", input$singlePropCL*100),
                  "\\(\\%\\ confidence\\ level.\\)"
            )
        )
    })
    
    # 1.1.3 single prop confidence interval
    output$single_prop_ci <- renderUI({
        withMathJax(
            # helpText("\\(Confidence\\ Interval: \\)"),
            paste("\\(\\hat{p}\\pm z^{*} \\cdot \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}=\\)",
                  get_ci_output(sampleStat = input$singlePropSampleProp, 
                                criticalValue=qnorm((1-input$singlePropCL)/2,mean=0, sd=1,lower.tail = FALSE),
                                se = get_single_prop_se(input$singlePropSampleProp, input$singlePropSampleSize)),
                  sprintf("\\(\\fbox{(%0.3f, %0.3f)}\\)",
                          get_ci(sampleStat = input$singlePropSampleProp, 
                                 criticalValue=qnorm((1-input$singlePropCL)/2,mean=0, sd=1,lower.tail = FALSE),
                                 se = get_single_prop_se(input$singlePropSampleProp, input$singlePropSampleSize))[1],
                          get_ci(sampleStat = input$singlePropSampleProp, 
                                 criticalValue=qnorm((1-input$singlePropCL)/2,mean=0, sd=1,lower.tail = FALSE),
                                 se = get_single_prop_se(input$singlePropSampleProp, input$singlePropSampleSize))[2]
                  )
            )
        )
        
    })
    
    #1.2.1 se for single prop test stat
    output$single_prop_test_se <- renderUI({
        withMathJax(
            # helpText("\\(Standard\\ Error\\ for\\ Hypothesis\\ Test: \\)"),
            paste("\\(SE=\\sqrt{\\frac{p_{0}(1-p_{0})}{n}}=\\)",
                  get_single_prop_se_output(input$singlePropNull, input$singlePropSampleSize),
                  sprintf("\\(=\\fbox{%0.3f}\\)",get_single_prop_se(input$singlePropNull, input$singlePropSampleSize))
            )
            
        )
    })
    
    #1.2.2 test stat for single prop test
    output$single_prop_test_stat <- renderUI({
        withMathJax(
            #         helpText("\\(Test\\ Statistics: \\)"),
            paste("\\(Z=\\frac{\\hat{p}-p_{0}}{SE}\\)",
                  get_test_stat_output(sampleStat = input$singlePropSampleProp,
                                       nullValue = input$singlePropNull,
                                       se = get_single_prop_se(input$singlePropNull, input$singlePropSampleSize)),
                  
                  sprintf("\\(=\\fbox{%0.3f}\\)", get_test_stat(sampleStat = input$singlePropSampleProp,
                                                                nullValue = input$singlePropNull,
                                                                se = get_single_prop_se(input$singlePropNull, input$singlePropSampleSize)))
            )
        )
    })
    
    # 1.2.3 single prop p-value
    single_prop_test_stat <- reactive({
        testStat <- get_test_stat(sampleStat = input$singlePropSampleProp, 
                                  nullValue = input$singlePropNull,
                                  se = get_single_prop_se(input$singlePropNull, input$singlePropSampleSize))
        return(testStat)
    })
    
    
    singlePropPvalue <- reactive({
        if(input$singlePropTail=="left") {pvalue = pnorm(single_prop_test_stat(), mean=0,sd=1)}
        else if (input$singlePropTail=="right") {pvalue = 1- pnorm(single_prop_test_stat(), mean=0,sd=1)}
        else if (input$singlePropTail=="two") {pvalue = ifelse(pnorm(single_prop_test_stat(), mean=0,sd=1) < 0.5,
                                                               2 * pnorm(single_prop_test_stat(), mean=0,sd=1),
                                                               2-2 * pnorm(single_prop_test_stat(), mean=0,sd=1))}
        
        return (pvalue)
        
    })
    output$single_prop_pvalue <- renderUI({
        withMathJax(
            # helpText("\\(p-value(6 dp): \\)"),
            paste("\\(p-value: \\)",
                  sprintf("\\(\\fbox{%0.6f}\\)", singlePropPvalue())
            )
        )
    })
    
    # for single mean page
    # 2.0 single mean conditions
    output$single_mean_condition <- renderUI({
        withMathJax(
            paste("Check conditions:",
                  sprintf("\\(Sample\\ size\\ n = {%1.0f}.\\)",input$singleMeanSampleSize)
            )
        )
    })
    
    # 2.1.1 single mean sd 
    output$single_mean_se <- renderUI({
        withMathJax(
            # helpText("\\(Standard\\ Error: \\)"),
            paste("\\(SE=\\frac{s}{\\sqrt{n}}=\\)",
                  get_single_mean_se_output(input$singleMeanSampleSd,input$singleMeanSampleSize),
                  sprintf("\\(=\\fbox{%0.3f}\\)",get_single_mean_se(input$singleMeanSampleSd,input$singleMeanSampleSize))
            )
        )
    })
    # 2.1.2 single mean t*
    output$single_mean_t <- renderUI({
        withMathJax(
            # helpText("\\(t^{*}-distribution: \\)"),
            paste(sprintf("\\(t^{*}=\\fbox{%0.3f}\\)", 
                          qt((1-input$singleMeanCL)/2, 
                             input$singleMeanSampleSize-1,
                             lower.tail=FALSE)),
                  sprintf("\\(when\\ df = \\underline{\\underline{%1.0f}}\\)", input$singleMeanSampleSize-1),
                  sprintf("\\(\\ at\\ \\underline{\\underline{%0.1f}} \\)", input$singleMeanCL*100),
                  "\\(\\%\\ confidence\\ level.\\)"
            )
        )
    })
    # 2.1.3 single mean confidence interval
    output$single_mean_ci <- renderUI({
        withMathJax(
            # helpText("\\(Confidence\\ Interval: \\)"),
            paste("\\(\\bar{x}\\pm t^{*}\\cdot \\frac{s}{\\sqrt{n}}=\\)",
                  get_ci_output(sampleStat = input$singleMeanSampleMean,
                                criticalValue = qt((1-input$singleMeanCL)/2, 
                                                   input$singleMeanSampleSize-1,
                                                   lower.tail=FALSE),
                                se = get_single_mean_se(input$singleMeanSampleSd,input$singleMeanSampleSize)),
                  sprintf("\\(\\fbox{(%0.3f, %0.3f)}\\)",
                          get_ci(sampleStat = input$singleMeanSampleMean,
                                 criticalValue = qt((1-input$singleMeanCL)/2, 
                                                    input$singleMeanSampleSize-1,
                                                    lower.tail=FALSE),
                                 se = get_single_mean_se(input$singleMeanSampleSd,input$singleMeanSampleSize))[1],
                          get_ci(sampleStat = input$singleMeanSampleMean,
                                 criticalValue = qt((1-input$singleMeanCL)/2, 
                                                    input$singleMeanSampleSize-1,
                                                    lower.tail=FALSE),
                                 se = get_single_mean_se(input$singleMeanSampleSd,input$singleMeanSampleSize))[2]
                  )
            )
        )
        
    })
    
    # 2.2.1 single mean test stat
    output$single_mean_test_stat <- renderUI({
        withMathJax(
            # helpText("\\(Test\\ Statistics: \\)"),
            paste("\\(t=\\frac{\\bar{x}-\\mu _{0}}{s/\\sqrt{n}}\\)",
                  get_test_stat_output(sampleStat = input$singleMeanSampleMean,
                                       nullValue = input$singleMeanNull,
                                       se = get_single_mean_se(input$singleMeanSampleSd,input$singleMeanSampleSize)),
                  sprintf("\\(=\\fbox{%0.3f}\\)", get_test_stat(sampleStat = input$singleMeanSampleMean,
                                                                nullValue = input$singleMeanNull,
                                                                se = get_single_mean_se(input$singleMeanSampleSd,input$singleMeanSampleSize)))
            )
        )
    })
    
    # 2.2.2 single mean p-value
    singleMeanPvalue <- reactive({
        testStat <- get_test_stat(sampleStat = input$singleMeanSampleMean, 
                                  nullValue = input$singleMeanNull,
                                  se = get_single_mean_se(sd = input$singleMeanSampleSd, 
                                                          n = input$singleMeanSampleSize))
        
        if(input$singleMeanTail=="left") {pvalue = pt(q = testStat,df = input$singleMeanSampleSize-1)}
        else if (input$singleMeanTail=="right") {pvalue = 1-pt(q = testStat,df = input$singleMeanSampleSize-1)}
        else if (input$singleMeanTail=="two") {pvalue = ifelse(pt(q = testStat,df = input$singleMeanSampleSize-1) < 0.5,
                                                               2*pt(q = testStat,df = input$singleMeanSampleSize-1),
                                                               2-2*pt(q = testStat,df = input$singleMeanSampleSize-1))}
        
        return (pvalue)
        
    })
    output$single_mean_pvalue <- renderUI({
        withMathJax(
            # helpText("\\(p-value(6 dp): \\)"),
            paste("\\(p-value: \\)",
                  sprintf("\\(\\fbox{%0.6f}\\)", singleMeanPvalue())
            )
        )
    })
    
    # difference in proportion
    # 3.0 diff in prop condition
    output$diff_prop_condition <- renderUI({
        withMathJax(
            paste("Check conditions:",
                  sprintf("\\(n_1\\hat{p_1}={%1.0f},\\ n_1(1-\\hat{p_1})={%1.0f}\\)",
                          input$propDiffCount1,
                          input$propDiffSampleSize1-input$propDiffCount1
                  ),";",
                  sprintf("\\(n_2\\hat{p_2}={%1.0f},\\ n_2(1-\\hat{p_2})={%1.0f}\\)",
                          input$propDiffCount2,
                          input$propDiffSampleSize2-input$propDiffCount2
                  )
            )
        )
        
    })
    
    
    # 3.1.1 single prop standard error
    output$prop_diff_se <- renderUI({
        withMathJax(
            # helpText("\\(Standard\\ Error: \\)"),
            paste("\\(SE=\\sqrt{\\frac{\\hat{p_1}(1-\\hat{p_1})}{n_1}+\\frac{\\hat{p_2}(1-\\hat{p_2})}{n_2}}=\\)",
                  get_prop_diff_se_output(input$propDiffCount1, input$propDiffSampleSize1,input$propDiffCount2,input$propDiffSampleSize2),
                  sprintf("\\(=\\fbox{%0.3f}\\)",get_prop_diff_se(input$propDiffCount1, input$propDiffSampleSize1,input$propDiffCount2,input$propDiffSampleSize2))
            )
        )
    })
    # 3.1.2 diff in prop z*
    output$prop_diff_z <- renderUI({
        withMathJax(
            # helpText("\\(z^{*}: \\)"),
            paste(sprintf("\\(z^{*}=\\fbox{%0.3f}\\)",
                          qnorm((1-input$propDiffCL)/2,mean=0, sd=1,lower.tail = FALSE)),
                  sprintf("\\(\\ at\\ \\underline{\\underline{%0.1f}} \\)", input$propDiffCL*100),
                  "\\(\\%\\ confidence\\ level.\\)"
            )
        )
    })
    
    # 3.1.3 diff in prop confidence interval
    output$prop_diff_ci <- renderUI({
        withMathJax(
            # helpText("\\(Confidence\\ Interval: \\)"),
            paste("\\(\\hat{p_{1}}-\\hat{p_{2}}\\pm z^{*} \\cdot 
                  \\sqrt{\\frac{\\hat{p_1}(1-\\hat{p_1})}{n_1}+\\frac{\\hat{p_2}(1-\\hat{p_2})}{n_2}}=\\)",
                  get_ci_output(sampleStat = input$propDiffCount1/input$propDiffSampleSize1-
                                    input$propDiffCount2/input$propDiffSampleSize2,
                                criticalValue=qnorm((1-input$propDiffCL)/2,mean=0, sd=1,lower.tail = FALSE),
                                se = get_prop_diff_se(input$propDiffCount1, input$propDiffSampleSize1,
                                                      input$propDiffCount2,input$propDiffSampleSize2)),
                  sprintf("\\(\\fbox{(%0.3f, %0.3f)}\\)",
                          get_ci(sampleStat = input$propDiffCount1/input$propDiffSampleSize1-
                                     input$propDiffCount2/input$propDiffSampleSize2,
                                 criticalValue=qnorm((1-input$propDiffCL)/2,mean=0, sd=1,lower.tail = FALSE),
                                 se = get_prop_diff_se(input$propDiffCount1, input$propDiffSampleSize1,
                                                       input$propDiffCount2,input$propDiffSampleSize2))[1],
                          get_ci(sampleStat = input$propDiffCount1/input$propDiffSampleSize1-
                                     input$propDiffCount2/input$propDiffSampleSize2,
                                 criticalValue=qnorm((1-input$propDiffCL)/2,mean=0, sd=1,lower.tail = FALSE),
                                 se = get_prop_diff_se(input$propDiffCount1, input$propDiffSampleSize1,
                                                       input$propDiffCount2,input$propDiffSampleSize2))[2]
                  )
            )
        )
        
    })
    
    
    # 3.2.1 pooled proportion
    pooled_prop <- reactive({
        p = get_prop_diff_pool_prop(
            input$propDiffCount1,
            input$propDiffSampleSize1,
            input$propDiffCount2,
            input$propDiffSampleSize2)
        return (p)
    })
    
    output$prop_diff_pool_prop <- renderUI({
        withMathJax(
            # helpText("\\(Pooled\\ proportion\\ \\hat{p}\\ for\\ p_1\\ and\\ p_2: \\)"),
            paste0("\\(\\hat{p}=\\frac{n_{1}\\hat{p_{1}}+n_{2}\\hat{p_{2}}}{n_{1}+n_{2}}=\\)",
                   sprintf("\\(\\frac{ %1.0f + %1.0f}{%1.0f + %1.0f}=\\)",
                           input$propDiffCount1, input$propDiffCount2,
                           input$propDiffSampleSize1,input$propDiffSampleSize2),
                   sprintf("\\(\\fbox{%0.3f}\\)", pooled_prop()
                   )
            )
        )
    })
    
    #3.2.2 se with pooled prop
    pooled_se <- reactive({
        se = get_pool_se(p = pooled_prop(), n1 = input$propDiffSampleSize1, n2= input$propDiffSampleSize2)
    })
    
    output$pooled_se <- renderUI({
        withMathJax(
            # helpText("\\(Standard\\ Error\\ with\\ pooled\\ proportion: \\)"),
            paste("\\(SE=\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n_1}+\\frac{\\hat{p}(1-\\hat{p})}{n_2}}=\\)",
                  sprintf("\\(\\sqrt{\\frac{ %0.3f(1- %0.3f)}{%1.0f}+\\frac{ %0.3f(1- %0.3f)}{%1.0f}}=\\)",
                          pooled_prop(),pooled_prop(),input$propDiffSampleSize1, 
                          pooled_prop(),pooled_prop(),input$propDiffSampleSize2),
                  sprintf("\\(\\fbox{%0.3f}\\)",pooled_se())
            )
        )
    })
    
    # 3.2.3 diff in prop test stat
    output$prop_diff_test_stat <- renderUI({
        withMathJax(
            # helpText("\\(Test\\ Statistics: \\)"),
            paste("\\(Z=\\frac{\\hat{p_{1}}-\\hat{p_{2}}-0}{SE}\\)",
                  get_test_stat_output(sampleStat = input$propDiffCount1/input$propDiffSampleSize1-
                                           input$propDiffCount2/input$propDiffSampleSize2,
                                       nullValue = input$propDiffNull,
                                       se = pooled_se()),
                  
                  sprintf("\\(=\\fbox{%0.3f}\\)", get_test_stat(sampleStat = input$propDiffCount1/input$propDiffSampleSize1-
                                                                    input$propDiffCount2/input$propDiffSampleSize2,
                                                                nullValue = input$propDiffNull,
                                                                se = pooled_se())
                  )
            )
        )
    })
    
    #3.2.4 diff in prop p-value
    prop_diff_test_stat <- reactive({
        testStat <- get_test_stat(sampleStat = input$propDiffCount1/input$propDiffSampleSize1-
                                      input$propDiffCount2/input$propDiffSampleSize2,
                                  nullValue = input$propDiffNull,
                                  se = pooled_se())
        return(testStat)
    })
    
    
    propDiffPvalue <- reactive({
        if(input$propDiffTail=="left") {pvalue = pnorm(prop_diff_test_stat(), mean=0,sd=1)}
        else if (input$propDiffTail=="right") {pvalue = 1- pnorm(prop_diff_test_stat(), mean=0,sd=1)}
        else if (input$propDiffTail=="two") {pvalue = ifelse(pnorm(prop_diff_test_stat(), mean=0,sd=1) < 0.5,
                                                             2*pnorm(prop_diff_test_stat(), mean=0,sd=1),
                                                             2-2*pnorm(prop_diff_test_stat(), mean=0,sd=1))}
        
        return (pvalue)
        
    })
    output$prop_diff_pvalue <- renderUI({
        withMathJax(
            # helpText("\\(p-value(6 dp): \\)"),
            paste("\\(p-value: \\)",
                  sprintf("\\(\\fbox{%0.6f}\\)", propDiffPvalue())
            )
        )
    })
    
    # 4.0 diff in mean conditions
    output$diff_mean_condition <- renderUI({
        withMathJax(
            paste("Check conditions:",
                  sprintf("\\(Sample\\ size\\ n_{1} = {%1.0f},\\)",input$diffMeanSize1),
                  sprintf("\\(sample\\ size\\ n_{2} = {%1.0f}.\\)",input$diffMeanSize2)
            )
        )
    })
    
    # 4.1.1 diff in mean sd
    diff_mean_se <- reactive({
        se <- get_diff_mean_se(sd1 = input$diffMeanSd1, n1 = input$diffMeanSize1, 
                               sd2 = input$diffMeanSd2, n2 = input$diffMeanSize2)
        return(se)
    })
    
    output$diff_mean_se <- renderUI({
        withMathJax(
            
            paste("\\(SE=\\sqrt{\\frac{s_{1}^{2}}{n_{1}}+\\frac{s_{2}^{2}}{n_{2}}}=\\)",
                  get_diff_mean_se_output(sd1 = input$diffMeanSd1, n1 = input$diffMeanSize1, 
                                          sd2 = input$diffMeanSd2, n2 = input$diffMeanSize2),
                  sprintf("\\(=\\fbox{%0.3f}\\)",diff_mean_se())
            )
        )
    })
    
    # 4.1.2 diff in mean t*
    output$diff_mean_t <- renderUI({
        withMathJax(
            # helpText("\\(t^{*}-distribution: \\)"),
            paste(sprintf("\\(t^{*}=\\fbox{%0.3f}\\)",
                          qt((1-input$diffMeanCL)/2,
                             min(input$diffMeanSize1,input$diffMeanSize2)-1,
                             lower.tail=FALSE)),
                  sprintf("\\(when\\ df = \\underline{\\underline{%1.0f}}\\)", 
                          min(input$diffMeanSize1,input$diffMeanSize2)-1),
                  sprintf("\\(\\ at\\ \\underline{\\underline{%0.1f}} \\)", input$diffMeanCL*100),
                  "\\(\\%\\ confidence\\ level.\\)"
            )
        )
    })
    
    # 4.1.3 diff in means confidence interval
    diff_mean_ci <- reactive({
        ci <- get_ci(sampleStat = input$diffMeanXabr1-input$diffMeanXbar2,
                     criticalValue = qt((1-input$diffMeanCL)/2,
                                        min(input$diffMeanSize1,input$diffMeanSize2)-1,
                                        lower.tail=FALSE),
                     se = diff_mean_se())
        return(ci)
    })
    output$diff_mean_ci <- renderUI({
        withMathJax(
            # helpText("\\(Confidence\\ Interval: \\)"),
            paste("\\(\\bar{x_{1}}-\\bar{x_{2}}\\pm t^{*}\\cdot SE=\\)",
                  get_ci_output(sampleStat = input$diffMeanXabr1-input$diffMeanXbar2,
                                criticalValue = qt((1-input$diffMeanCL)/2,
                                                   min(input$diffMeanSize1,input$diffMeanSize2)-1,
                                                   lower.tail=FALSE),
                                se = diff_mean_se()),
                  sprintf("\\(\\fbox{(%0.3f, %0.3f)}\\)",
                          diff_mean_ci()[1],
                          diff_mean_ci()[2] )
            )
        )
        
    })
    
    # 4.2.1 diff in means test stat
    output$diff_mean_test_stat <- renderUI({
        withMathJax(
            # helpText("\\(Test\\ Statistics: \\)"),
            paste("\\(t=\\frac{(\\bar{x_{1}}-\\bar{x_{2}})-null}{\\sqrt{\\frac{s_{1}^{2}}{n_{1}}+\\frac{s_{2}^{2}}{n_{2}}}}\\)",
                  get_test_stat_output(sampleStat = input$diffMeanXabr1-input$diffMeanXbar2,
                                       nullValue = input$diffMeanNull,
                                       se = diff_mean_se()),
                  sprintf("\\(=\\fbox{%0.3f}\\)", get_test_stat(sampleStat = input$diffMeanXabr1-input$diffMeanXbar2,
                                                                nullValue = input$diffMeanNull,
                                                                se = diff_mean_se()))
            )
        )
    })
    
    # 4.2.2 diff in mean p-value
    diffMeanPvalue <- reactive({
        testStat <- get_test_stat(sampleStat = input$diffMeanXabr1-input$diffMeanXbar2,
                                         nullValue = input$diffMeanNull,
                                         se = diff_mean_se())
        
        if(input$diffMeanTail=="left") {pvalue = pt(q = testStat,df = min(input$diffMeanSize1,input$diffMeanSize2)-1)}
        else if (input$diffMeanTail=="right") {pvalue = 1-pt(q = testStat,df = min(input$diffMeanSize1,input$diffMeanSize2)-1)}
        else if (input$diffMeanTail=="two") {pvalue = ifelse(pt(q = testStat,df = min(input$diffMeanSize1,input$diffMeanSize2)-1) < 0.5,
                                                       2*2*pt(q = testStat,df = min(input$diffMeanSize1,input$diffMeanSize2)-1),
                                                       2-2*pt(q = testStat,df = min(input$diffMeanSize1,input$diffMeanSize2)-1))}
        
        return (pvalue)
        
    })
    output$diff_mean_pvalue <- renderUI({
        withMathJax(
            # helpText("\\(p-value(6 dp): \\)"),
            paste("\\(p-value: \\)",
                  sprintf("\\(\\fbox{%0.6f}\\)", diffMeanPvalue())
            )
        )
    })
    
    # 5.0 paired mean conditions
    output$paired_mean_condition <- renderUI({
        withMathJax(
            paste("Check conditions:",
                  sprintf("\\(Sample\\ size\\ n = {%1.0f}.\\)",input$pairMeanSampleSize)
            )
        )
    })

    # 5.1.1 paired mean sd
    output$paired_mean_se <- renderUI({
        withMathJax(
            # helpText("\\(Standard\\ Error: \\)"),
            paste("\\(SE=\\frac{s_{D}}{\\sqrt{n_{D}}}=\\)",
                  get_single_mean_se_output(input$pairMeanSampleSd,input$pairMeanSampleSize),
                  sprintf("\\(=\\fbox{%0.3f}\\)",get_single_mean_se(input$pairMeanSampleSd,input$pairMeanSampleSize))
            )
        )
    })
    # 5.1.2 single mean t*
    output$pair_mean_t <- renderUI({
        withMathJax(
            # helpText("\\(t^{*}-distribution: \\)"),
            paste(sprintf("\\(t^{*}=\\fbox{%0.3f}\\)",
                          qt((1-input$pairMeanCL)/2,
                             input$pairMeanSampleSize-1,
                             lower.tail=FALSE)),
                  sprintf("\\(when\\ df = \\underline{\\underline{%1.0f}}\\)", input$pairMeanSampleSize-1),
                  sprintf("\\(\\ at\\ \\underline{\\underline{%0.1f}} \\)", input$pairMeanCL*100),
                  "\\(\\%\\ confidence\\ level.\\)"
            )
        )
    })
    # 5.1.3 single mean confidence interval
    paired_mean_se <- reactive({
        se = get_single_mean_se(input$pairMeanSampleSd,input$pairMeanSampleSize)
        return(se)
    })
    output$pair_mean_ci <- renderUI({
        withMathJax(
            # helpText("\\(Confidence\\ Interval: \\)"),
            paste("\\(\\bar{x_{D}}\\pm t^{*}\\cdot \\frac{s_{D}}{\\sqrt{n_{D}}}=\\)",
                  get_ci_output(sampleStat = input$pairMeanXbar,
                                criticalValue = qt((1-input$pairMeanCL)/2,
                                                   input$pairMeanSampleSize-1,
                                                   lower.tail=FALSE),
                                se = paired_mean_se()),
                  sprintf("\\(\\fbox{(%0.3f, %0.3f)}\\)",
                          get_ci(sampleStat = input$pairMeanXbar,
                                 criticalValue = qt((1-input$pairMeanCL)/2,
                                                    input$pairMeanSampleSize-1,
                                                    lower.tail=FALSE),
                                 se =  paired_mean_se())[1],
                          get_ci(sampleStat = input$pairMeanXbar,
                                 criticalValue = qt((1-input$pairMeanCL)/2,
                                                    input$pairMeanSampleSize-1,
                                                    lower.tail=FALSE),
                                 se =  paired_mean_se())[2]
                  )
            )
        )

    })

    # 5.2.1 pair mean test stat
    output$pair_mean_test_stat <- renderUI({
        withMathJax(
            # helpText("\\(Test\\ Statistics: \\)"),
            paste("\\(t=\\frac{\\bar{x_{D}}-\\mu_{D}}{s_{D}/\\sqrt{n_{D}}}\\)",
                  get_test_stat_output(sampleStat = input$pairMeanXbar,
                                       nullValue = input$pairMeanNull,
                                       se = get_single_mean_se(input$pairMeanSampleSd,input$pairMeanSampleSize)),
                  sprintf("\\(=\\fbox{%0.3f}\\)", get_test_stat(sampleStat = input$pairMeanXbar,
                                                                nullValue = input$pairMeanNull,
                                                                se = paired_mean_se()))
            )
        )
    })
    # 
    # 5.2.2 single mean p-value
    pairMeanPvalue <- reactive({
        testStat <- get_test_stat(sampleStat = input$pairMeanXbar,
                                  nullValue = input$pairMeanNull,
                                  se = paired_mean_se())

        if(input$pairMeanTail=="left") {pvalue = pt(q = testStat,df = input$pairMeanSampleSize-1)}
        else if (input$pairMeanTail=="right") {pvalue = 1-pt(q = testStat,df = input$pairMeanSampleSize-1)}
        else if (input$pairMeanTail=="two") {pvalue = ifelse(pt(q = testStat,df = input$pairMeanSampleSize-1) < 0.5,
                                                               2*pt(q = testStat,df = input$pairMeanSampleSize-1),
                                                               2-2*pt(q = testStat,df = input$pairMeanSampleSize-1))}

        return (pvalue)

    })
    output$pair_mean_pvalue <- renderUI({
        withMathJax(
            # helpText("\\(p-value(6 dp): \\)"),
            paste("\\(p-value: \\)",
                  sprintf("\\(\\fbox{%0.6f}\\)", pairMeanPvalue())
            )
        )
    })
    
    

    
    
    
})
