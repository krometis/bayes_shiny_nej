#load analysis libraries, functions, and data
source("lib/reliability_header.r")
#source("reliability_functions.r")

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Reliability Estimation"),
  theme = shinytheme("cerulean"),
  navbarPage(
    "Select Tab",
    
    # Overview Tab
    tabPanel("Overview",
             fluidRow(
               column(width = 8,
                      h1("Analysis Overview"),
                      h2("Introduction"),
                      div(img(src = 'stryker_icv.png', height = '200px', width = '300px')),
                      p(" "),
                      p("Welcome to the R Shiny Reliability Estimation app! This app shows the steps and results of a Bayesian reliability analysis via informative priors using developmental and operational test data from a user-uploaded CSV. Below, you will find descriptions of each tab's function and significance."),
                      h2("Analysis Methods and Assumptions"),
                      p("In this analysis, we assume that each variant's reliability metric follows a Weibull distribution. The shape parameter (which influences the shape of the distribution) is assumed to be the same for each variant, while each variant's Weibull has its own unique scale parameter (which influences the spread/width of the distribution). Therefore, we perform inference on the shape parameter (common to all variants) and each variant's respective scale parameter (specific to each variant). In this context, a 'variant' refers to a specific version or model of the system that has been adapted or modified for a particular purpose or role. If your system does not have multiple variants, just fill the 'Variant' column with the same value for each observation."),
                      h2("Tab 1: Dataset"),
                      p("This tab shows the distribution of the reliability observations from each test phase and type of system failure (if specified). Use the dropdown menu to select a specific variant's reliability metric distribution to view."),
                      h2("Tab 2: Prior Parameters"),
                      p("Here, you can view the prior distributions on the shape and scale parameters of the Weibull distributions that the variants' reliability distributions are assumed to follow. The shape and scale parameters both have gamma priors. Choose appropriate ranges for the sliders to be, and use them to change the mean and standard deviation of the gamma priors as desired. You can view the reliability distribution implied by your modified priors in the right-most histogram plot. Once you have chosen the priors that you wish to use for the analysis, click the 'Select these Priors' button."),
                      h2("Tab 3: DT Inference"),
                      p("In this tab, you can view and confirm the prior parameters that you have chosen, and click the 'Run DT Analysis with Selected Priors' button to perform DT inference using your custom priors. The analysis will take a couple minutes to complete. Once the analysis is complete, you can view the resulting shape and scale parameter estimates. Use the dropdown menu to select a specific variant's scale parameter estimates to view."),
                      h2("Tab 4: Transfer DT to OT"),
                      p("This tab shows the results of fitting distributions to the DT inference results. The shape parameter inference results were fitted to a normal distribution while the scale parameters were fitted to a gamma distribution. You can observe the closeness of the fit by comparing the theoretical density, CDF, Q-Q, and P-P plots to the observed ones (observed are in black, theoretical are in red). The closer the observed results are to the theoretical results, the better the fit."),
                      h2("Tab 5: OT Inference"),
                      p("Finally, this tab runs the OT inference and plots the final estimates for each parameter and each variant's resulting reliability estimate. You will see the DT and OT estimates for each parameter, as well as the estimated reliability distribution for each vehicle from each analysis phase. Below the plots, you can also see an estimate of the probability of the selected variant's mean reliability being above the user-specified target threshold."),
                      h1("Analysis Requirements/Instructions"),
                      p("In order for the app to perform the analysis properly, the uploaded CSV must follow a few specific criteria:"),
                      p("1. The CSV must have a column that contains the numerical reliability data for each observation."),
                      p("2. The CSV must have a column named 'TestPhase' which can take on one of two string values, 'DT' or 'OT', which indicates whether each observation is from the developmental test phase or the operational test phase of the system."),
                      p("3. The CSV must have a column named 'Variant' which contains a string value that indicates which system variant was tested for each observation. *Note*: if you are testing a system that does not have different variants, just create a 'Variant' column that has the same string value for each observation."),
                      p("4. If you wish to account for any right censored observations in you data, you must include a column named 'is_cen' with values 0 or 1 to indicate that the data point was not right censored, or was right censored, respectively. If you do not wish to account for right censored data in your analysis, then you do not need to change or add anything to your data."),
                      p("5. There cannot be any missing or invalid data in these columns.")
               
                    )
             )
    ),
    
    # First tab
    tabPanel("Dataset",
             h1("Reliability Metric Distributions"),
             p('This section plots histograms to visualize the distributions of the reliability metric of interest for each system variant, grouped by test phase.'),
             p("Upload your CSV by clicking the 'Browse...' button below. Make sure it meets the criteria listed in the 'Overview' tab."),
             p("After you have uploaded your CSV, use the dropdown menu below the file upload to specify the column that contains the numerical reliability data."),
             p("Next, press 'Confirm Selection' to  view histograms of your data."),
             fluidRow(
               column(width = 3, fileInput("file_upload", "Upload Reliability Data CSV", accept = ".csv"),
                      uiOutput("reliability_dropdown"),
                      #textInput("reliability_column", "Reliability Data Column Name:", ""),
                      #checkboxInput("censored_data", "Data indicates right-censored observations"),
                      actionButton("confirm", "Confirm Selection"),
                      uiOutput("variant_dropdown1"))
             ),
             fluidRow(
               column(width = 6, plotOutput("plot1", width = '600px', height = '450px')),
             ),
    ),
    
    # Second tab
    tabPanel("Prior Parameters",
             h1("Priors and Implied Mean Reliability"),
             p("In this tab, you can explore the impact of adjusting the mean and standard deviation of the gamma priors on the scale and shape parameters."),
             p("Use the text fields below to specify the minimum value and maximum value for the Scale Parameter Mean and Scale Parameter Standard Deviation values that you would like to explore. Press the 'Create Scale Parameter Mean and Std Sliders' button to create the sliders."),
             p("Once you have adjusted the prior parameters as you wish, click the 'Select these Priors' button and proceed to the next tab for DT inference."),
             fluidRow(
               column(width = 3,
                      sliderInput("slider1", "Shape Parameter Mean", min = 0.1, max = 5, value = 1),
                      sliderInput("slider2", "Shape Parameter Std", min = 0.1, max = 5, value = 0.5),
                      sliderInput("slider3", "Scale Parameter Mean", min = 0.1, max = 1, value = 0.1),
                      sliderInput("slider4", "Scale Parameter Std", min = 0.1, max = 1, value = 0.1),
                      
                      actionButton("select_priors", "Select these Priors"),
                      
                      h3("Advanced Settings"),
                      numericInput("eta_min", "Minimum Value of Scale Parameter Sliders", value = NULL),
                      numericInput("eta_max", "Maximum Value of Scale Parameter Sliders", value = NULL),
                      p(HTML("<b>*Note*: parameter inputs must be strictly greater than zero.</b>")),
                      actionButton("eta_slider", "Create Scale Parameter Mean and Std Sliders"),
                      numericInput("xmax", "Maximum Value of X Axis for Implied MMBSA Plot (0.95 quantile by default)",
                                   value = NULL),
               ),
               column(width = 2,
                      plotOutput("plot2", width = '850px', height = '600px'),
                      
               )
             ),
             
    ),
    
    # Third tab
    tabPanel("DT Inference",
             h1("DT Inference Results"),
             p("Here you can run the DT inference with the prior parameters you selected in the previous tab and see the results."),
             p("Click the 'Run DT Analysis with Selected Priors' button below to perform the inference."),
             fluidRow(
               column(width = 4,
                      verbatimTextOutput("prior_parameters"),
                      actionButton("dt_inference", "Run DT Analysis with Selected Priors"),
                      h1(" "),
                      uiOutput("variant_dropdown3"),
                      
                      column(width = 6, plotOutput("plot3", width = '450px'))
               ),
               
             ),
    ),
    # Fourth tab
    tabPanel("Transfer DT to OT",
             h1("Fitting Priors to Results from DT Inference"),
             p("This tab shows the results of fitting gamma distributions to the DT inference results. These fitted gamma distributions will be used as the priors for the OT inference in the next tab."),
             fluidRow(
               column(width = 6, uiOutput("variant_dropdown4"), plotOutput("plot4", width = '700px'))
             ),
             
             
    ),
    # Fifth tab
    tabPanel("OT Inference",
             h1("Results of the Posterior Inference on Shape and Scale Parameters"),
             p('In this tab, you can run the OT inference using the DT inference fitting results from the previous tab as priors.'),
             p('Use the slider to downweight the influence of the DT data as desired. A value of 1 means there will be no downweighting of the influence of DT data. A higher value means DT data has less impact on the inference results. Next, type a numerical value that represents your desired average reliability for the system into the text box below. Then click "Run OT Analysis" to start inference and see the results.'),
             p("You can also view the OT inference results and mean reliability estimates for each vehicle."),
             
             fluidRow(
               column(width = 6,
                      sliderInput("parameter", "DT Downweight Parameter", min = 1, max = 5, value = 1.5, step = 0.1, width = "50%"),
                      numericInput("threshold", "Targeted Average Reliability", value = NULL),
                      actionButton("ot_inference", "Run OT Analysis"),
                      uiOutput("variant_dropdown5"),
                      plotOutput("plot5", width = '1000px')
               ),
               
             ),

             
    ),
    # Download tab
    tabPanel("Download Results",
             h1("Results of the Posterior Inference on Shape and Scale Parameters"),
             p('In this tab, you can download .RData files containing the DT and OT sampling results generated by the analysis.'),
             h2("DT Results"),
             p("Click the buttons below to download the sampling results from the DT inference"),
             fluidRow(
               downloadButton("downloadDTsampling", "Download DT Sampling Results as RData"),
               downloadButton("downloadDTsamplingCSV", "Download DT Sampling Results as CSV"),
               
               # downloadButton("downloadDTplots", "Download DT Plots"),
             ),
             h2("OT Results"),
             p("Click the buttons below to download the sampling results from the OT inference"),
             fluidRow(
               downloadButton("downloadOTsampling", "Download OT Sampling Results as RData"),
               downloadButton("downloadOTsamplingCSV", "Download DT Sampling Results as CSV"),
               
               # downloadButton("downloadOTplots", "Download OT Plots"),
             ),
             
             
    ),
  ) 
  
)



# Define server logic
server <- function(input, output, session) {
  
  rv <- reactiveValues(data = NULL, nVariants = NULL, name = NULL,
                       threshold = NULL, variantNames = NULL)
  
  observeEvent(input$file_upload, {
    df <- read.csv(input$file_upload$datapath)
    
    output$reliability_dropdown <- renderUI({
      selectInput("reliability_column", "Select the Column that Contains the Reliability Data:",
                  choices = colnames(df))
    })
    

  })
  
  observeEvent(input$confirm, {
    
    output$plot1 <- renderPlot({
      NULL
    })
    
    output$plot2 <- renderPlot({
      NULL
    })
    
    output$plot3 <- renderPlot({
      NULL
    })
    
    output$plot4 <- renderPlot({
      NULL
    })
    
    output$plot5 <- renderPlot({
      NULL
    })
    
    if (is.null(input$file_upload) || is.null(input$reliability_column)){
      showNotification("You must input both a CSV data file and the reliability column name.",
                       id = "input_warning",
                       duration = 5, closeButton = FALSE, type = "error")
    } else {
      
      req(input$file_upload)
      req(input$reliability_column)
      df <- read.csv(input$file_upload$datapath)
      data = df[df[,input$reliability_column]!=0,]
      
      if('is_cen' %in% colnames(data)){
        censored_data = TRUE
      }else{
        censored_data = FALSE
        data$is_cen = 0
      }

      if(nrow(df) != nrow(data)){
        showNotification("Your reliability data contains observations where the reliability metric is zero. Zero values cause an error in the sampling algorithm used for the DT and OT inference. You may proceed with the analysis, but the rows with zeroes will be ignored. You may also edit the CSV to replace these observations with sensible values as you see fit, and reupload the updated CSV.",
                         id = "zero_warning", duration = NULL,
                         closeButton = TRUE, type = "warning")
      }
        
      data$VehicleFactor = as.numeric(factor(data$Variant))
      data$Variant = as.character(data$Variant)
      
      rv$data = data
      rv$nVariants = length(unique(data$Variant))
      rv$name = input$reliability_column
      rv$variantNames = sort(unique(data$Variant))
      data10quantile = round(as.numeric(quantile(rv$data[,rv$name], 0.1)), 2)
      maxData = round(max(rv$data[,rv$name]), 2)

      
      # Create scale parameter sliders for second tab based on data
      updateSliderInput(
        session,
        "slider3",
        min = ifelse(data10quantile>0, data10quantile, 0.1),
        max = maxData,
        value = (data10quantile + maxData)/2)
      updateSliderInput(
        session,
        "slider4",
        min = ifelse(data10quantile>0, data10quantile, 0.1),
        max = maxData,
        value = (data10quantile + maxData)/4)
      
      # Render the selected plot based on user input
      output$plot2 <- renderPlot({
        req(rv$data)
        
        # Change mean and std of prior parameters based on slider input
        beta_mean <- as.numeric(input$slider1)
        beta_std <- as.numeric(input$slider2)
        eta_mean <- as.numeric(input$slider3)
        eta_std <- as.numeric(input$slider4)
        
        pr.params = priorParameters(beta.mean.pr=beta_mean, beta.std.pr=beta_std,
                                    eta.mean.pr=eta_mean, eta.std.pr=eta_std)
        
        user.input = list(beta_mean = beta_mean,
                          beta_std = beta_std,
                          eta_mean = eta_mean,
                          eta_std = eta_std)
        
        pr.params.reactive(user.input)
        
        beta.pr.shape = pr.params$betashape
        beta.pr.rate = pr.params$betarate
        eta.pr.shape = pr.params$etashape
        eta.pr.rate = pr.params$etarate
        
        # Generate the plot using the slider values
        imp.MMBSA <- impliedMmbsa(n = 10000, pr.params)
        imp.MMBSA = data.frame(values = imp.MMBSA)
        
        xmax = ifelse(is.na(input$xmax), as.numeric(quantile(imp.MMBSA$values, 0.95)), input$xmax)
        
        p = ggplot(imp.MMBSA, aes(x = values)) +
          geom_density(fill = "black", color = "black", alpha = 0.3) +
          ggtitle(paste('Implied Mean',rv$name,'Distribution')) +
          xlab(paste('Mean', rv$name)) +
          ylab("Density") + xlim(c(min(imp.MMBSA), xmax))
        
        plot.x = seq(0,eta_mean*2, length.out = 1000)
        e = ggplot() + geom_line(data=as.data.frame(plot.x),aes(x=plot.x,y=dgamma(plot.x,eta.pr.shape,rate=eta.pr.rate)), color="blue", size = 1) + labs(title = paste("Prior on scale parameters",sep=""), x = "Scale Parameter", y = "Density") + theme(panel.background = element_rect(fill = "white", color = 'black', linewidth = 2), panel.grid.major = element_line(color = "gray", linetype = "dashed"))+
          DOTE_theme
        
        plot.x = seq(0, beta_mean*2, length.out = 1000)
        b = ggplot() + geom_line(data=as.data.frame(plot.x),aes(x=plot.x,y=dgamma(plot.x,beta.pr.shape,rate=beta.pr.rate)),color="blue", size = 1)+ labs(title = "Prior on shape parameter", x = "Shape Parameter", y = "Density") +theme(panel.background = element_rect(fill = "white", color = 'black', linewidth = 2),panel.grid.major = element_line(color = "gray", linetype = "dashed")) +
          DOTE_theme
        
        # Compute summary statistics
        summary_stats <- round(summary(imp.MMBSA$values))
        
        # Create the summary string
        summary_string <- paste0(
          "Min: ", summary_stats["Min."], "\n",
          "1st Quantile: ", summary_stats["1st Qu."], "\n",
          "Median: ", summary_stats["Median"], "\n",
          "Mean: ", summary_stats["Mean"], "\n",
          "3rd Quantile: ", summary_stats["3rd Qu."], "\n",
          "Max: ", summary_stats["Max."], "\n"
        )
        
        bottom_text <- textGrob(
          label = paste('Implied Mean', rv$name, 'Summary Statistics: ', "\n", summary_string),
          gp = gpar(fontsize=20)
        )
        # arrange the plots with the custom bottom text grob
        grid.arrange(b, e, p, bottom = bottom_text, ncol=3)
      })
      
      # Render the selected plot based on user input
      output$plot1 <- renderPlot({
        
        # Filter the data based on the selected vehicle variant
        if (input$selected_variant == "All") {
          plotReliability(data, xlab = rv$name, columnName = rv$name, rightCensor = censored_data)
        } else {
          plotReliability(data, variant = input$selected_variant, xlab = rv$name,
                          columnName = rv$name, rightCensor = censored_data)
        }
        
      })
      
      output$variant_dropdown1 <- renderUI({
        selectInput("selected_variant", "Select Variant:", choices = c("All", sort(unique(data$Variant))))
      })
      
    }
    
  })
  
  observeEvent(input$eta_slider, {
    updateSliderInput(
      session,
      "slider3",
      min = input$eta_min,
      max = input$eta_max,
      value = (input$eta_min + input$eta_max)/2)
    updateSliderInput(
      session,
      "slider4",
      min = input$eta_min,
      max = input$eta_max,
      value = (input$eta_min + input$eta_max)/4)
    
    
    
    # Render the selected plot based on user input
    output$plot2 <- renderPlot({
      req(rv$data)
      
      # Change mean and std of prior parameters based on slider input
      beta_mean <- as.numeric(input$slider1)
      beta_std <- as.numeric(input$slider2)
      eta_mean <- as.numeric(input$slider3)
      eta_std <- as.numeric(input$slider4)
      
      pr.params = priorParameters(beta.mean.pr=beta_mean, beta.std.pr=beta_std,
                                  eta.mean.pr=eta_mean, eta.std.pr=eta_std)
      
      user.input = list(beta_mean = beta_mean,
                        beta_std = beta_std,
                        eta_mean = eta_mean,
                        eta_std = eta_std)
      
      pr.params.reactive(user.input)
      
      beta.pr.shape = pr.params$betashape
      beta.pr.rate = pr.params$betarate
      eta.pr.shape = pr.params$etashape
      eta.pr.rate = pr.params$etarate
      
      # Generate the plot using the slider values
      imp.MMBSA <- impliedMmbsa(n = 10000, pr.params)
      imp.MMBSA = data.frame(values = imp.MMBSA)
      
      xmax = ifelse(is.na(input$xmax), as.numeric(quantile(imp.MMBSA$values, 0.95)), input$xmax)
      
      p = ggplot(imp.MMBSA, aes(x = values)) +
        geom_density(fill = "black", color = "black", alpha = 0.3) +
        ggtitle(paste('Implied Mean',rv$name,'Distribution')) +
        xlab(paste('Mean', rv$name)) +
        ylab("Density") + xlim(c(min(imp.MMBSA), xmax))
      
      plot.x = seq(0,eta_mean*2, length.out = 1000)
      e = ggplot() + geom_line(data=as.data.frame(plot.x),aes(x=plot.x,y=dgamma(plot.x,eta.pr.shape,rate=eta.pr.rate)), color="blue", size = 1) + labs(title = paste("Prior on scale parameters",sep=""), x = "Scale Parameter", y = "Density") + theme(panel.background = element_rect(fill = "white", color = 'black', linewidth = 2), panel.grid.major = element_line(color = "gray", linetype = "dashed"))+
        DOTE_theme
      
      plot.x = seq(0, beta_mean*2, length.out = 1000)
      b = ggplot() + geom_line(data=as.data.frame(plot.x),aes(x=plot.x,y=dgamma(plot.x,beta.pr.shape,rate=beta.pr.rate)),color="blue", size = 1)+ labs(title = "Prior on shape parameter", x = "Shape Parameter", y = "Density") +theme(panel.background = element_rect(fill = "white", color = 'black', linewidth = 2),panel.grid.major = element_line(color = "gray", linetype = "dashed")) +
        DOTE_theme
      
      # Compute summary statistics
      summary_stats <- round(summary(imp.MMBSA$values))
      
      # Create the summary string
      summary_string <- paste0(
        "Min: ", summary_stats["Min."], "\n",
        "1st Quantile: ", summary_stats["1st Qu."], "\n",
        "Median: ", summary_stats["Median"], "\n",
        "Mean: ", summary_stats["Mean"], "\n",
        "3rd Quantile: ", summary_stats["3rd Qu."], "\n",
        "Max: ", summary_stats["Max."], "\n"
      )
      
      bottom_text <- textGrob(
        label = paste('Implied Mean', rv$name, 'Summary Statistics: ', "\n", summary_string),
        gp = gpar(fontsize=20)
      )
      # arrange the plots with the custom bottom text grob
      grid.arrange(b, e, p, bottom = bottom_text, ncol=3)
    })
  })
  
  # Define a reactive value to store pr.params for use in tab 3
  pr.params.reactive <- reactiveVal(NULL)
  
  
  # Select Button Functionality in 2nd Tab
  observeEvent(input$select_priors, {
    # Prior Parameters by Default
    output$prior_parameters <- renderText({
      pr.params = pr.params.reactive()
      beta.pr.mean = input$slider1
      beta.pr.std = input$slider2
      eta.pr.mean = input$slider3
      eta.pr.std = input$slider4
      paste("Current Selected prior parameters: ", "\nShape Parameter Prior: Mean = ",
            beta.pr.mean, ",", "Standard Deviation = ", beta.pr.std, "\nScale Parameter Prior: Mean = ",
            eta.pr.mean, ",", "Standard Deviation = ", eta.pr.std)
    })
    
  })
  
  
  updated_dt = reactiveVal(NULL)
  
  observeEvent(input$dt_inference, {
    output$plot3 <- renderPlot({
      NULL
    })
    
    output$plot4 <- renderPlot({
      NULL
    })
    
    output$plot5 <- renderPlot({
      NULL
    })
    
    dt_results = updateDT_general(prior.inputs = pr.params.reactive(),
                                  dtDf = rv$data[rv$data$TestPhase=='DT',],
                                  nVariants = rv$nVariants,
                                  columnName = rv$name,
                                  variantNames = rv$variantNames)
    updated_dt(dt_results)
    dt_sampling_results = dt_results$mcmc.dt
    dt_plots = c(dt_results$dt_beta_plot, dt_results$dt_eta_plots)
    
    output$variant_dropdown3 <- renderUI({
      selectInput("selected_variant3", "Select Variant:",
                  choices = c("Shape Parameter", rv$variantNames),
                  selected = "Shape Parameter")
    })
    
    # Render the updated plot using the updated analysis results
    output$plot3 <- renderPlot({
      # Filter the data based on the selected vehicle variant
      if (input$selected_variant3 == "Shape Parameter") {
        grid.arrange(dt_results$dt_beta_plot)
        
      } else {
        i = which(rv$variantNames==input$selected_variant3)
        grid.arrange(dt_results$dt_eta_plots[[i]])
      }
      
    })
    
    output$variant_dropdown4 <- renderUI({
      selectInput("selected_variant4", "Select Variant:",
                  choices = c("Shape Parameter", rv$variantNames),
                  selected = "Shape Parameter")
    })
    
    # Render the selected plot based on user input
    output$plot4 <- renderPlot({
      # Filter the data based on the selected vehicle variant
      if (input$selected_variant4 == "Shape Parameter") {
        showNotification("Updating plot...",
                         id = "plot4_shape_update_start",
                         duration = NULL, closeButton = FALSE, type = "message")
        grid.arrange(plot_normal_fit_result(dt_results$fit.beta))
        showNotification("Plot update complete.",
                         id = "plot4_shape_update_complete",
                         duration = 5, closeButton = FALSE, type = "message")
        removeNotification("plot4_shape_update_start")
        } else {
        showNotification("Updating plot...",
                          id = "plot4_scale_update_start",
                          duration = NULL, closeButton = FALSE, type = "message")
        i = which(rv$variantNames==input$selected_variant4)
        grid.arrange(plot_gamma_fit_result(dt_results$eta.fit.list[[i]]))
        showNotification("Plot update complete.",
                         id = "plot4_scale_update_complete",
                         duration = 5, closeButton = FALSE, type = "message")
        removeNotification("plot4_scale_update_start")
      }
      
    })
    
    output$plot5 <- renderPlot(NULL)
    
    output$downloadDTsampling <- downloadHandler(
      filename = "DT_sampling_results.RData",
      content = function(file) {
        save(dt_sampling_results, file = file)
      }
    )
    
    output$downloadDTsamplingCSV <- downloadHandler(
      filename = "DT_sampling_results.csv",
      content = function(file) {
        write.csv(dt_sampling_results, file = file)
      }
    )
    
    
    # output$downloadDTplots <- downloadHandler(
    #   filename = "DT_plots.zip",
    #   content = function(file) {
    #     # Create a temporary directory to store individual plot files
    #     temp_dir <- tempdir()
    #     
    #     # Generate and save multiple plots as PNG files
    #     for (i in 1:(length(dt_plots))) {  # Example: Generate 5 plots
    #       png_file <- file.path(temp_dir, paste("plot", i, ".png", sep = ""))
    #       ggsave(png_file, dt_plots[[i]])
    #     }
    #     
    #     # Create a zip file containing the PNG files
    #     zip_file <- file.path(temp_dir, "plots.zip")
    #     zip(zip_file, files = list.files(temp_dir))
    #     
    #     # Move the zip file to the specified download location
    #     file.rename(zip_file, file)
    #   }
    # )
  })
  
  parameter = reactive({
    input$parameter
  })
  
  observeEvent(input$ot_inference, {
    
    if(!is.numeric(input$threshold)){
      showNotification("You must input a valid number for the threshold of interest.",
                       id = "warning5",
                       duration = 5, closeButton = FALSE, type = "error")
    } else {
      
      req(input$threshold)
      rv$threshold = input$threshold
      ot_results = updateOT(parameter(), updated_dt(),
                            otDf = rv$data[rv$data$TestPhase == 'OT',],
                            nVariants = rv$nVariants,
                            columnName = rv$name,
                            variantNames = rv$variantNames,
                            threshold = rv$threshold)
      updated_ot = ot_results$return_plots
      ot_sampling_results = ot_results$ot_sampling_results
      
      output$variant_dropdown5 <- renderUI({
        selectInput("selected_variant5", "Select Variant:",
                    choices = c("Shape Parameter", rv$variantNames),
                    selected = "Shape Parameter")
      })
      
      output$plot5 <- renderPlot({
        # Filter the data based on the selected vehicle variant
        if (input$selected_variant5 == "Shape Parameter") {
          grid.arrange(updated_dt()$dt_beta_plot, updated_ot$ot_beta_plot, ncol = 2)
        } else {
          i = which(rv$variantNames==input$selected_variant5)
          grid.arrange(updated_ot$ot_plots[[i]], bottom = updated_ot$textGrobs[[i]])
        }
      })
      
      output$downloadOTsampling <- downloadHandler(
        filename = "OT_sampling_results.RData",
        content = function(file) {
          save(ot_sampling_results, file = file)
        }
      )
      
      output$downloadOTsamplingCSV <- downloadHandler(
        filename = "OT_sampling_results.csv",
        content = function(file) {
          write.csv(ot_sampling_results, file = file)
        }
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)