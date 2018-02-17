library(shiny)
library(shinydashboard)
library(dplyr)
library(RColorBrewer)
library(ggridges)

source('global.R')
source('functions.R')

header <- dashboardHeader(title="Poverty explorer")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Welcome",
      tabName="welcome",
      icon=icon("key")),
    menuItem(
      text="Assignment",
      tabName="assignment",
      icon=icon("eye")),
    menuItem(
      text="Extra",
      tabName="extra",
      icon=icon("thumbs-up")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="welcome",
      fluidPage(
        
        h3('Welcome!'),
        fluidRow(
          column(12,
                 HTML("Thank you for taking the time to look at the R Shiny app I have created for the World Bank Programming Exercise. The code repository on github is available <a href=https://github.com/databrew/world_bank_poverty>here</a>. For the sake of time and general efficiency of the app, I have taken the liberty of making a few assumptions and adjustments to the data as outlined below:

<ul>
  <li>I was formally trained in Economics and I often used Stata for research during both undergrad and graduate school. However, I no longer have a license and therefore did not include a Stata log file. I was, nevertheless, able to translate the Stata code into an R program and ultimately accomplished the task.
  </li>
   
  <li>There is an additional tab, 'Extra', that examines the poverty incidence curve, the contributions to inequality, and a map which shows the headcount poverty ratio by province (overlayed on real provinces).
  </li>
  
  <li> For practical purposes, I transformed the data from 'wide format' to 'long format'. This way the number of columns and their names are independent of the variables. 
  </li>
  
  <li> Instead of analyzing the data at the different poverty lines {3000, 2000, 5500}, I created an input that allows the user to analyze many different poverty lines {0, 500, 1000, ... , 5500}, with corresponding plots and tables that react to the chosen input. In addition, the user can look at regions separately or in aggregate. 
  </li>
  
  <li>It was not clear to me from the instructions what each element of this database pertains to. For the purposes of this exercise, I am assuming that an observation is a place (town, municipality, etc.) and that each income variable refers to an individual person. In other words, I make the assumption that there are 30,000 places and 200 people per place.
  </li>
</ul>")
          )
                 ),
        fluidRow(
          br(), br(), br(), br(),
          div(img(src='wb_logo.png', align = "left", width = '200'), style="text-align: left; margin-left:10px;")
        )
          )
        ),
    tabItem(
      tabName="assignment",
      fluidPage(
        fluidRow(column(12,
                        sliderInput('poverty_line',
                                    'Select a poverty line',
                                    min = 500,
                                    max = 5000,
                                    value = 3000,
                                    step = 500),
                        textOutput('poverty_line_text'))),
        fluidRow(column(12,
                        radioButtons('by_region',
                                     '',
                                     choices = c('Breakdown by region',
                                                 'Combine all regions'),
                                     inline = TRUE),
                        tabsetPanel(type = 'tabs',
                                    
                                    tabPanel('Poverty headcount rate',
                                             textOutput('phr_text'),
                                             plotOutput('phr')),
                                    tabPanel('Poverty gap',
                                             textOutput('gap_text'),
                                             plotOutput('poverty_gap')),
                                    tabPanel('Squared poverty gap',
                                             textOutput('gap_squared_text'),
                                             plotOutput('poverty_gap_squared')),
                                    tabPanel('Gini and Theil',
                                             textOutput('g_and_t_text'),
                                             plotOutput('g_and_t')),
                                    tabPanel('Lorenz curves',
                                             textOutput('lorenz_text'),
                                             plotOutput('lorenz')),
                                    tabPanel('Table for all metrices',
                                             textOutput('data_tab_text'),
                                             DT::dataTableOutput('data_tab')),
                                    tabPanel('Distribution of income',
                                             plotOutput('distributions'))
                        )))
      )
    ),
    tabItem(
      tabName="extra",
      fluidPage(
        fluidRow(h3('Other poverty-related metrics'),
                 p('The below are a few more poverty-related visualizations and data points. This was not required by the assignment; it is simply a demonstration of features/capabilities.')),
        navlistPanel(widths = c(3,9),
          tabPanel('Poverty incidence curve',
                   fluidPage(
                     br(),
                     fluidRow(p('The poverty incidence curve shows the percentage of the population which is considered "poor" (by headcount) at different levels of income/consumption.')),
                     fluidRow(plotOutput('poverty_incidence'))
                   )),
          tabPanel('Contributions to inequality',
                   fluidPage(
                     br(),
                     fluidRow(p('At times it is useful to understand which factors contributes to inequality. In our (fake) data, we only have region as a potential explanatory variable, so our analysis is necessarily limited. That said, we can assess the with-in group contribution to income inequality from region (based on the General Entropy inequality measures with parameter values 0, 1, and 2).'),
                              br(),
                              p('The below shows contributions to inequality within, between, and across subgroups (regions) using the Atkinson approach, as imlemented by Elbers et al. ("Re-Interpreting Sub-Group Inequality Decompositions", World Bank Policy Research Working Paper, 2005)')),
                     fluidRow(verbatimTextOutput('inequality'))
                   )),
          tabPanel('Map',
                   fluidPage(
                     br(),
                     fluidRow(p('The fake data did not include any indication of what the regions are. For the purposes of demonstration, we\'ll assume that the 6 regions refer to the 6 southernmost provinces of Panama. The below map shows the headcount poverty ratio by province.')),
                     br(),
                     leafletOutput('leaf')
                   ))
        )
      )),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          column(12,
                 HTML('<a href=https://databrew.cc> Ben Brew </a>'),
                 p('Empowering research and analysis through collaborative data science.', align = 'center'),
                 div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                                    icon = icon("envelope", lib = "font-awesome")),
                       href="mailto:ben@databrew.cc",
                       align = 'center')), 
                 style = 'text-align:center;'
                 )
          
        )
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  output$poverty_line_text <-
    renderText({
      paste0('You have selected a poverty line of ', input$poverty_line)
    })
  
  output$phr_text <-
    renderText({
      "This is the proportion of the population that lives below the poverty line (Suresh D. Tendulkar and L. R. Jain)"
    })
  
  output$gap_text <-
    renderText({
      "The poverty gap is the mean distance separating the population from the poverty line, with the non-poor being given a distance of zero. In other words, it is the measure of the 'intensity' of poverty."
    })
  
  output$gap_squared_text <-
    renderText({
      "Squared Poverty Gap: Measure of the 'severity' of poverty. While the poverty gap takes into account the distance separating the poor from the poverty line, the squared poverty gap takes the square of that distance into account. When using the squared poverty gap, the poverty gap is weighted by itself, so as to give more weight to the very poor. Said differently, the squared poverty gap takes into account the inequality among the poor."
    })
  
  output$g_and_t_text <-
    renderText({
      "The Gini coefficient is a measure of income distribution, with zero indicating perfect equality and 1 representing perfect inequality. An advantage of the Gini coefficient is that it can used conduct a direct comparison between two populations, regardless of the their sizes. However, a disadvantage is that the Gini coefficient cannot take into account the 'structure' of the inequality within a population. For example, two countries can have the same Gini coefficient but different inequalities, the same way that Lorenz cures can have the same area yet different shapes. The Theil coefficient, however, improves on the Gini coefficient because it aggregates inequality within and across each group."
    })
  
  output$lorenz_text <-
    renderText({
      "the Lorenz curve, developed by Max O. Lorenz, shows the what percentage of income goes to the bottom x% of households"
    })
  
  output$data_tab_text <-
    renderText({
      "The table below aggregates our different mectrics into one downloadable table."
    })
  

  output$phr <- renderPlot({
    
    if(input$by_region == 'Breakdown by region'){
      by_region <- TRUE
    } else {
      by_region <- FALSE
    }
    
    poverty_line <- input$poverty_line
    
    if(by_region){
      plot_data <- 
        dfl %>%
        mutate(poor = as.numeric(value < poverty_line)) %>%
        group_by(region) %>%
        summarise(rate = weighted.mean(poor, w = weight) * 100) %>%
        ungroup %>%
        arrange(region)
    } else {
      plot_data <- 
        dfl %>%
        mutate(poor = as.numeric(value < poverty_line)) %>%
        summarise(rate = weighted.mean(poor, w = weight) * 100) %>%
        mutate(region = 'All regions')
    }
    cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(plot_data$region)))
    
    ggplot(data = plot_data %>% mutate(region = factor(region)),
           aes(x = region,
               y = rate,
               fill = region)) +
      geom_bar(stat = 'identity') +
      ggthemes::theme_fivethirtyeight() +
      labs(x = 'Region',
           y = 'Poverty headcount rate',
           title = paste0('Headcount rate (%) at poverty line of ', poverty_line),
           subtitle = paste0(
             ifelse(by_region, 'By region', 'All regions'),
             ', weighted'
           )) +
      scale_fill_manual(name = '',
                        values = cols) +
      theme(legend.position = 'none') +
      geom_label(aes(label = round(rate, digits = 1), vjust = 1,
                     fill = NA))
    
  })
  
  
  output$poverty_gap <- renderPlot({
    # POVERTY GAP
    # The poverty gap is the mean distance separating the population from the poverty line, with the non-poor being given
    # a distance of zero. 
    # Formula is (1 / total population) * sum((poverty line - income)/ poverty line)
    
    poverty_line <- input$poverty_line
    if(input$by_region == 'Breakdown by region'){
      by_region <- TRUE
    } else {
      by_region <- FALSE
    }
    
    # Downsample based on weights - probability of keeping should be based on weights.
    plot_data <- dfl
    plot_data <- sample_n(plot_data, size = 1000000, replace = TRUE,
                          weight = weight)
    
    if(by_region){
      plot_data <- plot_data %>%
        group_by(region) %>%
        summarise(gap = ineq::pov(x = value,
                                  k = poverty_line,
                                  parameter = 2,
                                  type = 'Foster',
                                  na.rm = TRUE)) %>%
        mutate(region = factor(region))
    } else {
      plot_data <- plot_data %>%
        summarise(gap = ineq::pov(x = value,
                                  k = poverty_line,
                                  parameter = 2,
                                  type = 'Foster',
                                  na.rm = TRUE)) %>%
        mutate(region = 'All regions')
    }
    
    
    
    cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(plot_data$region)))
    
    ggplot(data = plot_data,
           aes(x = region,
               y = gap,
               fill = region)) +
      geom_bar(stat = 'identity') +
      ggthemes::theme_fivethirtyeight() +
      labs(x = 'Region',
           y = 'Poverty gap',
           title = paste0('Poverty gap at poverty line of ', poverty_line),
           subtitle = paste0(ifelse(by_region, 'By region', 'All regions'),
                             ', downsampled based on weights')) +
      scale_fill_manual(name = '',
                        values = cols) +
      theme(legend.position = 'none') +
      geom_label(aes(label = round(gap, digits = 3), vjust = 1,
                     fill = NA))
    
  })
  
  output$poverty_gap_squared<- renderPlot({
    # SQUARED POVERTY GAP
    
    poverty_line <- input$poverty_line
    if(input$by_region == 'Breakdown by region'){
      by_region <- TRUE
    } else {
      by_region <- FALSE
    }
    
    # Downsample based on weights
    plot_data <- dfl
    plot_data <- sample_n(plot_data, size = 1000000, replace = TRUE,
                          weight = weight)
    
    
    if(by_region){
      plot_data <- plot_data %>%
        group_by(region) %>%
        summarise(gap = ineq::pov(x = value,
                                  k = poverty_line,
                                  parameter = 2,
                                  type = 'Foster',
                                  na.rm = TRUE)) %>%
        mutate(region = factor(region))
    } else {
      plot_data <- plot_data %>%
        summarise(gap = ineq::pov(x = value,
                                  k = poverty_line,
                                  parameter = 2,
                                  type = 'Foster',
                                  na.rm = TRUE)) %>%
        mutate(region = 'All regions')
    }
    
    # Square it
    plot_data$gap <- plot_data$gap ^ 2
    
    cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(plot_data$region)))
    
    ggplot(data = plot_data,
           aes(x = region,
               y = gap,
               fill = region)) +
      geom_bar(stat = 'identity') +
      ggthemes::theme_fivethirtyeight() +
      labs(x = 'Region',
           y = 'Squared gap',
           title = paste0('Squared poverty gap at poverty line of ', poverty_line),
           subtitle = paste0(ifelse(by_region, 'By region', 'All regions'),
                             ', downsampled based on weights')) +
      scale_fill_manual(name = '',
                        values = cols) +
      theme(legend.position = 'none') +
      geom_label(aes(label = round(gap, digits = 3), vjust = 1,
                     fill = NA))
    
    
  })
  
  output$g_and_t <- renderPlot({
    
    poverty_line <- input$poverty_line
    if(input$by_region == 'Breakdown by region'){
      by_region <- TRUE
    } else {
      by_region <- FALSE
    }
    
    # Downsample based on weights
    plot_data <- dfl
    plot_data <- sample_n(plot_data, size = 1000000, replace = TRUE,
                          weight = weight)
    
    
    # Get the gini coefficient
    if(by_region){
      out <- plot_data %>%
        group_by(region = factor(region)) %>%
        summarise(gini = ineq(x = value,
                              type = 'Gini',
                              na.rm = TRUE),
                  theil = ineq(x = value,
                                      type = 'Theil',
                                      na.rm = TRUE)) %>%
        gather(key, value, gini:theil)
    } else {
      out <- plot_data %>%
        summarise(gini = ineq(x = value,
                              type = 'Gini',
                              na.rm = TRUE),
                  theil = ineq(x = value,
                               type = 'Theil',
                               na.rm = TRUE)) %>%
        gather(key, value, gini:theil) %>%
        mutate(region = 'All regions')
      
    }
   
    cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(2)
    
     
    ggplot(data = out,
           aes(x = factor(region),
               y = value,
               group = key,
               fill = key)) +
      geom_bar(stat = 'identity',
               position = 'dodge',
               alpha = 0.6) +
      scale_fill_manual(name = 'Inequality measure',
                        values = cols) +
      # ggthemes::theme_fivethirtyeight() +
      labs(title = 'Gini and Theil coefficients for inequality',
           subtitle = 'Downsampled based on weights') +
      geom_label(aes(label = round(value, digits = 2)), 
                 position = position_dodge(width = 1),
                 vjust = 1,
                 fill = NA)

  })

  output$lorenz <- renderPlot({
    
    poverty_line <- input$poverty_line
    if(input$by_region == 'Breakdown by region'){
      by_region <- TRUE
    } else {
      by_region <- FALSE
    }
    
    # Downsample based on weights
    plot_data <- dfl
    plot_data <- sample_n(plot_data, size = 1000000, replace = TRUE,
                          weight = weight)
    
    
    # Get the gini coefficient
    if(by_region){
      regions_list <- list()
      regions <- sort(unique(plot_data$region))
      for(i in 1:length(regions)){
        this_region <- regions[i]
        sub_data <- plot_data %>% filter(region == this_region)
        lc <- Lc(sub_data$value)
        data <- data.frame(x = lc$p,
                           y = lc$L,
                           region = this_region)
        regions_list[[i]] <- data
      }
      out <- bind_rows(regions_list)
    } else {
      lc <- Lc(plot_data$value)
      out <- data.frame(x = lc$p,
                         y = lc$L,
                         region = 'All regions')
    }
    
    cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(out$region)))
    
    out <- out %>%
      mutate(x = x * 100,
             y = y * 100)
    
    
    ggplot(data = out %>% mutate(region = paste0('Region ', region)),
           aes(x = x,
               y = y,
               group = region,
               color = region)) +
      geom_line() +
      geom_abline(intercept = 0, slope = 1, color="black", 
                  linetype="dashed", alpha = 0.5) +
      scale_color_manual(name = 'Region',
                        values = cols) +
      ggthemes::theme_fivethirtyeight() +
      labs(title = 'Lorenz curves',
           subtitle = 'Empirical ordinary: x% of the population have y% of the wealth') +
      facet_wrap(~region) +
      theme(legend.position = 'none')
    
  })
  
    
  output$distributions <- renderPlot({
    if(input$by_region == 'Breakdown by region'){
      by_region <- TRUE
    } else {
      by_region <- FALSE
    }
    
    poverty_line <- input$poverty_line
    
    if(by_region){
      
      # Downsample based on weights
      plot_data <- dfl
      plot_data <- sample_n(plot_data, size = 100000, replace = TRUE,
                            weight = weight)
      # Define colors   
      n_cols <- length(unique(plot_data$region))
      cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(n_cols)
      ggplot(data = plot_data,
             aes(x = value,
                 y = factor(region),
                 fill = factor(region))) +
        geom_density_ridges(scale = 2.5,
                            alpha = 0.7) +
        scale_fill_manual(name = 'Region',
                          values = cols) +
        ggthemes::theme_fivethirtyeight() +
        guides(fill=guide_legend(ncol=length(unique(plot_data$region)))) +
        labs(title = 'Income distribution by region',
             subtitle = 'Downsampled based on weights') +
        geom_vline(xintercept = poverty_line,
                   lty = 2,
                   alpha = 0.6)
      
      
    } else {
      n_cols <- 1
      cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(n_cols)
      
      # Downsample based on weights
      plot_data <- dfl
      plot_data <- sample_n(plot_data, size = 100000, replace = TRUE,
                            weight = weight)
      ggplot(data = plot_data,
             aes(x = value)) +
        geom_density(fill = cols) +
        ggthemes::theme_fivethirtyeight() +
        labs(title = 'Income distribution (all regions)',
             subtitle = 'Downsampled based on weights') +
        geom_vline(xintercept = poverty_line,
                   lty = 2,
                   alpha = 0.6)
      
    }
  })
  
  output$poverty_incidence <- renderPlot({
    
    # Downsample based on weights
    plot_data <- dfl
    plot_data <- sample_n(plot_data, size = 100000, replace = TRUE,
                          weight = weight)
    
    # Arrange dataset by poverty
    plot_data <- plot_data %>% 
      arrange(value) %>%
      # Calculate order
      mutate(dummy = 1:nrow(plot_data)) %>%
      # Calculate percentile
      mutate(p = dummy / max(dummy) * 100) %>%
      # Calculate cumulative amounts and people
      mutate(cum_people = cumsum(weight),
             cum_consumption = cumsum(value)) %>%
      # simplify
      group_by(x = round(value, digits = -3)) %>%
      filter(id == dplyr::first(id)) %>%
      ungroup
    
    
    # Plot the poverty incidence curve
    ggplot(data = plot_data, aes(x = value, 
                                 y = cum_consumption / 
                                   max(cum_consumption) * 100)) +
      # geom_area(alpha = 0.6,
      #           fill = 'darkorange') +
      geom_line(lty = 1, 
                size = 2,
                color = 'darkorange',
                alpha = 0.6) +
      theme_fivethirtyeight() +
      labs(x = 'Consumption per capita',
           y = '% of population',
           title = 'Poverty incidence curve',
           subtitle = 'With first-order stochastic dominance') +
      ylim(0, 100)
  })
  
  output$inequality <- renderPrint({
    
    # Downsample based on weights
    plot_data <- dfl
    plot_data <- sample_n(plot_data, size = 100000, replace = TRUE,
                          weight = weight)
    plot_data$region <- factor(plot_data$region)
    
    for (i in 0:2){
      cat(paste0('#### Epsilon: ', i, '   -----------------------------------------------------------\n'))
      x <- IC2::decompAtkinson(x = ifelse(plot_data$value <= 1000, 1000, plot_data$value), 
                          z = factor(plot_data$region),
                          # w = plot_data$weight / sum(plot_data$weight), 
                          epsilon = i, 
                          decomp = "BDA", 
                          ELMO = FALSE)
      summary(x)
    }
  })
  
  output$leaf <- renderLeaflet({
    
    

    poverty_line <- 3000   
    plot_data <- 
        dfl %>%
        mutate(poor = as.numeric(value < poverty_line)) %>%
        group_by(region) %>%
        summarise(rate = weighted.mean(poor, w = weight) * 100) %>%
        ungroup %>%
        arrange(region) %>%
      mutate(region = as.numeric(factor(region)))
    cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(plot_data$region)))
    
    map <- pan
    map@data <- map@data %>% left_join(plot_data, by = 'region')
    map@data$percent <- map@data$rate
    
    palette = 'YlOrRd'
    bins <- sort(unique(round(c(c(0, quantile(map@data$percent, seq(0, 1, by = 0.1), na.rm = TRUE), max(map@data$percent, na.rm = TRUE) * 1.1)))))
    pal <- colorBin(palette, domain = map@data$percent, bins = bins)
    map <- map[!is.na(map@data$percent),]
    
    popper <- paste0(map@data$NAME_1, ': ', round(map@data$percent, digits = 2))
    
    leaflet() %>%
      addProviderTiles('Stamen.Terrain') %>%
      addPolygons(data = map,
                  fillColor = ~pal(map@data$percent),
                  fillOpacity = 0.8,
                  color = "#BDBDC3",
                  weight = 1,
                  # popup = popper,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = popper,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
    
  })
  
  output$data_tab <- renderDataTable({
    
    if(input$by_region == 'Breakdown by region'){
      by_region <- TRUE
    } else {
      by_region <- FALSE
    }
    
    poverty_line <- input$poverty_line
  
    
    if(by_region){
      ##########
      # get poverty head count rate
      table_data_phr <- 
        dfl %>%
        mutate(poor = as.numeric(value < poverty_line)) %>%
        group_by(region) %>%
        summarise(rate = weighted.mean(poor, w = weight) * 100) %>%
        ungroup %>%
        arrange(region)
      
      ##########
      # get poverty gap
      # Downsample based on weights - probability of keeping should be based on weights.
      table_data_pg <- dfl
      table_data_pg <- sample_n(table_data_pg, size = 1000000, replace = TRUE,
                               weight = weight)
      table_data_pg <- table_data_pg %>%
        group_by(region) %>%
        summarise(gap = ineq::pov(x = value,
                                  k = poverty_line,
                                  parameter = 2,
                                  type = 'Foster',
                                  na.rm = TRUE)) %>%
        mutate(region = factor(region))
      
      ######### get poverty gap squared
      table_data_pg_squared <- table_data_pg
      table_data_pg_squared$gap_squared  <- table_data_pg$gap
      table_data_pg_squared$gap <- NULL
      
      ######### get gini and theill coef
      table_data_gt <- dfl
      table_data_gt <- sample_n(table_data_gt, size = 1000000, replace = TRUE,
                               weight = weight)
      
      table_data_gt <- table_data_gt %>%
        group_by(region = factor(region)) %>%
        summarise(gini = ineq(x = value,
                              type = 'Gini',
                              na.rm = TRUE),
                  theil = ineq(x = value,
                               type = 'Theil',
                               na.rm = TRUE)) 
      
      # combine all four into table 
      table_data_full <- as.data.frame(cbind(table_data_phr, table_data_pg$gap, table_data_pg_squared$gap_squared, table_data_gt$gini, 
                                                table_data_gt$theil))
      colnames(table_data_full) <- c('Region','Poverty headcount rate', 'Poverty gap', 'Poverty gap squared', 
                                    'Gini coef', 'Theil coef')
    } else {
      ########## get poverty head count rate
      table_data_phr <- 
        dfl %>%
        mutate(poor = as.numeric(value < poverty_line)) %>%
        summarise(rate = weighted.mean(poor, w = weight) * 100) %>%
        mutate(region = 'All regions')
      
      ########## get poverty gap
      table_data_pg <- dfl
      table_data_pg <- sample_n(table_data_pg, size = 1000000, replace = TRUE,
                               weight = weight)
      table_data_pg <- table_data_pg %>%
        summarise(gap = ineq::pov(x = value,
                                  k = poverty_line,
                                  parameter = 2,
                                  type = 'Foster',
                                  na.rm = TRUE)) %>%
        mutate(region = 'All regions')
      
      ######### get poverty gap squared
      table_data_pg_squared <- table_data_pg
      table_data_pg_squared$gap_squared  <- table_data_pg$gap
      table_data_pg_squared$gap <- NULL
      
      ######### get gini and theill coef
      
      table_data_gt <- dfl
      table_data_gt <- sample_n(table_data_gt, size = 1000000, replace = TRUE,
                               weight = weight)
      table_data_gt <- table_data_gt %>%
        summarise(gini = ineq(x = value,
                              type = 'Gini',
                              na.rm = TRUE),
                  theil = ineq(x = value,
                               type = 'Theil',
                               na.rm = TRUE)) %>%
        mutate(region = 'All regions')
      
      # combine all four into table 
      region <- 'All'
      table_data_full <- as.data.frame(cbind(region, table_data_phr, table_data_pg$gap, table_data_pg_squared$gap_squared, table_data_gt$gini, 
                                            table_data_gt$theil))
      colnames(table_data_full) <- c('Region','Poverty headcount rate', 'Poverty gap', 'Poverty gap squared',
                                    'Gini coef', 'Theil coef')
      
      
    }
    
    x <- prettify(table_data_full, 
             download_options = TRUE)
    
    return(x)
    
    
  })
  
  
  
  
}

shinyApp(ui, server)