# Shinydashboard application for exploring the 2-18 OPD data set.


# load the libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(treemap)
library(qcc)

#load the data
OP_2018 <- read_csv("/home/greig/R-projects/OutPatients/OP_2018.csv")
# OP_2018 <- read_csv("/home/greig/R-projects/OutPatients/OP_2018.csv")
OP_Referral <- read_csv("/home/greig/R-projects/OutPatients/OP_2018_Referrals.csv")

# Demographic tab data

Num_of_appointments <- nrow(OP_2018)
Num_of_individuals_seen <- n_distinct(OP_2018$NHI)
Mean_no_appoints <- round(nrow(OP_2018) / n_distinct(OP_2018$NHI), digits = 1)
Gender_spread <- fct_count(OP_2018$Gender, sort = TRUE)
No_clinics <- OP_2018 %>% 
    group_by(NHI, Clinical_line) %>% 
    summarise(Number = n())
No_clinics <- No_clinics %>% 
    group_by(NHI) %>% 
    summarise(Number = n())
Mean_no_clinics_attended <- round(mean(No_clinics$Number), digits = 1)
Median_no_clinics_attended <- median(No_clinics$Number)

plot_age <- ggplot(OP_2018, aes(x = Age)) +
    geom_histogram(binwidth = 5, fill = "red") +
    labs(title = "What was the distribution of ages?") +
    theme_light()

plot_ethnic <- ggplot(OP_2018, aes(x = Age, fill = Ethnicity_2)) +
    geom_histogram(binwidth = 5, position = "fill") +
    labs(title = "Are ethnicities equally distributed equally across all age bands?",
         x = "Age bands",
         y = "Proportion of each age band") +
    guides(fill = guide_legend(title = "Ethnicity")) +
    theme_light()

Referral_source <- as.factor(OP_Referral$Referral_source)
Referral_source <- fct_count(Referral_source, sort = TRUE)

Source_data <- Referral_source$n
names(Source_data) <- Referral_source$f
# pareto.chart(Source_data, main = "Pareto chart of the sources the referrals to Outpatients (2018)", ylab = "Count", cumperc = seq(0,100, by = 20))


    
# shinydashboard proper
my_header <- dashboardHeader(
    title = "Outpatients explorer (2018)",
    titleWidth = 300
)

my_sidebar <- dashboardSidebar(
    sidebarMenu(id = "menu_Select",
        menuItem(
            "Demographics",
            tabName = "demo"
            ),
        menuItem(
            "Age",
            tabName = "age"
        ),
        menuItem(
            "Ethnicity",
            tabName = "ethnic"
        ),
        menuItem(
            "By locality",
            tabName = "by_locality"
        ),
        menuItem(
            "By referral source",
            tabName = "referral_source"
        ),
        menuItem(
            "Supply by clinic",
            tabName = "by_clinic"
        ),
        menuItem(
            "Demand by clinic",
            tabName = "by_referral"
        )
    )
)

my_body <- dashboardBody(
    tabItems(
        tabItem(tabName = "demo",
                fluidRow(
                    valueBox(
                        value = Num_of_individuals_seen,
                        subtitle = "was the total number of patients seen",
                        color = "green"
                    ),
                    valueBox(
                        value = Num_of_appointments,
                        subtitle = "appointments were provided",
                        color = "green"
                    ),
                    valueBox(
                        value = Mean_no_appoints,
                        subtitle = "is the mean number of appointments / patient",
                        color = "green"
                    )
                ),
                fluidRow(
                        box(
                            width = 12,
                            column(12, align = "center",plotOutput("num_clinics"), align = "right"))
                ),
                fluidRow(
                    valueBox(
                        value = Mean_no_clinics_attended,
                        subtitle = "is the average number of clinics attended by each patient",
                        color = "green"
                    ),
                    valueBox(
                        value = max(No_clinics$Number),
                        subtitle = "was the maximum number of different clinics attended by a single patient",
                        color = "green"
                    )
                )
        ),
        tabItem(tabName = "age",
                fluidRow(
                    h2("Age")
                ),
                fluidRow(
                    plotOutput("age")
                )
                ),
        tabItem(tabName = "ethnic",
                fluidRow(
                    h2("Ethnicity")
                ),
                fluidRow(
                    plotOutput("ethnic_spread")
                )),
        tabItem(tabName = "by_locality",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(
                            wellPanel(
                            selectInput(
                                inputId = "locality",
                                label = "Locality",
                                choices = levels(factor(OP_2018$Locality)),
                                multiple = FALSE,
                                selected = "Horowhenua District"
                            )),
                            wellPanel(
                            checkboxInput(inputId = "show_data_local",
                                          label = "Show data table",
                                          value = FALSE)
                        )),
                        mainPanel(
                            plotOutput("locality_tree"),
                            #h2("Data table"),
                            conditionalPanel("input.show_data_local == true", h2("Data table")),
                            DT::dataTableOutput("tbl")
                        )
                             
                        ))),
        tabItem(tabName = "referral_source",
                fluidRow(
                    h2("Source of referrals to Outpatients")
                ),
                fluidRow(
                    plotOutput("referral_spread")
                )),
        tabItem(tabName = "by_clinic",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(
                            wellPanel(
                            selectInput(
                                inputId = "clinic",
                                label = "Clinic",
                                choices = levels(factor(OP_2018$Clinical_line)),
                                multiple = FALSE,
                                selected = "Urology"
                            )),
                            wellPanel(
                            checkboxInput(inputId = "show_data_clinic",
                                          label = "Show data table",
                                          value = FALSE)
                        )),
                        mainPanel(
                            plotOutput("clinic_tree_2"),
                            #h2("Data table"),
                            conditionalPanel("input.show_data_clinic == true", h2("Data table")),
                            DT::dataTableOutput("clinic_tbl"),
                            plotOutput("clinic_tree")
                        )
                        
                    ))),
        tabItem(tabName = "by_referral",
                fluidPage(
                    sidebarLayout(
                        sidebarPanel(
                            wellPanel(
                                selectInput(
                                    inputId = "referral",
                                    label = "Referral",
                                    choices = levels(factor(OP_Referral$Department_description)),
                                    multiple = FALSE,
                                    selected = "Urology"
                                )),
                            wellPanel(
                                checkboxInput(inputId = "show_data_local_2",
                                              label = "Show data table",
                                              value = FALSE)
                            )),
                        mainPanel(
                            plotOutput("referral_tree"),
                            #h2("Data table"),
                            conditionalPanel("input.show_data_local_2 == true", h2("Data table")),
                            DT::dataTableOutput("tbl_2")
                        )
                        
                    )))
    ))


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "green",
    header <- my_header,
    sidebar <- my_sidebar,
    body <- my_body
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$num_clinics <- renderPlot({
        ggplot(No_clinics, aes(x = Number)) +
            geom_histogram(binwidth = 1) +
            labs(title = "How many different clinics did each patient attend?", 
                 subtitle = "Answer: it ranged from 1 to 12 different clinics per patient", 
                 x = "Number of different clinics attended per patient", y = "Number of individuals") +
            scale_x_continuous(breaks = seq(1,10,2)) +
            coord_cartesian(xlim = c(0,10)) +
            theme_light()
    })
    
    output$age <- renderPlot({
        plot_age
    })
    
    output$ethnic_spread <- renderPlot({
        plot_ethnic
    })
    
   output$referral_spread <- renderPlot({
       pareto.chart(Source_data, main = "Pareto chart of the sources the referrals to Outpatients (2018)", ylab = "Count", cumperc = seq(0,100, by = 20))
   })
    
    output$locality_tree <- renderPlot({
        tree_map_data = OP_2018 %>% 
            filter(Locality == input$locality) %>% 
            group_by(Funding_type) %>% 
            summarise(Count = n()) %>%
            arrange(Count)
        treemap(tree_map_data, index = "Funding_type", vSize = "Count", title = "What is the most common appointment type for a locality?")
    })
    
    
    output$tbl <-  DT::renderDataTable(
        if(input$show_data_local){
       tree_data <-  OP_2018 %>%
            filter(Locality == input$locality) %>%
            group_by(Funding_type) %>%
            summarise(Count = n()) %>%
            arrange(desc(Count))
       DT::datatable(tree_data,
                     options = list(pageLength = 5),
                     rownames = FALSE)
        })
    
    # output$clinic_tree <- renderPlot({
    #     tree_map_data = OP_2018 %>% 
    #         filter(Clinical_line == input$clinic) %>% 
    #         group_by(Funding_type) %>% 
    #         summarise(Count = n()) %>%
    #         arrange(Count)
    #     treemap(tree_map_data, index = "Funding_type", vSize = "Count", title = "What is the most common appointment type for this clinic?")
    # })
    
    output$referral_tree <- renderPlot({
        tree_map_data = OP_Referral %>% 
            filter(Department_description == input$referral) %>% 
            group_by(Referral_type) %>% 
            summarise(Count = n()) %>%
            arrange(Count)
        treemap(tree_map_data, index = "Referral_type", vSize = "Count", title = "What appointment type is most commonly request?")
    })
    
    
    output$tbl_2 <-  DT::renderDataTable(
        if(input$show_data_local_2){
            tree_data <-  OP_Referral %>%
                filter(Department_description == input$referral) %>%
                group_by(Referral_type) %>%
                summarise(Count = n()) %>%
                arrange(desc(Count))
            DT::datatable(tree_data,
                          options = list(pageLength = 5),
                          rownames = FALSE)
        })
    
    
    output$clinic_tree <- renderPlot({
        Pareto_raw <-  OP_2018 %>% 
            filter(Clinical_line == input$clinic) %>%
            group_by(Funding_type) %>%
            summarise(Count = n())
        Pareto_data <- Pareto_raw$Count
        names(Pareto_data) <- Pareto_raw$Funding_type
        pareto.chart(Pareto_data, main = "What is 75% of current clinic workload", ylab = "Number per year", cumperc = seq(0,100, by = 20))
    })
  
    output$clinic_tree_2 <- renderPlot({
        tree_map_data = OP_2018 %>% 
            filter(Clinical_line == input$clinic) %>% 
            group_by(Funding_type) %>% 
            summarise(Count = n()) %>%
            arrange(Count)
        treemap(tree_map_data, index = "Funding_type", vSize = "Count", title = "What appointment type is most commonly request?")
    })
      
    output$clinic_tbl <-  DT::renderDataTable(
        if(input$show_data_clinic){
            tree_data <-  OP_2018 %>%
                filter(Clinical_line == input$clinic) %>%
                group_by(Funding_type) %>%
                summarise(Count = n()) %>%
                arrange(desc(Count))
            DT::datatable(tree_data,
                          options = list(pageLength = 5),
                          rownames = FALSE)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
