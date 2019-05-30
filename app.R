
# load packages
library(shiny)
library(shinythemes)
library(rio)
library(tidyverse)
library(janitor)
library(colorblindr)
library(stringr)
library(DT)

# load and tidy data
    d <- import("./MasterKickstarter.csv", setclass = "tbl_df") %>% 
         clean_names()
    
    lower48 <- d %>% 
        select(-1:-3) %>% 
        filter(launched_at_y == 13 & 
               country == "USA" & 
               county != "Non-USA" & 
               state != "Non-USA" &
               status != "canceled") %>% 
        mutate(categories = as.factor(categories))
    
    levels(lower48$categories) <- sub("%.*", "", levels(lower48$categories))
    # Cam: Nice use of sub! Replacing the entire string with "film" seemed a bit redundant to me, so I wrote a regex for only removing the %20&%20video, but your way is definitely clearer. :)
    
    lower48 <- data.frame(lapply(lower48, function(lower48) {
        if (is.character(lower48)) {
            return(tolower(lower48))
        } else {
            return(lower48)
        }
    }))
    
# Cam: The following code streamlines the state choices a bit. 
state_choices <- as.character(1:49)
names(state_choices) <- str_to_title(unique(lower48$state))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"), # Cam: That's a great looking theme!

    # Application title
    titlePanel("KickStarter Data: Exploring Campaigns from 2013 by State"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(h4("Graph Customization Options:  "),
            selectInput("state", 
                        label = "Please select a state:",
                        choices = state_choices, 
                        selected = "31"),
            radioButtons("facet",
                        label = "Group to facet by:",
                        choices = c("Status" = "status",
                                    "Spotlight" = "spot_light",
                                    "Staff Pick" = "staff_pick"),
                        selected = "status"),
            submitButton("Apply changes", icon = icon("refresh"))
            # Cam: Very cool! I didn't know how to create refresh page button! The icon is also a nice touch. 
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ggdistPlot"),
           dataTableOutput("table") # Cam: Ah, looks like you needed to use dataTableOutput (not `tableOutput`)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ggdistPlot <- renderPlot({
        
        lower48_nest <- lower48 %>%
            group_by(state) %>%
            nest() %>%
            mutate(plot = map2(data, state, ~ggplot(.x, aes(backers_count, log(pledged))) +
                                   geom_point(aes(color = categories)) +
                                   geom_smooth(se = FALSE) +
                                   facet_wrap(input$facet) +
                                   labs(x = "Number of Backers", 
                                        y = "Amount Pledged ($)", 
                                        color = "Categories", 
                                        title = "Number of campaign backers and money pledged", 
                                        subtitle = glue::glue("Kickstarter data for the state of {.y}")) +
                                   scale_color_OkabeIto() +
                                   theme_minimal() +
                                   theme(plot.title = element_text(face = "bold", hjust = 0.5), 
                                         plot.subtitle = element_text(hjust = 0.5),
                                         legend.position = "bottom",
                                         legend.title = element_text(face = "bold"),
                                         axis.title = element_text(face = "bold"))))
        lower48_nest[[3]][as.numeric(input$state)]
        })
    
    output$table <- renderDataTable({
        
        lower48_nest <- lower48 %>% # Cam: lower48_nest did not exist in this chunk because you created it in the previous chunk.
            group_by(state) %>%     # This is the messy solution I came up with, but you would probably want to create the variable in
            nest() %>%              # a less local environment. In any case, nicely done on your `nest %>% mutate %>% map2`
            mutate(plot = map2(data, state, ~ggplot(.x, aes(backers_count, log(pledged))) +
                                   geom_point(aes(color = categories)) +
                                   geom_smooth(se = FALSE) +
                                   facet_wrap(input$facet) +
                                   labs(x = "Number of Backers", y = "Amount Pledged ($)", 
                                        color = "Categories", 
                                        title = "Number of campaign backers and money pledged", 
                                        subtitle = glue::glue("Kickstarter data for the state of {.y}")) +
                                   scale_color_OkabeIto() +
                                   theme_minimal() +
                                   theme(plot.title = element_text(face = "bold", hjust = 0.5), 
                                         plot.subtitle = element_text(hjust = 0.5),
                                         legend.position = "bottom",
                                         legend.title = element_text(face = "bold"),
                                         axis.title = element_text(face = "bold"))))
        
        check_args <- function(data, 
                               group_var, 
                               sum_var) {
            if (!is.data.frame(data)) {
                stop("Data supplied must be of type data frame.  Data supplied is not a data frame.")
                # Cam: Very detailed error messages! Great work!
            }
            if (!is.numeric(pull(data, !!enquo(sum_var)))) {
                stop("The variable to summarize must be numeric. The variable supplied is not numeric.")
            }
            if (is.numeric(pull(data, !!enquo(group_var)))) {
                warning("The grouping variable supplied is numeric, not categorical.")
            }
        }
        
        stat_calc <- function(data, 
                              group_var, 
                              outcome_var, 
                              .funs = list(n = ~length(.),
                                           n_valid = ~sum(!is.na(.)),
                                           n_miss = ~sum(is.na(.)),
                                           mean = ~mean(., na.rm = TRUE),
                                           sd = ~sd(., na.rm = TRUE),
                                           min = ~min(., na.rm = TRUE),
                                           max = ~max(., na.rm = TRUE))){
            
            # Cam: Very nicely done on the bang-bang and enquos! Stuff gives me the fantods. 
            check_args(data, !!enquo(group_var), !!enquo(outcome_var))
            
            data %>%
                group_by(!!enquo(group_var)) %>%
                summarize_at(vars(!!enquo(outcome_var)),
                             .funs)
        }
        
        as.data.frame(lower48_nest[[2]][as.numeric(input$state)]) %>%
            stat_calc(., input$facet, backers_count) %>%
            datatable()
    })  
}

# Run the application 
shinyApp(ui = ui, server = server)
