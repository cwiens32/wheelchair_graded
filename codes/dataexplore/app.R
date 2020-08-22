

# Load packages -----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(RSQLite)


# Gather data -------------------------------------------------------------

# connect to database
db <- '../../data/wc_graded.sqlite'
conn <- dbConnect(SQLite(), db)
# create datatable
wc_adj <- dbReadTable(conn, "wheelchair_adjustments")
tab_list <- merge(dbReadTable(conn, "table_list"),
                  dbReadTable(conn, "wheelchair_adjustments"))
# create table of all angle data
for (cnta in 1:nrow(tab_list)){
    if (cnta == 1){
        data_angle <- dbReadTable(conn, tab_list$angle[cnta]) %>% 
            mutate(subject_id = tab_list$subject_id[cnta],
                   session = tab_list$session[cnta],
                   condition = tab_list$condition[cnta],
                   trial = tab_list$trial[cnta],
                   cycle = tab_list$cycle[cnta],
                   seatback_angle = tab_list$seatback_angle[cnta],
                   seatback_height = tab_list$seatback_height[cnta],
                   seatback_position = tab_list$seatback_position[cnta],
                   axle = tab_list$axle[cnta],
                   seatheight_rear = tab_list$seatheight_rear[cnta],
                   seatheight_front = tab_list$seatheight_front[cnta],
                   stability = tab_list$stability[cnta],
                   posture = tab_list$posture[cnta])
    } else {
        data_angle <- rbind(data_angle,
                            dbReadTable(conn, tab_list$angle[cnta]) %>% 
                                mutate(subject_id = tab_list$subject_id[cnta],
                                       session = as.factor(tab_list$session[cnta]),
                                       condition = tab_list$condition[cnta],
                                       trial = tab_list$trial[cnta],
                                       cycle = tab_list$cycle[cnta],
                                       seatback_angle = tab_list$seatback_angle[cnta],
                                       seatback_height = tab_list$seatback_height[cnta],
                                       seatback_position = tab_list$seatback_position[cnta],
                                       axle = tab_list$axle[cnta],
                                       seatheight_rear = tab_list$seatheight_rear[cnta],
                                       seatheight_front = tab_list$seatheight_front[cnta],
                                       stability = tab_list$stability[cnta],
                                       posture = tab_list$posture[cnta]))
    }
}

# disconnect from database
dbDisconnect(conn)

# UI ----------------------------------------------------------------------

## Create ui
ui <- dashboardPage(
    dashboardHeader(title = "Data Exploration"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("dashboard")
        ),
        menuItem(
            "Calendar",
            icon = icon("th"),
            tabName = "calendar",
            badgeColor = "green"
        )
    )),
    dashboardBody(
        # create row containing filtering variables
        fluidRow(
            box(
                width = 12,
                tags$h3("Filter data"),
                status = "primary",
                panel(
                    selectizeGroupUI(
                        id = "datafilters",
                        params = list(
                            seatback_angle = list(inputId = "seatback_angle", title = "Seatback Angle:"),
                            seatback_height = list(inputId = "seatback_height", title = "Seatback Height:"),
                            seatback_position = list(inputId = "seatback_position", title = "Seatback Position:"),
                            axle = list(inputId = "axle", title = "Axle:"),
                            seatheight_rear = list(inputId = "seatheight_rear", title = "Seat Height - Rear:"),
                            seatheight_front = list(inputId = "seatheight_front", title = "Seat Height - Front:"),
                            stability = list(inputId = "stability", title = "Stability:"),
                            posture = list(inputId = "posture", title = "Posture:")
                        )
                    )
                )
            )),
        # create row containing plot
        fluidRow(box(
            # dataTableOutput('forcetable'),
            plotOutput('plot', height = 800),
            width = NULL
        )))
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # create filtered data table
    res_data <- callModule(
        module = selectizeGroupServer,
        id = "datafilters",
        data = data_angle,
        vars = colnames(data_angle[c(11:ncol(data_angle))])
    )
    
    output$plot <- renderPlot({
        
        ggplot(data = res_data()) +
            geom_point(aes(x = elbow_angle,
                           y = torso_angle,
                           color = session)) +
            facet_wrap(~subject_id) +
            labs(x = "Elbow Angle (deg)",
                 y = "Torso Angle (deg)",
                 color = "Session") +
            ylim(75,95) +
            coord_fixed(ratio=5)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
