

# Load packages -----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(RSQLite)


# Gather data -------------------------------------------------------------

# connect to database
db_graded <- '../../data/wc_graded.sqlite'
conn_graded <- dbConnect(SQLite(), db_graded)
db_level <- '../../../wheelchair_level/data/wc_level.sqlite'
conn_level <- dbConnect(SQLite(), db_level)
# create datatable
tab_list_graded <- left_join(merge(dbReadTable(conn_graded, "table_list"),
                                   dbReadTable(conn_graded, "wheelchair_adjustments") %>% 
                                       mutate(speed_rank = ifelse(is.na(speed_rank), 100+subject_id, speed_rank))),
                             dbReadTable(conn_graded, "results") %>% 
                                 rename(subject_id = Subject,
                                        session = Session,
                                        trial = TrialNumber,
                                        cycle = CycleNumber)) %>% 
    select(-"digi") %>% 
    mutate(study = "graded")
tab_list_level <- left_join(merge(dbReadTable(conn_level, "table_list"),
                                  dbReadTable(conn_level, "wheelchair_adjustments") %>% 
                                      mutate(speed_rank = ifelse(is.na(speed_rank), 100+subject_id, speed_rank))),
                            dbReadTable(conn_level, "results") %>% 
                                rename(subject_id = Subject,
                                       session = Session,
                                       trial = TrialNumber,
                                       cycle = CycleNumber)) %>% 
    mutate(study = "level")
tab_list <- rbind(tab_list_graded, tab_list_level)
# create table of all angle data
for (cnta in 1:nrow(tab_list)){
    # set which DB connection to use
    if (tab_list$study[cnta] == "graded"){
        conn_ov <- conn_graded
    } else {
        conn_ov <- conn_level
    }
    
    if (cnta == 1){
        data_all <- cbind(dbReadTable(conn_ov, tab_list$angle[cnta]),
                          dbReadTable(conn_ov, tab_list$force[cnta]) %>%
                              select(-index)) %>% 
            mutate(time = index*(1/240),
                   subject_id = tab_list$subject_id[cnta],
                   speed_rank = tab_list$speed_rank[cnta],
                   session = tab_list$session[cnta],
                   condition = tab_list$condition[cnta],
                   trial = tab_list$trial[cnta],
                   cycle = tab_list$cycle[cnta],
                   study = tab_list$study[cnta],
                   seshname = ifelse(tab_list$session[cnta] == 1, "Pre", "Post"),
                   seatback_angle = tab_list$seatback_angle[cnta],
                   seatback_height = tab_list$seatback_height[cnta],
                   seatback_position = tab_list$seatback_position[cnta],
                   axle = tab_list$axle[cnta],
                   seatheight_rear = tab_list$seatheight_rear[cnta],
                   seatheight_front = tab_list$seatheight_front[cnta],
                   stability = tab_list$stability[cnta],
                   posture = tab_list$posture[cnta],
                   elbext_start_ind = ifelse(index == tab_list$Start_Elb_Ext_Ind[cnta]-1, 1, 0),
                   elb_maxangvel_ind = ifelse(index == tab_list$RF_Elb_Ext_AngVel_Ind[cnta]-1, 1, 0),
                   rf_peak_ind = ifelse(index == tab_list$RF_peak_Ind[cnta]-1, 1, 0))
        
    } else {
        data_all <- rbind(data_all,
                          cbind(dbReadTable(conn_ov, tab_list$angle[cnta]),
                                dbReadTable(conn_ov, tab_list$force[cnta]) %>%
                                    select(-index)) %>%  
                                mutate(time = index*(1/240),
                                       subject_id = tab_list$subject_id[cnta],
                                       speed_rank = tab_list$speed_rank[cnta],
                                       session = as.factor(tab_list$session[cnta]),
                                       condition = tab_list$condition[cnta],
                                       trial = tab_list$trial[cnta],
                                       cycle = tab_list$cycle[cnta],
                                       study = tab_list$study[cnta],
                                       seshname = ifelse(tab_list$session[cnta] == 1, "Pre", "Post"),
                                       seatback_angle = tab_list$seatback_angle[cnta],
                                       seatback_height = tab_list$seatback_height[cnta],
                                       seatback_position = tab_list$seatback_position[cnta],
                                       axle = tab_list$axle[cnta],
                                       seatheight_rear = tab_list$seatheight_rear[cnta],
                                       seatheight_front = tab_list$seatheight_front[cnta],
                                       stability = tab_list$stability[cnta],
                                       posture = tab_list$posture[cnta],
                                       elbext_start_ind = ifelse(index == tab_list$Start_Elb_Ext_Ind[cnta]-1, 1, 0),
                                       elb_maxangvel_ind = ifelse(index == tab_list$RF_Elb_Ext_AngVel_Ind[cnta]-1, 1, 0),
                                       rf_peak_ind = ifelse(index == tab_list$RF_peak_Ind[cnta]-1, 1, 0)))
    }
}

# set session name as factor
data_all <- data_all %>% 
    mutate(seshname = factor(seshname, levels=c("Pre", "Post")))

# disconnect from database
dbDisconnect(conn_graded)
dbDisconnect(conn_level)
dbDisconnect(conn_ov)

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
        fluidRow(
            box(
                checkboxGroupInput(
                    inputId = "events",
                    label = "Choose events to identify",
                    choices = c("Peak RF (Yellow)", "Start of Elbow Extension (Black)", "Max Elbow Angular Velocity (Purple)")
            )),
            box(
                selectInput(
                    inputId = "signal",
                    label = "Choose which signal to plot",
                    choices = c("Angle-Angle" = "a-a",
                                "Reaction Force" = "rf",
                                "Reaction Force Orientation to Forearm" = "rfo")
                )
            )
        ),
        # create row containing plot
        fluidRow(
            box(
                title = "Level",
                plotOutput('plot_level', height = 800),
                width = 6
                ),
            box(
                title = "Graded",
                plotOutput('plot_graded', height = 800),
                width = 6
            )
        
        ),
        # create row for combined plot
        fluidRow(
            box(
                title = "Combined",
                plotOutput("plot_all", height = 800),
                width = NULL
            )
        )
        
        )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # create filtered data table
    res_data <- callModule(
        module = selectizeGroupServer,
        id = "datafilters",
        data = data_all,
        vars = colnames(data_all[c(11:ncol(data_all))])
    )
    
    # plot for LEVEL
    output$plot_level <- renderPlot({
        
        # which signal to plot
        if (input$signal == "a-a"){
            
            p <- ggplot(data = res_data() %>% 
                            filter(study == "level")) +
                geom_point(aes(x = elbow_angle,
                               y = torso_angle,
                               color = seshname)) +
                facet_wrap(~speed_rank) +
                labs(x = "Elbow Angle (deg)",
                     y = "Torso Angle (deg)",
                     color = "Session") +
                xlim(75,175) +
                ylim(75,95) +
                coord_fixed(ratio=5)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                p <- p + geom_point(aes(x = elbow_angle,
                                        y = torso_angle),
                                    data = res_data() %>% 
                                        filter(rf_peak_ind == 1,
                                               study == "level"),
                                    color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                p <- p + geom_point(aes(x = elbow_angle,
                                        y = torso_angle),
                                    data = res_data() %>% 
                                        filter(elb_maxangvel_ind == 1,
                                               study == "level"),
                                    color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                p <- p + geom_point(aes(x = elbow_angle,
                                        y = torso_angle),
                                    data = res_data() %>% 
                                        filter(elbext_start_ind == 1,
                                               study == "level"),
                                    color = "purple")
            }
            
        } else if (input$signal == "rf"){
            
            p <- ggplot(data = res_data() %>% 
                            filter(study == "level")) +
                geom_point(aes(x = time,
                               y = rf_mag,
                               color = seshname)) +
                facet_wrap(~speed_rank) +
                labs(x = "Time (s)",
                     y = "Reaction Force (N)",
                     color = "Session") +
                xlim(0, 0.9) +
                ylim(0, 300)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                p <- p + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(rf_peak_ind == 1,
                                               study == "level"),
                                    color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                p <- p + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(elb_maxangvel_ind == 1,
                                               study == "level"),
                                    color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                p <- p + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(elbext_start_ind == 1,
                                               study == "level"),
                                    color = "purple")
            }
            
        } else if (input$signal == "rfo"){
            
            p <- ggplot(data = res_data() %>% 
                            filter(study == "level")) +
                geom_hline(yintercept = 0) +
                geom_point(aes(x = time,
                               y = rf_angle2forearm,
                               color = seshname)) +
                facet_wrap(~speed_rank) +
                labs(x = "Time (s)",
                     y = "Reaction Force Orientation Relative to Forearm (deg)",
                     color = "Session") +
                xlim(0, 0.9) +
                ylim(-150, 150)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                p <- p + geom_point(aes(x = time,
                                        y = rf_angle2forearm),
                                    data = res_data() %>% 
                                        filter(rf_peak_ind == 1,
                                               study == "level"),
                                    color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                p <- p + geom_point(aes(x = time,
                                        y = rf_angle2forearm),
                                    data = res_data() %>% 
                                        filter(elb_maxangvel_ind == 1,
                                               study == "level"),
                                    color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                p <- p + geom_point(aes(x = time,
                                        y = rf_angle2forearm),
                                    data = res_data() %>% 
                                        filter(elbext_start_ind == 1,
                                               study == "level"),
                                    color = "purple")
            }
            
        }
        
        # display plot
        p
        
    })
    
    # plot for GRADED
    output$plot_graded <- renderPlot({
        
        # which signal to plot
        if (input$signal == "a-a"){
            
            q <- ggplot(data = res_data() %>% 
                            filter(study == "graded")) +
                geom_point(aes(x = elbow_angle,
                               y = torso_angle,
                               color = seshname)) +
                facet_wrap(~speed_rank) +
                labs(x = "Elbow Angle (deg)",
                     y = "Torso Angle (deg)",
                     color = "Session") +
                xlim(75,175) +
                ylim(75,95) +
                coord_fixed(ratio=5)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                q <- q + geom_point(aes(x = elbow_angle,
                                        y = torso_angle),
                                    data = res_data() %>% 
                                        filter(rf_peak_ind == 1,
                                               study == "graded"),
                                    color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                q <- q + geom_point(aes(x = elbow_angle,
                                        y = torso_angle),
                                    data = res_data() %>% 
                                        filter(elb_maxangvel_ind == 1,
                                               study == "graded"),
                                    color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                q <- q + geom_point(aes(x = elbow_angle,
                                        y = torso_angle),
                                    data = res_data() %>% 
                                        filter(elbext_start_ind == 1,
                                               study == "graded"),
                                    color = "purple")
            }
            
        } else if (input$signal == "rf"){
            
            q <- ggplot(data = res_data() %>% 
                            filter(study == "graded")) +
                geom_point(aes(x = time,
                               y = rf_mag,
                               color = seshname)) +
                facet_wrap(~speed_rank) +
                labs(x = "Time (s)",
                     y = "Reaction Force (N)",
                     color = "Session") +
                xlim(0, 0.9) +
                ylim(0, 300)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                q <- q + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(rf_peak_ind == 1,
                                               study == "graded"),
                                    color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                q <- q + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(elb_maxangvel_ind == 1,
                                               study == "graded"),
                                    color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                q <- q + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(elbext_start_ind == 1,
                                               study == "graded"),
                                    color = "purple")
            }
            
        } else if (input$signal == "rfo"){
            
            q <- ggplot(data = res_data() %>% 
                            filter(study == "graded")) +
                geom_hline(yintercept = 0) +
                geom_point(aes(x = time,
                               y = rf_angle2forearm,
                               color = seshname)) +
                facet_wrap(~speed_rank) +
                labs(x = "Time (s)",
                     y = "Reaction Force Orientation Relative to Forearm (deg)",
                     color = "Session") +
                xlim(0, 0.9) +
                ylim(-150, 150)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                q <- q + geom_point(aes(x = time,
                                        y = rf_angle2forearm),
                                    data = res_data() %>% 
                                        filter(rf_peak_ind == 1,
                                               study == "graded"),
                                    color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                q <- q + geom_point(aes(x = time,
                                        y = rf_angle2forearm),
                                    data = res_data() %>% 
                                        filter(elb_maxangvel_ind == 1,
                                               study == "graded"),
                                    color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                q <- q + geom_point(aes(x = time,
                                        y = rf_angle2forearm),
                                    data = res_data() %>% 
                                        filter(elbext_start_ind == 1,
                                               study == "graded"),
                                    color = "purple")
            }
            
        }
        
        # display plot
        q
        
    })
    
    # plot for LEVEL AND GRADED
    output$plot_all <- renderPlot({
        
        # which signal to plot
        if (input$signal == "a-a"){
            
            pq <- ggplot(data = res_data()) +
                geom_point(aes(x = elbow_angle,
                               y = torso_angle,
                               color = seshname,
                               shape = study)) +
                facet_wrap(~speed_rank) +
                labs(x = "Elbow Angle (deg)",
                     y = "Torso Angle (deg)",
                     color = "Session",
                     shape = "Condition") +
                ylim(75,95) +
                coord_fixed(ratio=5)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                pq <- pq + geom_point(aes(x = elbow_angle,
                                          y = torso_angle,
                                          shape = study),
                                      data = res_data() %>% 
                                          filter(rf_peak_ind == 1),
                                      color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                pq <- pq + geom_point(aes(x = elbow_angle,
                                          y = torso_angle,
                                          shape = study),
                                      data = res_data() %>% 
                                          filter(elb_maxangvel_ind == 1),
                                      color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                pq <- pq + geom_point(aes(x = elbow_angle,
                                          y = torso_angle,
                                          shape = study),
                                      data = res_data() %>% 
                                          filter(elbext_start_ind == 1),
                                      color = "purple")
            }
            
        } else if (input$signal == "rf"){
            
            pq <- ggplot(data = res_data()) +
                geom_point(aes(x = time,
                               y = rf_mag,
                               color = seshname,
                               shape = study)) +
                facet_wrap(~speed_rank) +
                labs(x = "Time (s)",
                     y = "Reaction Force (N)",
                     color = "Session",
                     shape = "Condition") +
                xlim(0, 0.9) +
                ylim(0, 300)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                pq <- pq + geom_point(aes(x = time,
                                          y = rf_mag),
                                      data = res_data() %>% 
                                          filter(rf_peak_ind == 1),
                                      color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                pq <- pq + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(elb_maxangvel_ind == 1),
                                    color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                pq <- pq + geom_point(aes(x = time,
                                        y = rf_mag),
                                    data = res_data() %>% 
                                        filter(elbext_start_ind == 1),
                                    color = "purple")
            }
            
        } else if (input$signal == "rfo"){
            
            pq <- ggplot(data = res_data()) +
                geom_hline(yintercept = 0) +
                geom_point(aes(x = time,
                               y = rf_angle2forearm,
                               color = seshname,
                               shape = study)) +
                facet_wrap(~speed_rank) +
                labs(x = "Time (s)",
                     y = "Reaction Force Orientation Relative to Forearm (deg)",
                     color = "Session",
                     shape = "Condition") +
                xlim(0, 0.9) +
                ylim(-150, 150)
            
            # display events (if selected)
            if ("Peak RF (Yellow)" %in% input$events){
                pq <- pq + geom_point(aes(x = time,
                                          y = rf_angle2forearm),
                                      data = res_data() %>% 
                                          filter(rf_peak_ind == 1),
                                      color = "yellow")
            }
            if ("Start of Elbow Extension (Black)" %in% input$events){
                pq <- pq + geom_point(aes(x = time,
                                          y = rf_angle2forearm),
                                      data = res_data() %>% 
                                          filter(elb_maxangvel_ind == 1),
                                      color = "black")
            }
            if ("Max Elbow Angular Velocity (Purple)" %in% input$events){
                pq <- pq + geom_point(aes(x = time,
                                          y = rf_angle2forearm),
                                      data = res_data() %>% 
                                          filter(elbext_start_ind == 1),
                                      color = "purple")
            }
            
        }
        
        # display plot
        pq
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
