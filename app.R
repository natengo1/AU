library(rsconnect)
library(shiny)
library(tidyverse)
library(ggrepel)
library(plotly)
library(intrval)

# read roster

roster <- readr::read_csv("roster.csv")

# read stat .csv's and clean

files_stat <- list.files(path = "./data/",
                         pattern = "stat")

data_stat <- map_dfr(files_stat, ~readr::read_csv(paste0("./data/",.x)) %>%
                         dplyr::mutate(filename = .x)) %>%
    tidyr::separate(col = filename,
                    into = c("date","captain1","captain2","filetype"),
                    sep = "_") %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  stat_total = rowSums(across(.cols = c(K:B)))) %>%
    dplyr::select(-RANK,-filetype) %>%
    janitor::clean_names() %>%
    dplyr::left_join(x = .,
                     y = roster) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(stat_total = sum(k,e,a,es,sa,se,r,re,d,b)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(athlete) %>%
    dplyr::mutate(stat_total_run = cumsum(stat_total)) %>%
    dplyr::ungroup()

# read win .csv's and clean

files_win <- list.files(path = "./data/",
                        pattern = "win")

data_win <- map_dfr(files_win, ~readr::read_csv(paste0("./data/",.x)) %>%
                        dplyr::mutate(filename = .x)) %>%
    tidyr::separate(col = filename,
                    into = c("date","captain1","captain2","filetype"),
                    sep = "_") %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  win_total = rowSums(across(.cols = c(`SETS WON`,`GAMES WON`)))) %>%
    dplyr::select(-RANK,-filetype) %>%
    janitor::clean_names() %>%
    dplyr::left_join(x = .,
                     y = roster) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(athlete) %>%
    dplyr::mutate(win_total_run = cumsum(win_total)) %>%
    dplyr::ungroup()



# read mvp .csv's and clean

files_mvp <- list.files(path = "./data/",
                        pattern = "mvp")

data_mvp <- map_dfr(files_mvp, ~readr::read_csv(paste0("./data/",.x)) %>%
                        dplyr::mutate(filename = .x)) %>%
    tidyr::separate(col = filename,
                    into = c("date","captain1","captain2","filetype"),
                    sep = "_") %>%
    dplyr::rename(first_place = `1ST PLACE`,
                  second_place = `2ND PLACE`,
                  third_place = `3RD PLACE`) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  mvp_total = rowSums(across(.cols = c(first_place,second_place,third_place)))) %>%
    dplyr::select(-RANK,-filetype) %>%
    janitor::clean_names() %>%
    dplyr::left_join(x = .,
                     y = roster) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(athlete) %>%
    dplyr::mutate(mvp_total_run = cumsum(mvp_total)) %>%
    dplyr::ungroup()


data_full <- dplyr::left_join(
    x = data_win,
    y = dplyr::left_join(
        x = data_stat,
        y = data_mvp)) %>%
    dplyr::mutate(points_total = rowSums(across(.cols = c(win_total,stat_total,mvp_total))),
                  points_total_run = rowSums(across(.cols = c(win_total_run,stat_total_run,mvp_total_run))))

ui <- fluidPage(

    titlePanel("Athletes Unlimited - Volleyball Points Viewer"),

    sidebarLayout(
        sidebarPanel(
            
            width = 2,
            
            sliderInput(inputId = "date",
                        label = "Date Range",
                        min = min(data_full$date), 
                        max = max(data_full$date),
                        value = c(min(data_full$date),
                                  max(data_full$date))),
            
            #selectizeInput(inputId = "metric",
            #               label = "Metrics",
            #               choices = unique(data_full$metric),
            #               multiple = TRUE),
            
            #selectizeInput(inputId = "athlete_select",
            #               label = "Select Single Athlete",
            #               choices = c("All",unique(data_full$athlete)),
            #               selected = "All"),
            
            selectizeInput(inputId = "athlete",
                           label = "Filter Athletes",
                           choices = unique(data_full$athlete),
                           multiple = TRUE),
            
            selectizeInput(inputId = "position",
                           label = "Position",
                           choices = unique(data_full$position),
                           multiple = TRUE),
            
            selectInput(inputId = "view_type",
                        label = "Cumulative | Match",
                        choices = c("Cumulative","Match"),
                        selected = "Cumulative"),
            
            selectInput(inputId = "total_stat",
                        label = "Total | Stat",
                        choices = c("Total","Stat"),
                        selected = "Total"),
            
            selectInput(inputId = "facet_on_off",
                        label = "Group",
                        choices = c("Position","None"),
                        selected = "Position"),
            
            selectInput(inputId = "facet_scale",
                        label = "Group Scale",
                        choices = c("Absolute","Relative"),
                        selected = "Absolute")
            
            #selectInput(inputId = "legend",
            #            label = "Name Legend",
            #            choices = c("On","Off"),
            #            selected = "On"),
            
            #selectInput(inputId = "text_label",
            #            label = "Name Labels",
            #            choices = c("On","Off"),
            #            selected = "Off"),

        ),

        mainPanel(
            
            width = 10,
            plotOutput("mainplot",
                       width = "1000px",
                       height = "675px")
        )
    )
)

server <- function(input, output) {

    output$mainplot <- renderPlot({
        
        plotdata <- data_full %>%
            #dplyr::filter(metric %in% isolate(input$metric) | is.null(isolate(input$metric))) %>%
            #dplyr::filter(athlete %in% isolate(input$athlete_select) | (isolate(input$athlete_select) == "All")) %>%
            dplyr::filter(athlete %ni% input$athlete | is.null(input$athlete)) %>%
            dplyr::filter(date >= min(input$date) & date <= max(input$date)) %>%
            dplyr::filter(position %in% input$position | is.null(input$position))
        
        p <- ggplot(
            plotdata,
            aes(x = date,
                group = athlete,
                color = athlete)) +
            geom_point(size = 3) +
            geom_line()
        
        p <- switch(
            input$view_type,
            Cumulative = switch(
                input$total_stat,
                Total = ggplot(
                    plotdata,
                    aes(x = date,
                        y = points_total_run,
                        group = athlete,
                        color = athlete)),
                Stat = ggplot(
                    plotdata,
                    aes(x = date,
                        y = stat_total_run,
                        group = athlete,
                        color = athlete))),
            Match = switch(
                input$total_stat,
                Total = ggplot(
                    plotdata,
                    aes(x = date,
                        y = points_total,
                        group = athlete,
                        color = athlete)),
                Stat = ggplot(
                    plotdata,
                    aes(x = date,
                        y = stat_total,
                        group = athlete,
                        color = athlete))))
        
        p <- p +
            geom_point(size = 3) +
            geom_line() +
            geom_text_repel(data = . %>%
                                dplyr::group_by(athlete) %>%
                                dplyr::filter(date == max(date)) %>%
                                dplyr::ungroup(),
                            aes(label = athlete),
                            size = 6) +
            theme_bw() +
            theme(legend.position = "none",
                  panel.grid.minor = element_blank(),
                  text = element_text(size = 18)) +
            labs(x = "Match Date",
                 y = "Points")
        
        p <- switch(input$facet_on_off,
                    Position = switch(
                        input$facet_scale,
                        Absolute = p +
                            facet_wrap(facets = vars(position)),
                        Relative = p +
                            facet_wrap(facets = vars(position),
                                       scales = "free")),
                    None = p)

        p
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
