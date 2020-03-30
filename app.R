library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

covidpull <- jsonlite::read_json(path = 'https://covidtracking.com/api/states/daily')
clean_cov <- function(x) {
    whichnull <- unlist(lapply(x, is.null))
    x[whichnull] <- NA
    unlist(x)
}
covid19_1 <- lapply(covidpull, clean_cov)
covid19 <- do.call("rbind", covid19_1) %>% 
    as_tibble(stringsAsFactor = FALSE) %>% 
    mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
    rename(tested_total = total, tested_positive = positive, tested_negative = negative,
           tested_pending = pending, deaths = death) %>% 
    tidyr::gather(key = "dx_measure", value = "cases", 
                  tested_total, tested_positive, tested_negative, deaths, tested_pending, hospitalized) %>% 
    mutate(cases = as.numeric(cases)) %>% 
    mutate(cases_clean = replace(cases, is.na(cases), 0))

tracksrc_html <- xml2::read_html("https://covidtracking.com/data/")
tracksrc_node <- rvest::html_nodes(tracksrc_html, ".state-grade , .state-notes p:nth-child(1) a , .state-name .state-link")
tracksrc_txt <- rvest::html_text(tracksrc_node) %>% 
    stringr::str_trim()
which_datasrc <- which(grepl("^Best current data source for", tracksrc_txt))
which_grade <- which(nchar(tracksrc_txt) == 1)
which_state <- which(!(1:length(tracksrc_txt) %in% c(which_datasrc, which_grade)))
get_stateblock <- function(x, pos) {
    if(pos < length(x)) return(x[pos]:(x[pos+1]-1))
    x[pos]:length(tracksrc_txt)
}
state_block <- lapply(seq_along(which_state), function(x) get_stateblock(x = which_state, x))
states <- tracksrc_txt[which_state]
has_grade <- which(unlist(lapply(state_block, length)) == 3)
grades <- rep(NA, length(states))
grades[has_grade] <- tracksrc_txt[nchar(tracksrc_txt) == 1]
tracksrc_src <- rvest::html_attr(tracksrc_node, "href") %>% 
    .[!is.na(.)] %>% 
    .[!grepl("^/data/state/", .)]
src_tbl <- data.frame(states, grades, tracksrc_src, stringsAsFactors = FALSE) %>% 
    left_join(data.frame(state.name, state.abb, stringsAsFactors = FALSE), by = c("states" = "state.name")) %>% 
    filter(!is.na(grades), !is.na(tracksrc_src)) %>% 
    select(state_abb = state.abb, state_name = states, data_grade = grades, data_source = tracksrc_src)
rm(tracksrc_html, tracksrc_node, tracksrc_txt)

allstates <- covid19 %>% 
    select(state) %>% 
    inner_join(select(src_tbl, state_abb), by = c("state" = "state_abb")) %>% 
    select(state) %>% 
    unlist() %>% 
    unique()
alltrends <- unique(covid19$dx_measure)
mindate <- min(covid19$date)
maxdate <- max(covid19$date)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "COVID Trends by State",
    theme = shinytheme("sandstone"),
    tabPanel(
        "State Trends",
        fluidRow(
            column(4, selectInput(
                "states",
                "Specify States to Plot:",
                choices = allstates, 
                selected = c("IA", "SC", "FL", "VA"), 
                multiple = TRUE)),
            column(4, selectInput(
                "trnds",
                "Specify Trends to Plot:",
                choices = alltrends, 
                selected = c("tested_positive", "hospitalized", "deaths"), 
                multiple = TRUE))
            ),
        fluidRow(
            column(4, sliderInput(
                "dtrange", 
                "Specify Date Range:", 
                min = mindate,
                max = maxdate,
                value = c(mindate, maxdate))),
            column(2, radioButtons("scale", label = "Free or Fixed Y-Axis?", 
                                   choices = c("free", "fixed"),
                                   selected = "free"))
            ),
        tabsetPanel(
            tabPanel("Trend Plot", plotOutput("plt")),
            tabPanel("Data Table", tableOutput("dtatbl")),
            tabPanel("Data Documentation", tableOutput("srctbl")),
            selected = "Trend Plot"
        )
    ),
    tabPanel(
        "About",
        includeMarkdown("appinfo.Rmd")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plt <- renderPlot({
        dx_order <- c("tested_total", "tested_pending", "tested_negative", "tested_positive", "hospitalized", "deaths")
        pltdf <- covid19 %>% 
            filter(state %in% input$states,
                   date >= input$dtrange[1],
                   date <= input$dtrange[2],
                   dx_measure %in% input$trnds) %>% 
            mutate(dx_measure = factor(dx_measure, levels = dx_order, labels = dx_order))
        ggplot(pltdf, aes(x = date, y = cases_clean, color = state, group = state)) + 
            geom_smooth(se = FALSE) + 
            geom_point(size = 2) + 
            facet_grid(dx_measure ~ ., scales = input$scale) + 
            ggthemes::theme_solarized(base_size = 14) + 
            ggthemes::scale_color_solarized() + 
            labs(title = "Trends: COVID-19 Diagnosis Results and Outcomes by State",
                 caption = paste0("Data from the COVID Tracking Project: https://covidtracking.com", 
                                  "\nSee documentation for important context about state-specific data collection"),
                 color = "State")
    }, width = "auto", height = "auto")
output$dtatbl <- renderTable(striped = TRUE, {
        covid19 %>% 
            filter(state %in% input$states,
                   date >= input$dtrange[1],
                   date <= input$dtrange[2]) %>% 
            select(state, date, dx_measure, cases_clean) %>% 
            group_by(state, date) %>% 
            tidyr::spread(key = dx_measure, value = cases_clean) %>% 
            ungroup() %>% 
            arrange(desc(date), state) %>% 
            mutate(date = as.character(date)) %>% 
            select(state, date, one_of("tested_total", "tested_negative", "tested_positive", "hospitalized", "deaths"))
    })
    output$srctbl <- renderTable(bordered = TRUE, striped = TRUE, rownames = FALSE, colnames = TRUE, {
        src_tbl %>% 
            filter(state_abb %in% input$states)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
