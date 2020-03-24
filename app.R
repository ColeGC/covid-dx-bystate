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
                  tested_total, tested_positive, tested_negative, deaths, tested_pending) %>% 
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

# Define UI for application that draws a histogram
ui <- navbarPage(
    "COVID Trends by State",
    theme = shinytheme("cyborg"),
    tabPanel(
        "State Trends",
        fluidRow(
            column(4, selectInput(
                "states",
                "Specify States to Plot:",
                choices = allstates, 
                selected = c("IA", "SC", "FL", "VA"), 
                multiple = TRUE)),
            column(6, selectInput(
                "trnds",
                "Select Trends to Plot:",
                choices = alltrends, 
                selected = c("tested_negative", "tested_positive", "deaths"), 
                multiple = TRUE)),
            column(2, radioButtons("scale", label = "Free or Fixed Y-Axis?", 
                                   choices = c("free", "fixed"),
                                   selected = "fixed")),
            tabsetPanel(
                tabPanel("Trend Plot", plotOutput("plt")),
                tabPanel("Data Documentation", tableOutput("srctbl")),
                selected = "Trend Plot"),
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
        pltdf <- covid19 %>% 
            filter(state %in% input$states,
                   dx_measure %in% input$trnds)
        ggplot(pltdf, aes(x = date, y = cases_clean, color = dx_measure, group = dx_measure)) + 
            geom_smooth(se = FALSE) + 
            geom_point(size = 2) + 
            facet_grid(state ~ ., scales = input$scale) + 
            theme_dark(base_size = 14) + 
            theme(text = element_text(colour = "gray80"),
                  rect = element_rect(fill = "black", colour = "gray60")) + 
            labs(title = "Reported COVID-19 Cases by Selected State, Time",
                 caption = paste0("Data from the COVID Tracking Project: https://covidtracking.com", 
                                  "\nSee documentation for important context about state-specific data collection"),
                 color = "Diagnosis Measure")
    })
    output$srctbl <- renderTable(bordered = TRUE, striped = TRUE, rownames = FALSE, colnames = TRUE, {
        src_tbl %>% 
            filter(state_abb %in% input$states)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
