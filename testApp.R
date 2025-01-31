library(shiny)
library(DT)
library(ggplot2)
library(teal.reporter)
library(xlsx)
library(dplyr)

# replace this with medra version
medraVersion = "1.0"

titles <- xlsx::read.xlsx("R/titles.xlsx", "Sheet1") %>% 
  dplyr::filter(dplyr::row_number() > 1) %>%
  mutate(TEXT = case_when(
    grepl("\\&meddrav\\..", TEXT) ~ gsub("\\&meddrav\\..", medraVersion, TEXT),
    grepl("~\\{super a\\}", TEXT) ~ gsub("~\\{super a\\}", "ᵃ", TEXT),
    grepl("~\\{super b\\}", TEXT) ~ gsub("~\\{super b\\}", "ᵇ", TEXT),
    grepl("~\\{super c\\}", TEXT) ~ gsub("~\\{super c\\}", "ᶜ", TEXT),
    grepl("~\\{super d\\}", TEXT) ~ gsub("~\\{super d\\}", "ᵈ", TEXT),
    grepl("~\\{super e\\}", TEXT) ~ gsub("~\\{super e\\}", "ᵉ", TEXT),
    grepl("~\\{super f\\}", TEXT) ~ gsub("~\\{super f\\}", "ᶠ", TEXT),
    grepl("~\\{super g\\}", TEXT) ~ gsub("~\\{super g\\}", "ᵍ", TEXT),
    grepl("~\\{super h\\}", TEXT) ~ gsub("~\\{super h\\}", "ʰ", TEXT),
    grepl("~\\{super i\\}", TEXT) ~ gsub("~\\{super i\\}", "ⁱ", TEXT),
    TRUE ~ TEXT
  ))

titles[nrow(titles) + 1,] = c("Generate Custom Title & Footer",
  "TITLE", "Click Yellow Button To Modify This Title")

titles[nrow(titles) + 1,] = c("Generate Custom Title & Footer",
  "FOOTNOTE1", "Click Yellow Button To Modify This Footer")

devtools::load_all()

ui <- fluidPage(
  titlePanel(""),
  tabsetPanel(
    tabPanel(
      "main App",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          uiOutput("encoding")
        ),
        mainPanel(
          plotOutput("dist_plot")
        )
      )
    ),
    ### REPORTER
    tabPanel(
      "Editor",
      reporter_editor_ui("editor")
    )
    ###
  )
)
server <- function(input, output, session) {
  
  output$encoding <- renderUI({
    tagList(
      ### REPORTER
      simple_reporter_ui("simple_reporter"),
      ###
    )
  })
  plot <- reactive({
    ggplot(data = mtcars, aes(x = mpg)) +
      geom_histogram(binwidth = 8)
  })
  output$dist_plot <- renderPlot(plot())
  
  ### REPORTER
  reporter <- Reporter$new()
  card_fun <- function(card = ReportCard$new(), comment) {
    card$set_name("Plot Module")
    card$append_text("My plot", "header2")
    card$append_plot(plot())
    card$append_rcode("x <- mtcars$mpg",
      echo = TRUE,
      eval = FALSE
    )

    if (!comment == "") {
      card$append_text("Comment", "header3")
      card$append_text(comment)
    }
    card
  }
  simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
  reporter_editor_srv("editor", reporter)
}
shinyApp(ui, server)
