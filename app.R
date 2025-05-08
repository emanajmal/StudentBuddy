library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), "StudentBuddy.db")
students_data <- dbGetQuery(con, "SELECT * FROM students")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e6f2ff;
        font-family: 'Segoe UI', sans-serif;
      }
      .buddy-card, .sidebar {
        min-height: 400px;
      }
      .buddy-card {
        background: linear-gradient(135deg, #ffffff, #d9f2ff);
        border-radius: 20px;
        padding: 25px;
        box-shadow: 0 10px 25px rgba(0, 0, 0, 0.1);
        transition: transform 0.3s ease-in-out;
        max-width: 450px;
        margin: 10px auto;
      }
      .buddy-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 12px 24px rgba(0, 0, 0, 0.15);
      }
      .buddy-card img {
        border-radius: 50%;
        margin-bottom: 20px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.15);
      }
      .buddy-card h3 {
        margin-top: 10px;
        margin-bottom: 10px;
        color: #003f5c;
      }
      .buddy-card p {
        margin: 5px 0;
        font-size: 15px;
        color: #333333;
      }
      .sidebar {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        max-width: 450px;
        margin: auto;
      }
      .btn {
        background-color: #0077b6;
        color: white;
        border: none;
      }
      .btn:hover {
        background-color: #005f8e;
      }
      .form-control {
        border-radius: 8px;
      }
    "))
  ),
  
  div(
    style = "text-align: center; font-size: 32px; font-weight: bold; margin-bottom: 20px;",
    "🎓 International Student Buddy System"
  ),
  
  fluidRow(
    column(
      width = 5, offset = 1,
      div(
        class = "sidebar",
        textInput("student_name", "Enter Your Name:"),
        selectInput("student_country", "Select Your Country:", choices = unique(students_data$country)),
        selectInput("student_program", "Select Your Program:", choices = unique(students_data$program)),
        actionButton("find_match", "Find Buddy"),
        uiOutput("select_buddy"),
        br(),
        actionButton("assign_match", "Assign This Match")
      )
    ),
    column(
      width = 5,
      uiOutput("match_cards")
    )
  )
)

server <- function(input, output, session) {
  buddy_country <- reactiveVal(NULL)
  buddy_program <- reactiveVal(NULL)
  
  observeEvent(input$find_match, {
    req(input$student_name != "")
    
    student_check <- dbGetQuery(con, paste0(
      "SELECT * FROM students WHERE LOWER(name) = LOWER('", input$student_name, "')")
    )
    
    if (nrow(student_check) == 0) {
      showModal(modalDialog(
        title = "Student Not Found",
        "You must be registered in the system to find a Buddy.",
        easyClose = TRUE
      ))
      return()
    }
    
    matched_ids <- dbGetQuery(con, "SELECT buddy_id FROM matches")$buddy_id
    
    country_match <- dbGetQuery(con, paste0(
      "SELECT * FROM buddies WHERE country = '", input$student_country,
      "' AND buddy_id NOT IN (", paste(c(0, matched_ids), collapse = ","), ")"
    )) %>% slice_sample(n = 1)
    
    program_match <- dbGetQuery(con, paste0(
      "SELECT * FROM buddies WHERE program = '", input$student_program,
      "' AND buddy_id NOT IN (", paste(c(0, matched_ids,
                                         if (nrow(country_match) > 0) country_match$buddy_id else NA), collapse = ","), ")"
    )) %>% slice_sample(n = 1)
    
    buddy_country(if (nrow(country_match) > 0) country_match else NULL)
    buddy_program(if (nrow(program_match) > 0) program_match else NULL)
  })
  
  output$match_cards <- renderUI({
    c_buddy <- buddy_country()
    p_buddy <- buddy_program()
    
    fluidRow(
      if (!is.null(c_buddy)) {
        column(
          width = 6,
          div(
            class = "buddy-card",
            tags$img(src = c_buddy$photo_url, width = "150px", height = "150px"),
            tags$h3(c_buddy$name),
            tags$p(strong("Country: "), c_buddy$country),
            tags$p(strong("Program: "), c_buddy$program),
            tags$p(style = "font-style: italic; color: gray;", c_buddy$bio)
          )
        )
      },
      if (!is.null(p_buddy)) {
        column(
          width = 6,
          div(
            class = "buddy-card",
            tags$img(src = p_buddy$photo_url, width = "150px", height = "150px"),
            tags$h3(p_buddy$name),
            tags$p(strong("Country: "), p_buddy$country),
            tags$p(strong("Program: "), p_buddy$program),
            tags$p(style = "font-style: italic; color: gray;", p_buddy$bio)
          )
        )
      }
    )
  })
  
  output$select_buddy <- renderUI({
    c_buddy <- buddy_country()
    p_buddy <- buddy_program()
    
    choices <- c()
    labels <- c()
    
    if (!is.null(c_buddy)) {
      choices <- c(choices, "country")
      labels <- c(labels, "Matched by Country")
    }
    if (!is.null(p_buddy)) {
      choices <- c(choices, "program")
      labels <- c(labels, "Matched by Program")
    }
    
    if (length(choices) > 0) {
      named_choices <- setNames(choices, labels)
      radioButtons("selected_match", "Select Your Buddy:", choices = named_choices)
    }
  })
  
  observeEvent(input$assign_match, {
    if (is.null(input$selected_match) || input$student_name == "") {
      showModal(modalDialog(
        title = "Selection Required",
        "Please select a match before assigning.",
        easyClose = TRUE
      ))
      return()
    }
    
    sid_query <- paste0("SELECT student_id FROM students WHERE LOWER(name) = LOWER('", input$student_name, "') LIMIT 1")
    sid <- dbGetQuery(con, sid_query)$student_id[1]
    
    buddy_data <- if (input$selected_match == "country") buddy_country() else buddy_program()
    bid <- buddy_data$buddy_id
    
    tryCatch({
      dbExecute(con, paste0(
        "INSERT INTO matches (student_id, buddy_id, status) VALUES (",
        sid, ", ", bid, ", 'Matched')"
      ))
      
      showModal(modalDialog(
        title = tags$h2("🎉 Match Successful!"),
        tags$p("You have been successfully matched with your Buddy!"),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Database Error",
        paste("Failed to assign match:", e$message),
        easyClose = TRUE
      ))
    })
  })
}

shinyApp(ui = ui, server = server)