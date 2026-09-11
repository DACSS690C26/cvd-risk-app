library(shiny)
library(CVrisk)

# ---- Override: same math as CVrisk::ascvd_10y_frs_simple, minus the
# ---- age <- ifelse(age < 30 | age > 74, NA, age) line that caps the input.
# ---- NOTE: ages outside 30-74 are extrapolation beyond the model's
# ---- validated range. Output is still clamped to 1-30% (as in the original).
frs_simple_uncapped <- function(gender, age, bmi, sbp, bp_med, smoker, diabetes) {
  gender <- tolower(gender)
  
  frs_simple_coef <- NULL
  utils::data(frs_simple_coef, package = "CVrisk", envir = environment())
  
  sex_df <- data.frame(gender)
  sex_df$id <- as.numeric(row.names(sex_df))
  model_coef <- merge(sex_df, frs_simple_coef)
  model_coef <- model_coef[order(model_coef$id), ]
  
  sbp_treated   <- ifelse(bp_med == 1, sbp, 1)
  sbp_untreated <- ifelse(bp_med == 0, sbp, 1)
  
  indv_sum <- log(age) * model_coef$ln_age +
    log(bmi) * model_coef$ln_bmi +
    log(sbp_treated) * model_coef$ln_treated_sbp +
    log(sbp_untreated) * model_coef$ln_untreated_sbp +
    smoker * model_coef$smoker +
    diabetes * model_coef$diabetes
  
  risk_score <- round((1 - (model_coef$baseline_survival^
                              exp(indv_sum - model_coef$group_mean))) * 100.000, 2)
  
  ifelse(risk_score < 1, 1, ifelse(risk_score > 30, 30, risk_score))
}

ui <- fluidPage(
  titlePanel("10-Year ASCVD Risk (Framingham Simple)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = c("male", "female")),
      numericInput("age", "Age", value = 55, min = 1, max = 99),
      numericInput("bmi", "BMI", value = 30, min = 10, max = 60),
      numericInput("sbp", "Systolic BP (mmHg)", value = 140, min = 90, max = 200),
      checkboxInput("bp_med", "On blood pressure medication", value = FALSE),
      checkboxInput("smoker", "Smoker", value = FALSE),
      checkboxInput("diabetes", "Diabetes", value = FALSE)
    ),
    
    mainPanel(
      h3("Estimated 10-year risk:"),
      h1(textOutput("risk")),
      uiOutput("age_note")
    )
  )
)

server <- function(input, output) {
  output$risk <- renderText({
    score <- frs_simple_uncapped(
      gender   = input$gender,
      age      = input$age,
      bmi      = input$bmi,
      sbp      = input$sbp,
      bp_med   = as.numeric(input$bp_med),
      smoker   = as.numeric(input$smoker),
      diabetes = as.numeric(input$diabetes)
    )
    paste0(round(score, 1), "%")
  })
  
  # Flag when the input is outside the model's validated 30-74 range
  output$age_note <- renderUI({
    if (input$age < 30 || input$age > 74) {
      tags$p(style = "color:#b00; font-size:0.9em;",
             "Note: age is outside the model's validated 30-74 range; ",
             "this value is an extrapolation.")
    }
  })
}

shinyApp(ui = ui, server = server)