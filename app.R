library(shiny)
library(DT)
library(dplyr)
library(shinythemes)  # Styling

city_data <- read.csv("global_cities_index_2024_1000.csv", stringsAsFactors = FALSE)

normalize_weights <- function(weights) {
  total_weight <- sum(weights)
  if (total_weight > 0) {
    normalized_weights <- weights / total_weight
  } else {
    # If total_weight is zero, return zeros
    normalized_weights <- rep(0, length(weights))
  }
  return(normalized_weights)
}

calculate_scores <- function(data, weights, utility_exponent_alpha) {
  # Apply Utility Exponent
  data <- data %>%
    mutate(
      Economics_adj = (1001 - Economics)^(utility_exponent_alpha),
      Human_Capital_adj = (1001 - Human.Capital)^(utility_exponent_alpha),
      Quality_of_Life_adj = (1001 - Quality.of.Life)^(utility_exponent_alpha),
      Environment_adj = (1001 - Environment)^(utility_exponent_alpha),
      Governance_adj = (1000 - Governance)^(utility_exponent_alpha)
    )
  
  # Calculate the score
  data <- data %>%
    mutate(
      Score = weights[1] * Economics_adj +
        weights[2] * Human_Capital_adj +
        weights[3] * Quality_of_Life_adj +
        weights[4] * Environment_adj +
        weights[5] * Governance_adj
    )
  # Rank the data
  data <- data %>%
    arrange(-Score, City) %>%
    mutate(Rank = row_number())
  return(data)
}

process_questionnaire_responses <- function(input) {
  # Collect responses and compute weights
  econ_scores <- c(
    as.numeric(input$econ_q1),
    as.numeric(input$econ_q2),
    as.numeric(input$econ_q3),
    as.numeric(input$econ_q4),
    as.numeric(input$econ_q5)
  )
  hc_scores <- c(
    as.numeric(input$hc_q1),
    as.numeric(input$hc_q2),
    as.numeric(input$hc_q3),
    as.numeric(input$hc_q4),
    as.numeric(input$hc_q5),
    as.numeric(input$hc_q6)
  )
  qol_scores <- c(
    as.numeric(input$qol_q1),
    as.numeric(input$qol_q2),
    as.numeric(input$qol_q3),
    as.numeric(input$qol_q4),
    as.numeric(input$qol_q5),
    as.numeric(input$qol_q6)
  )
  env_scores <- c(
    as.numeric(input$env_q1),
    as.numeric(input$env_q2),
    as.numeric(input$env_q3),
    as.numeric(input$env_q4),
    as.numeric(input$env_q5)
  )
  gov_scores <- c(
    as.numeric(input$gov_q1),
    as.numeric(input$gov_q2),
    as.numeric(input$gov_q3),
    as.numeric(input$gov_q4)
  )
  
  econ_weight <- mean(econ_scores)
  hc_weight <- mean(hc_scores)
  qol_weight <- mean(qol_scores)
  env_weight <- mean(env_scores)
  gov_weight <- mean(gov_scores)
  
  # Prepare weights vector
  weights <- c(econ_weight, hc_weight, qol_weight, env_weight, gov_weight)
  
  # Normalize weights
  normalized_weights <- normalize_weights(weights)
  
  # Process Utility Function questions to set 'utility_exponent_alpha'
  uf_scores <- c(
    as.numeric(input$uf_q1),
    as.numeric(input$uf_q2),
    as.numeric(input$uf_q3),
    as.numeric(input$uf_q4),
    as.numeric(input$uf_q5),
    as.numeric(input$uf_q6)
  )
  uf_average <- mean(uf_scores)
  
  # Map uf_average to utility_exponent_alpha
  # Maps the outcome of the survey into the utility function range
  utility_exponent_alpha <- 2 - (uf_average * 1.9)  
  
  # Return weights and utility exponent
  list(
    weights = normalized_weights,
    utility_exponent_alpha = utility_exponent_alpha
  )
}

# Likert scale choices
likert_choices <- c(
  "Strongly Disagree" = 1,
  "Disagree" = 2,
  "Neutral" = 3,
  "Agree" = 4,
  "Strongly Agree" = 5
)

ui <- fluidPage(
  theme = shinytheme("cerulean"),  
  withMathJax(),
  titlePanel("City Rankings"),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Home",
      mainPanel(
        h3("Welcome to the City Rankings Tool"),
        p("Please choose an option below:"),
        actionButton(
          "go_to_questionnaire",
          "State my preferences via questionnaire"
        ),
        br(),
        br(),
        actionButton("skip_to_sliders", "Skip to the sliders and rankings"),
        br(),
        br(),
        tags$hr(),
        tags$p("Learn more about this tool and dataset"),
        tags$a(
          href = "https://www.kentbutt.com/2024/10/17/city-ranking-tool/",
          "Learn More",
          target = "_blank"
        ),
        br(),
        p(
          "All data courtesy of Oxford Economics Global Cities Index 2024. https://www.oxfordeconomics.com/global-cities-index/"
        ),
        p(
          "Tool created by Kent Butt. Please contact me at kent@kentbutt.com if anything is broken or if you liked the tool."
        )
      )
    ),
    tabPanel(
      "Questionnaire",
      mainPanel(
        h3("Questionnaire"),
        p(
          "Note: All these questions were created arbitrarily, and there’s no proper social science theory behind using them to rank cities. Consider this pet project of mine to have roughly the same accuracy as a Buzzfeed 'Which Hogwarts House Are You' quiz."
        ),
        tags$hr(),
        h4("Economics"),
        radioButtons(
          "econ_q1",
          "1. I prefer to live in an economically powerful (high GDP) city.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "econ_q2",
          "2. I prioritize a city that’s projected to grow (economically) in the future.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "econ_q3",
          "3. I prefer to live in a city where more people are working (high employment rate).",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "econ_q4",
          "4. I prefer cities with stable economies instead of boom and bust cycles.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "econ_q5",
          "5. I prefer cities with a diverse economy rather than ones dominated by a single industry.",
          choices = likert_choices,
          selected = 3
        ),
        tags$hr(),
        h4("Human Capital"),
        radioButtons(
          "hc_q1",
          "1. I'd prefer to live in a city with a growing population.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "hc_q2",
          "2. I would prefer to live in a city with a younger population (i.e., lower ratio of elderly residents).",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "hc_q3",
          "3. The presence of top-ranked universities in a city is important to me.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "hc_q4",
          "4. I am more likely to move to a city if it has several major corporate headquarters.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "hc_q5",
          "5. I prefer to live in a city that has a highly educated population.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "hc_q6",
          "6. I value cities with a high proportion of foreign-born residents.",
          choices = likert_choices,
          selected = 3
        ),
        tags$hr(),
        h4("Quality of Life"),
        radioButtons(
          "qol_q1",
          "1. Income equality is more important to me than overall income in a city.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "qol_q2",
          "2. I care about the average income per person when evaluating where to live.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "qol_q3",
          "3. I want a city where the cost of living (for the average person) is a small share of income.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "qol_q4",
          "4. I would prefer to live in a city with a higher life expectancy.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "qol_q5",
          "5. Fast and reliable internet speeds are important to my quality of life.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "qol_q6",
          "6. I am more likely to live in a city with many recreational and cultural sites.",
          choices = likert_choices,
          selected = 3
        ),
        tags$hr(),
        h4("Environment"),
        radioButtons(
          "env_q1",
          "1. A city's air quality is a major factor in deciding if I want to live there.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "env_q2",
          "2. I'd rather live in a city with lower CO2 emissions (per person).",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "env_q3",
          "3. I want to avoid cities with frequent natural disasters, if I can.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "env_q4",
          "4. I prefer the temperature to be more consistent year-round, instead of having large seasonal changes in temperature.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "env_q5",
          "5. I prefer places with a consistent amount of rain, instead of places with a dry season and a wet season.",
          choices = likert_choices,
          selected = 3
        ),
        tags$hr(),
        h4("Governance"),
        radioButtons(
          "gov_q1",
          "1. The quality of institutions (e.g., rule of law) is a key factor in deciding where to live.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "gov_q2",
          "2. I prioritize political stability when choosing a city to live in.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "gov_q3",
          "3. The business environment (ease of doing business, low corruption) is important to me when evaluating where to live.",
          choices = likert_choices,
          selected = 3
        ),
        radioButtons(
          "gov_q4",
          "4. Civil liberties (political rights, freedom of expression) are a key factor in deciding where I would live.",
          choices = likert_choices,
          selected = 3
        ),
        tags$hr(),
        h4("Utility Function"),
        radioButtons(
          "uf_q1",
          "1. Do you typically prefer a city that’s decent in all factors, or a city that’s exceptional in one or two areas but weaker in others?",
          choices = c(
            "I prefer a city that’s decent in all factors." = 0,
            "I prefer a city that excels in one or two areas, even if it’s weaker in others." = 1
          ),
          selected = 0
        ),
        radioButtons(
          "uf_q2",
          "2. How much does a city’s performance in one factor (like air quality or governance) need to compensate for weaknesses in other areas for it to still be attractive to you?",
          choices = c(
            "A single exceptional factor can make up for weaknesses in others." = 1,
            "I consider a better score in one factor roughly makes up for a worse score in another." = 0.5,
            "A city’s weaknesses can significantly impact my perception, even if it excels in one factor." = 0
          ),
          selected = 0.5
        ),
        radioButtons(
          "uf_q3",
          "3. If a city performs poorly in one factor (e.g., high cost of living), how much does that negatively affect your willingness to live there, even if the other factors are favorable?",
          choices = c(
            "Not at all" = 0,
            "Somewhat" = 0.5,
            "A lot" = 1
          ),
          selected = 0.5
        ),
        radioButtons(
          "uf_q4",
          "4. If a city is already performing very well in one area (e.g., income levels or quality of life), how much more does further improvement in that area increase your preference for that city?",
          choices = c(
            "No additional preference" = 0,
            "Somewhat greater preference" = 0.5,
            "Much greater preference" = 1
          ),
          selected = 0.5
        ),
        radioButtons(
          "uf_q5",
          "5. Would you prefer a city that’s great in one key area important to you, or one that’s above average in all areas?",
          choices = c(
            "I prefer a city that’s great in one key area." = 1,
            "I prefer a city that’s above average in all areas." = 0
          ),
          selected = 0.5
        ),
        radioButtons(
          "uf_q6",
          "6. How willing are you to tolerate a city’s poor performance in one area (e.g., governance, air quality) if the city is excellent in everything else?",
          choices = c(
            "Not willing at all" = 0,
            "Somewhat willing" = 0.5,
            "Very willing" = 1
          ),
          selected = 0.5
        ),
        tags$hr(),
        actionButton("submit_questionnaire", "Submit"),
        br(),
        br(),
        tags$hr(),
        tags$p("Learn more about this tool and dataset"),
        tags$a(
          href = "https://www.kentbutt.com/2024/10/17/city-ranking-tool/",
          "Learn More",
          target = "_blank"
        ),
        br(),
        p(
          "All data courtesy of Oxford Economics Global Cities Index 2024. https://www.oxfordeconomics.com/global-cities-index/"
        ),
        p(
          "Tool created by Kent Butt. Please contact me at kent@kentbutt.com if anything is broken or if you liked the tool."
        )
      )
    ),
    tabPanel(
      "Sliders and Rankings",
      sidebarLayout(
        sidebarPanel(
          helpText(HTML("
                    <p><strong>Marginal Utility</strong> represents how much additional benefit a one-unit increase in a good provides.</p>
                    <p>It effectively changes how the averaging function handles extreme values.</p>
                    <p>Values of \\(\\alpha\\) close to 0 indicate decreasing marginal utility, \\(\\alpha = 1\\) represents linear utility (no change), and values of \\(\\alpha\\) above 1 represent increasing marginal utility.</p>
                    <p>The functional form used is $$\\text{Score} = \\sum_{i=1}^{5} w_i v_i^{\\alpha},$$ where:</p>
                    <ul>
                      <li>\\(\\mathbf{w}\\) is the vector of weights,</li>
                      <li>\\(\\mathbf{v}\\) is the vector of city's scores in each of the five categories. A city gets 1000 points in a category for being the best in the world, down to 1 point for being the worst.</li>
                      <li>\\(\\alpha\\) is the utility exponent.</li>
                    </ul>
                    <p> Cities are then ranked according to score.</p>
                  ")),
          # Marginal Utility Slider and Reset Button
          sliderInput(
            "utility_exponent_alpha",
            "Utility Exponent (\\(\\alpha\\))",
            min = 0.1,
            max = 2,
            value = 1,
            step = 0.1
          ),
          actionButton(
            "reset_utility_exponent_button",
            "Reset to Linear Utility (\\(\\alpha = 1\\))"
          ),
          br(),
          # Weight Sliders
          sliderInput(
            "weight_economics",
            "Economics Weight",
            min = 0,
            max = 1,
            value = 0.3,
            step = 0.01
          ),
          sliderInput(
            "weight_human_capital",
            "Human Capital Weight",
            min = 0,
            max = 1,
            value = 0.25,
            step = 0.01
          ),
          sliderInput(
            "weight_quality_of_life",
            "Quality of Life Weight",
            min = 0,
            max = 1,
            value = 0.25,
            step = 0.01
          ),
          sliderInput(
            "weight_environment",
            "Environment Weight",
            min = 0,
            max = 1,
            value = 0.1,
            step = 0.01
          ),
          sliderInput(
            "weight_governance",
            "Governance Weight",
            min = 0,
            max = 1,
            value = 0.1,
            step = 0.01
          ),
          # Reset Buttons for Weights
          actionButton("reset_to_default_weights_button", "Reset to Default Weights"),
          actionButton("set_equal_weights_button", "Set Equal Weights"),
          actionButton("normalize_weights_button", "Normalize weights to sum to 1"),
          br(),
          # Return to Questionnaire Button
          actionButton("return_to_questionnaire", "Return to Questionnaire"),
          br()
        ),
        mainPanel(
          dataTableOutput("rankings_table"),
          br(),
          tags$hr(),
          tags$p("Learn more about this tool and dataset"),
          tags$a(
            href = "http://kentbutt.com/city-utility-tool-global",
            "Learn More",
            target = "_blank"
          ),
          br(),
          p(
            "All data courtesy of Oxford Economics Global Cities Index 2024. https://www.oxfordeconomics.com/global-cities-index/"
          ),
          p(
            "Tool created by Kent Butt. Please contact me at kent@kentbutt.com if anything is broken or if you liked the tool."
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Observe 'go_to_questionnaire' button
  observeEvent(input$go_to_questionnaire, {
    updateTabsetPanel(session, "main_tabs", selected = "Questionnaire")
  })
  
  # Observe 'skip_to_sliders' button
  observeEvent(input$skip_to_sliders, {
    updateTabsetPanel(session, "main_tabs", selected = "Sliders and Rankings")
    # Set the default weights (optional)
  })
  
  # Observe 'Return to questionnaire' button
  observeEvent(input$return_to_questionnaire, {
    updateTabsetPanel(session, "main_tabs", selected = "Questionnaire")
  })
  
  # Observe 'submit_questionnaire' button
  observeEvent(input$submit_questionnaire, {
    # Process questionnaire responses
    questionnaire_results <- process_questionnaire_responses(input)
    
    # Update sliders
    updateSliderInput(session, "weight_economics", value = questionnaire_results$weights[1])
    updateSliderInput(session, "weight_human_capital", value = questionnaire_results$weights[2])
    updateSliderInput(session, "weight_quality_of_life", value = questionnaire_results$weights[3])
    updateSliderInput(session, "weight_environment", value = questionnaire_results$weights[4])
    updateSliderInput(session, "weight_governance", value = questionnaire_results$weights[5])
    
    # Update 'utility_exponent_alpha' slider
    updateSliderInput(session, "utility_exponent_alpha", value = questionnaire_results$utility_exponent_alpha)
    
    # Switch to 'Sliders and Rankings' tab
    updateTabsetPanel(session, "main_tabs", selected = "Sliders and Rankings")
  })
  
  # Observe the 'Reset' button for Utility Exponent
  observeEvent(input$reset_utility_exponent_button, {
    updateSliderInput(session, "utility_exponent_alpha", value = 1)
  })
  
  # Observe the 'Reset to Default Weights' Button
  observeEvent(input$reset_to_default_weights_button, {
    default_weights <- c(0.3, 0.25, 0.25, 0.1, 0.1)
    updateSliderInput(session, "weight_economics", value = default_weights[1])
    updateSliderInput(session, "weight_human_capital", value = default_weights[2])
    updateSliderInput(session, "weight_quality_of_life", value = default_weights[3])
    updateSliderInput(session, "weight_environment", value = default_weights[4])
    updateSliderInput(session, "weight_governance", value = default_weights[5])
  })
  
  # Observe the 'Normalize Weights' Button
  observeEvent(input$normalize_weights_button, {
    weights <- c(
      input$weight_economics,
      input$weight_human_capital,
      input$weight_quality_of_life,
      input$weight_environment,
      input$weight_governance
    )
    normalized_weights <- normalize_weights(weights)
    updateSliderInput(session, "weight_economics", value = normalized_weights[1])
    updateSliderInput(session, "weight_human_capital", value = normalized_weights[2])
    updateSliderInput(session, "weight_quality_of_life", value = normalized_weights[3])
    updateSliderInput(session, "weight_environment", value = normalized_weights[4])
    updateSliderInput(session, "weight_governance", value = normalized_weights[5])
  })
  
  # Observe the 'Set Equal Weights' Button
  observeEvent(input$set_equal_weights_button, {
    equal_weight <- 1 / 5
    updateSliderInput(session, "weight_economics", value = equal_weight)
    updateSliderInput(session, "weight_human_capital", value = equal_weight)
    updateSliderInput(session, "weight_quality_of_life", value = equal_weight)
    updateSliderInput(session, "weight_environment", value = equal_weight)
    updateSliderInput(session, "weight_governance", value = equal_weight)
  })
  
  # Reactive expression to calculate the rankings
  calculated_rankings <- reactive({
    # Get the weights
    weights <- c(
      input$weight_economics,
      input$weight_human_capital,
      input$weight_quality_of_life,
      input$weight_environment,
      input$weight_governance
    )
    
    # Normalize weights
    normalized_weights <- normalize_weights(weights)
    
    if (all(normalized_weights == 0)) {
      # All weights zero, return rank 1000 for all cities
      result <- city_data %>%
        arrange(City) %>%
        mutate(Score = 1000, Rank = 1000)
      return(result)
    }
    
    # Calculate scores
    data <- calculate_scores(city_data, normalized_weights, input$utility_exponent_alpha)
    
    data
  })
  
  # Render the table
  output$rankings_table <- renderDataTable({
    data <- calculated_rankings()
    # Display the rankings table with scores rounded to two decimal places
    datatable(
      data %>%
        select(Rank, City, Country),
      options = list(pageLength = 25),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)