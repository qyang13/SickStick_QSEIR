library(DT)

# Table tab summarizing raw data in tables
table_tab <- tabPanel(title = "Result Summary", value = "table_tab",
                     h3("Data Summary without SickStick"),
                     fluidRow(dataTableOutput("summary_table_nm",height = 80),style = "height:300px; overflow-y: scroll;overflow-x: scroll;"),
                     h3("Data Summary with SickStick"),
                     fluidRow(dataTableOutput("summary_table_ss",height = 80),style = "height:300px; overflow-y: scroll;overflow-x: scroll;")
)