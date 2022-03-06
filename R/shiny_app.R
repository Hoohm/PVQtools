# 
# questions = load_questions(csv_path = "inst/PVQ_multi.csv") %>%
#   select(-lang) %>%
#   rename("id"="pvq_21_id") %>%
#   mutate(id = as.character(id))
# form_list = convert_to_list(questions)
# 
# # Define the first form: basic information
# formInfo <- list(
#   id = "basicinfo",
#   questions = form_list,
#   storage = list(
#     # Right now, only flat file storage is supported
#     type = STORAGE_TYPES$FLATFILE,
#     # The path where responses are stored
#     path = "responses"
#   )
# )
# 
# 
# ui <- fluidPage(
#   h1("Personnal Value Questionnaire"),
#       formUI(formInfo)
# )
# 
# server <- function(input, output, session) {
#   formServer(formInfo)
# }
# 
# shinyApp(ui = ui, server = server)