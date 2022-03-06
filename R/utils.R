#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
load_mat <- function(infile){
  in.dt <- data.table::fread(infile, header = TRUE)
  in.dt <- in.dt[!duplicated(in.dt[, 1]), ]
  in.mat <- as.matrix(in.dt[, -1, with = FALSE])
  rownames(in.mat) <- unlist(in.dt[, 1, with = FALSE])
  in.mat
}


#' Combine questions and weights to form a key
#'
#'
#' @param questions tibble of questions
#' @param weights tibble of weights
#'
#' @return key tibble of keys with weights attributed to question ids
#' @export
create_key = function(questions, values){
  key = questions %>% inner_join(values, by =c("pvq_type", "id"))

    return(key)
}


#' Prepare questions as a list for dynamic generation of form
#'
#'
#' @param questions_tb a tibble of questions. Needs id and question columns
#'
#' @return list_form a list for the form
#' @export
convert_to_list = function(questions_tb){
  required = c("id","question")
  if (any(colnames(questions_tb) != required)) {
    ids = which(!c("id","question") %in% colnames(questions_tb))
    stop(paste("Columns in the questions_tb are wrong. Missing:", required[ids]))
  }
  list_form = list()

  for (q_id in questions_tb$id){
    question = questions_tb %>% filter(id == q_id) %>% pull(question)

    new = list(id = q_id,
               type = "multiplechoice",
               title = question,
               mandatory = TRUE,
               choices = list("Pas du tout comme moi",
                              "Pas comme moi",
                              "Un peu comme moi",
                              "Plutôt comme moi",
                              "Comme moi",
                              "Tout à fait comme moi"))
    list_form = append(list(new), list_form)
  }
  return(list_form)

}

#' Load up questions from a csv
#'
#'
#' @param csv_path csv path
#' @param pvq_type either 21 or 40
#' @param lang pick which language
#'
#' @return questions tibble of filtered questions
#' @export
load_questions = function(csv_path="inst/PVQ_questions.csv", pvq_type=21, lang='fr'){
  questions = read_csv(csv_path, col_types = cols()) %>%
    filter(lang == lang)
  if(nrow(questions) == 0){
    stop(paste("No questions loaded, check your filtering criterias\nPVQ type:", pvq_type, "\nLanguage:", lang ))
  }
  return(questions)
}

#' Load up choices from a csv
#'
#'
#' @param csv_path csv path
#' @param lang pick which language
#'
#' @return choices tibble of language filtered choices
#' @export
load_choices = function(csv_path="inst/PVQ_choices.csv", lang='fr'){
  choices = read_csv(csv_path, col_types = cols()) %>%
    filter(lang == lang)
  if(nrow(choices) == 0){
    stop(paste("No questions loaded, check your filtering criterias\nLanguage:", lang ))
  }
  return(choices)
}


#' Load answers in a long format from a csv
#'
#'
#' @param csv_path csv path
#'
#' @return answers
#' @export
load_answers = function(csv_path="inst/tests/tiny_answers.csv"){
  answers = read_csv(csv_path, col_types = cols())
  if(nrow(answers) == 0){
    stop("No answers loaded")
  }
  return(answers)
}

#' Load up values from a csv
#'
#'
#' @param csv_path csv path
#' @param pvq_type either 21 or 40
#'
#' @return values tibble of filtered values
#' @export
load_values = function(csv_path = "inst/PVQ_values.csv", pvq_type=21){
  values = read_csv(csv_path, col_types = cols()) %>%
    filter(pvq_type == pvq_type)
  if(nrow(values) == 0){
    stop(paste("No values loaded, check your filtering criterias\nPVQ type:", pvq_type))
  }
  return(values)
}


#' Calculate scores for plotting
#'
#'
#' @param answers Answers from a form
#' @param values Values tibble
#'
#' @return final_score tibble of scores per value
#' @export
calculate_scores = function(answers, values){
  scores = inner_join(answers, values, by = c("id", "pvq_type"))
  final_score = scores %>%
    mutate(MRAT = mean(score)) %>%
    group_by(attribute) %>%
    summarise(per_value_score = mean(score)-MRAT) %>%
    distinct() %>%
    ungroup()
  return(final_score)
}

#' Generate a radar plot from data
#'
#'
#' @param scores a named vector of results
#'
#' @return ggplot
#' @export
plot_radar = function(scores){
  plot = scores %>%
    mutate(per_value_score = per_value_score + abs(min(per_value_score))) %>%
    mutate(per_value_score = per_value_score/max(per_value_score)) %>%
    mutate(group = "ID") %>%
    pivot_wider(values_from = per_value_score, names_from = attribute) %>%
    ggradar()
  return(plot)
}

