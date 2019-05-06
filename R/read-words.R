#' Read word sheet
#'
#' @param topic Character string naming one of the topics
#' available from the sheet. Omit argument to get a list of those topics.
#'
#' @export
read_words_from_sheet <- function(topic = NULL) {
  key <- "1kQkXjkQFQctKWBplsAqDkI3oxczQDADUCA5hc1Gcn60"
  Words <- gs_read(gs_key(key))
  names(Words) <- c("time", "topic", "words", "who")
  topic_set  <- unique(Words$topic)
  if (is.null(topic)) {
    return(topic_set)
  } else {
    if ( ! topic %in% topic_set)
      stop(topic, "must be one of", paste(topic_set, collapse = ",  "))
  }

  Words %>%
    mutate(words = strsplit(as.character(words), "\n")) %>%
    unnest(words) %>%
    mutate(stars = nchar(gsub("[^\\*]", "", words))) %>%
    mutate(words = gsub("[ \\*]\\*+", "", words)) -> foo
  Unique_words <- foo %>%
    filter( ! grepl("^[ \t]*$", words)) %>% # no blank lines
    group_by(topic, words) %>%
    summarize(total = sum(stars),  ave = mean(stars))  %>%
    mutate(words = gsub("^[ \t]+", "", words)) %>%
    mutate(words = gsub("[ \t]+$", "", words)) %>%
    arrange(desc(total)) %>%
    ungroup() %>%
    dplyr::select(-topic)



}
