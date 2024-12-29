#' Returns the students responses to questions as a data.frame.
getStudentResponses <- function(srl) {
	studentdf <- data.frame(question = character(), 
							domain = character(),
							answer = character(),
							score = integer(),
							stringsAsFactors = FALSE)
	stopifnot(nrow(srl) == 1)
	tmp <- srl$itemGroups[[1]]
	for(itemGroup in seq_len(nrow(tmp))) { # Loop through item groups
		tmp2 <- tmp[itemGroup,]$items[[1]]
		for(item in seq_len(nrow(tmp2))) {
			ans <- tmp2[item,]$possibleItemAnswers[[1]][tmp2[item,]$possibleItemAnswers[[1]]$`_id` == tmp2[item,]$chosenItemAnswerId, ]
			studentdf <- rbind(studentdf, data.frame(
				question = tmp2[item,]$question,
				domain = tmp2[item,]$domainId,
				answer = ans$content,
				score = ans$score,
				stringsAsFactors = FALSE) )
		}
	}
	return(studentdf)
}
