library(stringr)
library(reshape2)
library(dplyr)
library(tidyverse)
source('analyses/R/getStudentResponses.R')

# load('data/daacs.umgc.rda')
data_raw_dir <- 'data-raw/umgc/'
rda_files <- list.files(data_raw_dir, pattern = '.rda')
rda_file <- paste0(data_raw_dir, rda_files[length(rda_files)])
load(rda_file)

### filter only students

# Exclude non-students

## this filter probably needs more work -- there are contaminators (e.g., Angela's students, Grad students...)
## For UMGC, it is hard to filter out all coahces and instructors. 
institution <- 'umgc'

remove.umgc <- c("jason@bryer.org", "admin", "ms.angelalui@gmail.com", "alui@albany.edu",
				 "David.Franklin@cuny.edu", "allison.patch@umgc.edu", "darragh.mcnally@umgc.edu",
				 "kathleen.hogan@umgc.edu", "stephanie.blaher@umgc.edu", 
				 "susan.hawkins-wilding@umgc.edu")

remove.ua <- c("da396741", "kr919691", "jm4660", "ha207", "df219164", "al428432", "or144957", "cy159679")

users <- users |>
	filter(roles == "ROLE_STUDENT") |>
	filter(!username %in% remove.umgc) # remove non-advisors


# filter to IES years

# UAlbany
user_assessments <- user_assessments |>
	filter(takenDate > "2022-05-05") |>
	filter(username %in% users$username)

# UMGC
user_assessments <- user_assessments |>
	filter(takenDate > "2022-08-16") |>
	filter(username %in% users$username)

##### Mathematics ##############################################################
math <- user_assessments[user_assessments$assessmentCategory == 'MATHEMATICS',]
math <- math[math$assessmentCategoryGroupId == 'mathematics',]
math <- math[math$assessmentType == 'CAT',]
math <- math[math$status == 'GRADED',]

pb <- txtProgressBar(min = 0, max = nrow(math), style = 3)
math.results <- data.frame(
	userId = rep(NA_character_, nrow(math)),
	attempt = rep(NA_integer_, nrow(math)),
	word_problems = rep(NA_real_, nrow(math)),
	lines_and_functions = rep(NA_real_, nrow(math)),
	variables_and_equations = rep(NA_real_, nrow(math)),
	number_and_calculation = rep(NA_real_, nrow(math)),
	statistics = rep(NA_real_, nrow(math)),
	geometry = rep(NA_real_, nrow(math)),
	mathTotal = rep(NA_real_, nrow(math)),
	mathStartDate = rep(as.POSIXct(NA), nrow(math)),
	mathCompletionDate = rep(as.POSIXct(NA), nrow(math)),
	stringsAsFactors = FALSE
)
math.items <- data.frame()
for(i in seq_len(nrow(math))) {
	id <- math[i,]$userId
	attempt <- length(which(math.results$userId == id)) + 1
	items <- getStudentResponses(math[i,])
	domains <- math[i,]$domainScores[[1]][,1:3]
	
	math.results[i,]$userId <- id
	math.results[i,]$attempt <- attempt
	
	math.results[i,]$word_problems <- mean(items[items$domain == 'word_problems',]$score)
	math.results[i,]$lines_and_functions <- mean(items[items$domain == 'lines_and_functions',]$score)
	math.results[i,]$variables_and_equations <- mean(items[items$domain == 'variables_and_equations',]$score)
	math.results[i,]$number_and_calculation <- mean(items[items$domain == 'number_and_calculation',]$score)
	math.results[i,]$statistics <- mean(items[items$domain == 'statistics',]$score)
	math.results[i,]$geometry <- mean(items[items$domain == 'geometry',]$score)
	
	# math.results[i,]$word_problems <- domains[domains$domainId == 'word_problems',]$rawScore
	# math.results[i,]$lines_and_functions <- domains[domains$domainId == 'lines_and_functions',]$rawScore
	# math.results[i,]$variables_and_equations <- domains[domains$domainId == 'variables_and_equations',]$rawScore
	# math.results[i,]$number_and_calculation <- domains[domains$domainId == 'number_and_calculation',]$rawScore
	# math.results[i,]$statistics <- domains[domains$domainId == 'statistics',]$rawScore
	# math.results[i,]$geometry <- domains[domains$domainId == 'geometry',]$rawScore
	
	math.results[i,]$mathTotal <- sum(items$score) / nrow(items)
	math.results[i,]$mathStartDate <- math[i,]$takenDate
	math.results[i,]$mathCompletionDate <- math[i,]$completionDate
	
	items <- math$itemGroups[[i]]$items
	for(j in seq_len(length(items))) {
		item <- items[[j]] # This is one item block of 6 items
		student.math.items <- data.frame()
		for(k in seq_len(nrow(item))) {
			selected <- which(item[k,]$chosenItemAnswerId == item[k,]$possibleItemAnswers[[1]]$`_id`)
			score <- item[k,]$possibleItemAnswers[[1]]$score[selected]
			student.math.items <- rbind(student.math.items,
										data.frame(userId = id,
												   attempt = attempt,
												   block = j,
												   difficulty = math$itemGroups[[i]]$difficulty[j],
												   question = item$itemContent[[1]]$content[k],
												   domain = item$domainId[k],
												   answer = selected,
												   score = score,
												   startDate = as.POSIXct(item[k,]$startDate, origin = '1970-01-01'),
												   endDate = as.POSIXct(item[k,]$completeDate, origin = '1970-01-01'),
												   stringsAsFactors = FALSE),
										stringsAsFactors = FALSE)
		}
		math.items <- rbind(math.items, student.math.items, stringsAsFactors = FALSE)
	}
	
	setTxtProgressBar(pb, i)
}
close(pb)

math.items.questions <- data.frame(question = unique(math.items$question), stringsAsFactors = FALSE)
math.items.questions$qid <- paste0('Q', str_pad(1:nrow(math.items.questions), width = 3, side = 'left', pad = '0'))
math.items <- merge(math.items, math.items.questions, by = 'question', all.y = TRUE)

rows <- math.items$attempt == 1
math.items.score <- reshape2::dcast(math.items[rows,], userId ~ qid, value.var = 'score',
						  fun.aggregate = function(x) { if(length(x) > 1) stop(paste0('Too many answers: ', length(x))) else x[1] })
math.items.response <- reshape2::dcast(math.items[rows,], userId ~ qid, value.var = 'answer',
							 fun.aggregate = function(x) { if(length(x) > 1) stop(paste0('Too many answers: ', length(x))) else x[1] })



##### Reading ##################################################################
read <- user_assessments[user_assessments$assessmentCategory == 'READING',]
read <- read[read$status == 'GRADED',]

pb <- txtProgressBar(min = 0, max = nrow(read), style = 3)
read.results <- data.frame(
	userId = rep(NA_character_, nrow(read)),
	attempt = rep(NA_integer_, nrow(read)),
	structure = rep(NA_real_, nrow(read)),
	inference = rep(NA_real_, nrow(read)),
	language = rep(NA_real_, nrow(read)),
	purpose = rep(NA_real_, nrow(read)),
	ideas = rep(NA_real_, nrow(read)),
	readTotal = rep(NA_real_, nrow(read)),
	readStartDate = rep(as.POSIXct(NA), nrow(read)),
	readCompletionDate = rep(as.POSIXct(NA), nrow(read)),
	stringsAsFactors = FALSE
)
read.items <- data.frame()
for(i in seq_len(nrow(read))) {
	id <- read[i,]$userId
	attempt <- length(which(read.results$userId == id)) + 1
	items <- getStudentResponses(read[i,])
	if(any(is.na(items$score))) {
		warning(paste0('Row ', i, ' for student id ', id, ' has NA scores'))
		# items <- items[complete.cases(items),]
		next;
	}
	domains <- read[i,]$domainScores[[1]][,1:3]
	
	structure <- mean(items[items$domain == 'structure',]$score)
	inference <- mean(items[items$domain == 'inference',]$score)
	language <- mean(items[items$domain == 'language',]$score)
	purpose <- mean(items[items$domain == 'purpose',]$score)
	ideas <- mean(items[items$domain == 'ideas',]$score)
	
	# structure <- domains[domains$domainId == 'structure',]$rawScore
	# inference <- domains[domains$domainId == 'inference',]$rawScore
	# language <- domains[domains$domainId == 'language',]$rawScore
	# purpose <- domains[domains$domainId == 'purpose',]$rawScore
	# ideas <- domains[domains$domainId == 'ideas',]$rawScore
	
	read.results[i,]$userId <- id
	read.results[i,]$attempt <- attempt
	read.results[i,]$structure <- ifelse(is.nan(structure), NA, structure)
	read.results[i,]$inference <- ifelse(is.nan(inference), NA, inference)
	read.results[i,]$language <- ifelse(is.nan(language), NA, language)
	read.results[i,]$purpose <- ifelse(is.nan(purpose), NA, purpose)
	read.results[i,]$ideas <- ifelse(is.nan(ideas), NA, ideas)
	read.results[i,]$readTotal <- sum(items$score) / nrow(items)
	read.results[i,]$readStartDate <- read[i,]$takenDate
	read.results[i,]$readCompletionDate <- read[i,]$completionDate
	
	items <- read$itemGroups[[i]]$items
	for(j in seq_len(length(items))) {
		item <- items[[j]] # This is one item block of 6 items
		student.read.items <- data.frame()
		for(k in seq_len(nrow(item))) {
			selected <- which(item[k,]$chosenItemAnswerId == item[k,]$possibleItemAnswers[[1]]$`_id`)
			score <- item[k,]$possibleItemAnswers[[1]]$score[selected]
			student.read.items <- rbind(student.read.items,
										data.frame(userId = id,
												   attempt = attempt,
												   block = j,
												   difficulty = read$itemGroups[[i]]$difficulty[j],
												   question = item[k,]$question,
												   domain = item$domainId[k],
												   answer = selected,
												   score = score,
												   startDate = as.POSIXct(item[k,]$startDate, origin = '1970-01-01'),
												   endDate = as.POSIXct(item[k,]$completeDate, origin = '1970-01-01'),
												   stringsAsFactors = FALSE),
										stringsAsFactors = FALSE)
		}
		read.items <- rbind(read.items, student.read.items, stringsAsFactors = FALSE)
	}
	
	setTxtProgressBar(pb, i)
}
close(pb)

read.items.questions <- data.frame(question = unique(read.items$question), stringsAsFactors = FALSE)
read.items.questions$qid <- paste0('Q', str_pad(1:nrow(read.items.questions), width = 3, side = 'left', pad = '0'))
read.items <- merge(read.items, read.items.questions, by = 'question', all.y = TRUE)

rows <- read.items$attempt == 1
read.items.score <- dcast(read.items[rows,], userId ~ qid, value.var = 'score',
						  fun.aggregate = function(x) { if(length(x) > 1) stop(paste0('Too many answers: ', length(x))) else x[1] })
read.items.response <- dcast(read.items[rows,], userId ~ qid, value.var = 'answer',
							 fun.aggregate = function(x) { if(length(x) > 1) stop(paste0('Too many answers: ', length(x))) else x[1] })


##### Self-Regulated Learning ##################################################

### To Do: Update to include the self-efficacy domain and subdomains
### and to remove self-efficacy as a subdomain under motivation. (5/18 AL)

srl <- user_assessments %>%
	filter(assessmentCategory == 'COLLEGE_SKILLS') %>%
	filter(status == "GRADED")


pb <- txtProgressBar(min = 0, max = nrow(srl), style = 3)
srl.results <- data.frame(
	row.names = 1:nrow(srl),
	userId = rep(NA_character_, nrow(srl)),
	attempt = rep(NA_integer_, nrow(srl)),
	# grit = rep(NA_real_, nrow(srl)),
	strategies = rep(NA_real_, nrow(srl)),
	motivation = rep(NA_real_, nrow(srl)),
	metacognition = rep(NA_real_, nrow(srl)),
	selfefficacy = rep(NA_real_, nrow(srl)),
	managing_time = rep(NA_real_, nrow(srl)),
	help_seeking = rep(NA_real_, nrow(srl)),
	managing_environment = rep(NA_real_, nrow(srl)),
	understanding = rep(NA_real_, nrow(srl)),
	anxiety = rep(NA_real_, nrow(srl)),
	mastery_orientation = rep(NA_real_, nrow(srl)),
	mindset = rep(NA_real_, nrow(srl)),
	# self_efficacy = rep(NA_real_, nrow(srl)),
	evaluation = rep(NA_real_, nrow(srl)),
	monitoring = rep(NA_real_, nrow(srl)),
	planning = rep(NA_real_, nrow(srl)),
	self_efficacy_for_mathematics2 = rep(NA_real_, nrow(srl)),
	selfefficacy_for_online_learning = rep(NA_real_, nrow(srl)),
	selfefficacy_for_reading = rep(NA_real_, nrow(srl)),
	selfefficacy_for_writing = rep(NA_real_, nrow(srl)),
	srlTotal = rep(NA_real_, nrow(srl)),
	srlStartDate = rep(as.POSIXct(NA), nrow(srl)),
	srlCompletionDate = rep(as.POSIXct(NA), nrow(srl)),
	stringsAsFactors = FALSE)
srl.items <- data.frame()
for(i in seq_len(nrow(srl))) {
	id <- srl[i,]$userId
	attempt <- length(which(srl.results$userId == id)) + 1
	items <- getStudentResponses(srl[i,])
	subdomains <- data.frame()
	for(sd in srl[i,]$domainScores[[1]]$subDomainScores) {
		subdomains <- rbind(subdomains, sd)
	}
	
	# Only necessary for the demo.daacs.net site. Early versions of the assessments
	# did not have subdomains.
	# if(nrow(subdomains) == 0 |
	#    !all(c('strategies','motivation','metacognition') %in%
	#    	    domains$domainId) |
	#    !all(c('managing_time','help_seeking',
	# 		 'managing_environment','understanding','anxiety','mastery_orientation',
	# 		 'mindset', 'self_efficacy','evaluation','monitoring','planning') %in%
	# 		 subdomains$domainId)) {
	# 	print(paste0('Skipping row ', i, '...'))
	# 	next
	# }
	
	# TODO: Fix managing environment item:
	# I let electronic devices (e.g., television, cellphones) distract me when I am studying.
	# Almost Never should be 4
	row <- which(items$question == 'I let electronic devices (e.g., television, cellphones) distract me when I am studying.')
	if(length(row) > 0) {
		newscore <- switch(items[row,]$answer,
						   'Almost Never' = 4,
						   'Not Very Often' = 3,
						   'Somewhat Often' = 2,
						   'Pretty Often' = 1,
						   'Almost Always' = 0)
		# scorediff <- newscore - items[row,]$score
		items[row,]$score <- newscore
	}
	
	srl.results[i,]$userId <- id
	srl.results[i,]$attempt <- attempt
	##	srl.results[i,]$grit <- mean(items[items$domain == 'grit',]$score)
	srl.results[i,]$strategies <- mean(items[items$domain %in% srl[i,]$domainScores[[1]][
		srl[i,]$domainScores[[1]] == 'strategies',]$subDomainScores[[1]]$domainId,]$score)
	srl.results[i,]$motivation <- mean(items[items$domain %in% srl[i,]$domainScores[[1]][
		srl[i,]$domainScores[[1]] == 'motivation',]$subDomainScores[[1]]$domainId,]$score)
	srl.results[i,]$metacognition <- mean(items[items$domain %in% srl[i,]$domainScores[[1]][
		srl[i,]$domainScores[[1]] == 'metacognition',]$subDomainScores[[1]]$domainId,]$score)
	srl.results[i,]$selfefficacy <- mean(items[items$domain %in% srl[i,]$domainScores[[1]][
		srl[i,]$domainScores[[1]] == 'selfefficacy',]$subDomainScores[[1]]$domainId,]$score)
	srl.results[i,]$managing_time <- mean(items[items$domain == 'managing_time',]$score)
	srl.results[i,]$help_seeking <- mean(items[items$domain == 'help_seeking',]$score)
	srl.results[i,]$managing_environment <- mean(items[items$domain == 'managing_environment',]$score)
	srl.results[i,]$understanding <- mean(items[items$domain == 'understanding',]$score)
	srl.results[i,]$anxiety <- mean(items[items$domain == 'anxiety',]$score)
	srl.results[i,]$mastery_orientation <- mean(items[items$domain == 'mastery_orientation',]$score)
	srl.results[i,]$mindset <- mean(items[items$domain == 'mindset',]$score)
	# srl.results[i,]$self_efficacy <- mean(items[items$domain == 'self_efficacy',]$score)
	srl.results[i,]$evaluation <- mean(items[items$domain == 'evaluation',]$score)
	srl.results[i,]$monitoring <- mean(items[items$domain == 'monitoring',]$score)
	srl.results[i,]$planning <- mean(items[items$domain == 'planning',]$score)
	srl.results[i,]$self_efficacy_for_mathematics2 <- mean(items[items$domain == 'self_efficacy_for_mathematics2',]$score)
	srl.results[i,]$selfefficacy_for_online_learning <- mean(items[items$domain == 'selfefficacy_for_online_learning',]$score)
	srl.results[i,]$selfefficacy_for_reading <- mean(items[items$domain == 'selfefficacy_for_reading',]$score)
	srl.results[i,]$selfefficacy_for_writing <- mean(items[items$domain == 'selfefficacy_for_writing',]$score)
	
	srl.results[i,]$srlTotal <- mean(items$score)
	srl.results[i,]$srlStartDate <- srl[i,]$takenDate
	srl.results[i,]$srlCompletionDate <- srl[i,]$completionDate
	
	srl.items <- rbind(srl.items,
					   cbind(userId = id, attempt = attempt, row = i, items),
					   stringsAsFactors = FALSE)
	
	setTxtProgressBar(pb, i)
}
close(pb)

srl.item.mapping <- srl.items[!duplicated(srl.items$question), c('question','domain'), drop = FALSE]
srl.item.mapping$questionId <- NA
for(i in unique(srl.item.mapping$domain)) {
	rows <- which(srl.item.mapping$domain == i)
	srl.item.mapping[rows,]$questionId <- paste0(i, 1:length(rows))
}
srl.items <- merge(srl.items, srl.item.mapping[,c('question', 'questionId')], by = 'question', all.x  = TRUE)

srl.items.answer <- dcast(srl.items, userId + attempt ~ question, value.var = 'answer',
						  fun.aggregate = function(x) { if(length(x) > 1) stop('Too many responses') else x[1] })
srl.items.score <- dcast(srl.items, userId + attempt ~ question, value.var = 'score',
						 fun.aggregate = function(x) { if(length(x) > 1) stop('Too many responses') else x[1] })


##### Writing ##################################################################
write <- user_assessments[user_assessments$assessmentCategory == 'WRITING',]
write <- write[write$status == 'GRADED',]

pb <- txtProgressBar(min = 0, max = nrow(write), style = 3)
write.results <- data.frame(
	userId = rep(NA_character_, nrow(write)),
	attempt = rep(NA_integer_, nrow(write)),
	scoringType = rep(NA_character_, nrow(write)),
	summary = rep(NA_character_, nrow(write)),
	suggestions = rep(NA_character_, nrow(write)),
	structure = rep(NA_character_, nrow(write)),
	transitions = rep(NA_character_, nrow(write)),
	ideas = rep(NA_character_, nrow(write)),
	cohesion = rep(NA_character_, nrow(write)),
	correct = rep(NA_character_, nrow(write)),
	complexity = rep(NA_character_, nrow(write)),
	usage = rep(NA_character_, nrow(write)), ## AL added usage
	punctuation = rep(NA_character_, nrow(write)), ## AL added punctuation
	conventions = rep(NA_character_, nrow(write)), ## comment out conventions for now.
	writeStartDate = rep(as.POSIXct(NA), nrow(write)),
	writeCompletionDate = rep(as.POSIXct(NA), nrow(write)),
	stringsAsFactors = FALSE
)
student.essays <- data.frame()
for(i in seq_len(nrow(write))) {
	id <- write[i,]$userId
	attempt <- length(which(write.results$userId == id))
	domains <- write[i,]$domainScores[[1]][,1:3]
	subdomains <- data.frame()
	for(sd in write[i,]$domainScores[[1]]$subDomainScores) {
		subdomains <- rbind(subdomains, sd)
	}
	
	write.results[i,]$userId <- id
	write.results[i,]$attempt <- attempt
	write.results[i,]$scoringType <- write[i,]$scoringType
	write.results[i,]$summary <- subdomains[subdomains$domainId == 'summary',]$rubricScore
	write.results[i,]$suggestions <- subdomains[subdomains$domainId == 'suggestions',]$rubricScore
	write.results[i,]$structure <- subdomains[subdomains$domainId == 'structure',]$rubricScore
	write.results[i,]$transitions <- subdomains[subdomains$domainId == 'transitions',]$rubricScore
	write.results[i,]$ideas <- subdomains[subdomains$domainId == 'ideas',]$rubricScore
	write.results[i,]$cohesion <- subdomains[subdomains$domainId == 'cohesion',]$rubricScore
	write.results[i,]$correct <- subdomains[subdomains$domainId == 'correct',]$rubricScore
	write.results[i,]$complexity <- subdomains[subdomains$domainId == 'complexity',]$rubricScore
	write.results[i,]$usage <- subdomains[subdomains$domainId == 'usage',]$rubricScore
	write.results[i,]$punctuation <- subdomains[subdomains$domainId == 'punctuation',]$rubricScore
	write.results[i,]$conventions <- domains[domains$domainId == 'conventions',]$rubricScore
	write.results[i,]$writeStartDate <- write[i,]$takenDate
	write.results[i,]$writeCompletionDate <- write[i,]$completionDate
	
	student.essays <- rbind(student.essays, data.frame(
		userId = id,
		attempt = attempt,
		essay = write$writingPrompt$sample[[i]],
		stringsAsFactors = FALSE
	))
	
	setTxtProgressBar(pb, i)
}
close(pb)

for(i in 4:13) {
	write.results[,i] <- as.integer(factor(write.results[,i],
										   levels = c('LOW','MEDIUM','HIGH'), ordered = TRUE))
}

write.results$writeTotal <- apply(write.results[,4:13] - 1, 1, sum) / (2 * 10)


##### Merge assessments ############################################################################
# Notes about merging:
# * Only the student's first results will be included.
# * A column is added to indicate the number of completions per assessment.

srl.results.first <- srl.results[!duplicated(srl.results$userId),]
student.srl.attempts <- as.data.frame(table(srl.results$userId),
									  responseName = 'srlAttempts', stringsAsFactors = FALSE)
math.results.first <- math.results[!duplicated(math.results$userId),]
student.math.attempts <- as.data.frame(table(math.results$userId),
									   responseName = 'mathAttempts', stringsAsFactors = FALSE)
read.results.first <- read.results[!duplicated(read.results$userId),]
read.attempts <- as.data.frame(table(read.results$userId),
							   responseName = 'readAttempts', stringsAsFactors = FALSE)
write.results.first <- write.results[!duplicated(write.results$userId),]
student.write.attempts <- as.data.frame(table(write.results$userId),
										responseName = 'writeAttempts', stringsAsFactors = FALSE)
student.essays.first <- student.essays[!duplicated(student.essays$userId),]

names(srl.results.first)[2:20] <- paste0('srl_', names(srl.results.first)[2:20])
names(math.results.first)[2:8] <- paste0('math_', names(math.results.first)[2:8])
names(read.results.first)[2:7] <- paste0('read_', names(read.results.first)[2:7])
names(write.results.first)[2:13] <- paste0('write_', names(write.results.first)[2:13])

nrow(write.results.first) == nrow(student.essays.first) # Should be TRUE

daacs <- merge(srl.results.first, student.srl.attempts, by.x = 'userId', by.y = 'Var1', all = TRUE)
daacs <- merge(daacs, math.results.first, by = 'userId', all = TRUE)
daacs <- merge(daacs, student.math.attempts, by.x = 'userId', by.y = 'Var1', all = TRUE)
daacs <- merge(daacs, read.results.first, by = 'userId', all = TRUE)
daacs <- merge(daacs, read.attempts, by.x = 'userId', by.y = 'Var1', all = TRUE)
daacs <- merge(daacs, write.results.first, by = 'userId', all = TRUE)
daacs <- merge(daacs, student.write.attempts, by.x = 'userId', by.y = 'Var1', all = TRUE)


### Replace userId with DAACS_ID
library(stringr)
idmapping <- daacs[!duplicated(daacs$userId), 
				   c('userId'), drop = FALSE]

idmapping$DAACS_ID <- 1:nrow(idmapping)

idmapping$DAACS_ID <- str_pad(idmapping$DAACS_ID, 3, pad = "0")

save(idmapping, file = paste0('data/mappings/umgc-mapping-', Sys.Date(), '.rda'))

### Replace userId with DAACS_ID

dataframes <- c(daacs, 
	math.items, math.items.response, math.items.score,
	math.results, read.items, read.items.response, read.items.score,
	read.results, srl.items, srl.items.answer, srl.items.score, srl.results,
	student.essays, student.essays.first, write.results)

mapping <- idmapping[!is.na(idmapping$userId),c('DAACS_ID', 'userId')]

## Do this for all dataframes. To Do: write this into a

write.results <- mapping %>%
	left_join(write.results) %>%
	select(-userId)


##### Save Data Files ##############################################################################
#out.dir <- '/Volumes/DAACSDATA/' # In TrueCrypt file
out.dir <- 'data/'

dir.exists(out.dir) # Make sure the directory exists

daacs.source <- institution

# users$roles <- sapply(users$roles, paste0, collapse = ',')
# names(users)[1] <- 'userId'
# write.csv(users, file = paste0('data_export/users-', mongo.db, '.csv'), row.names = FALSE)

save(daacs, #ExtractionTime, daacs.source,
	 srl.results, srl.items, srl.items.answer, srl.items.score, srl.item.mapping,
	 math.results, math.items, math.items.questions, math.items.score, math.items.response,
	 read.results, read.items, read.items.questions, read.items.score, read.items.response,
	 write.results, student.essays, student.essays.first, #EssayRatings, EssayRatings.calibration,
	 # events, #outcome,
	 # users, ## undo this when not deidentifying.
	 file = paste0(out.dir, 'DAACS-Results Anonymized-', daacs.source, '.rda'))

