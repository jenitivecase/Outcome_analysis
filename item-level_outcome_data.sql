SELECT book.BookletID, book.QuestionID, book.IsCorrect, book.IsDemo, book.atiDate, 
choice.OutcomeID, choice.QuestionOutcomeID, 
batch.AssessmentID, batch.ProgramTypeID,
asmt.AssessmentID, asmt.AssessmentStyleID, asmt.TestTypeID, 
outcome.OutcomeDescription, outcome.OutcomeTypeID,
outtype.OutcomeTypeName,
qbtb.QuestionID as qbtbQuestionID
	FROM asm.BookletAnswer.Bookletanswer (nolock) as book
	LEFT JOIN asm.Config.QuestionOutcome (nolock) as choice on choice.QuestionID = book.QuestionID
	LEFT JOIN asm.Config.Batch (nolock) as batch on batch.batchID = book.BatchID
	LEFT JOIN asm.Config.Assessment (nolock) as asmt on asmt.AssessmentID = batch.AssessmentID
	LEFT JOIN asm.Code.Outcome (nolock) as outcome on outcome.OutcomeID = choice.OutcomeID
	LEFT JOIN asm.Code.OutcomeType (nolock) as outtype on outtype.OutcomeTypeID = outcome.OutcomeTypeID
	LEFT JOIN contentBuilder.QBTB.TestSectionQuestionASMMappedValue (nolock) as qbtb on qbtb.ASMMappedValue = book.QuestionID
	WHERE book.IsDemo = 0 AND
	((book.atiDate >= '20151115' AND book.atiDate <= '20151231') OR
	(book.atiDate >= '20160401' AND book.atiDate <= '20160515')) AND
	batch.ProgramTypeID = 2 AND
	asmt.AssessmentStyleID = 2 AND
	asmt.TestTypeID = 3 AND
	asmt.AssessmentID IN (98934, 79104, 107016, 119463, 126959, 133086)