SELECT 
book.BookletID, 
book.QuestionID, 
book.IsCorrect, 
book.IsDemo, 
book.atiDate, 
batch.AssessmentID, 
batch.ProgramTypeID,
asmt.AssessmentID, 
asmt.AssessmentStyleID, 
asmt.TestTypeID,
(select 
 SUBSTRING(quest.Descriptor, 0, CHARINDEX(':', quest.Descriptor))) AS [TTR_Categ],
qbtbmap.QuestionID as qbtbQuestionID
	FROM asm.BookletAnswer.Bookletanswer (nolock) as book
	LEFT JOIN asm.Config.Question as quest ON quest.QuestionID = book.QuestionID
	LEFT JOIN asm.Config.Batch (nolock) as batch on batch.batchID = book.BatchID
	LEFT JOIN asm.Config.Assessment (nolock) as asmt on asmt.AssessmentID = batch.AssessmentID
	LEFT JOIN contentBuilder.QBTB.TestSectionQuestionASMMappedValue (nolock) as qbtbmap on qbtbmap.ASMMappedValue = book.QuestionID
	LEFT JOIN contentBuilder.QBTB.Question (nolock) as qbtbq on qbtbq.QuestionID = qbtbmap.QuestionID
	WHERE book.IsDemo = 0 AND
	((book.atiDate >= '20151115' AND book.atiDate <= '20151231') OR
	(book.atiDate >= '20160401' AND book.atiDate <= '20160515')) AND
	batch.ProgramTypeID = 2 AND
	asmt.AssessmentStyleID = 2 AND
	asmt.TestTypeID = 3 AND
	asmt.AssessmentID IN (98934, 79104, 107016, 119463, 126959, 133086) AND
	qbtbq.IsValidated = 1



