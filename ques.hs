-- Questionaire thingy

type Statement = String
type Options = [String]
type Answer = String

data Question = YesNoQuestion Statement Answer
              | MultipleChoiceQuestion Statement Options Answer
              | MultipleAnswerQuestion Statement Options Answer
              | LikertQuestion Statement

getStatement :: Question -> Statement
getStatement (YesNoQuestion s _) = s
getStatement (MultipleChoiceQuestion s _ _) = s
getStatement (MultipleAnswerQuestion s _ _) = s
getStatement (LikertQuestion s) = s


getOptions :: Question -> Options
getOptions (YesNoQuestion _ _) = ["Yes", "No"]
getOptions (MultipleChoiceQuestion _ o _) = o
getOptions (MultipleAnswerQuestion _ o _) = o
getOptions (LikertQuestion _) = ["Strongly Agree", "Agree", "Neither Agree nor Disagree", "Disagree", "Strongly Disagree"]
attempt :: Question -> Answer -> Bool

attempt (YesNoQuestion _ a) ua | a == ua = True
attempt (MultipleChoiceQuestion _ _ a) ua | a == ua = True
attempt (LikertQuestion _) ua = opt > 0 && opt <= 5
    where opt = read ua :: Integer
attempt (MultipleAnswerQuestion _ _ a) _ = True  -- TODO
attempt _ _ = False
