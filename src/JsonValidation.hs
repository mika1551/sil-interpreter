module JsonValidation
  ( validateJsonSyntax
  ) where

import qualified Data.Aeson.Decoding.Text as Decoding
import Data.Aeson.Decoding.Tokens (Number (..), TkArray (..), TkRecord (..), Tokens (..))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)

validateJsonSyntax :: Text -> Either String ()
validateJsonSyntax source = () <$ checkValue (Decoding.textToTokens source)

checkValue :: Tokens k String -> Either String k
checkValue token = case token of
  TkLit _ next -> Right next
  TkText _ next -> Right next
  TkNumber (NumInteger _) next -> Right next
  TkNumber _ _ -> Left "expected an integer"
  TkArrayOpen items -> checkArray items
  TkRecordOpen fields -> checkRecord [] fields
  TkErr message -> Left message

checkArray :: TkArray k String -> Either String k
checkArray items = case items of
  TkItem value -> checkValue value >>= checkArray
  TkArrayEnd next -> Right next
  TkArrayErr message -> Left message

checkRecord :: [Key.Key] -> TkRecord k String -> Either String k
checkRecord fields record = case record of
  TkPair key value
    | key `elem` fields -> Left ("duplicate JSON field " ++ show (Key.toString key))
    | otherwise -> checkValue value >>= checkRecord (key : fields)
  TkRecordEnd next -> Right next
  TkRecordErr message -> Left message
