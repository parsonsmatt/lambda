{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped where

import Lambda.Untyped.Parser as Parser
import qualified Decl.Parser as D
import Data.Monoid
import System.IO
import Text.Megaparsec
-- import Data.Text (Text)
-- import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Megaparsec.Text
import Control.Monad.State
import Lambda.Untyped.Eval as Eval

type Declaration = D.Decl Parser.Lambda

declaration :: Parser Declaration
declaration = D.declaration lambda

declarations :: Parser [Declaration]
declarations = many declaration

parseFile :: FilePath -> IO (Either ParseError [Declaration])
parseFile fp = parse declarations fp <$> T.readFile fp

mkEvalEnv :: Foldable f => f Declaration -> EvalEnv
mkEvalEnv = foldMap (\(D.Def x e) -> Map.singleton x (convert e))

evaluate :: Eval.Lambda -> EvalEnv -> Maybe Eval.Lambda
evaluate = eval . betaReduction

loadFromFile :: FilePath -> IO (Either ParseError EvalEnv)
loadFromFile file = do
    fmap (fmap mkEvalEnv . parse declarations file) (T.readFile file)

repl :: StateT EvalEnv IO a
repl = do
    response <- liftIO $ do
        T.putStr "λ> "
        hFlush stdout
        T.getLine
    case parse (declaration <|> D.Def "it" <$> lambda) "repl" response of
         Right (D.Def x expr) -> do
             liftIO $ T.putStrLn (x <> " = " <> pretty expr)
             let converted = convert expr
             modify (Map.insert x converted)
             when ("it" == x) $ do
                 mlambda <- evaluate converted <$> get
                 case mlambda >>= revert of
                      Nothing -> do
                          liftIO (T.putStrLn "Error evaluating")
                      Just expr' -> do
                          liftIO (T.putStrLn (pretty expr'))
             repl
         Left err -> do
             liftIO $ T.putStrLn (T.pack (show err))
             repl
