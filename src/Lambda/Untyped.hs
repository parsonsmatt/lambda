{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped where

import Data.Foldable
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
type ParseError' = ParseError Char Dec

declaration :: Parser Declaration
declaration = D.declaration lambda

declarations :: Parser [Declaration]
declarations = many declaration

parseFile :: FilePath -> IO (Either (ParseError Char Dec) [Declaration])
parseFile fp = parse declarations fp <$> T.readFile fp

mkEvalEnv :: Foldable f => f Declaration -> EvalEnv
mkEvalEnv = foldMap (\(D.Def x e) -> Map.singleton x (convert e))

evaluate :: Eval.Lambda -> EvalEnv -> Either Eval.EvalError Eval.Lambda
evaluate = eval

loadFromFile :: FilePath -> IO (Either (ParseError Char Dec) EvalEnv)
loadFromFile file = do
    fmap (fmap mkEvalEnv . parse declarations file) (T.readFile file)

repl :: StateT EvalEnv IO ()
repl = do
    response <- liftIO $ do
        T.putStr "λ> "
        hFlush stdout
        T.getLine
    handleCommand response $ do
        case parse (declaration <|> D.Def "it" <$> lambda) "repl" response of
             Right (D.Def x expr) -> do
                 liftIO $ T.putStrLn (x <> " = " <> pretty expr)
                 let converted = convert expr
                 modify (Map.insert x converted)
                 when ("it" == x) $ do
                     mlambda <- evaluate converted <$> get

                     liftIO . T.putStrLn $ case fullyReduce <$> mlambda of
                          Right lambda ->
                              case revert lambda of
                                   Right expr -> pretty expr
                                   Left err -> T.pack (show err)
                          Left err -> T.pack $ show err
             Left err -> do
                 liftIO $ T.putStrLn (T.pack (show err))

handleCommand :: (MonadIO m, MonadState EvalEnv m)
              => T.Text -> m () -> m ()
handleCommand input action = do
    if (T.head input == ':') then do
       let inputs = T.words input
       case head inputs of
            ":state" -> do
                liftIO . T.putStrLn . T.pack . show =<< get
            ":load" -> do
                let file = T.unpack . T.concat . drop 1 $ inputs
                defs <- liftIO $ loadFromFile file
                case defs of
                     Left err ->
                         liftIO . T.putStrLn . T.pack . show $ err
                     Right env -> do
                         modify (env <>)
                         liftIO $ T.putStrLn ("Loaded " <> T.pack file)

            _ -> return ()
    else action
