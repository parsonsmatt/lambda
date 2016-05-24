{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped where

import Lambda.Untyped.Parser
import Decl.Parser
import Text.Megaparsec.Text

parse :: Parser (Decl Lambda)
parse = declaration lambda
