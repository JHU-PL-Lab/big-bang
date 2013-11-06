{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, OverloadedStrings #-}

{--| temp toploop function for testing communicator function 
--}

module Language.TinyBang.ToploopTest where

import Data.Aeson
import Language.TinyBang.Communicator.ToHaskellObject as THO
import Language.TinyBang.Communicator.FromHaskellObject as FHO
import Language.TinyBang.Ast
import Language.TinyBang.Interpreter.DeepValues
import Language.TinyBang.Syntax.Lexer
import Language.TinyBang.Syntax.Parser
import Language.TinyBang.Syntax.Location
import Language.TinyBang.Interpreter
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Interface open to interpreter
messageHandler :: String -> String
messageHandler inpJsonStr = case (decode . BL.pack $ inpJsonStr) of
  Nothing -> BL.unpack . encode $ BatchModeErrorC . BMInterpreterFailure $ "Invalid Input"
  Just obj -> BL.unpack . encode . commandHandler $ obj

-- | Bridge function               
commandHandler :: ToHaskellObject -> FromHaskellObject
commandHandler cmdObj =
  case runCodeCommand usrInpSrc of
    Left err ->
      case err of
        BMLexFailure errMsg -> BatchModeErrorC $ BMLexFailure errMsg
        BMParserFailure errMsg -> BatchModeErrorC $ BMParserFailure errMsg 
        _ -> BatchModeErrorC err   
    Right ans -> BatchModeResultC ans
  where usrInpSrc = getInpStr cmdObj
        
-- | core function lex, parse and eval the user input
runCodeCommand :: String -> Either BatchModeError BatchModeResult
runCodeCommand inpSrc = do
  tokens <- doStep BMLexFailure $ lexTinyBang "<stdin>" inpSrc
  ast <- doStep BMParserFailure $ parseTinyBang
              ParserContext { contextDocument = UnknownDocument
                            , contextDocumentName = "<stdin>" }
              tokens
  (env, var) <- doStep BMEvalFailure $ evalTest ast            
  return $ BatchModeResult var (flowVarMap env) (cellVarMap env)
  where
    doStep errConstr computation =
      case computation of
        Left err -> Left $ errConstr err
        Right ans -> Right ans
        
    -- | tmp function for BMEvalFailure without [Clause]
    evalTest :: Expr -> Either EvalError (EvalEnv, FlowVar)    
    evalTest expr =
      case eval expr of
        Left (evalerr, clauseLst) -> Left evalerr
        Right resultTuple -> Right resultTuple

