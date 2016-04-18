module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Data.List (List(..), toList, fromList)
import Signal (Signal, (~>), runSignal)
import Signal.Channel (CHANNEL)
import Signal.Socket (SOCKET)
import Signal.Socket as S
import Test.Unit (test, runTest, TIMER, Assertion, timeout)
import Test.Unit.Console (TESTOUTPUT)

foreign import data EchoServer :: *
foreign import nEchoServer :: forall e. (Error -> Eff e Unit) -> (EchoServer -> Eff e Unit) -> Eff e Unit
foreign import nStopEchoServer :: forall e. EchoServer -> (Error -> Eff e Unit) -> (Unit -> Eff e Unit) -> Eff e Unit

echoServer :: forall e. Aff e EchoServer
echoServer = makeAff nEchoServer

stopEchoServer :: forall e. EchoServer -> Aff e Unit
stopEchoServer echo = makeAff $ nStopEchoServer echo



expectFn :: forall e a. (Eq a, Show a) => Signal a -> Array a -> Assertion (ref :: REF | e)
expectFn sig vals = makeAff \fail win -> do
  remaining <- newRef vals
  let getNext val = do
        nextValArray <- readRef remaining
        let nextVals = toList nextValArray
        case nextVals of
          Cons x xs -> do
            if x /= val then fail $ error $ "expected " ++ show x ++ " but got " ++ show val
              else case xs of
                Nil -> win unit
                _ -> writeRef remaining (fromList xs)
          Nil -> fail $ error "unexpected emptiness"
  runSignal $ sig ~> getNext

expect :: forall e a. (Eq a, Show a) => Int -> Signal a -> Array a -> Assertion (ref :: REF | e)
expect time sig vals = timeout time $ expectFn sig vals



onSocket :: forall e. S.Socket -> S.Event String -> Eff (socket :: SOCKET | e) Unit
onSocket socket e = case e of
  S.Connected -> do
    S.write socket "hai lol"
    return unit
  S.Data "hai lol" -> S.end socket
  _ -> return unit

main :: forall e. Eff (timer :: TIMER, avar :: AVAR, testOutput :: TESTOUTPUT, ref :: REF, socket :: SOCKET, channel :: CHANNEL | e) Unit
main = runTest do
  test "connect to echo server" do
    echo <- echoServer
    socket <- liftEff $ S.connect "127.0.0.1" 58537
    let sig = S.subscribe socket
    liftEff $ runSignal $ sig ~> onSocket socket
    expect 100 sig [S.Connecting, S.Connected, S.Data "hi\r\n", S.Data "hai lol", S.Closed]
    liftEff $ S.end socket
    stopEchoServer echo
  test "connection error" do
    socket <- liftEff $ S.connect "127.0.0.1" 58538
    let sig = S.subscribe socket
    expect 100 sig [S.Connecting, S.Error (error "connect ECONNREFUSED 127.0.0.1:58538")]
