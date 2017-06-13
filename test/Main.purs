module Test.Main where

import Prelude
import Data.Array as Array
import Data.List as List
import Signal.Socket as S
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Control.Monad.Eff.Timer (TIMER)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Signal (Signal, (~>), runSignal)
import Signal.Channel (CHANNEL)
import Signal.Socket (SOCKET)
import Test.Unit (Test, test, timeout)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

foreign import data EchoServer :: Type
foreign import nEchoServer :: forall e. (Error -> Eff e Unit) -> (EchoServer -> Eff e Unit) -> Eff e Unit
foreign import nStopEchoServer :: forall e. EchoServer -> (Error -> Eff e Unit) -> (Unit -> Eff e Unit) -> Eff e Unit

echoServer :: forall e. Aff e EchoServer
echoServer = makeAff nEchoServer

stopEchoServer :: forall e. EchoServer -> Aff e Unit
stopEchoServer echo = makeAff $ nStopEchoServer echo



expectFn :: forall e a. Eq a => Show a => Signal a -> Array a -> Test (ref :: REF | e)
expectFn sig vals = makeAff \fail win -> do
  remaining <- newRef vals
  let getNext val = do
        nextValArray <- readRef remaining
        let nextVals = List.fromFoldable nextValArray
        case nextVals of
          Cons x xs -> do
            if x /= val then fail $ error $ "expected " <> show x <> " but got " <> show val
              else case xs of
                Nil -> win unit
                _ -> writeRef remaining (Array.fromFoldable xs)
          Nil -> fail $ error "unexpected emptiness"
  runSignal $ sig ~> getNext

expect :: forall e a. Eq a => Show a => Int -> Signal a -> Array a -> Test (avar :: AVAR, timer :: TIMER, ref :: REF | e)
expect time sig vals = timeout time $ expectFn sig vals



debug :: forall a e. (Show a) => String -> a -> Eff (console :: CONSOLE | e) Unit
debug s o = log $ s <> show o

onSocket :: forall e. S.Socket -> S.Event String -> Eff (socket :: SOCKET | e) Unit
onSocket socket e = case e of
  S.Connected -> pure $ const unit $ S.write socket "hai lol"
  S.Data "hai lol" -> S.end socket
  _ -> pure unit

splitting :: forall e. S.Socket -> S.Event String -> Eff (socket :: SOCKET | e) Unit
splitting socket e = case e of
  S.Connected -> pure $ const unit $ S.write socket "hai lol "
  S.Data "hai lol " -> pure $ const unit $ S.write socket "omg\r\nomg "
  S.Data "omg\r\nomg " -> pure $ const unit $ S.write socket "wtf bbq\r\n"
  S.Data "wtf bbq\r\n" -> S.end socket
  _ -> pure unit

main :: forall e. Eff (timer :: TIMER, avar :: AVAR, testOutput :: TESTOUTPUT, ref :: REF, socket :: SOCKET, channel :: CHANNEL, console :: CONSOLE | e) Unit
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

  test "data signal" do
    echo <- echoServer
    socket <- liftEff $ S.connect "127.0.0.1" 58537
    let sig = S.subscribe socket
    liftEff $ runSignal $ sig ~> splitting socket
    let output = S.onlyData sig
    expect 100 output [Nothing, Just "hi\r\n", Just "hai lol ", Just "omg\r\nomg ", Just "wtf bbq\r\n", Nothing]
    liftEff $ S.end socket
    stopEchoServer echo

  test "line splitting" do
    echo <- echoServer
    socket <- liftEff $ S.connect "127.0.0.1" 58537
    let sig = S.subscribe socket
    liftEff $ runSignal $ sig ~> splitting socket
    let output = S.onlyData sig
    let lines = S.split "\r\n" $ output
    expect 100 lines [Nothing, Just "hi", Just "hai lol omg", Just "omg wtf bbq", Nothing]
    liftEff $ S.end socket
    stopEchoServer echo
