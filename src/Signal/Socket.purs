module Signal.Socket
  ( SOCKET
  , NodeSocket
  , Event(..)
  , ConnectOptions
  , Socket(..)
  , connectOpt
  , connect
  , subscribeRaw
  , decode
  , subscribeAs
  , subscribe
  , writeRaw
  , writeAs
  , write
  , setTimeout
  , end
  , destroy
  , pause
  , resume
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, message)
import Data.Foreign (Foreign, unsafeFromForeign, toForeign)
import Data.Maybe (Maybe(..))
import Data.Nullable as Null
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(UTF8))
import Signal (Signal)
import Signal.Channel (Channel, CHANNEL)
import Signal.Channel as Channel

foreign import data SOCKET :: !
foreign import data NodeSocket :: *

foreign import nodeConnect :: forall e. Foreign -> Channel (Event Buffer) -> (Foreign -> Eff (socket :: SOCKET, channel :: CHANNEL | e) Unit) -> Eff (socket :: SOCKET, channel :: CHANNEL | e) NodeSocket
foreign import nodeDecode :: String -> Buffer -> String
foreign import nodeEncode :: String -> String -> Buffer
foreign import nodeWriteRaw :: forall e. NodeSocket -> Buffer -> Eff (socket :: SOCKET | e) Boolean
foreign import nodeWrite :: forall e. NodeSocket -> String -> String -> Eff (socket :: SOCKET | e) Boolean
foreign import nodeSetTimeout :: forall e. NodeSocket -> Number -> Eff (socket :: SOCKET | e) Unit
foreign import nodeEnd :: forall e. NodeSocket -> Eff (socket :: SOCKET | e) Unit
foreign import nodeDestroy :: forall e. NodeSocket -> Eff (socket :: SOCKET | e) Unit
foreign import nodePause :: forall e. NodeSocket -> Eff (socket :: SOCKET | e) Unit
foreign import nodeResume :: forall e. NodeSocket -> Eff (socket :: SOCKET | e) Unit

data Event a = Connecting
             | Connected
             | Drain
             | Closed
             | Timeout
             | Data a
             | Error Error

instance eqEvent :: (Eq a) => Eq (Event a) where
  eq Connecting Connecting = true
  eq Connected Connected = true
  eq Drain Drain = true
  eq Closed Closed = true
  eq Timeout Timeout = true
  eq (Data a) (Data b) = a == b
  eq (Error a) (Error b) = message a == message b
  eq _ _ = false

instance showEvent :: (Show a) => Show (Event a) where
  show Connecting = "Connecting"
  show Connected = "Connected"
  show Drain = "Drain"
  show Closed = "Closed"
  show Timeout = "Timeout"
  show (Data a) = "Data <\"" ++ show a ++ "\">"
  show (Error e) = "Error <" ++ message e ++ ">"

type ConnectOptions =
  { port :: Int
  , host :: String
  , localAddress :: Maybe String
  , localPort :: Maybe Int
  , family :: Maybe Int
  }

data Socket = Socket NodeSocket (Channel (Event Buffer))

unpackNodeEvent :: Foreign -> { event :: String, buffer :: Maybe Buffer, error :: Maybe Error }
unpackNodeEvent ev' =
  let ev = unsafeFromForeign ev'
  in { event: ev.event
     , buffer: Null.toMaybe ev.buffer
     , error: Null.toMaybe ev.error
     }

processNodeEvent :: forall e. Channel (Event Buffer) -> Foreign -> Eff (socket :: SOCKET, channel :: CHANNEL | e) Unit
processNodeEvent channel ev = case unpackNodeEvent ev of
  { buffer: Just buffer } -> Channel.send channel $ Data buffer
  { error: Just error } -> Channel.send channel $ Error error
  { event: "connected" } -> Channel.send channel Connected
  { event: "drain" } -> Channel.send channel Drain
  { event: "close" } -> Channel.send channel Closed
  { event: "timeout" } -> Channel.send channel Timeout
  _ -> return unit

connectOpt :: forall e. ConnectOptions -> Eff (socket :: SOCKET, channel :: CHANNEL | e) Socket
connectOpt opts = do
  let opts' = { port: opts.port, host: opts.host,
                localAddress: Null.toNullable opts.localAddress,
                localPort: Null.toNullable opts.localPort,
                family: Null.toNullable opts.family }
  channel <- Channel.channel Connecting
  socket <- nodeConnect (toForeign opts') channel (processNodeEvent channel)
  return $ Socket socket channel

connect :: forall e. String -> Int -> Eff (socket :: SOCKET, channel :: CHANNEL | e) Socket
connect host port = connectOpt $ { host, port, localAddress: Nothing, localPort: Nothing, family: Nothing }

subscribeRaw :: Socket -> Signal (Event Buffer)
subscribeRaw (Socket _ channel) = Channel.subscribe channel

decode :: Encoding -> Signal (Event Buffer) -> Signal (Event String)
decode encoding s = process <$> s
  where process :: Event Buffer -> Event String
        process (Data b) = Data $ nodeDecode (show encoding) b
        process (Error e) = Error e
        process Connecting = Connecting
        process Connected = Connected
        process Closed = Closed
        process Drain = Drain
        process Timeout = Timeout

subscribeAs :: Encoding -> Socket -> Signal (Event String)
subscribeAs encoding = subscribeRaw >>> decode encoding

subscribe :: Socket -> Signal (Event String)
subscribe = subscribeRaw >>> decode UTF8

writeRaw :: forall e. Socket -> Buffer -> Eff (socket :: SOCKET | e) Boolean
writeRaw (Socket s _) = nodeWriteRaw s

writeAs :: forall e. Socket -> Encoding -> String -> Eff (socket :: SOCKET | e) Boolean
writeAs (Socket s _) encoding = nodeWrite s (show encoding)

write :: forall e. Socket -> String -> Eff (socket :: SOCKET | e) Boolean
write socket = writeAs socket UTF8

setTimeout :: forall e. Socket -> Number -> Eff (socket :: SOCKET | e) Unit
setTimeout (Socket s _) = nodeSetTimeout s

end :: forall e. Socket -> Eff (socket :: SOCKET | e) Unit
end (Socket s _) = nodeEnd s

destroy :: forall e. Socket -> Eff (socket :: SOCKET | e) Unit
destroy (Socket s _) = nodeDestroy s

pause :: forall e. Socket -> Eff (socket :: SOCKET | e) Unit
pause (Socket s _) = nodePause s

resume :: forall e. Socket -> Eff (socket :: SOCKET | e) Unit
resume (Socket s _) = nodeResume s
