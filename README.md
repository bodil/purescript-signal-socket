# purescript-signal-socket

Node sockets with a Signal interface.

## Documentation

Read the [API documentation](https://pursuit.purescript.org/packages/purescript-signal-socket/).

## Usage

Connect to a socket and subscribe to a signal of socket events:

```purescript
import Signal.Socket as Socket

main = do
  socket <- Socket.connect "localhost" 31337
  eventSignal <- Socket.subscribe socket
```

The signal is of type `Signal (Socket.Event String)`, and the
`Socket.Event` datatype looks like this:

```purescript
import Control.Monad.Eff.Exception (Error)

data Event a = Connecting
             | Connected
             | Drain
             | Closed
             | Timeout
             | Data a
             | Error Error
```

To process the event stream:

```purescript
  Signal.runSignal $ eventSignal ~> onSocketEvent
  where onSocketEvent Connected = Socket.write socket "hai lol!"
        onSocketEvent Timeout = Socket.end socket
        onSocketEvent (Data s) = Socket.write socket $ "hi " ++ s
        -- etc
```

## Licence

Copyright 2016 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program. If not, see
<http://www.gnu.org/licenses/>.
