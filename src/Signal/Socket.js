//module Signal.Socket

var net = require("net");

exports.nodeConnect = function(optsP) {
  return function(channel) {
    return function(send) {
      return function() {
        var opts = {};
        for (var key in optsP) {
          if (optsP.hasOwnProperty(key) && optsP[key] !== null) {
            opts[key] = optsP[key];
          }
        }
        var socket = net.connect(opts);
        socket.on("connect", function() {
          var lol = send({event: "connected"})();
        });
        socket.on("data", function(buffer) {
          send({event: "data", buffer: buffer})();
        });
        socket.on("drain", function() {
          send({event: "drain"})();
        });
        socket.on("close", function() {
          send({event: "close"})();
        });
        socket.on("timeout", function() {
          send({event: "timeout"})();
        });
        socket.on("error", function(error) {
          send({event: "error", error: error})();
        });
        return socket;
      };
    };
  };
};

exports.nodeDecode = function(encoding) {
  return function(buffer) {
    return buffer.toString(encoding);
  };
};

exports.nodeEncode = function(encoding) {
  return function(s) {
    return new Buffer(s, encoding);
  };
};

exports.nodeWriteRaw = function(socket) {
  return function(buffer) {
    return function() {
      return socket.write(buffer);
    };
  };
};

exports.nodeWrite = function(socket) {
  return function(encoding) {
    return function(data) {
      return function() {
        return socket.write(data, encoding);
      };
    };
  };
};

exports.nodeSetTimeout = function(socket) {
  return function(timeout) {
    return function() {
      socket.setTimeout(timeout);
    };
  };
};

exports.nodeEnd = function(socket) {
    return function() {
      socket.end();
  };
};

exports.nodeDestroy = function(socket) {
  return function() {
    socket.destroy();
  };
};

exports.nodePause = function(socket) {
  return function() {
    socket.pause();
  };
};

exports.nodeResume = function(socket) {
  return function() {
    socket.resume();
  };
};
