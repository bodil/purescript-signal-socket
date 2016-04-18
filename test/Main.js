//module Test.Main

var net = require("net");

exports.nEchoServer = function(reject) {
  return function(resolve) {
    return function() {
      var server = net.createServer(function(socket) {
        socket.write("hi\r\n");
        socket.pipe(socket);
      });
      server.on("error", function(err) {
        reject(err)();
      });
      server.listen(58537, "127.0.0.1", function() {
        resolve(server)();
      });
    };
  };
}

exports.nStopEchoServer = function(server) {
  return function(reject) {
    return function(resolve) {
      return function() {
        server.close(function() {
          resolve()();
        });
      };
    };
  };
};
