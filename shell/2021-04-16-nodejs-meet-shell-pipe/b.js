#!/usr/bin/env node

var stdin = process.openStdin();

var data = "";

stdin.on('data', function(chunk) {
  data += chunk;
  console.log("Received data")
  console.log(chunk)
});

stdin.on('end', function() {
  console.log("DATA:\n" + data + "\nEND DATA");
});
