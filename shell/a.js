#!/usr/bin/env node

setTimeout(function(){
  console.log(JSON.stringify({ foo: 'bar' }));
}, 10);

process.on("SIGPIPE", process.exit);
