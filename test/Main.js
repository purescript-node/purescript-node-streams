"use strict";

// module Test.Main


exports.writableStreamBuffer = function() {
  var W = require('stream-buffers').WritableStreamBuffer;
  return new W;
};

exports.getContentsAsString = function(w) {
  return function() {
    return w.getContentsAsString('utf8');
  };
};

exports.readableStreamBuffer = function() {
  var R = require('stream-buffers').ReadableStreamBuffer;
  return new R;
};

exports.putImpl = function(str) {
  return function(enc) {
    return function(r) {
      return function() {
        r.put(str, enc);
      };
    };
  };
};
