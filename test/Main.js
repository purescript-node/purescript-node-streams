"use strict";

export function writableStreamBuffer() {
  var W = require('stream-buffers').WritableStreamBuffer;
  return new W;
}

export function getContentsAsString(w) {
  return function() {
    return w.getContentsAsString('utf8');
  };
}

export function readableStreamBuffer() {
  var R = require('stream-buffers').ReadableStreamBuffer;
  return new R;
}

export function putImpl(str) {
  return function(enc) {
    return function(r) {
      return function() {
        r.put(str, enc);
      };
    };
  };
}

export const createGzip = require('zlib').createGzip;
export const createGunzip = require('zlib').createGunzip;

export function passThrough() {
    var s = require('stream');
    return new s.PassThrough();
}
