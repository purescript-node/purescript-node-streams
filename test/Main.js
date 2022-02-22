"use strict";

import zlib from "zlib";
import streamBuffers from "stream-buffers";
import stream from "stream";

export function writableStreamBuffer() {
  var W = streamBuffers.WritableStreamBuffer;
  return new W;
}

export function getContentsAsString(w) {
  return function () {
    return w.getContentsAsString('utf8');
  };
}

export function readableStreamBuffer() {
  var R = streamBuffers.ReadableStreamBuffer;
  return new R;
}

export function putImpl(str) {
  return function (enc) {
    return function (r) {
      return function () {
        r.put(str, enc);
      };
    };
  };
}

export const createGzip = zlib.createGzip;
export const createGunzip = zlib.createGunzip;

export function passThrough() {
  return new stream.PassThrough();
}
