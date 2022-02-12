"use strict";

import { WritableStreamBuffer, ReadableStreamBuffer } from 'stream-buffers';
import { PassThrough } from 'stream';

export function writableStreamBuffer() {
  return new WritableStreamBuffer;
}

export function getContentsAsString(w) {
  return function() {
    return w.getContentsAsString('utf8');
  };
}

export function readableStreamBuffer() {
  return new ReadableStreamBuffer;
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

export { createGzip, createGunzip } from 'zlib';

export function passThrough() {
    return new PassThrough;
}
