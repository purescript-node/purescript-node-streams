import { WritableStreamBuffer, ReadableStreamBuffer } from "stream-buffers";
import { PassThrough } from "stream";

export function writableStreamBuffer() {
  return new WritableStreamBuffer;
}

export function getContentsAsString(w) {
  return () => w.getContentsAsString("utf8");
}

export function readableStreamBuffer() {
  return new ReadableStreamBuffer;
}

export function putImpl(str) {
  return enc => r => () => {
    r.put(str, enc);
  };
}

export { createGzip, createGunzip } from "zlib";

export function passThrough() {
  return new PassThrough;
}
