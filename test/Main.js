export { createGzip, createGunzip } from "zlib";
import { PassThrough } from "stream";

export function passThrough() {
  return new PassThrough;
}
