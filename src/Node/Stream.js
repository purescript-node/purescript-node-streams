export const setEncodingImpl = (s, enc) => s.setEncoding(enc);

export const readChunkImpl = (useBuffer, useString, chunk) => {
  if (chunk instanceof Buffer) {
    return useBuffer(chunk);
  } else if (typeof chunk === "string") {
    return useString(chunk);
  } else {
    throw new Error(
      "Node.Stream.readChunkImpl: Unrecognised " +
        "chunk type; expected String or Buffer, got: " +
        chunk
    );
  }
};

export const resumeImpl = (r) => r.resume();

export const pauseImpl = (r) => r.pause;

export const isPausedImpl = (r) => r.isPaused;

export const pipeImpl = (r, w) => r.pipe(w);

export const unpipeAllImpl = (r) => r.unpipe();

export const unpipeImpl = (r, w) => r.unpipe(w);

export const readImpl = (r) => r.read();

export const readSizeImpl = (r, size) => r.read(size);

export const writeImpl = (w, buf) => w.write(buf);

export const writeCbImpl = (w, buf, cb) => w.write(buf, cb);

export const writeStringImpl = (w, str, enc) => w.write(str, enc);

export const writeStringCbImpl = (w, str, enc, cb) => w.write(str, enc, cb);

export const corkImpl = (w) => w.cork();

export const uncorkImpl = (w) => w.uncork();

export const setDefaultEncodingImpl = (w, enc) => w.setDefaultEncoding(enc);

export const endCbImpl = (w, cb) => w.end(cb);

export const endImpl = (w) => w.end();

export const destroyImpl = (w) => w.destroy();

export const destroyErrorImpl = (w, e) => w.destroy(e);
