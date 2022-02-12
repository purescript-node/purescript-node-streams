"use strict";

export {undefined};

export function setEncodingImpl(s) {
  return function (enc) {
    return function () {
      s.setEncoding(enc);
    };
  };
}

export function readChunkImpl(Left) {
  return function (Right) {
    return function (chunk) {
      if (chunk instanceof Buffer) {
        return Right(chunk);
      } else if (typeof chunk === "string") {
        return Left(chunk);
      } else {
        throw new Error(
          "Node.Stream.readChunkImpl: Unrecognised " +
            "chunk type; expected String or Buffer, got: " +
            chunk
        );
      }
    };
  };
}

export function onDataEitherImpl(readChunk) {
  return function (r) {
    return function (f) {
      return function () {
        r.on("data", function (data) {
          f(readChunk(data))();
        });
      };
    };
  };
}

export function onEnd(s) {
  return function (f) {
    return function () {
      s.on("end", f);
    };
  };
}

export function onFinish(s) {
  return function (f) {
    return function () {
      s.on("finish", f);
    };
  };
}

export function onReadable(s) {
  return function (f) {
    return function () {
      s.on("readable", f);
    };
  };
}

export function onError(s) {
  return function (f) {
    return function () {
      s.on("error", function (e) {
        f(e)();
      });
    };
  };
}

export function onClose(s) {
  return function (f) {
    return function () {
      s.on("close", f);
    };
  };
}

export function resume(s) {
  return function () {
    s.resume();
  };
}

export function pause(s) {
  return function () {
    s.pause();
  };
}

export function isPaused(s) {
  return function () {
    return s.isPaused();
  };
}

export function pipe(r) {
  return function (w) {
    return function () {
      return r.pipe(w);
    };
  };
}

export function unpipe(r) {
  return function (w) {
    return function () {
      return r.unpipe(w);
    };
  };
}

export function unpipeAll(r) {
  return function () {
    return r.unpipe();
  };
}

export function readImpl(readChunk) {
  return function (Nothing) {
    return function (Just) {
      return function (r) {
        return function (s) {
          return function () {
            var v = r.read(s);
            if (v === null) {
              return Nothing;
            } else {
              return Just(readChunk(v));
            }
          };
        };
      };
    };
  };
}

export function write(w) {
  return function (chunk) {
    return function (done) {
      return function () {
        return w.write(chunk, null, done);
      };
    };
  };
}

export function writeStringImpl(w) {
  return function (enc) {
    return function (s) {
      return function (done) {
        return function () {
          return w.write(s, enc, done);
        };
      };
    };
  };
}

export function cork(w) {
  return function () {
    return w.cork();
  };
}

export function uncork(w) {
  return function () {
    return w.uncork();
  };
}

export function setDefaultEncodingImpl(w) {
  return function (enc) {
    return function () {
      w.setDefaultEncoding(enc);
    };
  };
}

export function end(w) {
  return function (done) {
    return function () {
      w.end(null, null, function () {
        done();
      });
    };
  };
}

export function destroy(strm) {
  return function () {
    strm.destroy(null);
  };
}

export function destroyWithError(strm) {
  return function (e) {
    return function () {
      strm.destroy(e);
    };
  };
}
