/* global exports */
/* global Buffer */
"use strict";

// module Node.Stream

exports.setEncodingImpl = function(s) {
    return function(enc) {
        return function() {
            s.setEncoding(enc);
        };
    };
};

exports.onDataEitherImpl = function(left){
    return function(right){
        return function(s) {
            return function(f) {
                return function() {
                    s.on('data', function(chunk) {
                        if (chunk instanceof Buffer) {
                            f(right(chunk))();
                        }
                        else if (chunk instanceof String) {
                            f(left(chunk))();
                        }
                        else {
                            throw new Error(
                                "Node.Stream.onDataEitherImpl: Unrecognised" +
                                "chunk type; expected String or Buffer, got:" +
                                chunk);
                        }
                    });
                };
            };
        };
    };
};

exports.onEnd = function(s) {
    return function(f) {
        return function() {
            s.on('end', function() {
                f();
            });
        };
    };
};

exports.onError = function(s) {
    return function(f) {
        return function() {
            s.on('error', function() {
                f();
            });
        };
    };
};

exports.onClose = function(s) {
    return function(f) {
        return function() {
            s.on('close', function() {
                f();
            });
        };
    };
};

exports.resume = function(s) {
    return function() {
        s.resume();
    };
};

exports.pause = function(s) {
    return function() {
        s.pause();
    };
};

exports.isPaused = function(s) {
    return function() {
        return s.isPaused();
    };
};

exports.pipe = function(r) {
    return function(w) {
        return function() {
            return r.pipe(w);
        };
    };
};

exports.write = function(w) {
    return function(chunk) {
        return function(done) {
            return function() {
                return w.write(chunk, null, done);
            };
        };
    };
};

exports.writeStringImpl = function(w) {
    return function(enc) {
        return function(s) {
            return function(done) {
                return function() {
                    return w.write(s, enc, done);
                };
            };
        };
    };
};

exports.cork = function(w) {
    return function() {
        return w.cork();
    };
};

exports.uncork = function(w) {
    return function() {
        return w.uncork();
    };
};

exports.setDefaultEncodingImpl = function(w) {
    return function(enc) {
        return function() {
            w.setDefaultEncoding(enc);
        };
    };
};

exports.end = function(w) {
    return function(done) {
        return function() {
            w.end(null, null, function() {
                done();
            });
        };
    };
};
