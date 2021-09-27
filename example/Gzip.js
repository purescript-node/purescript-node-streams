"use strict";

exports.gzip = require("zlib").createGzip;
exports.fileStream = require("fs").createReadStream("example/Gzip.txt");
exports.stdout = process.stdout;
