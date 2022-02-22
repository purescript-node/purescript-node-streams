"use strict";

import zlib from "zlib";
import fs from "fs";

exports.gzip = zlib.createGzip;
exports.fileStream = fs.createReadStream("example/Gzip.txt");
exports.stdout = process.stdout;
