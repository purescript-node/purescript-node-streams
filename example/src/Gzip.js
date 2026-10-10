import { createReadStream } from "node:fs";
import process from "node:process";

export { createGzip as gzip } from "node:zlib";
export const fileStream = createReadStream("example/Gzip.txt");
export const stdout = process.stdout;
