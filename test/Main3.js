import fs from "node:fs";
import process from "node:process";

export const createReadStream = (filePath) => () => fs.createReadStream(filePath);
export const argv = () => process.argv;
