import fs from "node:fs";

export const createReadStream = (filePath) => () => fs.createReadStream(filePath);
export const createWriteStream = (filePath) => () => fs.createWriteStream(filePath);
