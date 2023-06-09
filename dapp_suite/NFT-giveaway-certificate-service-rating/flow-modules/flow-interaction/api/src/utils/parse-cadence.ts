import { CadenceParser } from "@onflow/cadence-parser";
import fs from "fs";

let memoCadenceParser;

function removeASTPosition(obj) {
  for(let prop in obj) {
    if (prop.match(/\w+Pos/g))
      delete obj[prop];
    else if (typeof obj[prop] === 'object')
      removeASTPosition(obj[prop]);
  }
  return obj
}

export async function parseCadence(cadence: string) {
  if (!memoCadenceParser) {
    const binary = fs.readFileSync(require.resolve('../../node_modules/@onflow/cadence-parser/dist/cadence-parser.wasm'))
    memoCadenceParser = await CadenceParser.create(binary);
  }

  return JSON.stringify(removeASTPosition(await memoCadenceParser.parse(cadence)));
}
