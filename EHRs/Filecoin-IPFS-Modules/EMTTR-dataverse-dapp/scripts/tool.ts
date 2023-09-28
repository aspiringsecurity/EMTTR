import fs from "fs";
import path from "path";
import crlf from "crlf";

export async function readModels() {
  const res = fs.readdirSync(`${process.cwd()}/models`);
  const schemas: Record<string, string> = {};

  await Promise.all(
    res.map(async (fileName) => {
      if (fileName === "fs") {
        return;
      }
      const __modelsDirname = `${process.cwd()}/models`;

      const endingType = await new Promise((resolve) => {
        crlf.get(
          `${__modelsDirname}/${fileName}`,
          null,
          function (err, endingType) {
            resolve(endingType);
          }
        );
      });

      if (endingType === "CRLF") {
        await new Promise((resolve) => {
          crlf.set(`${__modelsDirname}/${fileName}`, "LF", function () {
            resolve("");
          });
        });
      }

      const filePath = path.resolve(__modelsDirname, fileName);
      if (fs.statSync(filePath).isFile()) {
        schemas[fileName] = fs
          .readFileSync(filePath, { encoding: "utf8" })
          //@ts-ignore
          .replaceAll("\n", "");
      }
    })
  );
  return schemas;
}

export function writeToOutput(val: object | string) {
  const filePath = path.resolve(`${process.cwd()}/output`, "app.json");
  fs.writeFileSync(filePath, JSON.stringify(val));
}
