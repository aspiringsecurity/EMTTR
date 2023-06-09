import fs from "fs";

export async function writeFile(path: string, content: any): Promise<void> {
  return new Promise((res, rej) => {
    fs.writeFile(
      path,
      content,
      {
        encoding: "utf8",
        flag: "w",
        mode: 0o666,
      },
      (err) => {
        if (err) rej(err);
        else {
          res();
        }
      }
    );
  });
}
