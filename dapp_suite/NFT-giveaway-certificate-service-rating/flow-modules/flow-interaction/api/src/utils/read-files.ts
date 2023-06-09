import glob from "glob";
import fs from "fs";

export interface File {
  path: string;
  content: string;
}

export function readFiles(pattern: string): Promise<File[]> {
  return new Promise((res, rej) => {
    try {
      glob(pattern, {}, function (err, paths: string[]) {
        const fileReadPromises = paths.map(
          (path) =>
            new Promise<File>((fsRes, fsRej) => {
              try {
                fs.readFile(path, "utf8", function (err: any, data: any) {
                  if (err) {
                    fsRej(err);
                    return;
                  }
                  const file: File = {
                    path,
                    content: data,
                  };
                  fsRes(file);
                });
              } catch (e) {
                fsRej(e);
              }
            })
        );
        return Promise.all(fileReadPromises).then((files: File[]) =>
          res(files)
        );
      });
    } catch (e) {
      rej(e);
    }
  });
}
