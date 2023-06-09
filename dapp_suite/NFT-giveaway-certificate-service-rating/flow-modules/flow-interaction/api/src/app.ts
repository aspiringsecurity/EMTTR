import { json, urlencoded } from "body-parser";
import cors from "cors";
import express, { Request, Response } from "express";
import "express-async-errors";
import path from "path";
import templateRouter from "./routes/template";
import auditorsRouter from "./routes/auditors";
import { TemplateService } from "./services/template";

const V1 = "/v1/";

const corsOptions = {
  origin: "*",
  credentials: true,
  optionSuccessStatus: 200,
};

// Init all routes, setup middlewares and dependencies
const initApp = (
  templateService: TemplateService,
  auditorsJSONFile: JSON,
  namesJSONFile: JSON
) => {
  const app = express();

  app.use(cors(corsOptions));
  app.use(json());
  app.use(urlencoded({ extended: false }));
  app.use(V1, templateRouter(templateService, namesJSONFile));
  app.use(V1, auditorsRouter(auditorsJSONFile));

  app.all("*", async (req: Request, res: Response) => {
    return res.sendStatus(404);
  });

  return app;
};

export default initApp;
