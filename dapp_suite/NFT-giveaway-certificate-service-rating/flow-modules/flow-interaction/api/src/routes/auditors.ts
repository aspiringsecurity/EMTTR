import express, { Request, Response, Router } from "express";
import { body } from "express-validator";
import { validateRequest } from "../middlewares/validate-request";
import { TemplateService } from "../services/template";
import { genHash } from "../utils/gen-hash";
import { mixpanelTrack } from "../utils/mixpanel";
import { parseCadence } from "../utils/parse-cadence";
import fs from "fs";

function auditorsRouter(auditorsJSONFile: JSON): Router {
  const router = express.Router();

  router.get("/auditors", async (req: Request, res: Response) => {
    const network = req.query.network as string;

    if (!network) {
      mixpanelTrack("get_auditors", {
        network,
        status: 400,
      });

      res.status(400);
      return res.send(
        "GET /auditors -- 'network' in request parameters not found"
      );
    }

    if (typeof auditorsJSONFile[network] === "undefined") {
      mixpanelTrack("get_auditors", {
        network,
        status: 400,
      });

      res.status(400);
      return res.send(
        "GET /auditors -- 'network' in request parameters not supported"
      );
    }

    mixpanelTrack("get_auditors", {
      network,
      status: 200,
    });

    return res.send(auditorsJSONFile[network]);
  });

  return router;
}

export default auditorsRouter;
