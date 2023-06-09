import * as fcl from "@onflow/fcl";
import fetch from "node-fetch";
import { Template } from "../models/template";
import { readFiles } from "../utils/read-files";
import { writeFile } from "../utils/write-file";
import { genHash } from "../utils/gen-hash";
import { parseCadence } from "../utils/parse-cadence";

class TemplateService {
  config: any;

  constructor(config: any) {
    this.config = config;
  }

  async insertTemplate(template: string) {
    let newTemplate: Template;

    let templateJSON = JSON.parse(template);

    newTemplate = await Template.query().insertAndFetch({
      id: templateJSON.id,
      json_string: template,
    });

    return newTemplate;
  }

  async getTemplate(templateId: string) {
    let foundTemplate: Template;

    foundTemplate = (
      await Template.query().where({
        id: templateId,
      })
    )[0];

    let foundTemplateJson = foundTemplate?.json_string || null;

    if (typeof foundTemplateJson === "string") {
      foundTemplateJson = JSON.parse(foundTemplateJson);
    }

    return foundTemplateJson;
  }

  async getTemplateByCadenceASTHash(cadenceASTHash: string, network: string) {
    let foundTemplate: Template | null = null;

    if (network === "mainnet") {
      foundTemplate = (
        await Template.query().where({
          mainnet_cadence_ast_sha3_256_hash: cadenceASTHash,
        })
      )[0];
    } else if (network === "testnet") {
      foundTemplate = (
        await Template.query().where({
          testnet_cadence_ast_sha3_256_hash: cadenceASTHash,
        })
      )[0];
    }

    let foundTemplateJson = foundTemplate?.json_string || null;

    if (typeof foundTemplateJson === "string") {
      foundTemplateJson = JSON.parse(foundTemplateJson);
    }

    return foundTemplateJson;
  }

  async getTemplateManifest() {
    let templateManifest;
    try {
      templateManifest = (
        await readFiles(this.config.templateManifestFile)
      ).map((file: any) => file.content)[0];
      templateManifest = JSON.parse(templateManifest);
    } catch (e) {
      console.error("Error reading manifest file");
      return null;
    }
    return templateManifest;
  }

  async seed() {
    let templates: any[] = [];

    const localTemplates = (await readFiles(this.config.templateDir))
      .map((file: any) => file.content)
      .filter((file) => file !== null)
      .map((file) => JSON.parse(file));

    console.log(`Found ${localTemplates.length} local template files`);

    templates = templates.concat(localTemplates);

    const peers = this.config.peers ? this.config.peers.split(",") : [];

    for (const peer of peers) {
      console.log(`Fetching peer ${peer}`);
      const manifest: any = await fetch(peer)
        .then((res) => (res.status === 200 ? res.json() : null))
        .catch((e) => null);
      if (manifest) {
        console.log(
          `Found manifest from ${peer} with ${
            Object.values(manifest).length
          } entries`
        );
        templates = templates.concat(Object.values(manifest));
      }
    }

    const templateManifest = (await this.getTemplateManifest()) || {};

    console.log(
      `Found local manifest with ${
        Object.values(templateManifest).length
      } templates`
    );

    templates = templates.concat(Object.values(templateManifest));

    console.log(`Parsing ${templates.length} templates`);

    parseTemplatesLoop: for (const template of templates) {
      try {
        const parsedTemplate =
          typeof template === "object" ? template : JSON.parse(template);

        if (
          template.f_type !== "InteractionTemplate" ||
          template.f_version !== "1.0.0"
        ) {
          continue parseTemplatesLoop;
        }

        const recomputedTemplateID =
          await fcl.InteractionTemplateUtils.generateTemplateId({
            template: parsedTemplate,
          });
        if (recomputedTemplateID !== parsedTemplate.id)
          throw new Error(
            `recomputed=${recomputedTemplateID} template=${parsedTemplate.id}`
          );

        if (await Template.query().findById(parsedTemplate.id)) {
          console.log(`Skipping template with ID = ${parsedTemplate.id}`);
          continue parseTemplatesLoop;
        }

        let mainnet_cadence;
        try {
          mainnet_cadence = fcl.InteractionTemplateUtils.deriveCadenceByNetwork(
            {
              template: parsedTemplate,
              network: "mainnet",
            }
          );
        } catch (e) {}

        let testnet_cadence;
        try {
          testnet_cadence = fcl.InteractionTemplateUtils.deriveCadenceByNetwork(
            {
              template: parsedTemplate,
              network: "testnet",
            }
          );
        } catch (e) {}

        if (!testnet_cadence || testnet_cadence === "") {
          continue parseTemplatesLoop;
        }

        console.log(`Inserting template with ID = ${parsedTemplate.id}`);

        await Template.query().insertAndFetch({
          id: parsedTemplate.id,
          json_string: JSON.stringify(template),
          mainnet_cadence_ast_sha3_256_hash: mainnet_cadence
            ? await genHash(await parseCadence(mainnet_cadence))
            : undefined,
          testnet_cadence_ast_sha3_256_hash: testnet_cadence
            ? await genHash(await parseCadence(testnet_cadence))
            : undefined,
        });

        templateManifest[parsedTemplate.id] = parsedTemplate;
      } catch (e) {
        console.warn(`Skipping template error=${e}`);
      }
    }

    await writeFile(
      this.config.templateManifestFile,
      JSON.stringify(templateManifest, null, 2)
    );
  }
}

export { TemplateService };
