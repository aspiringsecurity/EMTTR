const defaultPort = 3000;
const defaultMigrationPath = "./dist/migrations";

export function getConfig(env) {
  env = env ?? process.env;

  const port = env.PORT || defaultPort;

  const accessApi = env.FLOW_ACCESS_API_URL;

  const dbPath = env.DATABASE_PATH!;
  const databaseUrl = env.DATABASE_URL;

  const auditorsJsonFile = env.AUDITORS_JSON_FILE;
  const namesJsonFile = env.NAMES_JSON_FILE;

  const databaseMigrationPath = env.MIGRATION_PATH || defaultMigrationPath;

  const templateDir = env.TEMPLATE_DIR;

  const peers = env.PEERS;

  const templateManifestFile = env.TEMPLATE_MANIFEST_FILE;

  console.log("ENV: ", env);
  console.log("accessApi", accessApi);
  console.log("databaseMigrationPath: ", databaseMigrationPath);

  return {
    port,
    accessApi,
    dbPath,
    databaseMigrationPath,
    databaseUrl,
    templateDir,
    auditorsJsonFile,
    namesJsonFile,
    peers,
    templateManifestFile,
  };
}
