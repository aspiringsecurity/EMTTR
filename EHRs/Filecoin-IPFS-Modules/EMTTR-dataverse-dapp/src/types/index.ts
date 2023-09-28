import { StreamContent } from "@dataverse/runtime-connector";

export interface Model {
  name: string;
  stream_id: string;
  isPublicDomain: boolean;
  encryptable?: string[];
}

export interface Output {
  createDapp: {
    id: string;
    streamIDs: Model[];
    website: string;
    name: string;
    slug: string;
    logo: string;
    description: string;
    defaultFolderName: string;
  };
}

export interface StreamsRecord {
  [streamId: string]: StreamRecord;
}

export interface StreamRecord {
  app: string;
  pkh: string;
  modelId: string;
  streamContent: StreamContent;
}
