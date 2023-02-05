import path from "path";
import dotenv from "dotenv";

interface ENV {
    NODE_ENV: string | undefined;
    PRIVATE_KEY: string | undefined;
    ALTERNATE_PRIVATE_KEY: string | undefined;

    AA_FACTORY_ADDRESS: string | undefined;
    ACCOUNT_ADDRESS: string | undefined;
    GREETER_ADDRESS: string | undefined;
    PLUGIN_ADDRESS: string | undefined;

    API_KEY: string | undefined;
    API_ENDPOINT: string | undefined;
}

interface Config {
    NODE_ENV: string;
    PRIVATE_KEY: string;
    ALTERNATE_PRIVATE_KEY: string;

    AA_FACTORY_ADDRESS: string;
    ACCOUNT_ADDRESS: string;
    GREETER_ADDRESS: string;
    PLUGIN_ADDRESS: string;

    API_KEY: string;
    API_ENDPOINT: string;
}

dotenv.config({ path: path.resolve(__dirname, '../.env')});

const getConfig = (): ENV => {
    return {
        NODE_ENV: process.env.NODE_ENV,
        PRIVATE_KEY: process.env.PRIVATE_KEY ? String(process.env.PRIVATE_KEY) : undefined,
        ALTERNATE_PRIVATE_KEY: process.env.ALTERNATE_PRIVATE_KEY ? String(process.env.ALTERNATE_PRIVATE_KEY) : undefined,

        AA_FACTORY_ADDRESS: process.env.AA_FACTORY_ADDRESS ? String(process.env.AA_FACTORY_ADDRESS) : undefined,
        ACCOUNT_ADDRESS: process.env.ACCOUNT_ADDRESS ? String(process.env.ACCOUNT_ADDRESS) : undefined,
        GREETER_ADDRESS: process.env.GREETER_ADDRESS ? String(process.env.GREETER_ADDRESS) : undefined,
        PLUGIN_ADDRESS: process.env.PLUGIN_ADDRESS ? String(process.env.PLUGIN_ADDRESS) : undefined,
        
        API_KEY: process.env.API_KEY ? String(process.env.API_KEY) : undefined,
        API_ENDPOINT: process.env.API_ENDPOINT ? String(process.env.API_ENDPOINT) : undefined,
    };
};

const getSanitzedConfig = (config: ENV): Config => {
    for (const [key, value] of Object.entries(config)) {
        if (value === undefined) {
            throw new Error(`Missing key '${key}' in .env`);
        }
    } 

    return config as Config;
}

const config = getConfig();

const sanitizedConfig = getSanitzedConfig(config);

export default sanitizedConfig;















