import contractABIPolygon from "./createTribunalABI.json";
import contractABIV2 from "./createTribunalABI-two.json";
export const polygonChainId = "0x89";
export const metisChainId = "0x257";
export const evmosChainId = "0x2328";
export const polygonExplorer = "https://polygonscan.com/";
export const metisExplorer = "https://goerli.explorer.metisdevops.link/";
export const evmosExplorer = "https://evm.evmos.dev/";
export const contractDPConstant = 1e18; // handle decimal places

const contract_address_polygon = import.meta.env
  .VITE_CREATE_TRIB_CONTRACT_ADDRESS_POLYGON;
const contract_address_metis = import.meta.env
  .VITE_CREATE_TRIB_CONTRACT_ADDRESS_METIS;
const contract_address_evmos = import.meta.env
  .VITE_CREATE_TRIB_CONTRACT_ADDRESS_EVMOS;

export const contract_addresses = {
  [polygonChainId]: contract_address_polygon,
  [metisChainId]: contract_address_metis,
  [evmosChainId]: contract_address_evmos,
};

export const explorers = {
  [polygonChainId]: polygonExplorer,
  [metisChainId]: metisExplorer,
  [evmosChainId]: evmosExplorer,
};

export const contractABIs = {
  [polygonChainId]: contractABIPolygon,
  [metisChainId]: contractABIV2,
  [evmosChainId]: contractABIV2,
};

export const supportedChainIds = [polygonChainId, metisChainId, evmosChainId];

export const supportedChains = {
  [polygonChainId]: "Polygon mainnet",
  [metisChainId]: "Metis testnet",
  [evmosChainId]: "Evmos testnet",
};
