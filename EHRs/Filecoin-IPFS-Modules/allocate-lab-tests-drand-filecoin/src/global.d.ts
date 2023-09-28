declare global {
    interface Window {
        ethereum: import("ethers").AbstractProvider;
    }
}
export {}