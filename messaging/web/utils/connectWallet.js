// Calls Metamask to connect wallet on clicking Connect Wallet button
export const connectWallet = async (devChainId) => {
    try {
        const { ethereum } = window;

        if (!ethereum) {
            console.log("Metamask not detected");
            return;
        }
        let chainId = await ethereum.request({ method: "eth_chainId" });
        console.log("Connected to chain:" + chainId);

        const rinkebyChainId = "0x4";

        const localhostChainId = `0x${Number(devChainId).toString(16)}`;

        if (chainId !== localhostChainId) {
            alert("You are not connected to the right network!");
            return;
        }

        const accounts = await ethereum.request({ method: "eth_requestAccounts" });

        console.log("Found account", accounts[0]);
        setCurrentAccount(accounts[0]);
    } catch (error) {
        console.log("Error connecting to metamask", error);
    }
};
