export // Checks if wallet is connected to the correct network
const checkCorrectNetwork = async (devChainId, setCorrectNetwork) => {
    const { ethereum } = window;
    let chainId = await ethereum.request({ method: "eth_chainId" });
    console.log("Connected to chain:" + chainId);

    const localhostChainId = `0x${Number(devChainId).toString(16)}`;

    if (chainId !== localhostChainId) {
        setCorrectNetwork(false);
    } else {
        setCorrectNetwork(true);
    }
};
