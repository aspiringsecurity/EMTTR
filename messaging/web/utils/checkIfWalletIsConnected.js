export const checkIfWalletIsConnected = async (setCurrentAccount) => {
    const { ethereum } = window;
    if (ethereum) {
        console.log("Got the ethereum obejct: ", ethereum);
    } else {
        console.log("No Wallet found. Connect Wallet");
    }

    const accounts = await ethereum.request({ method: "eth_accounts" });

    if (accounts.length !== 0) {
        console.log("Found authorized Account: ", accounts[0]);
        setCurrentAccount(accounts[0]);
    } else {
        console.log("No authorized account found");
    }
};
