// THIS EXAMPLE DOES NOT CHECK IF ETHEREUM ADDRESS HAS A VALID CHECKSUM
// USE WITH CAUTION
function isAddress(address) {
    return /^(0x)?[0-9a-f]{40}$/i.test(address);
}

async function getEligibleConversations(isoStartDate) {
    const conversationsResponse = await Functions.makeHttpRequest({
        url: `https://graph.facebook.com/v16.0/${secrets.FACEBOOK_PAGE_ID}/conversations?fields=messages.limit(1){message,from},updated_time&access_token=${secrets.FACEBOOK_GRAPH_API_KEY}`
    })

    if (conversationsResponse.status === 200) {
        const conversationsObject = conversationsResponse.data;
        const conversations = conversationsObject.data;

        const eligibleConversations = conversations.filter(conversation => new Date(conversation.updated_time) > isoStartDate);
        return eligibleConversations;
    } else {
        throw Error(conversationsResponse.statusText);
    }
}

async function chooseWinners(eligibleConversations, numberOfWinners) {
    let winnersArray = [];
    const length = eligibleConversations.length;

    for (let i = 0; i < length;) {
        // we are getting only the latest received message from the conversation with the user
        const current = eligibleConversations[i].messages.data[0].message;

        if (current.includes("#giveaway")) {
            const walletAddress = current.substr(current.indexOf("0x"), 42);
            if (isAddress(walletAddress)) {
                winnersArray.push({
                    walletAddress: walletAddress,
                    senderId: eligibleConversations[i].messages.data[0].from.id
                });
                if (winnersArray.length == numberOfWinners) {
                    return winnersArray;
                }
            }
        }

        ++i;
    }

    throw Error("No eligible addresses");
}

async function sendNotification(recipientId) {
    await Functions.makeHttpRequest({
        method: 'POST',
        url: `https://graph.facebook.com/v16.0/${secrets.FACEBOOK_PAGE_ID}/messages?recipient={'id':'${recipientId}'}&messaging_type=MESSAGE_TAG&message={'text':'Congratulations, you were successful in winning one of our special unique NFTs to celebrate the launch of our new product! Please check your wallet address that you specified in this conversation, you should now be able to see your NFT there, or in the Instagram Digital Collectibles album if you have linked the specified wallet address to your Instagram account.'}&tag=CONFIRMED_EVENT_UPDATE&access_token=${secrets.FACEBOOK_GRAPH_API_KEY}`
    })
}

async function main() {
    const isoStartDate = new Date(args[0]);
    const numberOfWinners = args[1];
    const testerAccounts = secrets.TESTER_ACCOUNTS_IDS && JSON.parse(secrets.TESTER_ACCOUNTS_IDS)

    const eligibleConversations = await getEligibleConversations(isoStartDate);
    if (eligibleConversations === undefined || eligibleConversations.length === 0) {
        throw Error("No eligible conversations");
    }

    // conversations are stored based on the latest update:
    // 1. the newest
    // 2. old
    // 3. the oldest
    // 
    // we want to find the earliest respdonding eligible address to award it with an NFT
    const sortedEligibleConversations = eligibleConversations.reverse();

    const chosenWinners = await chooseWinners(sortedEligibleConversations, numberOfWinners);

    const winners = chosenWinners.map(({ walletAddress }) => Buffer.from(walletAddress.slice(2), 'hex'))

    chosenWinners.forEach(async ({ senderId }) => {
        if (testerAccounts.includes(senderId)) {
            await sendNotification(senderId);
        }
    });

    return winners;
}


const winners = await main();

return Buffer.concat(winners);