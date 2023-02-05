import { createContext } from "react";
import { initializeApp } from "firebase/app";
import {
  addDoc,
  collection,
  getFirestore,
  serverTimestamp,
  onSnapshot,
  query,
  orderBy,
  limit,
  where,
} from "firebase/firestore";
import { CHAT_CONFIG, DEFAULT_ROOM } from "../api/chatConfig";
import { truncateWithEllipsis } from "../api/utils";

// Initialize Firebase
const app = initializeApp(CHAT_CONFIG);
const db = getFirestore(app);

export const hasChatConfig = CHAT_CONFIG.apiKey !== undefined;

export const sendMessage = async (account, message, proposalId) => {
  if (account.address === null) return;
  try {
    await addDoc(collection(db, "proposals", proposalId, "messages"), {
      address: account.address,
      displayName: account?.ens || truncateWithEllipsis(account.address, 8),
      text: message.trim(),
      timestamp: serverTimestamp(),
    });
  } catch (error) {
    console.error("ChatContext::sendMessage: ", error);
  }
};

export const getMessages = (callback, prevMessages, proposalId = DEFAULT_ROOM) => {
  const lastTimestamp = prevMessages
    ? prevMessages[prevMessages.length - 1]?.timestamp
    : null;
  const curQuery = lastTimestamp
    ? query(
        collection(db, "proposals", proposalId, "messages"),
        orderBy("timestamp", "desc"),
        where("timestamp", "<", lastTimestamp),
        limit(20)
      )
    : query(
        collection(db, "proposals", proposalId, "messages"),
        orderBy("timestamp", "desc"),
        limit(20)
      );
  return onSnapshot(curQuery, (querySnapshot) => {
    const messages = querySnapshot.docs.map((document) => ({
      id: document.id,
      ...document.data(),
    }));
    callback(messages);
  });
};

export const ChatContext = createContext();

if (!hasChatConfig)
  console.error("ChatContext: Chat env. config was not found");

export const ChatProvider = ({ children }) => {
  return (
    <ChatContext.Provider value={{ sendMessage, getMessages }}>
      {children}
    </ChatContext.Provider>
  );
};
