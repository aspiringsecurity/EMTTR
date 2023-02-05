import { useCallback, useEffect, useState } from "react";
import {
  getMessages,
  sendMessage,
  hasChatConfig,
} from "../context/chatContext";

const useMessages = (proposalId) => {
  const [messages, setMessages] = useState();
  const handleSendMessage = (account, message, pId = proposalId) => {
    sendMessage(account, message, pId).catch((err) =>
      console.error("useMessages::handleSendMessage: Unexpected Error", err)
    );
  };

  const getPrevMessages = useCallback(() => {
    getMessages(
      (m) => {
        setMessages(m);
      },
      messages,
      proposalId
    );
  }, [messages, proposalId]);

  useEffect(() => {
    // getMessages returns an unsubscribe method
    const unsubscribe = getMessages(
      (m) => {
        setMessages(m);
      },
      undefined,
      proposalId
    );

    return unsubscribe;
  }, [proposalId]);

  return {
    messages,
    sendMessage: handleSendMessage,
    getPrevMessages,
    hasChatConfig,
  };
};

export { useMessages };
