import moment from "moment";
import React, {
  useCallback,
  useContext,
  useEffect,
  useRef,
  useState,
} from "react";
import { truncateWithEllipsis } from "../api/utils";
import { UserContext } from "../context/UserContext";
import { useMessages } from "../hooks/useMessages";
import { getUniqueMsgs } from "../utils/unique";

const Message = ({ msg, user }) => {
  const date = moment(msg.timestamp?.toDate()).format("MMMM DD, YYYY");
  if (msg.address === user?.address)
    return (
      <div className="flex w-full mt-2 space-x-3 max-w-xs md:max-w-md ml-auto justify-end">
        <div>
          <span className="text-xs text-gray-500 leading-none">
            {truncateWithEllipsis(msg?.address)?.toLowerCase()}
          </span>
          <div className="bg-gold text-white p-3 rounded-l-lg rounded-br-lg">
            <p className="text-sm">{msg?.text}</p>
          </div>
          <span className="text-xs text-gray-500 leading-none">{date}</span>
        </div>
      </div>
    );
  return (
    <div className="flex w-full mt-2 space-x-3 max-w-xs md:max-w-md">
      <div>
        <span className="text-xs text-gray-500 leading-none">
          {truncateWithEllipsis(msg?.address)?.toLowerCase()}
        </span>
        <div className="bg-gray-200 p-3 rounded-r-lg rounded-bl-lg">
          <p className="text-sm">{msg?.text}</p>
        </div>
        <span className="text-xs text-gray-500 leading-none">{date}</span>
      </div>
    </div>
  );
};

const Discussion = ({ proposal }) => {
  const { user, udUser, authenticated } = useContext(UserContext);

  const { messages, sendMessage, getPrevMessages, hasChatConfig } = useMessages(
    proposal?._id
  );

  const [items, setItems] = useState(undefined);
  const [message, setMessage] = useState("");

  const isGettingMessagesRef = useRef();

  const handleSend = (msg) => {
    if (authenticated) sendMessage(user, msg);
  };

  const handlePrevMessages = useCallback(() => {
    // change page only if last remote call was successful
    if (messages && isGettingMessagesRef.current === false) {
      getPrevMessages();
      isGettingMessagesRef.current = true;
    }
  }, [getPrevMessages, messages]);

  const handleKeyPress = useCallback(
    (e) => {
      if (e.key === "Enter" && message !== "") {
        if (e.shiftKey || e.ctrlKey) return;
        handleSend(message);
        setMessage("");
      }
    },
    [handleSend, message]
  );

  const handleMessage = (e) => {
    setMessage(e.target.value);
  };

  useEffect(() => {
    if (messages !== undefined) {
      setItems((prevItems) => {
        return getUniqueMsgs(
          prevItems ? [...messages, ...prevItems] : messages
        ).sort(
          (a, b) =>
            moment(a.timestamp?.toDate()).valueOf() -
            moment(b.timestamp?.toDate()).valueOf()
        );
      });
      isGettingMessagesRef.current = false;
    }
  }, [messages]);

  return (
    <div
      className={`md:rounded-xl rounded border bg-skin-block-bg border-skin-border text-base my-4 h-[450px] md:h-[650px] ${
        messages?.length <= 0 ? "h-72 md:h-72" : ""
      }`}
    >
      <h4
        className="px-4 pt-3 block rounded-t-none md:rounded-t-lg border-y md:border-t-0 border-skin-border"
        style={{ paddingBottom: "12px" }}
      >
        Discussion
      </h4>
      <div className="flex flex-col h-[calc(100%-49px)] w-full">
        <div className="flex flex-col flex-grow w-full rounded-lg overflow-hidden">
          <div className="flex flex-col flex-grow p-4 overflow-auto">
            {items?.length > 0 ? (
              items.map((msg) => (
                <Message key={msg?.id} user={user} msg={msg} />
              ))
            ) : (
              <p className="text-[14px] text-center mt-16">
                No discussions yet
              </p>
            )}
          </div>
          <input
            className={`"flex items-center h-10 w-full rounded md:rounded-b-xl px-3 text-sm focus-visible:outline-none border-gray-200 border-t ${
              authenticated ? "mb-1" : ""
            }`}
            type="text"
            placeholder={
              authenticated
                ? "What do you think..."
                : "Connect your wallet to chat"
            }
            disabled={!authenticated}
            onKeyPress={handleKeyPress}
            value={message}
            onChange={handleMessage}
          />
        </div>
      </div>
    </div>
  );
};

export default Discussion;
