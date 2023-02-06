export const getUniqueMsgs = (msgs) =>
  msgs.filter(
    (msg, index, self) => index === self.findIndex((m) => m.id === msg.id)
  );
