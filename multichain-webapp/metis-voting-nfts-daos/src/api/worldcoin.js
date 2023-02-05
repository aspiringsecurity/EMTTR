export const verifyProof = async ({merkle_root, nullifier_hash, proof}, callback) => {
  const requestOptions = {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      merkle_root,
      nullifier_hash,
      action_id: "wid_staging_c1b12e7d5b193ed0d557cebd66c6537d",
      signal: "gameHaus-auth",
      proof
    }),
  };

  await fetch("https://developer.worldcoin.org/api/v1/verify", requestOptions)
    .then((response) => response.json())
    .then((data) => {
      // console.log(data);
      if (data.success) callback();
    });
};
