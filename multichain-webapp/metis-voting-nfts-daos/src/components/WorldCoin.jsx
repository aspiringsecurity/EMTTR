import { WorldIDWidget } from "@worldcoin/id";

const WorldCoinVerify = ({ setIsHuman }) => {
  const actionId = "wid_staging_c1b12e7d5b193ed0d557cebd66c6537d"; // TODO  move to env

  const verifyProof = async (proofData) => {
    const { signal, merkle_root, nullifier_hash, proof } = proofData;
    // console.log(proof); // TODO test this properly
    const url = "https://developer.worldcoin.org/api/v1/verify";
    const options = {
      method: "POST",
      headers: {
        accept: "application/json",
        "content-type": "application/json",
        "X-API-KEY": import.meta.env.VITE_MORALIS_WEB3_API_KEY,
      },
      body: JSON.stringify({
        signal,
        merkle_root,
        nullifier_hash,
        proof,
        action_id: actionId,
      }),
    };

    const res = await (await fetch(url, options)).json();
    // console.log(res); // TODO check the response object.

    if (res) setIsHuman(true);
  };

  return (
    <section>
      <div className=" h-screen flex justify-center items-center">
        <WorldIDWidget
          actionId={actionId}
          signal="Tribunal_verification"
          enableTelemetry
          onSuccess={(proof) => {
            verifyProof(proof);
          }}
          onError={(error) => console.error(error)}
        />
      </div>
    </section>
  );
};

export default WorldCoinVerify;
