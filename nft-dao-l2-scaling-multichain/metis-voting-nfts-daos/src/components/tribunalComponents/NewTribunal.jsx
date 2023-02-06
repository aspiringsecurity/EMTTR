import React, { useContext, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useCreateTribunalAction } from "../../api/createTribunal";
import { createTribunal } from "../../api/tribunals";
import { mmAuthenticate, truncateWithEllipsis } from "../../api/utils";
import FileUploader from "../Minter/uploadImage";
import { UserContext } from "../../context/UserContext";
import { useEffect } from "react";

const NewTribunal = () => {
  const navigate = useNavigate();
  const {
    isCreating,
    created,
    txLink,
    error: createTRibError,
    createTrib,
    newTrib,
  } = useCreateTribunalAction();
  const { user, authenticated } = useContext(UserContext);

  const [fileUrl, setFileUrl] = useState("");
  const [error, setError] = useState(null);
  const [success, setSuccess] = useState(null);
  const [submitting, setSubmitting] = useState(false);
  const [values, setValues] = useState({
    tribunalName: "",
    email: "",
    walletAddress: "",
    mintFee: 0,
    about: "",
  });

  const handleChange = (e) => {
    if (e.target.name === "mintFee") {
      setValues({
        ...values,
        [e.target.name]: e.target.value,
      });
    } else setValues({ ...values, [e.target.name]: e.target.value });
  };
  const handleValidate = () => {
    const { tribunalName, email, walletAddress, mintFee, about } = values;
    if (
      tribunalName &&
      email &&
      walletAddress &&
      about &&
      fileUrl !== "" &&
      parseFloat(mintFee) > 0
    ) {
      return true;
    }
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setSubmitting(true);

    try {
      if (!user) {
        setError("Please connect your wallet to continue");
        return;
      }
      if (!handleValidate() || !user) return;

      const ver = await mmAuthenticate({
        signingMessage: "Verify wallet address to start your Tribunal",
      });

      if (ver) {
        const { address, name } = user;
        const { tribunalName, email, walletAddress, mintFee, about } = values;

        const [newTribAdd, chainId] = await createTrib(
          tribunalName,
          walletAddress,
          fileUrl,
          parseFloat(mintFee)?.toFixed(4)
        );

        const tribunal = {
          tribunalName,
          email,
          walletAddress,
          mintFee: parseFloat(mintFee)?.toFixed(4),
          about,
          fileUrl,
          creator: address,
          creatorName: name,
          contractAddress: newTribAdd,
          chainId,
        };

        const response = await createTribunal(tribunal);
        if (response) {
          setSuccess("Tribunal created successfully");
          setTimeout(() => {
            setSuccess(null);
            navigate(`/tribunal/${response._id}`);
          }, 5000);
        }
      } else {
        setError("Wallet authentication failed.");
        setTimeout(() => setError(null), 5000);
      }
    } catch (err) {
      console.log(err);
      setError(err.message);
      setTimeout(() => setError(null), 5000);
    }

    setSubmitting(false);
  };

  useEffect(() => {
    if (user) setValues((prev) => ({ ...prev, walletAddress: user.address }));
  }, [user]);

  useEffect(() => {
    if (createTRibError) setError(createTRibError.message);
  }, [isCreating, created, createTRibError]);

  return (
    <div className="mt-10 sm:mt-0">
      <div className="md:grid md:grid-cols-3 md:gap-6">
        <div className="md:col-span-1">
          <div className="px-4 sm:px-0">
            <h3 className="text-lg font-medium leading-6 text-gray-900">
              Tribunal Information
            </h3>
            <p className="mt-1 text-sm text-gray-600">
              Tribunal NFTs (TRIB) are minted on the Polygon. Please switch to
              Polygon Network to continue
            </p>
          </div>
        </div>
        <div className="mt-5 md:mt-0 md:col-span-2">
          <form action="#" method="POST">
            <div className="shadow overflow-hidden sm:rounded-md">
              <div className="px-4 py-5 bg-white sm:p-6">
                <div className="grid grid-cols-6 gap-6">
                  <div className="col-span-6 sm:col-span-3">
                    <label
                      htmlFor="tribunalName"
                      className="block text-sm font-medium text-gray-700"
                    >
                      Tribunal name*
                    </label>
                    <input
                      type="text"
                      name="tribunalName"
                      id="tribunalName"
                      autoComplete="given-name"
                      value={values.tribunalName}
                      onChange={handleChange}
                      className="h-[38px] px-2 focus:outline-none mt-1 focus:ring-gold border focus:border-gold block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                    />
                  </div>
                  <div className="col-span-6 sm:col-span-3">
                    <label
                      htmlFor="email"
                      className="block text-sm font-medium text-gray-700"
                    >
                      Email address*
                    </label>
                    <input
                      type="text"
                      name="email"
                      id="email"
                      autoComplete="email"
                      value={values.email}
                      onChange={handleChange}
                      className="h-[38px] px-2 focus:outline-none mt-1 focus:ring-gold border focus:border-gold block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                    />
                  </div>
                  <div className="col-span-6 sm:col-span-4">
                    <label
                      htmlFor="walletAddress"
                      className="block text-sm font-medium text-gray-700"
                    >
                      Wallet address (Mint Fee is sent here)*
                    </label>
                    <input
                      type="text"
                      name="walletAddress"
                      id="walletAddress"
                      autoComplete="walletAddress"
                      value={values.walletAddress}
                      onChange={handleChange}
                      className="h-[38px] px-2 focus:outline-none mt-1 focus:ring-gold border focus:border-gold block w-full shadow-sm sm:text-sm border-gray-300 rounded-md"
                    />
                  </div>
                  <div className="col-span-6 sm:col-span-2">
                    <label
                      htmlFor="mintFee"
                      className="block text-sm font-medium text-gray-700"
                    >
                      Mint Fee (in base token)
                    </label>
                    <input
                      type="number"
                      step={0.0001}
                      name="mintFee"
                      id="mintFee"
                      min={0}
                      autoComplete="mintFee"
                      placeholder="Free"
                      value={values.mintFee}
                      onChange={handleChange}
                      className={`${
                        values.mintFee < 0 ? "border-red-500" : ""
                      } h-[38px] px-2 focus:outline-none mt-1 focus:ring-gold border focus:border-gold block w-full shadow-sm sm:text-sm border-gray-300 rounded-md`}
                    />
                  </div>
                  <div className="col-span-6">
                    <label
                      htmlFor="about"
                      className="block text-sm font-medium text-gray-700"
                    >
                      About this Tribunal*
                    </label>
                    <div className="mt-1">
                      <textarea
                        id="about"
                        name="about"
                        rows={4}
                        className="focus:outline-none px-2 py-2 shadow-sm focus:ring-gold focus:border-gold mt-1 block w-full sm:text-sm border border-gray-300 rounded-md"
                        placeholder=""
                        value={values.about}
                        onChange={handleChange}
                      />
                    </div>
                    <p className="mt-2 text-sm text-gray-500">
                      Brief description for this Tribunal.
                    </p>
                  </div>{" "}
                  <div className="col-span-6">
                    <label
                      htmlFor="tribunalNftName"
                      className="block text-sm font-medium text-gray-700"
                    >
                      Tribunal NFT photo*
                    </label>
                    <FileUploader fileUrl={fileUrl} setFileUrl={setFileUrl} />
                  </div>
                </div>
              </div>
              <div className="px-4 py-3 bg-gray-50 text-center sm:px-6">
                <button
                  type="submit"
                  className={`${
                    handleValidate() ? "" : "opacity-30"
                  } inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-gold hover:bg-gold focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-gold`}
                  onClick={handleSubmit}
                  disabled={!handleValidate()}
                >
                  {submitting ? "Creating..." : "Create"}
                </button>
                {(success || error || txLink) && (
                  <>
                    <p
                      className={`${
                        error ? "" : "hidden"
                      }text-center text-sm text-red-500 mt-2`}
                    >
                      {error}
                    </p>
                    <p
                      className={`${
                        success ? "" : "hidden"
                      }text-center text-sm text-green-500 mt-2`}
                    >
                      {success}
                    </p>
                    <a
                      target="_blank"
                      href={txLink}
                      className={`${
                        txLink ? "" : "hidden"
                      }text-center text-sm text-gold mt-2`}
                    >
                      {truncateWithEllipsis(txLink, 26)}
                    </a>
                  </>
                )}
              </div>
            </div>
          </form>
        </div>
      </div>
    </div>
  );
};

export default NewTribunal;
