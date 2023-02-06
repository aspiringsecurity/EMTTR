import React, { useContext, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import CheckNFTs, { VerifyNFTs } from "../../api/checkNFTs";
import { createProposal } from "../../api/proposals";
import { UserContext } from "../../context/UserContext";
import DateInput from "../DateInput";

const NewProposal = ({ dao }) => {
  const navigate  = useNavigate()
  const { user } = useContext(UserContext);

  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState(null);
  const [success, setSuccess] = useState(null);
  const [title, setTitle] = useState(null);
  const [desc, setDesc] = useState(null);
  const [startDate, setStartDate] = useState(null);
  const [endDate, setEndDate] = useState(null);

  const handleSubmit = async (event) => {
    setSubmitting(true);
    try {
      if (title.target && desc.target && startDate.target && endDate.target) {
        if (
          title.target.value.length > 0 &&
          desc.target.value.length > 0 &&
          startDate.target.value.length > 0 &&
          endDate.target.value.length > 0
        ) {
          if (user && dao) {
            const hasNFt = await VerifyNFTs(dao.contract_address, user.address);

            if (hasNFt) {
              const data = {
                title: title.target.value,
                desc: desc.target.value,
                startDate: startDate.target.value,
                endDate: endDate.target.value,
                author: user.address,
                authorName: user.name,
                votes: {},
                tribunalName: dao.tribunalName,
                tribunalId: dao._id,
                tribunalAddress: dao.contract_address,
              };
              const response = await createProposal(data);
              if (response) {
                setSuccess("Proposal created successfully");
                setTimeout(() => setSuccess(null), 5000);
                setTimeout(() => {
                  navigate(`/${dao._id}/proposals`)
                }, 5000);
              }

              event.preventDefault();
            } else {
              setError("Wallet authentication failed.");
              setTimeout(() => setError(null), 5000);
            }
          } else {
            setError(
              "No NFTs for this Tribunal in your wallet. Are you signed in?"
            );
            setTimeout(() => setError(null), 5000);
          }
        } else {
          setError("A field is empty. All fields are required.");
          setTimeout(() => setError(null), 5000);
        }
      } else {
        setError("All fields are required.");
        setTimeout(() => setError(null), 5000);
      }
    } catch (err) {
      console.log(err);
      setError(err.message);
      setTimeout(() => setError(null), 5000);
    }

    setSubmitting(false);
  };

  return (
    <div className="lg:flex max-w-3xl justify-start lg:ml-72">
      <div className="xl:w-9/12 lg:pr-5 relative" id="content-left">
        <div className=" mb-3 flex relative">
          <div className="w-full px-5 md:px-0 flex justify-between items-center">
            <div className="flex items-center flex-auto">
              <h2 className="font-serif text-xl">New Proposals</h2>
            </div>
            <Link
              to={`/${dao.slug}/proposals`}
              className="flex items-center flex-auto justify-end"
            >
              <svg
                xmlns="http://www.w3.org/2000/svg"
                className="icon icon-tabler icon-tabler-arrow-narrow-left"
                width="24"
                height="24"
                viewBox="0 0 24 24"
                strokeWidth="1.5"
                stroke="#2c3e50"
                fill="none"
                strokeLinecap="round"
                strokeLinejoin="round"
              >
                <path stroke="none" d="M0 0h24v24H0z" fill="none" />
                <line x1="5" y1="12" x2="19" y2="12" />
                <line x1="5" y1="12" x2="9" y2="16" />
                <line x1="5" y1="12" x2="9" y2="8" />
              </svg>
              <h2 className="text-sm">Back</h2>
            </Link>
          </div>
          <div className="relative inline-block text-left h-full">
            <div className="inline-flex items-center w-full h-full cursor-pointer"></div>
          </div>
        </div>

        <div className="md:rounded-xl md:border bg-skin-block-bg border-skin-border text-base text-skin-text !border-skin-link text-skin-link mb-8">
          <div className="p-4 leading-5 sm:leading-6">
            <i className="iconfont iconwarning mr-1 float-left"></i>
            <div className="leading-5">
              <span>
                You need to have a minimum of 1 NFT from this DAO to submit a
                proposal.
              </span>
            </div>
          </div>
        </div>
        <div className="px-4 md:px-0">
          <div className="flex flex-col">
            <div className="mb-6">
              <label className="s-label">Title</label>
              <div>
                <div className="z-10 relative">
                  <input
                    onChange={setTitle}
                    maxLength="128"
                    // className="text-md font-semibold s-input w-full !rounded-full"
                    className="py-2 px-4 w-full min-h-[40px] border-gray-200 border focus:border !rounded-xl text-base h-full mt-0 mb-4 focus-visible:outline-none"
                  />
                </div>
                <div className="s-error -mt-[38px] opacity-0 h-6"> </div>
              </div>
            </div>
            <div className="mb-6">
              <div className="flex justify-between">
                <label className="s-label">Description</label>
                <div className="text-xs">max 14,400</div>
              </div>
              <div>
                <div className="min-h-[240px] peer border rounded-t-xl overflow-hidden focus-within:border-skin-text">
                  <textarea
                    className="py-2 px-4 w-full min-h-[240px] border-none !rounded-xl text-base h-full mt-0 focus-visible:outline-none"
                    maxLength="14400"
                    onChange={setDesc}
                  ></textarea>
                </div>
                {/* Images */}
                {/* <label className="relative flex justify-between border border-skin-border rounded-b-xl py-1 px-2 items-center peer-focus-within:border-skin-text border-t-0">
                  <input
                    accept="image/jpg, image/jpeg, image/png"
                    type="file"
                    className="opacity-0 absolute p-[5px] top-0 right-0 bottom-0 left-0 w-full ml-0"
                  />
                  <span className="pointer-events-none relative pl-1 text-sm">
                    <span>
                      Attach images by dragging &amp; dropping, selecting or
                      pasting them.
                    </span>
                  </span>
                  <a
                    href="https://docs.github.com/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax"
                    target="_blank"
                    className="whitespace-nowrap relative inline"
                    rel="noopener noreferrer"
                  >
                    <i
                      className="iconfont iconmarkdown text-skin-text"
                      style={{ fontSize: "16px", lineHeight: "16px" }}
                    ></i>
                  </a>
                </label> */}
              </div>
            </div>
            <div className="mb-6">
              <div className="z-10 relative flex justify-between">
                <DateInput title="Start Date" handleChange={setStartDate} />
                <DateInput title="End Date" handleChange={setEndDate} />
              </div>
            </div>
            {/* <div className="flex flex-col mb-8">
              <label className="">Discussion (optional)</label>
              <input
                className="s-input w-full border-gray-200 border h-10 p-2 px-4 !rounded-full"
                placeholder="e.g. https://forum.balancer.fi/proposal..."
              />
            </div> */}

            <div className="">
              <div className="p-4 leading-5 flex flex-col justify-center sm:leading-6">
                <button
                  onClick={handleSubmit}
                  className="text-white bg-gold border-0 py-2 px-6 focus:outline-none hover:bg-yellow-600 rounded text-lg mt-10 sm:mt-0"
                >
                  {submitting ? (
                    <span className="flex justify-center">
                      <svg
                        role="status"
                        className="mr-2 -ml-1 mt-1 w-5 h-5 text-gray-200 animate-spin dark:text-gray-600 fill-brown"
                        viewBox="0 0 100 101"
                        fill="none"
                        xmlns="http://www.w3.org/2000/svg"
                      >
                        <path
                          d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
                          fill="currentColor"
                        />
                        <path
                          d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
                          fill="currentFill"
                        />
                      </svg>
                      <span className="ml-2"> Submitting</span>
                    </span>
                  ) : (
                    "Submit"
                  )}
                </button>
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
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default NewProposal;
