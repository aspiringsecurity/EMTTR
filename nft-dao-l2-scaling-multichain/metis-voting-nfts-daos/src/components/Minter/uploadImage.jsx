// FileUpload Component : Uploads the selcted File and returns the URL after uploading the file .
// import React from "react";
import { Web3Storage } from "web3.storage";
import { useRef } from "react";
import { useState } from "react";

const FileUploader = ({ fileUrl, setFileUrl }) => {
  // drag state
  const [dragActive, setDragActive] = useState(false);
  const [fileSelected, setfileSelected] = useState(false);
  const [uploading, setUploading] = useState(false);
  const inputRef = useRef(null);

  // uploads the file to IPFS
  const uploadFile = async (ufile) => {
    // const ipfs = window.IpfsHttpClient.create(
    //   "https://ipfs.infura.io:5001/api/v0"
    // );

    // setUploading(true);

    // try {
    //   const added = await ipfs.add(ufile);
    //   const url = `https://ipfs.infura.io/ipfs/${added.path}`;
    //   setFileUrl(url);
    // } catch (err) {
    //   console.error("Error uploading the file : ", err);
    // }
    // setUploading(false);

    const client = new Web3Storage({
      token: import.meta.env.VITE_WEB3STORAGE_TOKEN,
    });

    const files = [ufile];

    setUploading(true);
    const cid = await client.put(files);
    const url = `https://${cid}.ipfs.w3s.link/${ufile.name}`;
    setUploading(false);
    setFileUrl(url);

    return cid;
  };

  // handle drag events
  const handleDrag = function (e) {
    e.preventDefault();
    e.stopPropagation();
    if (e.type === "dragenter" || e.type === "dragover") {
      setDragActive(true);
    } else if (e.type === "dragleave") {
      setDragActive(false);
    }
  };

  // handle file selection
  const handleFile = function (file) {
    uploadFile(file);
    setfileSelected(true);

    var reader = new FileReader();
    reader.onload = function (e) {
      document.getElementById("preview").src = e.target.result;
    };
    reader.readAsDataURL(file);
  };

  // triggers when file is dropped
  const handleDrop = function (e) {
    e.preventDefault();
    e.stopPropagation();
    setDragActive(false);
    if (e.dataTransfer.files && e.dataTransfer.files[0]) {
      handleFile(e.dataTransfer.files[0]);
    }
  };

  // triggers the input when the button is clicked
  const onButtonClick = () => {
    inputRef.current.click();
  };

  // triggers when file is selected with click
  const handleChange = function (e) {
    e.preventDefault();
    // 10Mb limit
    if (e.target.files[0].size > 10485760) {
      alert("File is too big!");
      return;
    }
    if (e.target.files && e.target.files[0]) {
      handleFile(e.target.files[0]);
    }
  };

  return (
    <div className="relative">
      {dragActive && (
        <div
          className="absolute top-0 left-0 right-0 bottom-0 w-full h-full cursor-pointer"
          id="drag-file-element"
          onDragEnter={handleDrag}
          onDragLeave={handleDrag}
          onDragOver={handleDrag}
          onDrop={handleDrop}
        ></div>
      )}
      <div
        onDragEnter={handleDrag}
        onClick={onButtonClick}
        className={`${
          dragActive ? "border-gray-400" : "border-gray-300"
        } mt-1 flex justify-center px-6 pt-5 pb-6 border border-dashed rounded-md cursor-pointer`}
      >
        <input
          ref={inputRef}
          id="file-upload"
          name="file-upload"
          type="file"
          multiple={false}
          className="sr-only focus:outline-none"
          onChange={handleChange}
          accept="image/png, image/gif, image/jpeg"
        />
        {fileSelected ? (
          <div>
            <img
              id="preview"
              className="max-w-24 max-h-24 text-center"
              src="#"
              alt=""
            />
          </div>
        ) : (
          <div className="space-y-1 text-center">
            <svg
              className="mx-auto h-12 w-12 text-gray-400"
              stroke="currentColor"
              fill="none"
              viewBox="0 0 48 48"
              aria-hidden="true"
            >
              <path
                d="M28 8H12a4 4 0 00-4 4v20m32-12v8m0 0v8a4 4 0 01-4 4H12a4 4 0 01-4-4v-4m32-4l-3.172-3.172a4 4 0 00-5.656 0L28 28M8 32l9.172-9.172a4 4 0 015.656 0L28 28m0 0l4 4m4-24h8m-4-4v8m-12 4h.02"
                strokeWidth={2}
                strokeLinecap="round"
                strokeLinejoin="round"
              />
            </svg>
            <div className="flex text-sm text-gray-600">
              <p className="relative cursor-pointer bg-white rounded-md font-medium text-gold hover:text-yellow-600 focus-within:outline-none">
                <span>Upload a file</span>
              </p>
              <p className="pl-1">or drag and drop</p>
            </div>
            <p className="text-xs text-gray-500">PNG, JPG, GIF up to 10MB</p>
          </div>
        )}
      </div>
      <a
        href={fileUrl}
        target="_blank"
        className="block text-sm text-gold w-72 overflow-hidden text-ellipsis whitespace-nowrap"
      >
        {fileSelected
          ? uploading
            ? "Uploading..."
            : fileUrl
            ? fileUrl
            : ""
          : ""}
      </a>
    </div>
  );
};

export default FileUploader;
