import Image from "next/image";
import { useAccount, useConnect, useEnsName, useDisconnect } from "wagmi";
import { InjectedConnector } from "wagmi/connectors/injected";
import toast from "react-hot-toast";
import Link from "next/link";
import { useState } from "react";
import { useSmartContract } from "../hooks/useSmartContract";

const Navbar = (props) => {
  const { createDataDao } = useSmartContract();

  const { address, isConnected } = useAccount();
  const { setIsAuthenticated } = props;
  setIsAuthenticated(isConnected);
  // TODO: Wallaby does not support ENS. Need a separate Ethereu provider to useEnsName
  // const { data: ensName } = useEnsName({ address });
  const { disconnect } = useDisconnect({
    onSuccess() {
      toast("Account disconnected!", {
        style: {
          border: "2px solid #000",
        },
      });
      setIsAuthenticated(false);
    },
    onError() {
      toast.error("Failed to disconnect account!", {
        style: {
          border: "2px solid #000",
        },
      });
    },
  });
  const { connect } = useConnect({
    chainId: 31415,
    connector: new InjectedConnector(),
    onSuccess() {
      toast.success("Account connected!", {
        style: {
          border: "2px solid #000",
        },
      });
      setIsAuthenticated(true);
    },
    onError() {
      toast.error("Error connecting account!", {
        style: {
          border: "2px solid #000",
        },
      });
    },
  });

  return (
    <div id="navbar" className="navbar sticky top-0 z-50 text-tertiary">
      <div className="navbar-start">
        <div className="dropdown">
          <label tabIndex={0} className="btn btn-ghost lg:hidden">
            <svg
              xmlns="http://www.w3.org/2000/svg"
              className="h-5 w-5"
              fill="none"
              viewBox="0 0 24 24"
              stroke="currentColor"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth="2"
                d="M4 6h16M4 12h8m-8 6h16"
              />
            </svg>
          </label>
          <ul
            tabIndex={0}
            className="menu menu-compact dropdown-content mt-3 p-2 shadow rounded-box w-52 bg-quaternary"
          >
            <li>
              <Link href="/">Home</Link>
            </li>
            <li>
              <Link href="/about">About Us</Link>
            </li>
          </ul>
        </div>
        <Image
          className="hover:animate-spin cursor-pointer"
          src="/wizzie.png"
          width={50}
          height={50}
          alt="yo"
          onClick={() => {
            window.location.href = "https://github.com/Open-Sorcerer";
          }}
        />
      </div>
      <div className="navbar-end">
        <div className="flex space-x-8 mr-8 text-lg">
          <Link
            href="/"
            id="create"
            className="hidden md:block relative before:content-[''] before:absolute before:block before:w-full before:h-[1px] 
              before:bottom-0 before:left-0 before:bg-tertiary
              before:hover:scale-x-100 before:scale-x-0 before:origin-top-left
              before:transition before:ease-in-out before:duration-300
              active:after:content-[''] active:after:absolute active:after:block active:after:w-full active:after:h-[1px]"
          >
            Home
          </Link>
          <Link
            href="/about"
            className="hidden md:block relative before:content-[''] before:absolute before:block before:w-full before:h-[1px] 
              before:bottom-0 before:left-0 before:bg-tertiary
              before:hover:scale-x-100 before:scale-x-0 before:origin-top-left
              before:transition before:ease-in-out before:duration-300"
          >
            About Us
          </Link>
        </div>
        <button
          onClick={() => {
            if (isConnected) {
              disconnect();
            } else {
              connect();
            }
          }}
          className="relative inline-block px-4 py-2 font-medium group "
        >
          <span className="absolute rounded-lg inset-0 w-full h-full transition duration-200 ease-out transform translate-x-1 translate-y-1 bg-tertiary border-[2px] border-black group-hover:-translate-x-0 group-hover:-translate-y-0"></span>
          <span className="absolute rounded-lg inset-0 w-full h-full bg-white border-2 border-black group-hover:bg-tertiary"></span>
          {address ? (
            <span className="relative text-black">
              {address.slice(0, 6) + "..." + address.slice(-4)}
            </span>
          ) : (
            <span className="relative text-black">Connect Wallet</span>
          )}
        </button>
      </div>
    </div>
  );
};

export default Navbar;
