import React, { Fragment, useContext } from "react";
import { Link } from "react-router-dom";
import logo from "../assets/png/logo.png";
import { Disclosure, Menu, Transition } from "@headlessui/react";
import { BellIcon, MenuIcon, XIcon } from "@heroicons/react/outline";
import { UserContext } from "../context/UserContext";
import Metamask from "../assets/svg/metamask.svg?component";
import Unstoppable from "../assets/svg/unstoppable.svg?component";
import Coinbase from "../assets/svg/coinbase.svg?component";

const navigation = [
  { name: "Create A Tribunal", href: "/tribunal/new", current: false },
  { name: "All Tribunals", href: "/#all-tribunals", current: false },
  // { name: "Projects", href: "#", current: false },
  // { name: "Calendar", href: "#", current: false },
];

function classNames(...classes) {
  return classes.filter(Boolean).join(" ");
}
const Navbar = () => {
  const {
    user,
    udUser,
    signOut,
    authenticated,
    handleAuthenticate,
    handleCBAuthenticate,
  } = useContext(UserContext);

  return (
    <Disclosure as="nav" className="">
      {({ open }) => (
        <>
          <div className="max-w-7xl mx-auto px-2 sm:px-6 lg:px-8">
            <div className="relative flex items-center justify-between h-16">
              <div className="absolute inset-y-0 left-0 flex items-center sm:hidden">
                {/* Mobile menu button*/}
                <Disclosure.Button className="inline-flex items-center justify-center p-2 rounded-md text-gray-400 hover:text-white hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white">
                  <span className="sr-only">Open main menu</span>
                  {open ? (
                    <XIcon className="block h-6 w-6" aria-hidden="true" />
                  ) : (
                    <MenuIcon className="block h-6 w-6" aria-hidden="true" />
                  )}
                </Disclosure.Button>
              </div>
              <div className="flex-1 flex items-center justify-center sm:items-stretch sm:justify-between">
                <div className="flex-shrink-0 flex items-center">
                  <Link
                    to="/"
                    className="cursor-pointer flex title-font font-medium items-center text-gray-900 mb-4 md:mb-0 rounded-sm"
                  >
                    <div className="flex justify-center items-end h-[50px]">
                      <img
                        className="w-[41px] h-[41px]"
                        src={logo}
                        alt="logo"
                      />
                    </div>
                    <span className="text-xl font-serif">Tribunals</span>
                  </Link>
                </div>
                <div className="hidden sm:block sm:ml-6">
                  <div className="flex space-x-4 py-3">
                    {navigation.map((item) => (
                      <Link
                        key={item.name}
                        to={item.href}
                        className={classNames(
                          item.current
                            ? "border-gold border-b-[1.5px]"
                            : "text-gray-900 hover:bg-gray-100 rounded",
                          "px-3 py-2 text-sm text-gray-900"
                        )}
                        aria-current={item.current ? "page" : undefined}
                      >
                        {item.name}
                      </Link>
                    ))}
                  </div>
                </div>
              </div>
              <div className="absolute inset-y-0 right-0 flex items-center pr-2 sm:static sm:inset-auto sm:ml-6 sm:pr-0">
                <Menu as="div" className="ml-3 relative">
                  <div>
                    <Menu.Button
                      className={`${
                        authenticated ? "border-none" : "bg-cadet"
                      } border flex text-sm rounded-full focus:outline-none focus:ring focus:ring-offset-1 focus:ring-offset-cadet focus:ring-white ease-in-out`}
                    >
                      {authenticated && (
                        <p
                          className="hover:text-gray-600 rounded
                          px-3 py-2 text-black font-serif text-base font-medium ease-in-out"
                        >
                          {user.name}
                        </p>
                      )}
                      <span className="sr-only">Open user menu</span>
                      <img
                        className="h-7 w-7 rounded-full self-center m-[1px] bg-white"
                        src={
                          udUser
                            ? "https://i.imgur.com/yZBav17.png"
                            : "https://i.imgur.com/PaecHIK.png"
                        }
                        alt=""
                      />
                    </Menu.Button>
                  </div>
                  <Transition
                    as={Fragment}
                    enter="transition ease-out duration-100"
                    enterFrom="transform opacity-0 scale-95"
                    enterTo="transform opacity-100 scale-100"
                    leave="transition ease-in duration-75"
                    leaveFrom="transform opacity-100 scale-100"
                    leaveTo="transform opacity-0 scale-95"
                  >
                    <Menu.Items className="origin-top-right z-10 absolute right-0 mt-2 w-48 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5 focus:outline-none">
                      <Menu.Item>
                        {({ active }) => (
                          <Link
                            to="#"
                            className={classNames(
                              active ? "bg-gray-100" : "",
                              "block px-4 py-2 text-sm text-gray-700"
                            )}
                          >
                            Your NFTs
                          </Link>
                        )}
                      </Menu.Item>
                      <Menu.Item>
                        {({ active }) => (
                          <div
                            className={classNames(
                              active ? "bg-gray-100" : "",
                              "block px-4 py-2 text-sm text-gray-700"
                            )}
                          >
                            {authenticated ? (
                              <p
                                className="block text-sm text-gray-700 cursor-pointer"
                                onClick={() => {
                                  signOut();
                                }}
                              >
                                Sign Out
                              </p>
                            ) : (
                              <>
                                <p className="block text-sm text-gray-700">
                                  {authenticated ? "Sign Out" : "Sign In:"}
                                </p>
                                <div className="flex w-full justify-left py-2">
                                  <button
                                    onClick={(e) => {
                                      e.stopPropagation();
                                      handleCBAuthenticate();
                                    }}
                                    className="flex-shrink-0 flex text-cadet hover:bg-blue-700 hover:bg-opacity-5 border-blue-700 border py-2 px-4 focus:outline-none rounded text-lg mt-10 sm:mt-0 mr-2"
                                  >
                                    <Coinbase className="w-6" />
                                  </button>
                                  <button
                                    onClick={(e) => {
                                      e.stopPropagation();
                                      handleAuthenticate();
                                    }}
                                    className="flex-shrink-0 flex text-cadet hover:bg-gold hover:bg-opacity-5 border-gold border py-2 px-4 focus:outline-none rounded text-lg mt-10 sm:mt-0 mx-2"
                                  >
                                    <Metamask className="w-[22px] self-center" />
                                  </button>
                                  {/* <button
                                    onClick={handleUAuthenticate}
                                    className="flex-shrink-0 flex text-cadet hover:bg-blue-600 hover:bg-opacity-5 border-blue-600 border py-2 px-4 focus:outline-none rounded text-lg mt-10 sm:mt-0 ml-2"
                                  >
                                    <Unstoppable className="w-6" />
                                  </button> */}
                                </div>
                              </>
                            )}
                          </div>
                        )}
                      </Menu.Item>
                    </Menu.Items>
                  </Transition>
                </Menu>
              </div>
            </div>
          </div>

          <Disclosure.Panel className="sm:hidden">
            <div className="px-2 pt-2 pb-3 space-y-1">
              {navigation.map((item) => (
                <Disclosure.Button
                  key={item.name}
                  as="a"
                  href={item.href}
                  className={classNames(
                    item.current
                      ? "bg-gray-900 text-white"
                      : "text-gray-300 hover:bg-gray-700 hover:text-white",
                    "block px-3 py-2 rounded-md text-base font-medium"
                  )}
                  aria-current={item.current ? "page" : undefined}
                >
                  {item.name}
                </Disclosure.Button>
              ))}
            </div>
          </Disclosure.Panel>
        </>
      )}
    </Disclosure>
  );
};

export default Navbar;
