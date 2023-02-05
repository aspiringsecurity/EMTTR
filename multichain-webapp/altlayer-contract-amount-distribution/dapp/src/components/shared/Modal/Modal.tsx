import React from "react"
import OutsideClickHandler from "react-outside-click-handler"

interface Props {
    children: React.Node
    title: string
    isOpen: boolean
    onClose: () => void
}

const Modal = ({ title, isOpen, onClose, children }: Props) => {
    return isOpen ? (
        <>
            <div className="justify-center items-center flex overflow-x-hidden overflow-y-auto fixed inset-0 z-50 outline-none focus:outline-none">
                <div className="relative w-auto my-6 mx-auto max-w-3xl">
                    {/*content*/}
                    <OutsideClickHandler onOutsideClick={onClose}>
                        <div className="border-0 rounded-lg shadow-lg relative flex flex-col w-full bg-zinc-800 outline-none focus:outline-none">
                            {/*header*/}
                            <div className="flex items-start justify-between p-5 border-b border-solid border-zinc-400 rounded-t">
                                <h3 className="text-3xl font-semibold">{title}</h3>
                                <button
                                    className="p-1 ml-auto bg-transparent border-0 text-gray-200 float-right text-3xl leading-none font-semibold outline-none focus:outline-none"
                                    onClick={() => onClose()}
                                >
                                    <span className="bg-transparent text-gray-200 h-6 w-6 text-2xl block outline-none focus:outline-none">
                                        ×
                                    </span>
                                </button>
                            </div>
                            <div className="relative p-6 flex-auto">{children}</div>
                        </div>
                    </OutsideClickHandler>
                </div>
            </div>
            <div className="opacity-25 fixed inset-0 z-40 bg-black"></div>
        </>
    ) : null
}

export default Modal
