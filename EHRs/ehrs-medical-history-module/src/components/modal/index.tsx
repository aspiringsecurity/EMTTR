import { CSSProperties, ReactNode } from 'react'
import './modal.scss'

interface ModalProps {
  open: boolean
  children: ReactNode
  modalStyle?: CSSProperties
  modalContentStyle?: CSSProperties
}

const Modal = ({
  children,
  open,
  modalContentStyle,
  modalStyle,
}: ModalProps) => {

  return (
    <div className={`modal-container ${open ? 'show' : ''}`} style={{ ...modalStyle }}>
      <div
        className="modal-content-style"
        style={{ ...modalContentStyle }}
      >
        {children}
      </div>
    </div>
  )
}

export default Modal
