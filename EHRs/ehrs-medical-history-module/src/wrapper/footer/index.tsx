import './footer.scss'

const Footer = () => {
  return (
    <div className="footer">
      <div className="footer-row-1">
        <img
          src="assets/images/opdready-namelogo.png"
          alt="namelogo"
          className="footer-logo"
        />
        <div className="infrastructure-section">
          <h4 className="infrastructure-label">Infrastructure</h4>
          <div className="infrastructure">
            <img
              src="/assets/svgs/polygon.svg"
              alt="polygon"
              width={50}
              height={50}
            />
            <img
              src="/assets/svgs/ipfs.svg"
              alt="ipfs"
              width={50}
              height={50}
            />
          </div>
        </div>
      </div>
      <a
        className="copyright-text"
        href="https://mumbai.polygonscan.com/address/0xc904c95d0cbf50342fd92c8ab4764819f5641808"
        target={'_blank'}
        rel="noreferrer noopener"
      >
        View Contract
      </a>
      <p className="copyright-text">© 2023 OpdReady</p>
    </div>
  )
}

export default Footer
