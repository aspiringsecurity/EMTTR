import { useNavigate } from 'react-router-dom'
import { Button } from '../../components'
import './landing.scss'

const Landing = () => {
  const navigate = useNavigate()

  return (
    <div className="landing">
      <div className="main-section">
        <div className="main-section-col-1">
          <h1 className="main-section-col-1-title">
            Store your medical history{' '}
            <span style={{ color: 'var(--primary)', fontFamily: 'inherit' }}>
              securely
            </span>
            .
          </h1>
          <p className="main-section-col-1-desc">
            OpdReady helps you avail your medical history online so that you can
            avoid carrying files on your next doctor's appointment.
          </p>
          <Button onClick={() => navigate('/dapp')}>Get Started</Button>
        </div>
        <img
          src="/assets/images/opdready-landingimage.png"
          alt="opdready-landingimage"
          className="main-section-col-2"
        />
      </div>
      <div className="how-it-works-container">
        <h2 className="how-it-works-container-title">How it works</h2>
        <div className="how-it-works-section">
          <img
            src="/assets/images/polygon-wallet.png"
            alt="polygon-wallet"
            className="how-it-works-section-col-1"
          />
          <div className="how-it-works-section-col-2">
            <h3 className="how-it-works-col-2-title">Connect wallet</h3>
            <p className="how-it-works-col-2-desc">
              Connect your wallet with the website. At the moment, the website
              runs on the mumbai polygon testnet. To add the mumbai network to
              your wallet,{' '}
              <a
                href="https://wiki.polygon.technology/docs/develop/metamask/config-polygon-on-metamask#add-the-polygon-network-manually"
                target={'_blank'}
                rel={'noreferrer noopener'}
                style={{
                  fontFamily: 'GilroyMedium',
                  textDecoration: 'none',
                  color: 'inherit',
                }}
              >
                click here
              </a>
              .
            </p>
          </div>
        </div>
        <div className="how-it-works-section">
          <img
            src="/assets/images/pay-gas.png"
            alt="pay-gas"
            className="how-it-works-section-col-1"
          />
          <div className="how-it-works-section-col-2">
            <h3 className="how-it-works-col-2-title">Pay gas</h3>
            <p className="how-it-works-col-2-desc">
              You will be asked to pay some gas to store your IPNS key. The
              IPNS key is created by us and then stored on the smart contract
              on the mumbai network against your address. The same key is used
              to retrieve the details of the user that are stored on IPFS. This
              is a one time payment.
            </p>
          </div>
        </div>
        <div className="how-it-works-section">
          <img
            src="/assets/images/fill-form.png"
            alt="fill-form"
            className="how-it-works-section-col-1"
          />
          <div className="how-it-works-section-col-2">
            <h3 className="how-it-works-col-2-title">Fill the form</h3>
            <p className="how-it-works-col-2-desc">
              Fill out your details which is a relatively simple process.
            </p>
          </div>
        </div>
        <div className="how-it-works-section">
          <img
            src="/assets/images/ipfs-image.png"
            alt="ipfs-image"
            className="how-it-works-section-col-1"
          />
          <div className="how-it-works-section-col-2">
            <h3 className="how-it-works-col-2-title">Submit the form</h3>
            <p className="how-it-works-col-2-desc">
              On submission, the details are stored as a metadata file for the
              user on the IPFS network. The same details are retrieved back when
              the user reconnects the wallet. You can also edit the information
              and save it. The IPNS key is already stored against your address
              and will act as nothing but a simple pointer to your metadata. The
              new IPFS hash generated is stored against it.
            </p>
          </div>
        </div>
      </div>
      <div className="characteristics-section">
        <div className="characteristics-list">
          <div className="characteristic">
            <img
              src="/assets/images/opdready-secure.png"
              alt="secure"
              className="characteristic-image"
            />
            <h3 className="characteristic-title">Secure</h3>
            <p className="characteristic-desc">
              Data is stored securely on the smart contract and can only be
              viewed by either the user or the authorized addresses.
            </p>
          </div>
          <div className="characteristic">
            <img
              src="/assets/images/opdready-timesaving.png"
              alt="timesaving"
              className="characteristic-image"
            />
            <h3 className="characteristic-title">Time saving</h3>
            <p className="characteristic-desc">
              All you have to do is carry your phone on your next appointment.
              It'll save time as the doctors can now avoid asking repetitive
              questions.
            </p>
          </div>
          <div className="characteristic">
            <img
              src="/assets/images/opdready-hasslefree.png"
              alt="hasslefree"
              className="characteristic-image"
            />
            <h3 className="characteristic-title">Hassle free</h3>
            <p className="characteristic-desc">
              Just store your data once and forget about it. Can be easily
              edited as well.
            </p>
          </div>
        </div>
      </div>
    </div>
  )
}

export default Landing
