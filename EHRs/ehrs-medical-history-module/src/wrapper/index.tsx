import Routes from '../routes'
import Footer from './footer'
import Header from './header'
import './wrapper.scss'

const Wrapper = () => {
  return (
    <>
      <div className="wrapper">
        <Header />
        <div className="fake-header-div" />
        <div className="page-content">
          <Routes />
        </div>
        <Footer />
      </div>
      <div className="mobile">
        <img
          src="/assets/images/opdready-namelogo.png"
          alt="opdready-namelogo"
          width={90}
          height={'auto'}
        />
        <p className="desc">Please use a Desktop to view the app</p>
      </div>
    </>
  )
}

export default Wrapper
