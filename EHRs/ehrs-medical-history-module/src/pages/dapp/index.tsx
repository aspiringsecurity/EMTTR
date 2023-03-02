import { ConnectKitButton } from 'connectkit'
import { BigNumber, Contract, ethers, Signer } from 'ethers'
import { useEffect, useState } from 'react'
import { useAccount, useSigner } from 'wagmi'
import { Button } from '../../components'
import { CONTRACT_ADDRESS, MedicalFormData, rem } from '../../utils'
import CONTRACTABI from '../../abi/contract-abi.json'
import './dapp.scss'
import * as Name from 'w3name'
import MedicalForm from './medicalform'
import { Web3Storage } from 'web3.storage'
import Modal from '../../components/modal'

const web3StorageClient = new Web3Storage({
  token:
    'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJkaWQ6ZXRocjoweDY2YTNFYzgzMGRjOUEwZjRiN0QwNjQ0MTA2RDk3MzYxNTUzMTZiRDAiLCJpc3MiOiJ3ZWIzLXN0b3JhZ2UiLCJpYXQiOjE2NzU2MTEzOTc4MzUsIm5hbWUiOiJ0ZXN0X21lZCJ9.MyH_YJh7npTIdPWdpEegdFvNnI0n6i5B6N2JAa1Pg1g',
})

const App = () => {
  const { isConnected, address: accountAddress } = useAccount()
  const { data: signer } = useSigner()

  const [appContract, setAppContract] = useState<Contract | null>(null)
  const [addressExists, setAddressExists] = useState(false)
  const [ipnsName, setIpnsName] = useState<number[]>([])
  const [latestRevision, setLatestRevision] = useState<Name.Revision>()
  const [prefilledFormData, setPreFilledFormData] = useState<MedicalFormData>({
    firstName: '',
    lastName: '',
    contact: '',
    sex: '',
    age: '',
    bloodGroup: '',
    height: '',
    weight: '',
    lifestyle: '',
    alcohol: '',
    smoking: '',
    allergies: '',
    accountAddress: '',
  })
  const [loader, setLoader] = useState({
    loading: false,
    message: '',
  })

  const checkProfileExists = async () => {
    try {
      setLoader({
        loading: true,
        message: 'Fetching Profile',
      })
      const data: Array<BigNumber> = await appContract!.getData(accountAddress)
      if (data.length) {
        setIpnsName(data.map((d) => d.toNumber()))
        const name = await Name.from(
          new Uint8Array(data.map((d) => d.toNumber())),
        )
        getLatestRevision(name)
      }
      setLoader({
        loading: false,
        message: '',
      })
    } catch (e) {
      setAddressExists(false)
      setLoader({
        loading: false,
        message: '',
      })
    }
  }

  const createProfile = async () => {
    const name = await Name.create()
    setLoader({
      loading: true,
      message: 'Creating Name',
    })
    const key = name.key.bytes
      .toString()
      .split(',')
      .map((s) => Number(s))
    await appContract!.setData(
      name.key.bytes
        .toString()
        .split(',')
        .map((s) => Number(s)),
    )
    setLoader({
      loading: false,
      message: '',
    })
    setIpnsName(key)
  }

  const onFormSubmit = async (val: MedicalFormData) => {
    setLoader({
      loading: true,
      message: 'Creating Metadata',
    })
    const storedName = await Name.from(new Uint8Array(ipnsName))
    const name = Name.parse(storedName.toString())
    const file = new Blob([JSON.stringify({ ...val, accountAddress })], {
      type: 'application/json',
    })
    const files = [new File([file], `${val.firstName}-medical-metadata.json`)]
    const fileCid = await web3StorageClient.put(files, {
      maxRetries: 3,
      wrapWithDirectory: false,
    })
    setLoader({
      ...loader,
      message: 'Storing File',
    })
    const value = `/ipfs/${fileCid}`
    try {
      const revision = await Name.resolve(name)
      if (revision.value) {
        const newRevision = await Name.increment(revision, value)
        await Name.publish(newRevision, storedName.key)
        setLoader({
          ...loader,
          message: 'File Stored',
        })
        setLatestRevision(newRevision)
      }
    } catch (e) {
      const revision = await Name.v0(storedName, value)
      await Name.publish(revision, storedName.key)
      setLoader({
        ...loader,
        message: 'File Stored',
      })
      setLatestRevision(revision)
    }
    setLoader({
      loading: false,
      message: '',
    })
  }

  const getLatestRevision = async (name: Name.Name) => {
    const revision = await Name.resolve(name)
    setLatestRevision(revision)
  }

  const retrieveStoredFormData = async () => {
    setLoader({
      loading: true,
      message: 'Getting latest Revision',
    })
    try {
      const response = await fetch(`https://ipfs.io${latestRevision?.value}`)
      const data = await response.json()
      setPreFilledFormData({ ...data, accountAddress })
    } catch (e) {
      console.log(e)
    }
    setLoader({
      loading: false,
      message: '',
    })
  }

  useEffect(() => {
    if (isConnected && signer) {
      const contract = new ethers.Contract(
        CONTRACT_ADDRESS,
        CONTRACTABI,
        signer as Signer,
      )
      setAppContract(contract)
    }
  }, [isConnected, signer])

  useEffect(() => {
    if (appContract) {
      checkProfileExists()
    }
  }, [appContract])

  useEffect(() => {
    if (ipnsName) {
      setAddressExists(true)
    }
  }, [ipnsName])

  useEffect(() => {
    if (latestRevision?.value) retrieveStoredFormData()
  }, [latestRevision])

  return (
    <>
      <div className="dapp">
        {!isConnected ? (
          <div className="not-connected">
            <h2 className="not-connected-title">Connect wallet to continue</h2>
            <ConnectKitButton.Custom>
              {({ show }) => {
                return <Button onClick={show}>Connect Wallet</Button>
              }}
            </ConnectKitButton.Custom>
          </div>
        ) : addressExists ? (
          <MedicalForm
            prefilledFormData={prefilledFormData}
            onSubmit={(val) => onFormSubmit(val)}
          />
        ) : (
          <div className="does-not-exist">
            <h3 className="does-not-exist-title">Create a Profile</h3>
            <p className="does-not-exist-desc">
              You don't have a profile created with us. This process creates an
              IPFS name and stores it on the contract to retrieve your details
              the next time you visit us. All you need to do is pay gas (which
              is a one time payment) to create the profile.
            </p>
            <Button onClick={createProfile}>Create Profile</Button>
          </div>
        )}
      </div>
      <Modal open={loader.loading}>
        <div
          style={{
            padding: `${rem(15)}`,
            borderRadius: `${rem(10)}`,
            background: `var(--white)`,
          }}
        >
          <p
            style={{
              fontFamily: 'GilroyMedium',
              fontSize: rem(20),
              lineHeight: '130%',
              color: 'var(--black)',
            }}
          >
            {loader.message}
          </p>
        </div>
      </Modal>
    </>
  )
}

export default App
