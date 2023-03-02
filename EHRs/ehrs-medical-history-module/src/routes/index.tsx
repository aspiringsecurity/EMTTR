import { Routes as Switch, Route, Navigate } from 'react-router-dom'
import { Landing, Dapp } from '../pages'

const Routes = () => {
  return (
    <Switch>
      <Route path="/" element={<Navigate to="/home" />} />
      <Route path="/home" element={<Landing />} />
      <Route path="/dapp" element={<Dapp />} />
    </Switch>
  )
}

export default Routes
