import { createStore, combineReducers, compose, applyMiddleware } from 'redux'
import { persistReducer } from 'redux-persist'
import storage from 'redux-persist/lib/storage'
import window from 'global'
import auth from './auth'
import notes from './notes'
import users from './users'
import thunk from 'redux-thunk'

const reducer = combineReducers({ auth, notes, users })

const persistConfig = { storage, key: 'root', blacklist: [ 'notes' ] }
const persistedReducer = persistReducer(persistConfig, reducer)

const enhancers = compose(
  applyMiddleware(thunk),
  window.__REDUX_DEVTOOLS_EXTENSION__ ? window.__REDUX_DEVTOOLS_EXTENSION__() : compose
)

export const store = createStore(
  persistedReducer,
  enhancers
)

