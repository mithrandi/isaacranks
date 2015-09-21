import {createStore} from 'redux'
import {reduxReactRouter} from 'redux-router'
import createHistory from 'history/lib/createBrowserHistory'
import rootReducer from '../reducers'

export default function configureStore(initialState) {
  return reduxReactRouter({createHistory})(createStore)(rootReducer, initialState)
}
