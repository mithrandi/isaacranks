import {createStore, applyMiddleware, compose} from 'redux'
import {reduxReactRouter} from 'redux-router'
import {createHistory} from 'history'
import thunkMiddleware from 'redux-thunk'
import rootReducer from '../reducers'
import fetchMiddleware from '../middleware/fetch'

export default function configureStore(initialState) {
  return compose(
    applyMiddleware(
      thunkMiddleware,
      fetchMiddleware),
    reduxReactRouter({createHistory})
  )(createStore)(rootReducer, initialState)
}
