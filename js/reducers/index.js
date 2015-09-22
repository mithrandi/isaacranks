import {combineReducers} from 'redux'
import {routerStateReducer} from 'redux-router'
import ranks from './ranks'

export default combineReducers(
  { router: routerStateReducer
  , ranks }
  )
