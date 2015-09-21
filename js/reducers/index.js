import {combineReducers} from 'redux'
import {routerStateReducer} from 'redux-router'
import ranks from './ranks'

const rootReducer = combineReducers(
  { router: routerStateReducer
  , ranks }
  )

export default rootReducer
