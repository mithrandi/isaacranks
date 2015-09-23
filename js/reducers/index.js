import {combineReducers} from 'redux'
import {routerStateReducer} from 'redux-router'
import ranks from './ranks'
import voting from './voting'

export default combineReducers(
  { router: routerStateReducer
  , ranks
  , voting
  })
