import {combineReducers} from 'redux'
import {routerStateReducer} from 'redux-router'
import ranks from './ranks'
import voting from './voting'
import changes from './changes'

export default combineReducers(
  { router: routerStateReducer
  , ranks
  , voting
  , changes
  })
