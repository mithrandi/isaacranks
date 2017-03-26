import {combineReducers} from 'redux'
import {routerReducer} from 'react-router-redux'
import ranks from './ranks'
import voting from './voting'
import changes from './changes'

export default combineReducers(
    { router: routerReducer
    , ranks
    , voting
    , changes
    })
