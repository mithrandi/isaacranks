import {LOAD_CHANGES_LOADING, LOAD_CHANGES_SUCCESS, LOAD_CHANGES_FAILURE} from '../constants/ActionTypes'
import {fromJS, Record} from 'immutable'

export const Changes = Record(
  { 'loading': false
  , 'error': null
  , 'changes': null
  })

export default function changes(state = new Changes(), action) {
  switch (action.type) {
    case LOAD_CHANGES_LOADING:
      return new Changes({'loading': true})
    case LOAD_CHANGES_SUCCESS:
      return new Changes({'changes': fromJS(action.data)})
    case LOAD_CHANGES_FAILURE:
      return new Changes({'error': action.error})
  }
  return state
}
