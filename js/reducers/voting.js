import * as A from '../constants/ActionTypes'
import {fromJS, Map, List} from 'immutable'

const initialState = fromJS(
  { ballots: {}
  })

export default function voting(state = initialState, action){
  switch (action.type) {
    case A.LOAD_BALLOT:
      return state.updateIn(
        ['ballots', action.version],
        List(),
        (ballots) => ballots.push(fromJS(action.data)))
  }
  return state
}
