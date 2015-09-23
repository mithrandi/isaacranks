import {VOTE_BALLOT, LOAD_BALLOT, LOAD_BALLOT_FAILURE, RESET_BALLOT} from '../constants/ActionTypes'
import {fromJS, Map, List} from 'immutable'

const initialState = fromJS(
  { 'rebirth':
    { ballots: []
    , voting: false
    }
  , 'afterbirth':
    { ballots: []
    , voting: false
    }
  })

export default function voting(state = initialState, action){
  switch (action.type) {
    case LOAD_BALLOT:
      if (action.vote)
        state = state.updateIn(
          [action.version, 'voting'],
          voting => voting - 1)
      return state.updateIn(
        [action.version, 'ballots'],
        ballots => ballots.push(fromJS(action.data)))

    case VOTE_BALLOT:
      state = state.updateIn(
          [action.version, 'voting'],
          voting => voting + 1)
      return state.updateIn(
        [action.version, 'ballots'],
        ballots => ballots.filter(b =>
          b.get('ballotLeft') !== action.ballot
          && b.get('ballotRight') !== action.ballot))

    case LOAD_BALLOT_FAILURE:
      const message = action.error.toString()
      return fromJS({'error': message})

    case RESET_BALLOT:
      return initialState
  }
  return state
}
