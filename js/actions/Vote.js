import {VOTE_BALLOT, LOAD_BALLOT, LOAD_BALLOT_FAILURE, RESET_BALLOT, REROLL_BALLOT} from '../constants/ActionTypes'
import {FETCH_DATA} from '../middleware/fetch'
import {List} from 'immutable'

export function loadBallot(version) {
  return (dispatch, getState) => {
    const v = getState().voting.get(version)
    if (!v || v.get('voting') || v.get('ballots').size >= 2)
      return null
    return dispatch(
      { [FETCH_DATA]:
        { method: 'GET'
        , uri: `/${version}/vote`
        , started: null
        , success: LOAD_BALLOT
        , failure: LOAD_BALLOT_FAILURE
        }
      , version: version
      })
  }
}

export function voteFor(version, ballot) {
  const fd = new FormData()
  fd.append('ballot', ballot)
  fd.append('fancy', true)
  return (
    { [FETCH_DATA]:
      { method: 'POST'
      , uri: `/${version}/vote`
      , data: fd
      , started: VOTE_BALLOT
      , success: LOAD_BALLOT
      , failure: LOAD_BALLOT_FAILURE
      }
    , version: version
    , ballot: ballot
    , vote: true
    })
}

export function reset() {
  return {type: RESET_BALLOT}
}

export function reroll(version) {
  return {type: REROLL_BALLOT, version: version}
}
