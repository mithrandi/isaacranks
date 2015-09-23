import * as A from '../constants/ActionTypes'
import {FETCH_DATA} from '../middleware/fetch'
import {List} from 'immutable'

export function loadBallot(version) {
  return (dispatch, getState) => {
    if (getState().voting.getIn(['ballots', version], List()).size >= 2)
      return null
    return dispatch(
      { [FETCH_DATA]:
        { method: 'GET'
        , uri: `/${version}/vote`
        , started: null
        , success: A.LOAD_BALLOT
        , failure: A.LOAD_BALLOT_FAILURE
        }
      , version: version
      })
  }
}
