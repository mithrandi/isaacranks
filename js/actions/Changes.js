import {LOAD_CHANGES_LOADING, LOAD_CHANGES_SUCCESS, LOAD_CHANGES_FAILURE} from '../constants/ActionTypes'
import {FETCH_DATA} from '../middleware/fetch'

export function loadChanges() {
  return (dispatch, getState) => {
    const c = getState().changes
    if (c.loading || c.changes)
      return null
    return dispatch(
      { [FETCH_DATA]:
        { method: 'GET'
        , uri: `/changes`
        , started: LOAD_CHANGES_LOADING
        , success: LOAD_CHANGES_SUCCESS
        , failure: LOAD_CHANGES_FAILURE
        }
      })
  }
}
