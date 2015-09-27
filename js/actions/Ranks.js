import * as A from '../constants/ActionTypes'
import {FETCH_DATA} from '../middleware/fetch'

export function loadRanks(version) {
  return (dispatch, getState) => {
    if (getState().ranks.hasIn(['ranks', version]))
      return null
    return dispatch(
      { [FETCH_DATA]:
        { method: 'GET'
        , uri: `/${version}/ranks`
        , started: A.LOAD_RANKS_LOADING
        , success: A.LOAD_RANKS_SUCCESS
        , failure: A.LOAD_RANKS_FAILURE
        }
      , version: version
      })
  }
}

export function resetRanks(version) {
  return { type: A.LOAD_RANKS_RESET
         , version: version
         }
}

export function togglePool(name) {
  return { type: A.TOGGLE_POOL
         , name: name
         }
}

export function allPools() {
  return { type: A.POOLS_ALL }
}

export function noPools() {
  return { type: A.POOLS_NONE }
}
