import {LOAD_RANKS_LOADING, LOAD_RANKS_SUCCESS, LOAD_RANKS_FAILURE, LOAD_RANKS_RESET, TOGGLE_FILTER, FILTERS_ALL, FILTERS_NONE} from '../constants/ActionTypes'
import {FETCH_DATA} from '../middleware/fetch'

export function loadRanks(version) {
  return (dispatch, getState) => {
    if (getState().ranks.hasIn(['ranks', version]))
      return null
    return dispatch(
      { [FETCH_DATA]:
        { method: 'GET'
        , uri: `/${version}/ranks`
        , started: LOAD_RANKS_LOADING
        , success: LOAD_RANKS_SUCCESS
        , failure: LOAD_RANKS_FAILURE
        }
      , version: version
      })
  }
}

export function resetRanks(version) {
  return { type: LOAD_RANKS_RESET
         , version: version
         }
}

export function toggleFilter(filterType, name) {
  return { type: TOGGLE_FILTER
         , filterType: filterType
         , name: name
         }
}

export function allFilters() {
  return { type: FILTERS_ALL }
}

export function noFilters() {
  return { type: FILTERS_NONE }
}
