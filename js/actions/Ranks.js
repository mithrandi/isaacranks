import {LOAD_RANKS, ERROR_FAIL, TOGGLE_POOL, POOLS_ALL, POOLS_NONE} from '../constants/ActionTypes'

export function loadRanks(data) {
  return { type: LOAD_RANKS
         , data: data
         }
}

export function errorFail(e) {
  return { type: ERROR_FAIL
         , error: e
         }
}

export function togglePool(name) {
  return { type: TOGGLE_POOL
         , name: name
         }
}

export function allPools() {
  return { type: POOLS_ALL }
}

export function noPools() {
  return { type: POOLS_NONE }
}
