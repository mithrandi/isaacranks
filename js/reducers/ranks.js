import * as A from '../constants/ActionTypes'
import {Set} from 'immutable'
import poolNames from '../constants/pools'

const initialState = (
  { ranks: {}
  , pools: Set(poolNames.map(([name]) => name))
  })

export default function ranks(state = initialState, action){
  switch (action.type) {
    case A.LOAD_RANKS_LOADING:
      return Object.assign(
        {}, state,
        { ranks: Object.assign(
          {}, state.ranks, {[action.version]: {loading: true}})
        })
    case A.LOAD_RANKS_SUCCESS:
      return Object.assign(
        {}, state,
        { ranks: Object.assign(
          {}, state.ranks, {[action.version]: action.data})
        })
    case A.LOAD_RANKS_FAILURE:
      return Object.assign(
        {}, state,
        { ranks: Object.assign(
          {}, state.ranks, {[action.version]: {error: action.error.toString()}})
        })
    case A.TOGGLE_POOL:
      const pools = state.pools
      if (pools.has(action.name))
        return Object.assign(
          {}, state, { pools: pools.remove(action.name) })
      return Object.assign(
        {}, state, { pools: pools.add(action.name) })
    case A.POOLS_ALL:
      return Object.assign(
        {}, state,
        { pools: Set(poolNames.map(([name]) => name)) })
    case A.POOLS_NONE:
      return Object.assign(
        {}, state, { pools: Set() })
  }
  return state
}
