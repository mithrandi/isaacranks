import {LOAD_RANKS, ERROR_FAIL, TOGGLE_POOL, POOLS_ALL, POOLS_NONE} from '../constants/ActionTypes'
import {Set} from 'immutable'
import poolNames from '../constants/pools'

const initialState = (
  { loading: true
  , error: null
  , pools: Set(poolNames.map(([name, label]) => name))
  })

export default function ranks(state = initialState, action){
  switch (action.type) {
    case LOAD_RANKS:
      return Object.assign(
        {}, state,
        { ranks: action.data
        , loading: false
        , error: null
        })
    case ERROR_FAIL:
      return Object.assign(
        {}, state,
        { loading: false
        , error: action.error.toString()
        })
    case TOGGLE_POOL:
      const pools = state.pools
      if (pools.has(action.name))
        return Object.assign(
          {}, state, { pools: pools.remove(action.name) })
      else
        return Object.assign(
          {}, state, { pools: pools.add(action.name) })
    case POOLS_ALL:
      return Object.assign(
        {}, state,
        { pools: Set(poolNames.map(([name, label]) => name)) })
    case POOLS_NONE:
      return Object.assign(
        {}, state, { pools: Set() })
  }
  return state
}
