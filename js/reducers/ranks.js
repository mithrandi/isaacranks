import * as A from '../constants/ActionTypes'
import {Map, Record, List, Set, fromJS} from 'immutable'
import pools from '../constants/pools'

const allPools = pools.map(([name, desc]) => name).toSet()

export const RanksState = Record(
  { ranks: Map()
  , pools: allPools
  })

const RanksR = Record(
  { items: List()
  , votesCast: 0
  , meanVotes: 0
  , minRating: 0
  , maxRating: 0
  , latestDump: null
  })

export default function ranks(state = new RanksState(), action) {
  switch (action.type) {
    case A.LOAD_RANKS_LOADING:
      return state.setIn(['ranks', action.version], fromJS({loading: true}))
    case A.LOAD_RANKS_SUCCESS:
      return state.setIn(['ranks', action.version], new RanksR(action.data))
    case A.LOAD_RANKS_FAILURE:
      return state.setIn(['ranks', action.version], fromJS({error: action.error.toString()}))
    case A.LOAD_RANKS_RESET:
      return state.deleteIn(['ranks', action.version])
    case A.TOGGLE_POOL:
      return state.updateIn(
        ['pools', action.version],
        pools => pools.has(action.name) ? pools.remove(action.name) : pools.add(action.name))
    case A.POOLS_ALL:
      return state.set('pools', allPools)
    case A.POOLS_NONE:
      return state.set('pools', Set())
  }
  return state
}
