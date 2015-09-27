import {Map, Record, List, Set, fromJS} from 'immutable'
import * as A from '../constants/ActionTypes'
import filters from '../constants/filters'


const allFilters = filters.map(
  values => values.map(([name, desc]) => name).toSet())
const noFilters = filters.map(
  values => Set())


export const RanksState = Record(
  { ranks: Map()
  , filters: allFilters
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
    case A.TOGGLE_FILTER:
      return state.updateIn(
        ['filters', action.filterType],
        filters => filters.has(action.name) ? filters.remove(action.name) : filters.add(action.name))
    case A.FILTERS_ALL:
      return state.set('filters', allFilters)
    case A.FILTERS_NONE:
      return state.set('filters', noFilters)
  }
  return state
}
