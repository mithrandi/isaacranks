import 'babel-core/polyfill'
import React, {PropTypes as P} from 'react'
import {createStore} from 'redux'
import {Provider, connect} from 'react-redux'
import {Set} from 'immutable'
import {Button, ButtonGroup, ButtonToolbar} from 'react-bootstrap'


class Item extends React.Component {
  render() {
    const {index, item, norm} = this.props
    return (
      <tr>
        <td>{index + 1}</td>
        <td>{item.isaacId}</td>
        <td>{item.name}</td>
        <td>{item.description}</td>
        <td>{item.rating.toFixed(2)}</td>
        <td>{norm.toFixed(2)}</td>
        <td>{item.votes}</td>
      </tr>
    )
  }
}

class FilterButton extends React.Component {
  render() {
    const {name, pools, label, onToggle} = this.props
    const bsStyle = pools.has(name) ? 'success' : 'danger';
    return <Button bsStyle={bsStyle} onClick={() => onToggle(name)}>{label}</Button>
  }
}

const poolNames =
  [ ["ItemRoom", "Item"]
  , ["Shop", "Shop"]
  , ["BossRoom", "Boss"]
  , ["DevilRoom", "Devil"]
  , ["AngelRoom", "Angel"]
  , ["SecretRoom", "Secret"]
  , ["Library", "Library"]
  , ["GoldenChest", "Golden Chest"]
  , ["RedChest", "Red Chest"]
  , ["CurseRoom", "Curse"]
  , ["Beggar", "Beggar"]
  , ["DemonBeggar", "Demon Beggar"]
  , ["KeyBeggar", "Key Beggar"]
  ]

class Filters extends React.Component {
  render() {
    const {pools, onToggle} = this.props
    const buttons = poolNames.map(
      ([name, label]) =>
        <FilterButton name={name} key={name} label={label} pools={pools} onToggle={onToggle} />)
    return (
      <ButtonToolbar>
        <ButtonGroup>
          {buttons}
        </ButtonGroup>
      </ButtonToolbar>
    )
  }
}

class RanksTable extends React.Component {
  render() {
    const {minRating, maxRating, pools} = this.props
    const ratingRange = maxRating - minRating
    const items = this.props.items.map((item, index) => {
      const norm = (item.rating - minRating) / ratingRange * 1000
      if (Set(item.pools).intersect(pools).size > 0)
        return <Item key={item.isaacId} index={index} item={item} norm={norm} />
    })
    return (
      <div className="table-responsive">
        <table className="table table-condensed table-hover">
          <thead>
            <tr>
              <th>#</th>
              <th>Item ID</th>
              <th>Name</th>
              <th>Description</th>
              <th>Rating</th>
              <th>Normalized</th>
              <th>Votes</th>
            </tr>
          </thead>
          <tbody>
            {items}
          </tbody>
        </table>
      </div>
    )
  }
}

class Ranks extends React.Component {
  render() {
    if (this.props.loading || this.props.error) {
      const message = this.props.loading ? 'Loading...' : this.props.error
      return (
        <div className="jumbotron">
          <h1>Item ranks</h1>
          <p>{message}</p>
        </div>
      )
    }
    const {items, votesCast, meanVotes, minRating, maxRating} = this.props.ranks
    const dispatch = this.props.dispatch
    const onToggle = (name) => dispatch(togglePool(name))
    return (
      <div>
        <div className="jumbotron">
          <h1>Item ranks</h1>
          <p>{votesCast} votes total, mean of {meanVotes.toFixed(2)} per item.</p>
          <p>Normalized rating is normalized to range [0, 1000].</p>
        </div>
        <Filters pools={this.props.pools} onToggle={onToggle} />
        <RanksTable items={items} minRating={minRating} maxRating={maxRating} pools={this.props.pools} />
      </div>
    )
  }
}
Ranks.propTypes = (
  { loading: P.bool.isRequired
  , error: P.string
  , ranks: P.shape(
    { votesCast: P.number.isRequired
    , meanVotes: P.number.isRequired
    , minRating: P.number.isRequired
    , maxRating: P.number.isRequired
    , items: P.arrayOf(
      P.shape(
        { isaacId: P.number.isRequired
        , imageId: P.string.isRequired
        , name: P.string.isRequired
        , description: P.string.isRequired
        , wiki: P.string.isRequired
        , rating: P.number.isRequired
        , votes: P.number.isRequired
        , pools: P.arrayOf(P.string.isRequired).isRequired
        }))
    })
  })

const RanksC = connect((state) => state)(Ranks)


const LOAD_RANKS = 'LOAD_RANKS'
function loadRanks(data) {
  return { type: LOAD_RANKS
         , data: data
         }
}

const ERROR_FAIL = 'ERROR_FAIL'
function errorFail(e) {
  return { type: ERROR_FAIL
         , error: e
         }
}

const TOGGLE_POOL = 'TOGGLE_POOL'
function togglePool(name) {
  return { type: TOGGLE_POOL
         , name: name
         }
}

const initialState = (
  { loading: true
  , error: null
  , pools: Set.of('ItemRoom')
  })

const store = createStore((state = initialState, action) => {
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
        return Object.assign({}, state, { pools: pools.remove(action.name) })
      else
        return Object.assign({}, state, { pools: pools.add(action.name) })
  }
  return state
})

React.render(
  <Provider store={store}>
    {() => <RanksC />}
  </Provider>,
  document.getElementById('main')
)

fetch(
  '',
  { headers: new Headers({'Accept': 'application/json'})
  }).then((response) => {
    if (response.status != 200) {
      throw new Error(response.statusText)
    }
    return response.json()
  }).then((data) => {
    store.dispatch(loadRanks(data))
  }).catch((e) => {
    store.dispatch(errorFail(e))
  })
