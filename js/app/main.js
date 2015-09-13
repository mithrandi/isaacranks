import 'babel-core/polyfill'
import React, {PropTypes as P} from 'react'
import {createStore} from 'redux'
import {Provider, connect} from 'react-redux'


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
        <td>{JSON.stringify(item.pools)}</td>
      </tr>
    )
  }
}

class RanksTable extends React.Component {
  render() {
    const {minRating, maxRating} = this.props
    const ratingRange = maxRating - minRating
    const items = this.props.items.map((item, index) => {
      const norm = (item.rating - minRating) / ratingRange * 1000
      return <Item key={item.isaacId} index={index} item={item} norm={norm} />
    })
    return (
      <div className="table-responsive">
        <table className="table table-condensed table-hover">
          <tr>
            <th>#</th>
            <th>Item ID</th>
            <th>Name</th>
            <th>Description</th>
            <th>Rating</th>
            <th>Normalized</th>
            <th>Votes</th>
            <th>Pools</th>
          </tr>
          {items}
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
    return (
      <div>
        <div className="jumbotron">
          <h1>Item ranks</h1>
          <p>{votesCast} votes total, mean of {meanVotes.toFixed(2)} per item.</p>
          <p>Normalized rating is normalized to range [0, 1000].</p>
        </div>
        <RanksTable items={items} minRating={minRating} maxRating={maxRating} />
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

const initialState = (
  { loading: true
  , error: null
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
