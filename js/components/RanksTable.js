import React, {PropTypes as P} from 'react'
import {Set} from 'immutable'
import IP from 'react-immutable-proptypes'
import Item from './Item'

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

RanksTable.propTypes =
{ minRating: P.number.isRequired
, maxRating: P.number.isRequired
, pools: IP.set.isRequired
, items: P.arrayOf(P.object).isRequired
}

export default RanksTable
