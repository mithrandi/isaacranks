import React, {PropTypes as P} from 'react'

export default class Item extends React.Component {
  static propTypes =
  { index: P.number.isRequired
  , item: P.object.isRequired
  , norm: P.number.isRequired
  }

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
