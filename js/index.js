import 'babel/polyfill'
import React, {Component, PropTypes as P} from 'react'
import ReactDOM from 'react-dom'
import {Provider} from 'react-redux'
import {Route, Link} from 'react-router'
import {ReduxRouter} from 'redux-router'
import App from './containers/App'
import Ranks from './containers/Ranks'
import configureStore from './store/configureStore'
import {loadRanks, errorFail} from './actions/Ranks'

const store = configureStore()

class Root extends Component {
  render() {
    return (
      <Provider store={store}>
        <ReduxRouter>
          <Route path="/" component={App}>
            <Route path=":version/ranks" component={Ranks} />
          </Route>
        </ReduxRouter>
      </Provider>
      )
  }
}

while (document.body.firstChild)
  document.body.removeChild(document.body.firstChild)
const container = document.createElement('div')
document.body.appendChild(container)
ReactDOM.render(<Root />, container)

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
