import 'babel/polyfill'
import React, {Component} from 'react'
import ReactDOM from 'react-dom'
import {Provider} from 'react-redux'
import {Route} from 'react-router'
import {ReduxRouter} from 'redux-router'
import App from './containers/App'
import Vote from './containers/Vote'
import Ranks from './containers/Ranks'
import configureStore from './store/configureStore'

class Root extends Component {
  render() {
    return (
      <Provider store={this.props.store}>
        <ReduxRouter>
          <Route path="/" component={App}>
            <Route path=":version/vote" component={Vote} />
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

const store = configureStore()
ReactDOM.render(<Root store={store} />, container)
