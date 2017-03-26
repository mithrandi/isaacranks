import React from 'react'
import ReactDOM from 'react-dom'
import {Provider} from 'react-redux'
import {Route} from 'react-router-dom'
import {ConnectedRouter} from 'react-router-redux'
import createHistory from 'history/createBrowserHistory'

import App from './containers/App'
import Vote from './containers/Vote'
import Ranks from './containers/Ranks'
import Donate from './containers/Donate'
import Changes from './containers/Changes'
import configureStore from './store/configureStore'

class Root extends React.Component {
    render() {
        return (
            <Provider store={this.props.store}>
              <ConnectedRouter history={this.props.history}>
                <App>
                  <Route path="/:version/vote" component={Vote} />
                  <Route path="/:version/ranks" component={Ranks} />
                  <Route path="/donate" component={Donate} />
                  <Route path="/changes" component={Changes} />
                </App>
              </ConnectedRouter>
            </Provider>
        )
    }
}

function startup() {
    while (document.body.firstChild)
        document.body.removeChild(document.body.firstChild)
    const container = document.createElement('div')
    document.body.appendChild(container)

    const history = createHistory()
    const store = configureStore(history)
    ReactDOM.render(<Root store={store} history={history} />, container)
}

startup()
