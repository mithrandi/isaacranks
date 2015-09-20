import 'babel-core/polyfill'
import React from 'react'
import {Provider} from 'react-redux'
import Ranks from './containers/Ranks'
import configureStore from './store/configureStore'
import {loadRanks, errorFail} from './actions/Ranks'

const store = configureStore()

React.render(
  <Provider store={store}>
    {() => <Ranks />}
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
