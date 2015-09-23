export const FETCH_DATA = Symbol('FETCH_DATA')

export default function fetchMiddleware(/*store*/) {
  return next => action => {
    const request = action[FETCH_DATA]
    if (request === undefined)
      return next(action)

    const {method, uri, data, started, success, failure} = request
    const actionWith = (props) => Object.assign({}, action, props)
    if (started != null)
      next(actionWith({type: started}))
    fetch(
      uri,
      { method: method
      , headers: new Headers({'Accept': 'application/json'})
      , body: data
      }).then((response) => {
        if (response.status != 200) {
          throw new Error(response.statusText)
        }
        return response.json()
      }).then((data) => {
        next(actionWith({type: success, data: data}))
      }).catch((e) => {
        next(actionWith({type: failure, error: e}))
      })
  }
}
