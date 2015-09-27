import {Map, List} from 'immutable'

export const POOL = Symbol('FILTER_POOL')
export const TYPE = Symbol('FILTER_TYPE')

export default Map(
  [ [ TYPE
    , List(
      [ ['active', 'Active']
      , ['passive', 'Passive']
      , ['familiar', 'Familiar']
      ])
    ]
  , [ POOL
    , List(
      [ ['ItemRoom', 'Item']
      , ['Shop', 'Shop']
      , ['BossRoom', 'Boss']
      , ['DevilRoom', 'Devil']
      , ['AngelRoom', 'Angel']
      , ['SecretRoom', 'Secret']
      , ['Library', 'Library']
      , ['GoldenChest', 'Golden Chest']
      , ['RedChest', 'Red Chest']
      , ['CurseRoom', 'Curse']
      , ['Beggar', 'Beggar']
      , ['DemonBeggar', 'Demon Beggar']
      , ['KeyBeggar', 'Key Beggar']
      , ['MISC', 'Misc']
      ])
    ]
  ])
