module Handler.Changes (getChangesR) where

import Import

getChangesR :: Handler TypedContent
getChangesR = selectRep $ do
  provideRep . defaultLayout $ do
    setTitle "Changelog"
    addScript (StaticR js_bundle_js)
    $(widgetFile "changes")
  provideJson changes
  where changes :: [(Text, [Text])]
        changes =
          [ ( "2019-12-23"
            , [ "The Boring update"
              , "Items from Booster Packs #4 and #5 have been added for Afterbirth+."
              , "The Ranks pages now load much more quickly."
              ]
            )
          , ( "2017-07-23"
            , [ "The Booster Pack #3 update"
              , "Items from Booster Packs #2 and #3 have been added for Afterbirth+."
              , "Fixed wiki links for all Afterbirth+ items (some were broken due to changes in page titles)."
              ]
            )
          , ( "2017-03-31"
            , [ "The Afterbirth+ update!"
              , "Afterbirth+ voting: the ranks are starting from scratch, as with Afterbirth."
              , "Items from the first booster pack are already included; future booster pack items will be added on without a ranking reset, as I do not expect these to significantly impact synergies (whereas Afterbirth and Afterbirth+ introduced significant changes to synergies)."
              , "New ranking algorithm that should perform better than the old ELO-based one. The new algorithm does not assign any kind of rating, so the columns relating to this have been removed from the Ranks page. The new algorithm has been retroactively applied to the Rebirth and Afterbirth rankings as well."
              , "Minor visual tweaks."
              , "Vote data export disabled due to limitations with the old export format; please contact me if you're interested in the raw vote data."
              , "Fixed the images for A Dollar, A Quarter, and Money = Power."
              ]
            )
          , ( "2016-01-24"
            , [ "The Afterbirth update!"
              , "Afterbirth voting: the ranks for all items are starting from zero, due to changes in existing items and other gameplay considerations."
              , "Rebirth voting: the old Rebirth rankings are still available for viewing and voting, should anyone be interested."
              , "Subreddit: This now exists at https://www.reddit.com/r/isaacranks/ — feel free to report bugs or discuss the site if you're into the Reddit thing."
              ]
            )
          , ( "2015-10-01"
            , [ "The \"It Lives!\" update!"
              , "Reworked the client-side code; a modern browser with JavaScript enabled is now required for all functionality, but the UI should be a lot more robust."
              , "Dump the vote data on a periodic basis; see the ranks page for a link to the latest dump. Feel free to contact me if you'd like to do something with the vote data that isn't satisfied by this dump."
              , "Added filtering by item pool and type to the ranks page."
              , "Added item icons and wiki links to the ranks page."
              , "Replaced image sprite (new version has uniform item sizes, and is smaller)."
              ]
            )
          , ( "2015-01-12"
            , [ "Implemented asynchronous voting; voting should now feel much faster, especially on higher-latency connections."
              , "Prevent accidentally voting multiple times in a row caused by holding down one of the key bindings."
              ]
            )
          , ( "2015-01-03"
            , [ "Item titles and images now link to the wiki."
              , "Added keyboard shortcuts for voting."
              ]
            )
          , ( "2015-01-02"
            , [ "Made the ranks table narrower."
              , "Tweaked the voting page display for medium size displays (eg. tablets)."
              , "Added this changelog."
              ]
            )
          ]
