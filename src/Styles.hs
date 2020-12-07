{-# LANGUAGE QuasiQuotes #-}

{- |
    Module      :  Styles
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Styles" provides CSS templates (widgets) for site pages.
-}
module Styles ( fptalksStyleW ) where

import Yesod.Core

default ()

--------------------------------------------------------------------------------

-- | Default styles.
fptalksStyleW :: (Yesod site) => WidgetFor site ()
fptalksStyleW =  do clearStyleW; pageStyleW; formStyleW; boxStyleW

--------------------------------------------------------------------------------

{- Clear styles. -}

clearStyleW :: (Yesod site) => WidgetFor site ()
clearStyleW =  toWidgetHead [cassius|
*
  padding: 0
  margin:  0

html
  background: white
  height:     100%
  width:      100%

html, div
  display: block

body
  background: #fefefe
  min-height: 100vh
  text-align: left
  position:   relative
  color:      #111

p, pre
  margin-inline-start: 0
  margin-block-start:  1em
  margin-inline-end:   0
  margin-block-end:    1em
  margin-top:          0.7rem
  display:             block

body
  font-feature-settings: "kern" 1, "liga" 0
  letter-spacing: 0.0015rem

body, input, input::placeholder, .btn
  font: 400 17px/1.43 "PT Sans", "Segoe UI", "Roboto", "Oxygen-Sans", "Cantarell", "Helvetica Neue", "sans-serif"
|]

--------------------------------------------------------------------------------

{- Top-livel page styles. -}

pageStyleW :: (Yesod site) => WidgetFor site ()
pageStyleW =  toWidgetHead [cassius|
#page-header
  border-bottom: 5px solid rgba(69, 59, 97, 0.5)
  background:    #5E5184
  text-align:    center
  font-size:     1.2em
  position:      relative
  margin:        0 auto
  color:         white

@media only screen and (max-width: 999px)
  #page-header
    padding:  6px 0 4px 0
    overflow: hidden

#page-content
  margin:  2em 0 0 0
  padding: 0 3rem
|]

--------------------------------------------------------------------------------

{- Form styles. -}

formStyleW :: (Yesod site) => WidgetFor site ()
formStyleW =  toWidgetHead [cassius|
.auth
  border-radius: 0.3em
  border:        1px solid #ddd
  
  background:    #f7f7f7
  min-width:     350px
  max-width:     500px
  padding:       10px
  margin:        20% auto
  width:         30%

form div
  inline-size: 100%
  margin:      0 auto
  
  justify-content: center
  flex-direction:  column
  display:         flex

input
  margin-bottom: 5px
  inline-size:   99%

input[type=submit]
  inline-size: 100%

input, input::placeholder, .btn
  font-weight: bold
  text-align:  center
|]

--------------------------------------------------------------------------------

{- Message box. -}

boxStyleW :: (Yesod site) => WidgetFor site ()
boxStyleW =  toWidgetHead [cassius|
.message-box
  font-family: monospace
  white-space: pre
  background:  #f7f7f7
  overflow:    auto
  padding:     0.5rem 1rem
  margin:      1em 0 0 0
  
  border-radius: 0.3em
  border:        1px solid #ddd
|]


