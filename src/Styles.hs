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
fptalksStyleW =  do htmlStyleW; topStyleW; formStyleW; msgStyleW; parStyleW

--------------------------------------------------------------------------------

{- HTML document styles. -}

htmlStyleW :: (Yesod site) => WidgetFor site ()
htmlStyleW =  toWidgetHead [cassius|
*
  padding: 0
  margin:  0

html
  background: white
  height:     100%
  width:      100%

html, div, header, p, pre
  display: block

body
  background: #fefefe
  min-height: 100vh
  text-align: left
  position:   relative
  color:      #111

body, input, input::placeholder, textarea, textarea::placeholder, .btn
  font: 400 17px/1.43 "PT Sans", "Segoe UI", "Roboto", "Oxygen-Sans", "Cantarell", "Helvetica Neue", "sans-serif"
  font-feature-settings: "kern" 1, "liga" 0
  letter-spacing: 0.0015em
|]

--------------------------------------------------------------------------------

{- Top containers styles. -}

topStyleW :: (Yesod site) => WidgetFor site ()
topStyleW =  toWidgetHead [cassius|
header
  border-bottom: 5px solid rgba(69, 59, 97, 0.5)
  background:    #5e5184
  text-align:    center
  font-size:     1.2em
  position:      fixed
  margin:        0 auto
  color:         white
  right:         0
  left:          0
  top:           0

article
  padding-top: 100px

footer
  position: absolute
  bottom:   0
  right:    0
  left:     0

article, footer
  max-width: 1450px
  margin:    0 auto

@media only screen and (max-width: 999px)
  header
    padding:  6px 0 4px 0
    overflow: hidden
  
  article, footer
    width: 75vw

@media only screen and (max-width: 1279px)
  article, footer
    width: 68vw

@media only screen and (min-width: 1280px)
  article, footer
    width: 60vw
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
  width:         40%

form div
  inline-size: 100%
  margin:      0 auto
  
  justify-content: center
  flex-direction:  column
  display:         flex

nav a
  color: white

input
  margin-bottom: 5px
  inline-size:   99%

input.user
  color: #5E5184

input[type=submit].link
  background-color: transparent
  text-align:       left
  border:           none

input, input::placeholder, .btn
  font-weight: bold
  text-align:  center

a
  text-decoration: none

a:active, a:hover, input[type=submit].link
  text-decoration: underline
|]

--------------------------------------------------------------------------------

{- Message styles. -}

msgStyleW :: (Yesod site) => WidgetFor site ()
msgStyleW =  toWidgetHead [cassius|
.message-box
  font-family: monospace
  white-space: pre
  overflow:    auto
  padding:     0.5em 0.5em
  
  border-radius: 0.3em
  border:        1px solid #ddd

textarea
  background: #f7f7f7
  overflow:   hidden
  outline:    none
  resize:     vertical
|]

--------------------------------------------------------------------------------

{- Paragraph styles. -}

parStyleW :: (Yesod site) => WidgetFor site ()
parStyleW =  toWidgetHead [cassius|
p, pre
  margin-inline-start: 0
  margin-block-start:  0
  margin-inline-end:   0
  margin-block-end:    0

ul
  list-style: none

li
  margin-bottom: 10px

li.message
  border-radius: 0.3em
  background:    #f7f7f7
  padding:       0.5em 0.5em
  border:        1px solid #ddd

p.message
  margin-block:  0
  white-space:   pre
  overflow:      visible

p.sender
  font-size: 70%

#messageForm
  grid-template-columns: 9fr 1fr
  grid-gap:              5px
  display:               grid
  margin:                1em 0 0 0

#messageFormSubmit
  height: 100%
|]

