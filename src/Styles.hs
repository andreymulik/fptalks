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
fptalksStyleW =  do htmlW; topW; formW; parW; accountW; msgW

--------------------------------------------------------------------------------

{- HTML document styles. -}

htmlW :: (Yesod site) => WidgetFor site ()
htmlW =  toWidgetHead [cassius|
*
  padding: 0
  margin:  0

html
  background: white
  min-width:  350px
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

topW :: (Yesod site) => WidgetFor site ()
topW =  toWidgetHead [cassius|
header
  border-bottom: 5px solid rgba(69, 59, 97, 0.5)
  background:    #5E5184
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

formW :: (Yesod site) => WidgetFor site ()
formW =  toWidgetHead [cassius|
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

.link
  background-color: transparent
  text-align:       left
  border:           none

input.link.user
  text-align: center

input, input::placeholder, .btn
  font-weight: bold
  text-align:  center

a
  text-decoration: none

a:active, a:hover, .link
  text-decoration: underline

div.column
  grid-template-column: 1fr
  display:              grid

input.name
  font-weight: normal
  border:      none
|]

--------------------------------------------------------------------------------

{- Message styles. -}

msgW :: (Yesod site) => WidgetFor site ()
msgW =  toWidgetHead [cassius|
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

.user
  color: #5E5184

.small
  font-size: 70%

#messageForm
  grid-template-columns: 9fr 1fr
  grid-gap:              5px
  display:               grid
  margin:                1em 0 0 0

#messageFormSubmit
  height: 100%

.trash
  background: url("/static/trash.png") no-repeat top left
  border:     none
  height:     20px
  width:      20px
  float:      right
|]

--------------------------------------------------------------------------------

{- Account styles. -}

accountW :: (Yesod site) => WidgetFor site ()
accountW =  toWidgetHead [cassius|
#accountBox
  text-decoration: underline
  font-weight:     bold
  text-align:      center
  min-width:       250px
  max-width:       80px
  color:           #5E5184
  width:           50%
  float:           left

#userIcon
  text-align: center
  max-height: 250px
  min-height: 80px
  max-width:  250px
  min-width:  80px
  float:      bottom

#userContacts
  float: right
|]

--------------------------------------------------------------------------------

{- Paragraph styles. -}

parW :: (Yesod site) => WidgetFor site ()
parW =  toWidgetHead [cassius|
p, pre
  margin-inline-start: 0
  margin-block-start:  0
  margin-inline-end:   0
  margin-block-end:    0
|]

