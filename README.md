# purescript-css-dsl
The main goal of this library is to implement an efficient and type-safe DSL to render stylesheets.
The intended usage is to expose your styles in a record and then access them from your `VDOM` (e.g. React) using typelevel strings that reference your stylesheet (names are checked to exist in the sheet).

# example
Stylesheet:
```purs
module UI.Styles where
import CSS
import Data.Foldable (fold)
import Prelude (($), (<<<))
import React.DOM.Props as P

styled = P.className <<< className sheet

sheet = 
  { header: root [ color $ rgb 100 0 0 ]
  , row: fold
      [ root [ display flex
             , flexDirection row
             ]
      , select (child "img") [ width $ px 10 ]
      ]
  }
```
Component usage:
```purs
h3 [ styled (Cls :: Cls "header") ] [ text name ]
```
You still need to add the stylesheet to the document before using it, there's a helper function:
```purs
CSS.embed $ CSS.renderSheet Styles.sheet
```

# notes

The DSL uses 'tagged' values, which are in fact just plain strings underneath, but it forces you to compose them in ways that produce valid CSS.

For instance, code `[ display flex, flexDirection row ]` produces a JavaScript array like: `[ "display:flex", "flex-direction":"row" ]`. An interesting consequence of this is that a good optimizing JavaScript compiler (e.g. Closure Compiler) can eliminate most of the DSL and convert your stylesheets to strings. 

