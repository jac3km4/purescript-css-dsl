module CSS.DSL where
import Prelude

import Data.Foldable (intercalate)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Row (class Cons)
import CSS.Internal

data Cls (sym :: Symbol) = Cls

newtype Spec = Spec (Selector -> CSS) 

instance semigroupSpec :: Semigroup Spec where
  append (Spec fa) (Spec fb) = Spec \selector ->
    fa selector <> fb selector

instance monoidSpec :: Monoid Spec where
  mempty = Spec $ const mempty

clazz :: String -> Selector
clazz name = unsafeSelector $ "." <> name

child :: String -> Selector
child name = unsafeSelector $ " " <> name

hover :: Selector
hover = unsafeSelector ":hover"

className :: ∀ r r' a l
           . (IsSymbol l)
          => Cons l a r' r
          => Record r
          -> Cls l
          -> String
className rec _ = reflectSymbol $ SProxy :: SProxy l

data RenderCSS = RenderCSS

instance renderCSSRecord :: (IsSymbol sym) =>
  FoldingWithIndex RenderCSS (SProxy sym) CSS Spec CSS where
  foldingWithIndex RenderCSS prop acc (Spec run) =
    acc <> run (clazz (reflectSymbol prop))

renderSheet :: ∀ r. HFoldlWithIndex RenderCSS CSS { | r } CSS => { | r } -> CSS
renderSheet = hfoldlWithIndex RenderCSS (unsafeCSS mempty)

embed :: CSS -> Effect Unit
embed = runEffectFn1 unsafeAddCSSToDocument <<< renderCSS

select :: Selector -> Array Rule -> Spec
select sub rls = Spec \selector ->
  unsafeCSS $ renderSelector (selector <> sub) <> "{" <> renderRules rls <> "}"

root :: Array Rule -> Spec
root = select mempty

backgroundColor :: Color -> Rule
backgroundColor = unsafeRule "background-color" <<< renderColor

color :: Color -> Rule
color = unsafeRule "color" <<< renderColor

rgb :: Int -> Int -> Int -> Color
rgb r g b = unsafeColor $ "rgb(" <> intercalate "," (map show [r, g, b]) <> ")"

position :: Position -> Rule
position = unsafeRule "position" <<< renderPosition 

absolute :: Position
absolute = unsafePosition "absolute"

relative :: Position
relative = unsafePosition "relative"

fixed :: Position
fixed = unsafePosition "fixed"

width :: Dimension -> Rule
width = unsafeRule "width" <<< renderDimension

height :: Dimension -> Rule
height = unsafeRule "height" <<< renderDimension

top :: Dimension -> Rule
top = unsafeRule "top" <<< renderDimension

left :: Dimension -> Rule
left = unsafeRule "left" <<< renderDimension

right :: Dimension -> Rule
right = unsafeRule "right" <<< renderDimension

bottom :: Dimension -> Rule
bottom = unsafeRule "bottom" <<< renderDimension

fontSize :: Dimension -> Rule
fontSize = unsafeRule "font-size" <<< renderDimension

percent :: Int -> Dimension
percent num = unsafeDimension $ show num <> "%"

px :: Int -> Dimension
px num = unsafeDimension $ show num <> "px"

em :: Int -> Dimension
em num = unsafeDimension $ show num <> "em"

display :: Display -> Rule
display = unsafeRule "display" <<< renderDisplay

flexDirection :: FlexDirection -> Rule
flexDirection = unsafeRule "flex-direction" <<< renderFlexDirection

flexWrap :: FlexWrap -> Rule
flexWrap = unsafeRule "flex-wrap" <<< renderFlexWrap

justifyContent :: Align -> Rule
justifyContent = unsafeRule "justify-content" <<< renderAlign

justifyItems :: Align -> Rule
justifyItems = unsafeRule "justify-items" <<< renderAlign

alignContent :: Align -> Rule
alignContent = unsafeRule "align-content" <<< renderAlign

alignItems :: Align -> Rule
alignItems = unsafeRule "align-items" <<< renderAlign

flex :: Display
flex = unsafeDisplay "flex"

block :: Display
block = unsafeDisplay "block"

inlineBlock :: Display
inlineBlock = unsafeDisplay "inline-block"

row :: FlexDirection
row = unsafeFlexDirection "row"

rowReverse :: FlexDirection
rowReverse = unsafeFlexDirection "row-reverse"

column :: FlexDirection
column = unsafeFlexDirection "column"

columnReverse :: FlexDirection
columnReverse = unsafeFlexDirection "columnReverse"

wrap :: FlexWrap
wrap = unsafeFlexWrap "wrap"

nowrap :: FlexWrap
nowrap = unsafeFlexWrap "nowrap"

wrapReverse :: FlexWrap
wrapReverse = unsafeFlexWrap "wrap-reverse"

center :: Align
center = unsafeAlign "center"

leftAlign :: Align
leftAlign = unsafeAlign "left"

rightAlign :: Align
rightAlign = unsafeAlign "right"

start :: Align
start = unsafeAlign "start"

end :: Align
end = unsafeAlign "end"

textAlign :: Align -> Rule
textAlign = unsafeRule "text-align" <<< renderAlign

borderRadius :: Dimension -> Dimension -> Dimension -> Dimension -> Rule
borderRadius a b c d = unsafeRule "border-radius" (intercalate " " (map renderDimension [a, b, c, d]))

padding :: Dimension -> Dimension -> Dimension -> Dimension -> Rule
padding a b c d = unsafeRule "padding" (intercalate " " (map renderDimension [a, b, c, d]))

margin :: Dimension -> Dimension -> Dimension -> Dimension -> Rule
margin a b c d = unsafeRule "margin" (intercalate " " (map renderDimension [a, b, c, d]))

