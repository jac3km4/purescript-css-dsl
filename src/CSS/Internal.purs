module CSS.Internal where
import Prelude

import Data.Foldable (intercalate)
import Effect.Uncurried (EffectFn1)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Rule :: Type
foreign import data CSS :: Type
foreign import data Color :: Type
foreign import data Position :: Type
foreign import data Dimension :: Type
foreign import data Border :: Type
foreign import data Display :: Type
foreign import data FlexDirection :: Type
foreign import data FlexWrap :: Type
foreign import data Align :: Type
foreign import data Selector :: Type

instance semigroupCSS :: Semigroup CSS where
  append a b = unsafeCSS $ renderCSS a <> renderCSS b

instance monoidCSS :: Monoid CSS where
  mempty = unsafeCSS ""
  
instance semigroupSelector :: Semigroup Selector where
  append a b = unsafeSelector $ renderSelector a <> renderSelector b

instance monoidSelector :: Monoid Selector where
  mempty = unsafeSelector ""

foreign import unsafeAddCSSToDocument :: EffectFn1 String Unit

renderCSS :: CSS -> String
renderCSS = unsafeCoerce

unsafeCSS :: String -> CSS
unsafeCSS = unsafeCoerce

renderRule :: Rule -> String
renderRule = unsafeCoerce

renderRules :: Array Rule -> String
renderRules rules = intercalate ";" rules'
  where
    rules' :: Array String
    rules' = unsafeCoerce rules

unsafeRule :: String -> String -> Rule
unsafeRule name value = unsafeCoerce $ name <> ":" <> value

renderColor :: Color -> String
renderColor = unsafeCoerce

unsafeColor :: String -> Color
unsafeColor = unsafeCoerce

renderPosition :: Position -> String
renderPosition = unsafeCoerce

unsafePosition :: String -> Position
unsafePosition = unsafeCoerce

renderDimension :: Dimension -> String
renderDimension = unsafeCoerce

unsafeDimension :: String -> Dimension
unsafeDimension = unsafeCoerce

renderBorder :: Border -> String
renderBorder = unsafeCoerce

unsafeBorder :: String -> Border
unsafeBorder = unsafeCoerce

renderDisplay :: Display -> String
renderDisplay = unsafeCoerce

unsafeDisplay :: String -> Display
unsafeDisplay = unsafeCoerce

renderFlexDirection :: FlexDirection -> String
renderFlexDirection = unsafeCoerce

unsafeFlexDirection :: String -> FlexDirection
unsafeFlexDirection = unsafeCoerce

renderFlexWrap :: FlexWrap -> String
renderFlexWrap = unsafeCoerce

unsafeFlexWrap :: String -> FlexWrap
unsafeFlexWrap = unsafeCoerce

renderAlign :: Align -> String
renderAlign = unsafeCoerce

unsafeAlign :: String -> Align
unsafeAlign = unsafeCoerce

renderSelector :: Selector -> String
renderSelector = unsafeCoerce

unsafeSelector :: String -> Selector
unsafeSelector = unsafeCoerce
