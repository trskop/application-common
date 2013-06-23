{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module:       $HEADER$
-- Description:  Abstraction over application mode implementation.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (FlexibleContexts, FlexibleInstances,
--               MultiParamTypeClasses)
--
-- Abstraction over application mode implementation.
module Main.ApplicationMode.Class
    (
    -- * ApplicationMode
      ApplicationMode(..)
    , updateConfiguration
    , updateConfiguration'
    , changeAction
    , getConfiguration
    , getAction
    , runApplication
    , whenAction
    )
    where

import Control.Applicative (Applicative(..))
import Control.Arrow (Arrow((&&&)), (>>>))
import Data.Default.Class (Default(def))
import Data.Functor.FlipT (FlipT(..), flipmap)
import Data.Semigroup (Semigroup(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Control.Comonad (Comonad(..))


-- {{{ ApplicationMode --------------------------------------------------------

-- | While this might look complex at first, it turns out quite simple to
-- actually define what application does and also understand it later on. The
-- core principle is that everything is broken down to simple pieces.
--
-- * There has to exist default value for action and also for configuration.
--   Default value of action is what the application will do by default. And
--   for configuration it defines what imput parameters and values are used
--   by default, when no command line options are provided.
--
-- * From the default values of action and configuration it has to be able to
--   derive default value for 'ApplicationMode' instance. So if there is any
--   additional information stored inside it, then it has to also have some
--   kind of default value. This helps to simplify mental model.
--
-- * Action has to be instance of 'Semigroup'. This defines transitions between
--   actions and if there is any information shared between different actions.
--   E.g. for action that handles command line argument errors it might join
--   error messages. So how action changes is defined only on one place and
--   it's done in a way that doesn't rely on current value of configuration.
--
-- * Functor instances allow usage of 'fmap' and 'flipmap' to modify
--   configuration and action, respectively. It also allows to use action and
--   configuration stacks.
--
-- * Applicative and monad instance simplify definition of some generic
--   combinators that update both, action and configuration.
--
-- * Comonad instances allows to extract configuration and action values. They
--   also make it possible to define generic combinators that modify only
--   configuration or action, but have access to both while doing so.
class
    ( Default a
    , Default c
    , Default (f a c)
    , Semigroup a
    , Functor (f a)
    , Functor (FlipT f c)
    , Applicative (f a)
    , Comonad (f a)
    , Comonad (FlipT f c)
    , Monad (f a)
    ) => ApplicationMode f a c
  where
    optErrors :: [String] -> Endo (f a c)

    optError :: String -> Endo (f a c)
    optError msg = optErrors [msg]

-- | Modify configuration wrapped in application mode.
updateConfiguration :: ApplicationMode f a c => (c -> c) -> Endo (f a c)
updateConfiguration = Endo . fmap

-- | Change action wrapped in application mode, using its 'Semigroup' instance.
changeAction :: ApplicationMode f a c => a -> Endo (f a c)
changeAction a = Endo $ flipmap (<> a)

-- | Similar to 'updateConfiguration', but function that does the update will
-- get action as an argument as well as configuration.
updateConfiguration' :: ApplicationMode f a c => (a -> c -> c) -> Endo (f a c)
updateConfiguration' f =
    Endo . extend $ getAction &&& getConfiguration >>> uncurry f

-- | Extract configuration from application mode.
getConfiguration :: ApplicationMode f a c => f a c -> c
getConfiguration = extract

-- | Extract action from application mode.
getAction :: ApplicationMode f a c => f a c -> a
getAction = extract . FlipT

-- | If predicate holds return endomorphism passed as second argument,
-- otherwise return 'mempty'.  Similar to @Control.Monad.when@.
whenAction
    :: ApplicationMode f a c
    => (a -> Bool)
    -- ^ Predicate on action.  If it returns @True@ then endomorphism passed
    -- as a second argument will be returned as a result, otherwise it returns
    -- 'mempty'.
    -> Endo (f a c)
    -- ^ Emdomorphism that will be returned if predicate passed as first
    -- argument holds.
    -> Endo (f a c)
whenAction p (Endo f) = Endo $ \ am -> if p $ getAction am then f am else am

-- | Runs @'Endo' (f a c)@ with default values and passes action and
-- configuration to the function passed as a first argument. If you want to
-- read configuration file or modify action/configuration depending on some
-- other condition then do it in here.
--
-- If the function mentioned above returns a @'Just' (a, c)@, then the pair
-- @(a, c)@ is used to run @'Endo' (f a c)@ again, but instead of default
-- values these new values are used. Now the function passed as a second
-- argument is evaluated using new value of action and configuration.
runApplication
    :: (ApplicationMode f a c, Applicative m, Functor m, Monad m)
    => Endo (f a c)
    -> (a -> c -> m (Maybe (a, c)))
    -> (a -> c -> m r)
    -> m r
runApplication endo f g = uncurry f defMode >>= uncurry g . fromMaybe defMode
  where
    defMode = getAction &&& getConfiguration $ appEndo endo def

-- }}} ApplicationMode --------------------------------------------------------
