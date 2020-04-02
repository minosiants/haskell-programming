-- Madness.hs

module Madness where

import Data.Monoid

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj = 
      e <> "! he said " <>
      adv <> " as he jumped into his car " <>
      noun <> " and drove off with his " <>
      adj <> " wife."

madlibbinbetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinbetter' e adv noun adj = mconcat 
    [e, 
    "! he said ", 
    adv, 
    " as he jumped into his car ",
    noun,
    " and drove off with his ",
    adj,
    " wife."]


