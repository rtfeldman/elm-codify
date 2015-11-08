module Codify where

import Json.Decode exposing (..)
import Dict exposing (Dict)

type Type
    = IntType Int
    | FloatType Float
    | StringType String
    | ListType (List Type)
    | RecordType String (Dict String Type)
    | NullType
    | UndefinedType


decodeType : Decoder Type
decodeType =
    oneOf
        [ map IntType int
        , map FloatType float
        , map StringType string
        , map ListType (list decodeType)
        , map (RecordType "") (dict decodeType)
        , null NullType
        , succeed UndefinedType
        ]


fromJson : String -> Result String Type
fromJson json =
    json
        |> decodeString decodeType


fromValue : Value -> Result String Type
fromValue value =
    value
        |> decodeValue decodeType
