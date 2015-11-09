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
        [ decodePrimitive
        , map ListType (list decodeTypeB) -- should be able to just recursively "decodeType" here
        , map (RecordType "") (dict decodeTypeB)
        ]

-- HACK to work around some recursive decoder bug
decodeTypeB : Decoder Type
decodeTypeB =
    oneOf
        [ decodePrimitive
        , map ListType (list decodeTypeC)
        , map (RecordType "") (dict decodeTypeC)
        ]

-- HACK to work around some recursive decoder bug
decodeTypeC : Decoder Type
decodeTypeC =
    oneOf
        [ decodePrimitive
        , map ListType (list decodeTypeD)
        , map (RecordType "") (dict decodeTypeD)
        ]

-- HACK to work around some recursive decoder bug
decodeTypeD : Decoder Type
decodeTypeD =
    oneOf
        [ decodePrimitive
        , map ListType (list decodeTypeE)
        , map (RecordType "") (dict decodeTypeE)
        ]

-- HACK to work around some recursive decoder bug
decodeTypeE : Decoder Type
decodeTypeE =
    oneOf
        [ decodePrimitive
        , map ListType (list decodeTypeF)
        , map (RecordType "") (dict decodeTypeF)
        ]

-- HACK to work around some recursive decoder bug
decodeTypeF : Decoder Type
decodeTypeF =
    oneOf
        [ decodePrimitive
        , map ListType (list decodeTypeF)
        , map (RecordType "") (dict decodeTypeF)
        ]


decodePrimitive : Decoder Type
decodePrimitive =
    oneOf
        [ map IntType int
        , map FloatType float
        , map StringType string
        , null NullType
        ]


fromJson : String -> Result String Type
fromJson json =
    decodeString decodeType json


fromValue : Value -> Result String Type
fromValue value =
    decodeValue decodeType value
