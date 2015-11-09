module Codify where

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Result exposing (Result)

type Type
    = IntType
    | FloatType
    | StringType
    | ListType Type
    | MaybeType Type
    | RecordType (Dict String Type)
    | TupleType (List Type)
    | AliasType String


unitType : Type
unitType =
    TupleType []


aliases : Dict String Type
aliases =
    Dict.empty


decodeRecordTypeHelp
    :  (String, Type)
    -> Result String (List ( String, Type ))
    -> Result String (List ( String, Type ))
decodeRecordTypeHelp pair result =
    Result.map (\list -> pair :: list) result


decodeRecordTypePairs : Decoder Type -> List (String, Type) -> Result String Type
decodeRecordTypePairs decodeType pairs =
    List.foldl decodeRecordTypeHelp (Ok []) pairs
        |> Result.map Dict.fromList
        |> Result.map RecordType


decodeRecordType : Decoder Type -> Decoder Type
decodeRecordType decodeType =
    customDecoder (keyValuePairs decodeType) (decodeRecordTypePairs decodeType)


decodeListType : Decoder Type -> Decoder Type
decodeListType decodeType =
    customDecoder (list decodeType)
        <| \typeList ->
            case typeList of
                [] ->
                    -- Assume when they provide [] that it's going to end up being a
                    -- List of some sort. We can't infer what kind, so we default to
                    -- List Unit. We could assume they want Unit itself, but that's
                    -- almost never true in practice, so List is a better guess.
                    Ok (ListType unitType)

                firstType :: otherTypes ->
                    if List.all ((==) firstType) otherTypes then
                        -- If the JS array is homogenous, assume they want a List.
                        Ok (ListType firstType)
                    else
                        -- If the JS array is heterogenous, they get a Tuple.
                        Err "Heterogeneous JS arrays decode as Tuples, not Lists."


decodeType : Decoder Type
decodeType =
    oneOf
        [ decodePrimitive
        , decodeListType decodeTypeB -- should be able to just recursively "decodeType" here
        , map TupleType (list decodeTypeB) -- should be able to just recursively "decodeType" here
        , decodeRecordType decodeTypeB
        ]


-- HACK to work around some recursive decoder bug
decodeTypeB : Decoder Type
decodeTypeB =
    oneOf
        [ decodePrimitive
        , decodeListType decodeTypeC
        , map TupleType (list decodeTypeC)
        , decodeRecordType decodeTypeC
        ]

-- HACK to work around some recursive decoder bug
decodeTypeC : Decoder Type
decodeTypeC =
    oneOf
        [ decodePrimitive
        , decodeListType decodeTypeD
        , map TupleType (list decodeTypeD)
        , decodeRecordType decodeTypeD
        ]

-- HACK to work around some recursive decoder bug
decodeTypeD : Decoder Type
decodeTypeD =
    oneOf
        [ decodePrimitive
        , decodeListType decodeTypeE
        , map TupleType (list decodeTypeE)
        , decodeRecordType decodeTypeE
        ]

-- HACK to work around some recursive decoder bug
decodeTypeE : Decoder Type
decodeTypeE =
    oneOf
        [ decodePrimitive
        , decodeListType decodeTypeF
        , map TupleType (list decodeTypeF)
        , decodeRecordType decodeTypeF
        ]

-- HACK to work around some recursive decoder bug
decodeTypeF : Decoder Type
decodeTypeF =
    oneOf
        [ decodePrimitive
        , decodeListType decodeTypeF
        , map TupleType (list decodeTypeF)
        , decodeRecordType decodeTypeF
        ]


decodePrimitive : Decoder Type
decodePrimitive =
    oneOf
        [ map (\_ -> IntType) int
        , map (\_ -> FloatType) float
        , map (\_ -> StringType) string
        , null (MaybeType unitType)
        ]


fromJson : String -> Result String Type
fromJson json =
    decodeString decodeType json


fromValue : Value -> Result String Type
fromValue value =
    decodeValue decodeType value
